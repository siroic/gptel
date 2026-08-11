;;; gptel-fork-test.el --- Characterization tests for orga-fork features -*- lexical-binding: t; -*-

;;; Commentary:

;; CHARACTERIZATION tests for orga-fork-only features of gptel.
;;
;; These tests pin the CURRENT observable behaviour of features that exist
;; only in the orga fork of karthink/gptel.  They are the acceptance
;; criteria for the fork-retirement work (Phases 2-3): any change that
;; retires or upstreams a fork feature must keep these green, or the
;; behaviour change must be deliberate and the test updated consciously.
;;
;; They are descriptive, not prescriptive: where current behaviour is
;; arguably wrong, the test documents what the code does today.
;;
;; Batch-safe: no network access, no real LLM calls.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gptel)
(require 'gptel-context)
(require 'gptel-anthropic)


;;;; 1. gptel--validate-tool-args
;;
;; NOTE (deviation from a naive expectation): this function does NOT signal
;; an error.  It characterizes a return-string contract, not a signal: it
;; returns nil when the args are valid, and an error *string* (meant to be
;; fed back to the LLM) when required arguments are missing.  Hence no
;; `should-error' anywhere below.

(defun gptel-fork-test--tool (&rest args)
  "Make a gptel-tool via the internal constructor (no global registration)."
  (apply #'gptel--make-tool-internal args))

(defconst gptel-fork-test--args
  '((:name "location" :type string :description "d")
    (:name "unit" :type string :description "d" :optional t)))

(ert-deftest gptel-fork-validate-tool-args-happy ()
  "All required arguments present -> nil (valid)."
  (let ((tool (gptel-fork-test--tool
               :name "test_tool" :description "d"
               :function #'ignore :args gptel-fork-test--args)))
    (should-not (gptel--validate-tool-args
                 tool (list :location "Paris" :unit "C")))))

(ert-deftest gptel-fork-validate-tool-args-missing ()
  "Missing required argument -> error STRING naming tool and argument."
  (let* ((tool (gptel-fork-test--tool
                :name "test_tool" :description "d"
                :function #'ignore :args gptel-fork-test--args))
         (err (gptel--validate-tool-args tool (list :unit "C"))))
    (should (stringp err))
    (should (string-match-p (regexp-quote "test_tool") err))
    (should (string-match-p (regexp-quote "location") err))))

(ert-deftest gptel-fork-validate-tool-args-optional ()
  "Omitting an :optional argument is fine -> nil."
  (let ((tool (gptel-fork-test--tool
               :name "test_tool" :description "d"
               :function #'ignore :args gptel-fork-test--args)))
    (should-not (gptel--validate-tool-args tool (list :location "Paris")))))

(ert-deftest gptel-fork-validate-tool-args-explicit-nil ()
  "Explicit nil value counts as present (`plist-member' semantics) -> nil."
  (let ((tool (gptel-fork-test--tool
               :name "test_tool" :description "d"
               :function #'ignore :args gptel-fork-test--args)))
    (should-not (gptel--validate-tool-args tool (list :location nil)))))


;;;; 2. gptel--kill-tool-processes

(ert-deftest gptel-fork-kill-tool-processes ()
  "Live tool processes are killed, their buffers killed, slot cleared."
  (skip-unless (executable-find "sleep"))
  (let* ((kill-buffer-query-functions nil) ; batch safety: no "running process?" prompt
         (buf (generate-new-buffer " *gptel-fork-test-proc*"))
         (proc (start-process "gptel-fork-test-sleep" buf "sleep" "100"))
         (info (list :tool-processes (list proc))))
    (unwind-protect
        (progn
          (should (process-live-p proc))
          (gptel--kill-tool-processes info)
          (accept-process-output proc 0.1)
          (sleep-for 0.1)
          (should-not (process-live-p proc))
          (should-not (buffer-live-p buf))
          (should-not (plist-get info :tool-processes)))
      (when (process-live-p proc)
        (set-process-sentinel proc #'ignore)
        (delete-process proc))
      (when (buffer-live-p buf) (kill-buffer buf)))))


;;;; 3. gptel-abort

(ert-deftest gptel-fork-abort-records-reason-and-cause ()
  "`gptel-abort' records :abort-reason/:abort-cause, calls abort-fn, -> ABRT."
  (let* ((buf (generate-new-buffer " *gptel-fork-test-abort*"))
         (called nil)
         (info (list :buffer buf))
         (fsm (gptel-make-fsm :state 'TYPE :table nil :handlers nil :info info))
         (gptel--request-alist
          (list (cons 'fake-proc (cons fsm (lambda () (setq called t)))))))
    (unwind-protect
        (progn
          (gptel-abort buf 'system "test cause")
          (should (eq (plist-get info :abort-reason) 'system))
          (should (equal (plist-get info :abort-cause) "test cause"))
          (should called)
          (should (eq (gptel-fsm-state fsm) 'ABRT)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest gptel-fork-abort-defaults-to-user ()
  "Without REASON, the abort reason defaults to `user'; no :abort-cause."
  (let* ((buf (generate-new-buffer " *gptel-fork-test-abort2*"))
         (info (list :buffer buf))
         (fsm (gptel-make-fsm :state 'TYPE :table nil :handlers nil :info info))
         (gptel--request-alist
          (list (cons 'fake-proc (cons fsm #'ignore)))))
    (unwind-protect
        (progn
          (gptel-abort buf)
          (should (eq (plist-get info :abort-reason) 'user))
          (should-not (plist-member info :abort-cause)))
      (when (buffer-live-p buf) (kill-buffer buf)))))


;;;; 4. gptel--current-fsm / gptel-current-info

(ert-deftest gptel-fork-current-fsm-bound-during-handler-dispatch ()
  "`gptel--current-fsm' is bound while state handlers run, nil outside."
  (should-not gptel--current-fsm)
  (should-not (gptel-current-info))
  (let* ((ran nil) (seen-fsm 'unset) (seen-info 'unset)
         (info (list :marker 'known))
         (fsm (gptel-make-fsm
               :state 'INIT :table nil :info info
               :handlers (list (list 'TEST
                                     (lambda (_m)
                                       (setq ran t
                                             seen-fsm gptel--current-fsm
                                             seen-info (gptel-current-info))))))))
    (gptel--fsm-transition fsm 'TEST)
    (should ran)
    (should (eq seen-fsm fsm))
    ;; NOTE: the transition pushes onto the info plist's :history, which
    ;; replaces the head cons of the plist; so the handler's info is `eq'
    ;; to the fsm's *current* info, not necessarily to the original object.
    (should (eq seen-info (gptel-fsm-info fsm)))
    (should (eq (plist-get seen-info :marker) 'known))
    (should (equal (plist-get seen-info :history) '(INIT)))
    (should (eq (gptel-fsm-state fsm) 'TEST)))
  (should-not gptel--current-fsm))


;;;; 5. gptel--current-tool-call

(ert-deftest gptel-fork-current-tool-call-bound-during-tool-invocation ()
  "`gptel--current-tool-call' is bound to the tool-call plist during the call."
  (should-not gptel--current-tool-call)
  (let* ((buf (generate-new-buffer " *gptel-fork-test-tool*"))
         (captured nil)
         (gptel-confirm-tool-calls nil)
         (tool (gptel-fork-test--tool
                :name "test_tool" :description "d"
                :args '((:name "location" :type string :description "d"))
                :function (lambda (_location)
                            (setq captured (copy-sequence gptel--current-tool-call))
                            "ok")))
         (tool-call (list :name "test_tool" :args (list :location "Paris")
                          :id "call_42"))
         (info (list :backend 'stub-backend :buffer buf
                     :tools (list tool) :tool-use (list tool-call)))
         (fsm (gptel-make-fsm :state 'TOOL :table nil :handlers nil :info info)))
    (unwind-protect
        (progn
          (gptel--handle-tool-use fsm)
          (should captured)
          (should (equal (plist-get captured :id) "call_42"))
          (should (equal (plist-get captured :name) "test_tool")))
      (when (buffer-live-p buf) (kill-buffer buf))))
  (should-not gptel--current-tool-call))


;;;; 6. Anthropic thinking blocks -> info :reasoning

(defun gptel-fork-test--anthropic-backend ()
  (gptel--make-anthropic :name "test" :host "example.com"
                         :endpoint "/v1/messages" :models nil))

(ert-deftest gptel-fork-anthropic-empty-thinking-no-reasoning ()
  "An empty thinking block does not set :reasoning at all."
  (let* ((backend (gptel-fork-test--anthropic-backend))
         (info (list :data 'ignored))
         (response (list :content (vector (list :type "thinking" :thinking "")
                                          (list :type "text" :text "hello"))))
         (out (gptel--parse-response backend response info)))
    (should (equal out "hello"))
    (should-not (plist-member info :reasoning))))

(ert-deftest gptel-fork-anthropic-nonempty-thinking-sets-reasoning ()
  "A non-empty thinking block is accumulated into info :reasoning."
  (let* ((backend (gptel-fork-test--anthropic-backend))
         (info (list :data 'ignored))
         (response (list :content (vector (list :type "thinking" :thinking "let me think")
                                          (list :type "text" :text "hello"))))
         (out (gptel--parse-response backend response info)))
    (should (equal out "hello"))
    (should (equal (plist-get info :reasoning) "let me think"))))


;;;; 7. gptel-context-prefer-buffer

(ert-deftest gptel-fork-context-prefer-buffer ()
  "When non-nil, unsaved buffer contents win over on-disk contents."
  (let* ((kill-buffer-query-functions nil)
         (file (make-temp-file "gptel-fork-test-" nil ".txt" "disk contents\n"))
         (buf nil))
    (unwind-protect
        (progn
          (setq buf (find-file-noselect file))
          (with-current-buffer buf
            (goto-char (point-max))
            (insert "buffer only line\n")
            (should (buffer-modified-p)))
          (let ((gptel-context-prefer-buffer t))
            (should (string-match-p
                     (regexp-quote "buffer only line")
                     (with-temp-buffer
                       (gptel-context--insert-file-string file)
                       (buffer-string)))))
          (let* ((gptel-context-prefer-buffer nil)
                 (str (with-temp-buffer
                        (gptel-context--insert-file-string file)
                        (buffer-string))))
            (should (string-match-p (regexp-quote "disk contents") str))
            (should-not (string-match-p (regexp-quote "buffer only line") str))))
      (when (buffer-live-p buf)
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf))
      (ignore-errors (delete-file file)))))


;;;; 8. read-only text property stripping in gptel--with-buffer-copy

(ert-deftest gptel-fork-with-buffer-copy-strips-read-only ()
  "The prompt copy has `read-only' properties stripped, so it stays editable."
  (let ((src (generate-new-buffer " *gptel-fork-test-src*"))
        (beg nil) (end nil))
    (unwind-protect
        (progn
          (with-current-buffer src
            (let ((inhibit-read-only t))
              (insert "hello")
              (put-text-property (point-min) (point-max) 'read-only t))
            (setq beg (point-min) end (point-max)))
          (let ((copy (gptel--with-buffer-copy src beg end
                        (should-not (get-text-property (point-min) 'read-only))
                        (goto-char (point-max))
                        ;; Must not signal "Text is read-only":
                        (insert " world")
                        (should (equal (buffer-string) "hello world"))
                        (current-buffer))))
            (when (buffer-live-p copy)
              (with-current-buffer copy (set-buffer-modified-p nil))
              (kill-buffer copy))))
      (when (buffer-live-p src)
        (with-current-buffer src (set-buffer-modified-p nil))
        (kill-buffer src)))))


;;;; 9. Reasoning hooks

(ert-deftest gptel-fork-reasoning-hooks-stream ()
  "Streaming reasoning runs pre- then post-reasoning hooks."
  (let* ((buf (generate-new-buffer " *gptel-fork-test-reason*"))
         (order nil))
    (unwind-protect
        (let* ((pos (with-current-buffer buf
                      (fundamental-mode)
                      (insert "prefix\n")
                      (copy-marker (point-max))))
               (info (list :include-reasoning t :position pos :buffer buf))
               (gptel-pre-reasoning-hook (list (lambda () (push 'pre order))))
               (gptel-post-reasoning-hook (list (lambda () (push 'post order)))))
          (gptel--display-reasoning-stream "thinking..." info)
          (gptel--display-reasoning-stream t info)
          (should (equal (nreverse order) '(pre post))))
      (when (buffer-live-p buf)
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf)))))

(ert-deftest gptel-fork-reasoning-hooks-non-streaming ()
  "Non-streaming reasoning insertion runs pre- then post-reasoning hooks."
  (let* ((buf (generate-new-buffer " *gptel-fork-test-reason2*"))
         (order nil))
    (unwind-protect
        (let* ((pos (with-current-buffer buf
                      (fundamental-mode)
                      (insert "prefix\n")
                      (copy-marker (point-max))))
               (info (list :include-reasoning t :position pos :buffer buf))
               (gptel-pre-reasoning-hook (list (lambda () (push 'pre order))))
               (gptel-post-reasoning-hook (list (lambda () (push 'post order)))))
          (gptel--insert-response '(reasoning . "thinking...") info)
          (should (equal (nreverse order) '(pre post))))
      (when (buffer-live-p buf)
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf)))))


;;;; 10. :id in tool-call hook plists

(ert-deftest gptel-fork-pre-tool-call-hook-receives-id ()
  "`gptel-pre-tool-call-functions' receives a plist including the :id."
  (let ((buf (generate-new-buffer " *gptel-fork-test-pre-tool*"))
        (captured nil))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq-local gptel-pre-tool-call-functions
                        (list (lambda (pl) (setq captured (copy-sequence pl)) nil))))
          (let* ((tool-call (list :name "test_tool" :args (list :a 1) :id "call_42"))
                 (info (list :buffer buf :backend "be" :model "mo"
                             :tool-use (list tool-call)))
                 (fsm (gptel-make-fsm :state 'TOOL :table nil :handlers nil
                                      :info info)))
            (gptel--handle-pre-tool fsm)
            (should captured)
            (should (equal (plist-get captured :id) "call_42"))
            (should (equal (plist-get captured :name) "test_tool"))
            (should (equal (plist-get captured :args) (list :a 1)))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest gptel-fork-post-tool-call-hook-receives-id ()
  "`gptel-post-tool-call-functions' receives a plist with :id and :result."
  (let ((buf (generate-new-buffer " *gptel-fork-test-post-tool*"))
        (captured nil))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq-local gptel-post-tool-call-functions
                        (list (lambda (pl) (setq captured (copy-sequence pl)) nil))))
          (let* ((tool-call (list :name "test_tool" :args (list :a 1)
                                  :id "call_42" :result "ok"))
                 (info (list :buffer buf :backend "be" :model "mo"
                             :tool-use (list tool-call)))
                 (fsm (gptel-make-fsm :state 'TOOL :table nil :handlers nil
                                      :info info)))
            (gptel--handle-post-tool fsm)
            (should captured)
            (should (equal (plist-get captured :id) "call_42"))
            (should (equal (plist-get captured :result) "ok"))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(provide 'gptel-fork-test)
;;; gptel-fork-test.el ends here
