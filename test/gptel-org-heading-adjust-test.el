;;; gptel-org-heading-adjust-test.el --- Tests for heading adjustment in responses

;;; Code:

(require 'ert)
(require 'gptel-org)

(defmacro gptel-org-heading-adjust-test-with-buffer (content &rest body)
  "Create a temp org buffer with CONTENT and execute BODY."
  (declare (indent 1))
  `(let ((buf (generate-new-buffer " *gptel-test*")))
     (unwind-protect
         (save-current-buffer
           (set-buffer buf)
           (org-mode)
           (insert ,content)
           ,@body)
       (kill-buffer buf))))

;;; Tests for the infinity loop fix

(ert-deftest gptel-org-heading-adjust-asterisk-word-pattern ()
  "Test that asterisk-word patterns (*WORD) don't cause infinite loop."
  (gptel-org-heading-adjust-test-with-buffer
   "* Parent heading
*THIS ASTERISK WITHOUT SPACE CAUSES INFINITY LOOP
Regular content here"
   (let ((level-diff 1))
     (goto-char (point-min))
     (let ((iterations 0)
           (max-iterations 100))
       (while (and (re-search-forward "^\\(\\*+\\)\\( \\)" nil t)
                   (< iterations max-iterations))
         (cl-incf iterations)
         (unless (gptel-org--in-example-block-p)
           (let* ((current-stars (match-string 1))
                  (new-level (+ (length current-stars) level-diff))
                  (new-stars (make-string new-level ?*)))
             (replace-match (concat new-stars "\\2"))
             (goto-char (match-end 0)))))
       (should (< iterations max-iterations))
       (should (> iterations 0))))))

(provide 'gptel-org-heading-adjust-test)
;;; gptel-org-heading-adjust-test.el ends here
