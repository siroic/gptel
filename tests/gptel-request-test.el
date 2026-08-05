;;; gptel-request-test.el --- Tests for gptel-request      -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Keywords: convenience, tools

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; ERT tests for `gptel--sanitize-string', which escapes raw-byte
;; characters so tool results can be JSON-serialized.

;;; Code:

(require 'ert)
(require 'gptel-request)

;; NB: "\200"-"\377" string literals are unibyte, so this converts them
;; to multibyte raw-byte characters (#x3FFF00..#x3FFFFF), like the bytes
;; a tool result read from a binary process/buffer would contain.
(defconst gptel--raw-byte-sample (string-to-multibyte "\200\201\377"))

(ert-deftest gptel--sanitize-string-raw-bytes ()
  "Raw bytes are escaped so `json-serialize' accepts the result."
  (let ((result (gptel--sanitize-string (string-to-multibyte "abc\200def"))))
    (should (string-match-p "\\\\x80" result))
    (should (stringp (json-serialize (list :r result))))))

(ert-deftest gptel--sanitize-string-exact-escape-format ()
  "Each byte becomes \\xNN, zero-padded hex, preserving order."
  (should (equal (gptel--sanitize-string gptel--raw-byte-sample)
                 "\\x80\\x81\\xFF")))

(ert-deftest gptel--sanitize-string-all-raw-bytes ()
  "A string consisting only of raw bytes is fully escaped."
  (let ((result (gptel--sanitize-string (string-to-multibyte "\200\201\377"))))
    (should (equal result "\\x80\\x81\\xFF"))
    (should (stringp (json-serialize (list :r result))))))

(ert-deftest gptel--sanitize-string-empty ()
  "The empty string passes through unchanged and serializes."
  (let ((empty ""))
    (should (eq empty (gptel--sanitize-string empty)))
    (should (stringp (json-serialize (list :r (gptel--sanitize-string empty)))))))

(ert-deftest gptel--sanitize-string-clean-string ()
  "A multibyte string without raw bytes passes through byte-identical."
  (let ((clean "hello world"))
    (should (eq clean (gptel--sanitize-string clean)))))

(ert-deftest gptel--sanitize-string-clean-unibyte-ascii ()
  "A unibyte ASCII string passes through as the same object."
  (let ((clean (string-to-unibyte "hello world")))
    (should (eq clean (gptel--sanitize-string clean)))))

(ert-deftest gptel--sanitize-string-mixed-unicode-and-raw ()
  "Unicode chars are preserved verbatim while raw bytes are escaped."
  (let ((result (gptel--sanitize-string
                 (concat "päivä" (string-to-multibyte "\200")))))
    (should (equal result "päivä\\x80"))
    (should-not (cl-loop for i below (length result)
                         thereis (<= #x3FFF00 (aref result i) #x3FFFFF)))
    (should (stringp (json-serialize (list :r result))))))

(ert-deftest gptel--sanitize-string-unibyte ()
  "Unibyte strings with bytes 0x80+ are escaped like multibyte raw bytes."
  (let ((result (gptel--sanitize-string (string-to-unibyte "abc\200"))))
    (should (equal result "abc\\x80"))
    (should (stringp (json-serialize (list :r result))))))

(ert-deftest gptel--sanitize-string-idempotent ()
  "Sanitizing an already-sanitized string is a no-op (same object)."
  (let ((once (gptel--sanitize-string gptel--raw-byte-sample)))
    (should (eq once (gptel--sanitize-string once)))))

(ert-deftest gptel--sanitize-string-tool-result-choke-point ()
  "Sanitizing a printed tool result guarantees `json-serialize' succeeds."
  ;; Shape produced by the Eval tool: (format \"Result:\\n%S\" ...) of a
  ;; byte-code/primitive function object.
  (let ((result (gptel--sanitize-string
                 (format "Result:\n%S" (symbol-function 'ignore)))))
    (should (stringp (json-serialize (list :r result)))))
  ;; A tool result that IS a raw-byte string (as `gptel--to-string'
  ;; passes strings through) must also serialize after sanitizing.
  (should (stringp
           (json-serialize
            (list :r (gptel--sanitize-string
                      (string-to-multibyte "\200\201")))))))

(ert-deftest gptel--sanitize-string-documents-original-failure ()
  "Raw bytes make `json-serialize' signal; the sanitized string does not."
  (should-error (json-serialize (list :a (string-to-multibyte "\200"))))
  (should-error (json-serialize (string-to-unibyte "abc\200")))
  (should (stringp
           (json-serialize
            (list :a (gptel--sanitize-string (string-to-multibyte "\200")))))))

;;; gptel-request-test.el ends here
