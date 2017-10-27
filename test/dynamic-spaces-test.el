;;; dynamic-spaces-test.el --- Tests for dynamic-spaces.

;; Copyright (C) 2015  Anders Lindgren

;; Author: Anders Lindgren

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for dynamic-spaces.

;; Implementation notes:
;;
;; Normally, a test case can call a function to check if it performs a
;; specific task. Unfortunately, for `dynamic-spaces', what a function
;; does depends on what was bound to a specific key.
;;
;; This code test this by queing up a number of key events and then
;; exit to the main command-loop using recursive edit. By queing
;; `exit-recursive-edit' as the last event, control is immediately
;; handed back to the test program.

;; Dependencies:
;;
;; This package rely in the `faceup' extension to `ert'. The feature
;; that is used is the ability to define a test function to be it's
;; own "explainer" function. Also, the multi-line aware
;; `faceup-test-equal' is used.

;;; Code:

(require 'dynamic-spaces)
(require 'ert)
;; For `faceup-test-equal'.
(require 'faceup)


(defun dynamic-spaces-keys-to-events (keys)
  (mapcar (lambda (key)
            ;; `kbd' returns "" when applied to " ".
            (let ((k (if (equal key " ")
                         32
                       (kbd key))))
              (cons t
                    (cond ((stringp k)
                           (string-to-char k))
                          ((vectorp k)
                           (aref k 0))
                          (t
                           k)))))
          keys))

(defun dynamic-spaces-check (before after &rest keys)
  "Check that BEFORE is transformed to AFTER if KEYS are pressed."
  ;; Que up a number of event, end with `exit-recursive-edit'. Exit
  ;; the function to the command-loop (by calling recursive-edit).
  ;; Since the last qued event is `exit-recursive-edit', control
  ;; immediately returns here.
  (let ((unread-command-events
         (append
          (dynamic-spaces-keys-to-events keys)
          ;; Uncomment the following line to stay in recursive edit.
          ;;
          ;; This is a vector, but after being appled to `append' only
          ;; the elements remain.
          (kbd "C-M-c")          ; Exit recursive edit.
          unread-command-events)))
    (with-temp-buffer
      (dynamic-spaces-mode 1)
      (insert before)
      ;; Move point to "*", and remove the star.
      (goto-char (point-min))
      (when (search-forward "*" nil t)
        (goto-char (match-beginning 0))
        (delete-char 1))
      (save-window-excursion
        (select-window (display-buffer (current-buffer)))
        (recursive-edit))
      (insert "*")
      (faceup-test-equal after (buffer-substring (point-min)
                                                 (point-max))))))
(faceup-defexplainer dynamic-spaces-check)

(ert-deftest dynamic-spaces-basics ()
  ;; ----------------------------------------
  ;; Insert

  ;; ----------
  ;; Plain inserts
  (should (dynamic-spaces-check
           "alpha*             beta"
           "alphax*            beta"
           "x"))
  (should (dynamic-spaces-check
           "alpha*             beta"
           "alpha *            beta"
           " "))

  (should (dynamic-spaces-check
           "alpha*   beta"
           "alphax*  beta"
           "x"))
  (should (dynamic-spaces-check
           "alpha*   beta"
           "alpha *  beta"
           " "))

  (should (dynamic-spaces-check
           "alpha*  beta"
           "alphax*  beta"
           "x"))
  (should (dynamic-spaces-check
           "alpha*  beta"
           "alpha * beta"
           " "))

  ;; Special case: Inserting a character in the middle of a space
  ;; group should not be compensated.
  (should (dynamic-spaces-check
           "alpha * beta"
           "alpha x* beta"
           "x"))
  ;; Special case: It should be possible to press space up to the next
  ;; space group.
  (should (dynamic-spaces-check
           "alpha * beta"
           "alpha  *beta"
           " "))


  ;; ----------
  ;; Inserts with two groups.
  (should (dynamic-spaces-check
           "alpha*             beta    gamma"
           "alphax*            beta    gamma"
           "x"))
  (should (dynamic-spaces-check
           "alpha*   beta   gamma"
           "alphax*  beta   gamma"
           "x"))
  (should (dynamic-spaces-check
           "alphax*  beta   gamma"
           "alphaxx*  beta  gamma"
           "x"))
  ;; ----------
  ;; Inserts a space and more text.
  (should (dynamic-spaces-check
           "alpha*      beta"
           "alpha x*    beta"
           " "
           "x"))
  ;; ----------
  ;; Inserts a character that splits a space group, with one space at
  ;; the end should not insert extra space.
  (should (dynamic-spaces-check
           "alpha * beta"
           "alpha x* beta"
           "x"))

  (should (dynamic-spaces-check
           "alpha *       beta"
           "alpha xxxx*   beta"
           "C-u"
           "x"))

  (should (dynamic-spaces-check
           "alpha  *      beta"
           "alpha  xxxx*  beta"
           "C-u"
           "x"))

  (should (dynamic-spaces-check
           "alpha   *     beta"
           "alpha   xxxx*  beta"
           "C-u"
           "x"))

  (should (dynamic-spaces-check
           "alpha    *    beta"
           "alpha    xxxx*  beta"
           "C-u"
           "x"))

  (should (dynamic-spaces-check
           "alpha     *   beta"
           "alpha     xxxx*  beta"
           "C-u"
           "x"))

  (should (dynamic-spaces-check
           "alpha      *  beta"
           "alpha      xxxx*  beta"
           "C-u"
           "x"))

  ;; Here, no extra space should be inserted.
  (should (dynamic-spaces-check
           "alpha       * beta"
           "alpha       xxxx* beta"
           "C-u"
           "x"))

  ;; ----------------------------------------
  ;; Delete

  ;; ----------
  ;; Delete backward
  (should (dynamic-spaces-check
           "alpha*             beta"
           "alph*              beta"
           "DEL"))
  ;; ----------
  ;; Delete backward at bol (ensure that last space is retained).
  (should (dynamic-spaces-check
           "alpha    beta\n*gamma    delta"
           "alpha    beta*gamma    delta"
           "DEL")))


(ert-deftest dynamic-spaces-delete-beginning-end ()
  (should (dynamic-spaces-check
           "1234*    5678"
           "1234*   5678"
           "C-d"))

  (should (dynamic-spaces-check
           "*    5678"
           "*   5678"
           "C-d"))

  (should (dynamic-spaces-check
           " *    5678"
           " *   5678"
           "C-d"))

  (should (dynamic-spaces-check
           "123*4    5678"
           "123*     5678"
           "C-d"))

  (should (dynamic-spaces-check
           "12*34    5678"
           "12*4     5678"
           "C-d"))

  (should (dynamic-spaces-check
           "alpha*      beta gamma     delta"
           "alpha* gamma               delta"
           "M-d"))

  (should (dynamic-spaces-check
           "1234   *5678"
           "1234  *5678"
           "DEL"))

  (should (dynamic-spaces-check
           "1234  * 5678"
           "1234 *  5678"
           "DEL"))

  (should (dynamic-spaces-check
           "1234   *5678"
           "*5678"
           "M-DEL"))

  (should (dynamic-spaces-check
           "1234  * 5678"
           "*       5678"
           "M-DEL"))

  nil)


(ert-deftest dynamic-spaces-two-space-groups ()
  "Check that some small space groups are ignored.

In normal editing, such as when deleting a word, small space
groups tend to be created.  Check that these are ignored."
  ;; Deleting words.
  (should (dynamic-spaces-check
           "alpha beta gamma* delta"
           "alpha * delta"
           "M-DEL"
           "M-DEL"))
  (should (dynamic-spaces-check
           "alpha * beta"
           "* beta"
           "M-DEL"))

  nil)


(ert-deftest dynamic-spaces-tab ()
  ;; ----------------------------------------
  ;; Insert

  ;; Inserting a character before a tab (so that it doesn't push it
  ;; over a tab stop), the spaces doesn't have to be adjusted.
  (should (dynamic-spaces-check
           "12*\tbeta"
           "12x*\tbeta"
           "x"))
  ;; When inserting a charcter so that a tab is pushed over a tab
  ;; stop, the tab character must be removed.
  (should (dynamic-spaces-check
           "1234567*\t\tbeta"
           "1234567x*\tbeta"
           "x"))
  ;; Special case: A single column tab character counts as a space group.
  ;; Ensure that it's replace with two spaces when inserting before it.
  (should (dynamic-spaces-check
           "1234567*\tbeta"
           "1234567x*  beta"
           "x"))

  ;; ----------------------------------------
  ;; Delete
  (should (dynamic-spaces-check
           "12*\tbeta"
           "1*\tbeta"
           "DEL"))
  ;; Deleting a character so that a tab moves past a tab stop.
  ;;
  ;; (Note; In this case, the package could also have inserted another
  ;; tab character, but the effect is the same.)
  (should (dynamic-spaces-check
           "12345678*\tbeta"
           "1234567*\t        beta"
           "DEL"))

  )


(ert-deftest dynamic-spaces-strings ()
  ;; Spaces inside strings should never be adjusted.
  (should (dynamic-spaces-check
           "abc* \"inside     string\""
           "abcx* \"inside     string\""
           "x"))
  ;; Spaces after strings should be adjusted.
  (should (dynamic-spaces-check
           "abc* \"inside     string\"    def"
           "abcx* \"inside     string\"   def"
           "x"))
  ;; Spaces after strings should be adjusted, even if point is in string.
  (should (dynamic-spaces-check
           "\"inside*     string\"    def"
           "\"insidex*     string\"   def"
           "x"))
  (should (dynamic-spaces-check
           "   *alpha      beta"
           "   \"*alpha      beta"
           "\""))
  ;; Deleting a quote makes the rest of the line a string, so it
  ;; should not be adjusted.
  (should (dynamic-spaces-check
           "alpha \"\"*      beta"
           "alpha \"*      beta"
           "DEL"))
  ;; Deleting the leading quote should not readjust the spaces inside
  ;; the origibal string.  Not should it adjust spaces after the
  ;; string, as they are part of a new string.
  (should (dynamic-spaces-check
           "*\"inside     string\"    def"
           "*inside     string\"    def"
           "<deletechar>"))
  (should (dynamic-spaces-check
           "\"*inside     string\"    def"
           "*inside     string\"    def"
           "DEL"))
  ;; Ditto, when inserting a quote.
  (should (dynamic-spaces-check
           "*inside     string\"    def"
           "\"*inside     string\"    def"
           "\"")))


(ert-deftest dynamic-spaces-sentence-end ()
  "Double spaces at the end of a sentence doesn't count as a space group."
  (let ((sentence-end-double-space t))
    ;; Two spaces after a space should not be considered a space group.
    (should (dynamic-spaces-check
             "a*bc.  def"
             "*bc.  def"
             "DEL"))
    ;; Three spaces should.
    (should (dynamic-spaces-check
             "a*bc.   def"
             "*bc.    def"
             "DEL"))
    ;; A tab should.
    (should (dynamic-spaces-check
             "a*bc.\tdef"
             "*bc.\tdef"
             "DEL"))
    ;; A tab should (on a tab boundary).
    (should (dynamic-spaces-check
             "1*234567.\tdef"
             "*234567.\t        def"
             "DEL"))
    ;; A space and a tab should.
    (should (dynamic-spaces-check
             "a*bc. \tdef"
             "*bc. \tdef"
             "DEL"))
    ;; A space and a tab should (on a tab boundary).
    (should (dynamic-spaces-check
             "1*23456. \tdef"
             "*23456. \t        def"
             "DEL"))))


(ert-deftest dynamic-spaces-insert-sentence-end ()
  "Test inserting before a sentence end."
  (let ((sentence-end-double-space nil))
    (should (dynamic-spaces-check
             "a*bc.   def"
             "ax*bc.  def"
             "x")))
  (let ((sentence-end-double-space t))
    (should (dynamic-spaces-check
             "a*bc.   def"
             "ax*bc.   def"
             "x"))))



(provide 'dynamic-spaces-test)

;;; dynamic-spaces-test.el ends here
