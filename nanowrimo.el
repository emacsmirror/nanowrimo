;;; nanowrimo.el --- Track progress for nanowrimo

;; Copyright (C) 2013  Ivan Andrus <darthandrus@gmail.com>

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `http://www.gnu.org/licenses/'.

;; Author:	Ivan Andrus <darthandrus at gmail.com>
;; URL: https://bitbucket.org/gvol/nanowrimo-mode

;;; Commentary:
;;
;; If you plan to use this with org-mode you should install the great
;; org-wc.el from

;; TODO: make more complicated tracking of goals, what I have from
;; yesterday etc.

;; TODO: Perhaps make `nanowrimo-mode' hook into auto-fill-mode?  Then
;; it wouldn't be accurate after deletion...  But it might be faster

;;; Code:

;;{{{ Customizable variables

(defgroup nanowrimo nil
  "Word count display for nanowrimo."
  :group 'editing
  :prefix "nanowrimo-")

(defcustom nanowrimo-show-wpm t
  "If non-nil, show words per minute in the mode line."
  :group 'nanowrimo
  :type 'bool
  :safe 'boolp)

(defcustom nanowrimo-show-estimate t
  "If non-nil, show an estimate of how long it will take to finish today's goal."
  :group 'nanowrimo
  :type 'bool
  :safe 'boolp)

(defcustom nanowrimo-total-goal 50000
  "How many words you would like to write today."
  :group 'nanowrimo
  :type 'integer
  :safe 'integerp)

(defcustom nanowrimo-start-date
  (format-time-string "Nov 1 00:00:00 %Y")
  "The date string for the first day of NaNoWriMo."
  :group 'nanowrimo
  :type 'string
  :safe 'stringp)

(defcustom nanowrimo-num-days 30
  "How many days you would like to write for."
  :group 'nanowrimo
  :type 'integer
  :safe 'integerp)

(defcustom nanowrimo-today-goal
  (ceiling nanowrimo-total-goal nanowrimo-num-days)
  "How many words there were at the beginning of the session."
  :group 'nanowrimo
  :type 'integer
  :safe 'integerp)

(defcustom nanowrimo-count-words-function nil
  "Function used to actually count the words.
It should return an integer, the number of words to be counted.

This can be used to filter out unwanted words, e.g. when editing
LaTeX, HTML and the markup should not be counted.  It should
count as small a portion of the buffer as possible since it will
be called after every change."
  :group 'nanowrimo
  :type 'function)

(defcustom nanowrimo-org-table-name "nanocalc"
  "The date string for the first day of NaNoWriMo."
  :group 'nanowrimo
  :type 'string
  :safe 'stringp)

(defcustom nanowrimo-finish-functions nil
  "Functions to call when `nanowrimo-mode' is turned off."
  :group 'nanowrimo
  :type '(repeat function))

;;}}}
;;{{{ Internal Variables

(defvar nanowrimo--cur-wc 0
  "The number of words in the current buffer.")
;; (make-variable-buffer-local 'nanowrimo-buffer)

;; (defvar nanowrimo-lines 0
;;   "The number of lines in the current buffer")
;; (make-variable-buffer-local 'nanowrimo-line)

(defvar nanowrimo--display ""
  "The string to display for the mode line.")

(defvar nanowrimo--start-time nil
  "When command `nanowrimo-mode' was turned on.
Used to compute WPM and estimates.")
;; (make-variable-buffer-local 'nanowrimo--start-time)

(defvar nanowrimo--start-wc 0
  "How many words there were at the beginning of the session.")
;; (make-variable-buffer-local 'nanowrimo--start-wc)

(defvar nanowrimo--org-table-skeleton
  "#+NAME: %s
#+CONSTANTS: base=250 target=1667 increment=4 logbase=100. tdlogbase=100. tclogbase=200.
| <3> | <6>    | <5>   | <7>     | <7>     | <5>   | <6>    | <6>    | <7>     |
| Day | Words  | Quota | Chain   | Cum Wds | Targ  | Cum T  | Plus/min | Score   |
|-----+--------+-------+---------+---------+-------+--------+--------+---------|
|   1 |        |       |         |         |       |        |        |         |
#+TBLFM: e$3='(if (eq \"@-1$2\" \"\") \"\" (calcFunc-max $base (+ $base (* $increment @-1$4))));L::$4='(if (eq \"$2\" \"\") \"\" (if (>= $2 $base) (calcFunc-max 0. (+ 1. (+ (calcFunc-max 0. @-1$4) (string-to-number (calc-eval \"log(div($2,$3),$logbase)\"))) )) (if (> @-1$4 0) -1 (- @-1$4 1))));L::$5='(if (eq \"$2\" \"\") \"\" (+ $2 @-1$5));L::$6='(if (eq \"@-1$2\" \"\") \"\" $target);L::$7='(if (eq \"@-1$2\" \"\") \"\" (+ $target @-1$7));L::$8='(if (eq \"$2\" \"\") \"\" (- $5 $7));L::$9='(if (eq \"$2\" \"\") \"\" (if (> $4 0) (calcFunc-max 0 (+ (string-to-number (calc-eval \"log(div($5,$7),$tclogbase)\")) (string-to-number (calc-eval \"log(div($2,$6),$tdlogbase)\")) (calcFunc-max $4 1) @-1$9)) (calcFunc-max 0 (+ @-1$9 $4))));L::@3$3=$base;N::@3$4='(if (eq \"@3$2\" \"\") \"\" (if (>= @3$2 $base) (+ 1.0 (string-to-number (calc-eval \"log(div(@3$2, @3$3),$logbase)\"))) -1.));L::@3$5='(if (eq \"@3$2\" \"\") \"\" @3$2);L::@3$6=$target;N::@3$7=$target;N::@3$8='(if (eq \"@3$2\" \"\") \"\" (- @3$5 @3$7));L::@3$9='(if (eq \"@3$2\" \"\") \"\" (if (> @3$4 0) (calcFunc-max 0 (+ (string-to-number (calc-eval \"log(div(@3$5,@3$7),$tclogbase)\")) (string-to-number (calc-eval \"log(div(@3$2,@3$6),$tdlogbase)\") ) @3$4)) @3$4));L")

;;}}}
;;{{{ Basic Functions

(defun nanowrimo-count-words-region (start end)
  "Count the number of words in the region."
  (save-excursion
    (let ((n 0))
      (goto-char start)
      (while (< (point) end)
        (if (forward-word 1)
            (setq n (1+ n))))
      n)))

(defun nanowrimo-org-count-subtree ()
  "Count words in the current subtree.
This require the org-wc package."
  (org-word-count-aux (save-excursion (or (outline-previous-heading)
                                          (point-min)))
                      (save-excursion (or (outline-next-heading)
                                          (point-max)))))

(defun nanowrimo-count-words ()
  "Count the words in the current buffer (or relevant portion).
If `nanowrimo-count-words-function' is set, call that, otherwise
call `org-word-count-aux' on the current subtree, if it's
available and the buffer is in `org-mode'.  It falls back to
simply counting words in the entire buffer."
  (cond
   ((functionp nanowrimo-count-words-function)
    (funcall nanowrimo-count-words-function))
   ((and (eq major-mode 'org-mode)
         (require 'org-wc nil t))
    (nanowrimo-org-count-subtree))
   (t
    (nanowrimo-count-words-region (point-min) (point-max)))))

(defun nanowrimo-mode-update (&optional first last len)
  "Update the word count in the mode line.
The optional arguments FIRST LAST and LEN are so that it can be
added to `after-change-functions'."
  (interactive)
  (let* ((wc (nanowrimo-count-words))
         (wc-cur (- wc nanowrimo--start-wc))
         (min (/ (time-to-seconds (time-subtract (current-time) nanowrimo--start-time)) 60))
         (wpm (/ wc-cur min))
         (words-left (- nanowrimo-today-goal wc))
         (estimate (/ words-left wpm)))
    (setq nanowrimo--display
          (concat (format "%sw" wc)
                  (when nanowrimo-show-wpm
                    (format " %dwpm" wpm))
                  (when (and nanowrimo-show-estimate
                             (> estimate 0)
                             ;; When starting, estimate is huge and we don't want to depress anyone
                             (< estimate 600))
                    (format " %dmin" estimate))))))

(define-minor-mode nanowrimo-mode
  "Display the number of words, WPM and estimate to finish in the mode line."
  nil "" '()
  (if nanowrimo-mode
      (progn
        (add-to-list 'global-mode-string 'nanowrimo--display)
        (add-to-list 'after-change-functions 'nanowrimo-mode-update)
        (setq nanowrimo--start-wc (nanowrimo-count-words))
        (setq nanowrimo--start-time (current-time))
        (nanowrimo-mode-update))
    (remove-from-list 'global-mode-string 'nanowrimo--display)
    (remove-hook 'after-change-functions 'nanowrimo-mode-update)))

;;}}}
;;{{{ Maintaining org table of score

(defun nanowrimo-days-into-nanowrimo ()
  "Returns how many days into NaNoWriMo today is."
  (days-between (format-time-string "%c") nanowrimo-start-date))

(defun nanowrimo-update-org-table (&optional novisit)
  "Update the org-mode table calculating the score.
Suitable for adding to `nanowrimo-finish-functions'."
  (interactive "P")

  (when (and (eq major-mode 'org-mode)
             (require 'calc-ext nil t))
   (let ((wc (nanowrimo-count-words))
         (days (nanowrimo-days-into-nanowrimo))
         (p nil))
     (save-excursion
       (if (< days 1)
           (user-error "Today is not a NaNoWriMo day.")
         (goto-char (point-min))
         (re-search-forward
          (org-babel-named-data-regexp-for-name nanowrimo-org-table-name))
         (re-search-forward "^\\s *|")
         (sit-for 1)
         (nanowrimo-verify-org-table)
         ;; This is a bit of a hack to find the right row
         (re-search-backward (format "^\\s *| +%d |" days))
         (setq p (point))
         ;; Replace the field with our new
         (org-table-get-field 1 (format "%s" wc))
         (org-table-recalculate t nil)))
     (when (and p (not novisit))
       (goto-char p)))))

(defun nanowrimo-insert-org-table ()
  "Insert an org-mode table for keeping track of progress.
If a table with a name of `nanowrimo-org-table-name' already exists
then it is merely updated to contain the correct number of days."
  (interactive)
  (when (and (eq major-mode 'org-mode)
             (require 'calc-ext nil t))
    (let ((p (save-excursion
               (goto-char (point-min))
               (and (re-search-forward
                     (org-babel-named-data-regexp-for-name nanowrimo-org-table-name) nil t)
                    (re-search-forward "^\\s *|" nil t)
                    (point)))))
      (if p
          (goto-char p)
        (when (not (bolp)) (insert "\n"))
        (insert (format nanowrimo--org-table-skeleton nanowrimo-org-table-name))
        (re-search-backward "^|"))
      (nanowrimo-verify-org-table))))

(defun nanowrimo-verify-org-table ()
  "Ensure that the org table has the right number of rows.

Expects point to be in a table and ensures that the first column
contains 1, 2, 3, ... up to `nanowrimo-num-days'."
  (if (not (org-table-p))
      (user-error "Not at a table")
    (org-table-goto-line 1)
    (while (not (string-match "^ *[0-9]+ *$" (org-table-get-field 1)))
      (forward-line))
    (let ((i 1))
      (while (<= i nanowrimo-num-days)
        (if (org-table-p)
            (progn
              (org-table-get-field 1 (format "%s" i))
              (forward-line))
          (beginning-of-line)
          (insert (format "| %s\n" i)))
        (setq i (1+ i))))
    (forward-line -1)
    (org-table-align)))

;;}}}

(provide 'nanowrimo)

;;; nanowrimo.el ends here
