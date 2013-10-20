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

(defcustom nanowrimo-today-goal (/ nanowrimo-total-goal 30.0)
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

;;}}}
;;{{{ Functions

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

(provide 'nanowrimo)

;;; nanowrimo.el ends here
