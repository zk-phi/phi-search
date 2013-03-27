;;; phi-search.el --- inferior isearch compatible with "multiple-cursors"

;; Copyright (C) 2013 zk_phi

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: zk_phi
;; URL: http://hins11.yu-yake.com/
;; Version: 1.0.0

;;; Commentary:

;; Add following expression in your init file :
;;
;;   (require 'phi-search)
;;
;; and bind command "phi-search"
;;
;;   (global-set-key (kbd "C-s") 'phi-search)

;; When the command is called, a small window appears at the bottom of
;; the current window. Insert query there in regular expression, to start
;; searching. While searching, following keybindings are available :
;;
;; o [C-s] 'phi-search-again-or-next
;;
;;   If query is blank, use the last query and start searching. Otherwise,
;;   move to the next match.
;;
;; o [C-r] 'phi-search-again-or-previous
;;
;;   If query is blank, use the last query and start searching. Otherwise,
;;   move to the previous match.
;;
;; o [RET] 'phi-search-complete
;;
;;   Finish search.
;;
;; o [C-g] 'phi-search-abort
;;
;;   Abort search and back to the original position.
;;
;; You may change keybindings. See "phi-search-keybindings".

;; Note : When multiple-cursors is extended to multiple buffers, this command
;;        will be no longer usable.

;;; Change Log:

;; 1.0.0 first released

;;; Code:

;; * constante

(defconst phi-search-version "1.0.0")

;; * utilities

(defun phi-search--search-backward (query limit)
  "a handy version of search-backward-regexp"
  (ignore-errors
   (let* ((pos1 (point))
          (pos2 (search-backward-regexp query limit t)))
     (if (not (and pos2 (= pos1 pos2))) pos2
       (backward-char 1)
       (phi-search--search-backward query limit)))))

(defun phi-search--search-forward (query limit)
  "a handy version of search-forward-regexp"
  (ignore-errors
    (let* ((pos1 (point))
           (pos2 (search-forward-regexp query limit t)))
      (if (not (and pos2 (= pos1 pos2))) pos2
        (forward-char 1)
        (phi-search--search-forward query limit)))))

(defmacro phi-search--with-nurumacs (&rest body)
  "if nurumacs is installed, use it"
  `(if (boundp 'nurumacs-version)
       (progn
         (nurumacs--pre-command-function)
         ,@body
         (nurumacs--post-command-function))
     ,@body))

;; * private functions for TARGET buffer

;; variables

(defvar phi-search-limit 1000
  "maximum number of accepted matches")

(defvar phi-search--overlays nil
  "overlays currently active in this target window. is ordered.")
(make-variable-buffer-local 'phi-search--overlays)

(defvar phi-search--original-position nil
  "stores position when this search started.")
(make-variable-buffer-local 'phi-search--original-position)

(defvar phi-search--offset nil
  "the first matching item counting from the original position")
(make-variable-buffer-local 'phi-search--offset)

(defvar phi-search--selection nil
  "stores which item in phi-search--overlays is selected.
this value must be nil, if nothing is matched.")
(make-variable-buffer-local 'phi-search--selection)

(defvar phi-search--last-executed nil
  "stores the last query")
(make-variable-buffer-local 'phi-search--last-executed)

;; faces

(make-face 'phi-search-match-face)
(set-face-attribute 'phi-search-match-face nil
                    :background "#194854")

(make-face 'phi-search-selection-face)
(set-face-attribute 'phi-search-selection-face nil
                    :background "#594854")

;; functions

(defun phi-search--delete-overlays ()
  "delete all overlays in this target buffer"
  (mapc 'delete-overlay phi-search--overlays)
  (setq phi-search--overlays nil)
  (setq phi-search--offset nil)
  (setq phi-search--selection nil)
  (goto-char phi-search--original-position))

(defun phi-search--make-overlays-for (query)
  "make overlays for all matching items in this target buffer"
  (save-excursion
    (goto-char (point-max))
    (phi-search--make-overlays-for-1 query phi-search--original-position)
    (setq phi-search--offset (length phi-search--overlays))
    (phi-search--make-overlays-for-1 query nil)
    (setq phi-search--offset (- (length phi-search--overlays)
                                phi-search--offset)))
  (when (zerop (length phi-search--overlays))
    (message "no matches"))
  (when (>= (length phi-search--overlays) phi-search-limit)
    (message "more than %d matches" phi-search-limit)
    (setq phi-search--overlays nil))
  (setq phi-search--selection nil))

(defun phi-search--make-overlays-for-1 (query limit)
  (while (when (phi-search--search-backward query limit)
           (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
             (overlay-put ov 'face 'phi-search-match-face)
             (add-to-list 'phi-search--overlays ov)
             (< (length phi-search--overlays) phi-search-limit)))))

(defun phi-search--select (n)
  "select Nth matching item. return position, or nil for failure."
  (when (and (>= n 0)
             (< n (length phi-search--overlays)))
    ;; unselect old item
    (when phi-search--selection
      (overlay-put (nth phi-search--selection phi-search--overlays)
                   'face 'phi-search-match-face))
    ;; select new item if there
    (let ((ov (nth n phi-search--overlays)))
      (setq phi-search--selection n)
      (overlay-put ov 'face 'phi-search-selection-face)
      (goto-char (overlay-start ov)))))

;; * private functions for PROMPT buffer

;; variables

(defvar phi-search--target nil
  "the target (window . buffer) which this prompt buffer is for")
(make-variable-buffer-local 'phi-search--target)

;; functions

(defmacro phi-search--with-target-buffer (&rest body)
  "eval body with target buffer selected."
  `(progn
     ;; assert that window and buffer live
     (cond ((null phi-search--target)
            (error "phi-search: unexpected error (phi-search--target is nil)"))
           ((not (window-live-p (car phi-search--target)))
            (error "phi-search: target window is deleted"))
           ((not (buffer-live-p (cdr phi-search--target)))
            (error "phi-search: target buffer is killed")))
     ;; select the target window, binding variables for the prompt buffer
     (let ((target phi-search--target))
       (with-selected-window (car target)
         ;; if buffer is switched, switch back to the target
         (unless (eq (current-buffer) (cdr target))
           (switch-to-buffer (cdr target))
           (message "phi-search: buffer is switched"))
         ;; eval body
         ,@body))))

(defun phi-search--update (&rest _)
  "update overlays for the target buffer"
  (when phi-search--target
    (let ((query (buffer-string)))
      (phi-search--with-target-buffer
       (phi-search--delete-overlays)
       (unless (string= query "")
         (phi-search--make-overlays-for query)
         (when phi-search--overlays
           (unless (phi-search--select phi-search--offset)
             (setq phi-search--offset 0)
             (phi-search--select phi-search--offset))
           (message
            (format "matched %d items" (length phi-search--overlays)))))))))

(add-hook 'after-change-functions 'phi-search--update)

;; * start/end phi-search

(defvar phi-search-keybindings
  `((,(kbd "C-s") . phi-search-again-or-next)
    (,(kbd "C-r") . phi-search-again-or-previous)
    (,(kbd "C-g") . phi-search-abort)
    (,(kbd "RET") . phi-search-complete))
  "keybindings used in phi-search prompt")

(defun phi-search--initialize ()
  (setq phi-search--original-position (point))
  (let ((target (cons (selected-window) (current-buffer))))
    (select-window (split-window-vertically -4))
    (switch-to-buffer (generate-new-buffer "*phi-prompt*"))
    (setq phi-search--target target))
  (dolist (kb phi-search-keybindings)
    (local-set-key (car kb) (cdr kb))))

(defun phi-search--clean ()
  (let ((wnd (car phi-search--target))
        (str (buffer-string)))
    (kill-buffer (current-buffer))
    (delete-window (selected-window))
    (select-window wnd)
    (setq phi-search--original-position nil)
    (setq phi-search--last-executed str)))

;; * interactive commands

(defun phi-search ()
  "repeatable incremental search command"
  (interactive)
  (phi-search--initialize))

(defun phi-search-next ()
  "select next item."
  (interactive)
  (phi-search--with-target-buffer
   (when (null phi-search--selection)
     (error "nothing matched"))
   (phi-search--with-nurumacs
    (unless (phi-search--select (1+ phi-search--selection))
      (message "no more matches")
      (phi-search--select 0)))))

(defun phi-search-previous ()
  "select previous item."
  (interactive)
  (phi-search--with-target-buffer
   (when (null phi-search--selection)
     (error "nothing matched"))
   (phi-search--with-nurumacs
    (unless (phi-search--select (1- phi-search--selection))
      (message "no more matches")
      (phi-search--select
       (1- (length phi-search--overlays)))))))

(defun phi-search-again-or-next ()
  "search again with the last query, or search next item"
  (interactive)
  (let ((str (phi-search--with-target-buffer
              phi-search--last-executed)))
    (if (not (string= (buffer-string) ""))
        (phi-search-next)
      (delete-region (point-min) (point-max))
      (insert str))))

(defun phi-search-again-or-previous ()
  "search again with the last query, or search previous item"
  (interactive)
  (let ((str (phi-search--with-target-buffer
              phi-search--last-executed)))
    (if (not (string= (buffer-string) ""))
        (phi-search-previous)
      (delete-region (point-min) (point-max))
      (insert str))))

(defun phi-search-abort ()
  "abort phi-search"
  (interactive)
  (phi-search--with-target-buffer
   (phi-search--with-nurumacs
    (phi-search--delete-overlays)))
  (phi-search-complete))

(defun phi-search-complete ()
  "set repeatable command as \"this-command\" and quit phi-search.
if optional arg command is non-nil, call command after that."
  (interactive)
  (let ((query (buffer-string)))
    (phi-search--with-target-buffer
     ;; generate a repeatable command and set as "this-command"
     (let (command)
       (if (null phi-search--selection)
           ;; if nothing is selected, generate a command that do nothing
           (setq command `(lambda ()
                            (interactive)
                            nil))
         ;; otherwise, generate a command that jumps N items backward/forward
         (let ((n (- phi-search--selection phi-search--offset)))
           (if (< n 0)
               ;; backward
               (setq command `(lambda ()
                                (interactive)
                                (dotimes (n ,(abs n))
                                  (unless (phi-search--search-backward ,query nil)
                                    (goto-char (point-max))
                                    (phi-search--search-backward ,query nil)))))
             ;; forward
             (setq command `(lambda ()
                              (interactive)
                              (dotimes (n ,(1+ n))
                                (unless (phi-search--search-forward ,query nil)
                                  (goto-char (point-min))
                                  (phi-search--search-forward ,query nil)))
                              (goto-char (match-beginning 0)))))))
       ;; behave as if this command is "the" command
       (setq this-command command
             this-original-command command)
       (when (boundp 'mc--this-command)
         (setq mc--this-command command)))
     ;; clear overlays
     (save-excursion (phi-search--delete-overlays))))
  (phi-search--clean))

;; * provide

(provide 'phi-search)

;;; phi-search.el ends here
