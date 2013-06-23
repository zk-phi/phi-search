;;; phi-search.el --- another incremental search command, compatible with "multiple-cursors"

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
;; Version: 1.1.3

;;; Commentary:

;; Add following expression in your init file :
;;
;;   (require 'phi-search)
;;
;; and bind command "phi-search"
;;
;;   (global-set-key (kbd "C-s") 'phi-search)

;; In *phi-search* buffer, following commands are available.
;;
;; - [C-s] phi-search-again-or-next
;;
;;   Move to the next matching item. If query is blank, use the last
;;   query.
;;
;; - [C-r] phi-search-again-or-previous
;;
;;   Similar to phi-search-again-or-next, but move to the previous item.
;;
;; - [RET] phi-search-complete
;;
;;   Finish searching.
;;
;; - [C-g] phi-search-abort
;;
;;   Finish searching, and move back to the original position.

;; For more details, see "Readme".

;;; Change Log:

;; 1.0.0 first released
;; 1.0.1 working better with regions
;;       added phi-search-complete-and-xxxx commands
;; 1.0.2 fixed phi-search-complete-and-xxxx commands
;;       better compatibility for multiple-cursors
;; 1.0.3 fixed bug that nurumacs does not work while inserting query
;;       changed mode-line-format
;;       renamed some private functions
;; 1.1.0 cleaned-up
;;       removed "phi-search-keybindings" and added "phi-search-mode-map"
;;       now calls "isearch" if the window is popwin window
;; 1.1.1 use "sublimity" not "nurumacs"
;; 1.1.2 added phi-search-backward command
;; 1.1.3 better integration with sublimity

;;; Code:

;; * constants

(defconst phi-search-version "1.1.3")

;; * customs

(defgroup phi-search nil
  "another incremental search command, compatible with \"multiple-cursors\""
  :group 'emacs)

(defcustom phi-search-limit 1000
  "maximum number of accepted matches"
  :group 'phi-search)

(defcustom phi-search-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-s") 'phi-search-again-or-next)
    (define-key map (kbd "C-r") 'phi-search-again-or-previous)
    (define-key map (kbd "C-g") 'phi-search-abort)
    (define-key map (kbd "C-n") 'phi-search-maybe-next-line)
    (define-key map (kbd "C-p") 'phi-search-maybe-previous-line)
    (define-key map (kbd "C-f") 'phi-search-maybe-forward-char)
    (define-key map (kbd "RET") 'phi-search-complete)
    map)
  "keymap for the phi-search prompt buffers"
  :group 'phi-search)

;; * faces

(make-face 'phi-search-match-face)
(set-face-attribute 'phi-search-match-face nil
                    :background "#194854")

(make-face 'phi-search-selection-face)
(set-face-attribute 'phi-search-selection-face nil
                    :background "#594854")

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

(defmacro phi-search--with-sublimity (&rest body)
  "if sublimity is installed, use it"
  `(if (boundp 'sublimity-scroll-version)
       (progn
         (sublimity--pre-command)
         ,@body
         (sublimity--post-command))
     ,@body))

;; * private functions for TARGET buffer

;; variables

(defvar phi-search--original-position nil
  "stores position where this search started from.")
(make-variable-buffer-local 'phi-search--original-position)

(defvar phi-search--original-region nil
  "stores region substring this search started with.")
(make-variable-buffer-local 'phi-search--original-region)

(defvar phi-search--overlays nil
  "overlays currently active in this target buffer. is ordered.")
(make-variable-buffer-local 'phi-search--overlays)

(defvar phi-search--selection nil
  "stores which item is currently selected.
this value must be nil, if nothing is matched.")
(make-variable-buffer-local 'phi-search--selection)

(defvar phi-search--last-executed nil
  "stores the last query")
(make-variable-buffer-local 'phi-search--last-executed)

;; functions

(defun phi-search--delete-overlays ()
  "delete all overlays in THIS target buffer, and go to the original position"
  (mapc 'delete-overlay phi-search--overlays)
  (setq phi-search--overlays nil
        phi-search--selection nil)
  (goto-char phi-search--original-position))

(defun phi-search--make-overlays-for (query)
  "make overlays for all matching items in THIS target buffer."
  (save-excursion
    ;; POINT -> BOF
    (goto-char phi-search--original-position)
    (phi-search--make-overlays-for-1 query nil)
    ;; EOF -> POINT
    (goto-char (point-max))
    (phi-search--make-overlays-for-1 query phi-search--original-position))
  (let ((num (length phi-search--overlays)))
    ;; check errors
    (cond ((zerop num)
           (message "no matches")
           (setq phi-search--selection nil))
          ((>= num phi-search-limit)
           (message "more than %d matches" phi-search-limit)
           (phi-search--delete-overlays)))))

(defun phi-search--make-overlays-for-1 (query limit)
  (while (when (phi-search--search-backward query limit)
           (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
             (overlay-put ov 'face 'phi-search-match-face)
             (add-to-list 'phi-search--overlays ov)
             (< (length phi-search--overlays) phi-search-limit)))))

(defun phi-search--select (n)
  "select Nth matching item and go there.
returns the position of the item, or nil for failure."
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
      (goto-char (overlay-end ov)))))

;; * private functions for PROMPT buffer

;; minor mode

(define-minor-mode phi-search-mode
  "minor mode for phi-search prompt buffer"
  :init-value nil
  :global nil
  :map phi-search-mode-map
  (if phi-search-mode
      (progn
        (add-hook 'after-change-functions 'phi-search--update nil t)
        (when (fboundp 'sublimity-mode) (sublimity-mode -1)))
    (remove-hook 'after-change-functions 'phi-search--update t)))

(defvar phi-search--direction nil
  "non-nil iff backward")
(make-variable-buffer-local 'phi-search--direction)

;; variables

(defvar phi-search--target nil
  "the target (window . buffer) which this prompt buffer is for")
(make-variable-buffer-local 'phi-search--target)

(defvar phi-search--mode-line-format
  '(" *phi-search*"
    (:eval (let (total selection)
             (phi-search--with-target-buffer
              (setq selection phi-search--selection
                    total (length phi-search--overlays)))
             (when selection
               (format " [ %d / %d ]" (1+ selection) total))))))

;; functions

(defmacro phi-search--with-target-buffer (&rest body)
  "eval body with the target buffer selected.
\"backward\", \"target\" and \"query\" are brought"
  `(progn
     ;; assert that window and buffer live
     (cond ((null phi-search--target)
            (error "phi-search: unexpected error (phi-search--target is nil)"))
           ((not (window-live-p (car phi-search--target)))
            (error "phi-search: target window is deleted"))
           ((not (buffer-live-p (cdr phi-search--target)))
            (error "phi-search: target buffer is killed")))
     ;; select the target window, binding variables for the prompt buffer
     (let ((target phi-search--target)
           (query (buffer-string))
           (backward phi-search--direction))
       (with-selected-window (car target)
         ;; if buffer is switched, switch back to the target
         (unless (eq (current-buffer) (cdr target))
           (switch-to-buffer (cdr target))
           (message "phi-search: buffer is switched"))
         ;; eval body
         ,@body))))

(defun phi-search-next ()
  "select next item."
  (phi-search--with-target-buffer
   (when (null phi-search--selection)
     (error "nothing matched"))
   (phi-search--with-sublimity
    (unless (phi-search--select (1+ phi-search--selection))
      (phi-search--select 0)
      (message "no more matches")))))

(defun phi-search-previous ()
  "select previous item."
  (phi-search--with-target-buffer
   (when (null phi-search--selection)
     (error "nothing matched"))
   (phi-search--with-sublimity
    (unless (phi-search--select (1- phi-search--selection))
      (phi-search--select (1- (length phi-search--overlays)))
      (message "no more matches")))))

(defun phi-search--update (&rest _)
  "update overlays for the target buffer"
  (phi-search--with-target-buffer
   (phi-search--with-sublimity
    (phi-search--delete-overlays)
    (phi-search--make-overlays-for query)
    ;; try to select the first item
    (phi-search--select
     (if backward
         (1- (length phi-search--overlays))
       0)))))

;; * start/end phi-search

(defun phi-search--initialize ()
  ;; store point
  (setq phi-search--original-position (point))
  ;; store region
  (when (and (use-region-p)
             (not (= (region-beginning) (region-end))))
    (setq phi-search--original-region
          (buffer-substring (region-beginning) (region-end)))
    (deactivate-mark))
  ;; make prompt buffer and window
  (let ((target (cons (selected-window) (current-buffer)))
        (str (or phi-search--original-region "")))
    (select-window (split-window-vertically -4))
    (switch-to-buffer (generate-new-buffer "*phi-search*"))
    (phi-search-mode 1)
    (setq phi-search--target target
          mode-line-format phi-search--mode-line-format)
    (insert str)))

(defun phi-search--clean ()
  (let ((wnd (car phi-search--target))
        (str (buffer-string)))
    ;; delete prompt buffer
    (kill-buffer (current-buffer))
    (delete-window (selected-window))
    (select-window wnd)
    ;; clear variables
    (setq phi-search--original-position nil
          phi-search--original-region nil
          phi-search--selection nil
          phi-search--overlays nil
          phi-search--last-executed str)))

;; * generate repeatable commands

(defvar phi-search--region-query nil
  "query for a generated command, must be cursor-local")
(make-variable-buffer-local 'phi-search--region-query)

(defun phi-search--command-do-nothing ()
  '(lambda ()
     (interactive)
     nil))

(defun phi-search--command-do-search (query n &optional cmd use-region)
  ;; phi-search--region-query must be cursor-local
  (when (boundp 'mc/cursor-specific-vars)
    (add-to-list 'mc/cursor-specific-vars 'phi-search--region-query))
  ;; generate command
  (let* ((pre-process
          (if use-region
              '(progn (setq phi-search--region-query
                            (buffer-substring (region-beginning) (region-end)))
                      (deactivate-mark))
            nil))
         (query
          (if use-region 'phi-search--region-query query))
         (post-process
          (if cmd `(call-interactively (quote ,cmd)))))
    `(lambda ()
       (interactive)
       ,pre-process
       (dotimes (n ,(1+ n))
         (unless (phi-search--search-forward ,query nil)
           (goto-char (point-min))
           (phi-search--search-forward ,query nil)))
       ,post-process)))

;; * interactive commands

;;;###autoload
(defun phi-search ()
  "incremental search command compatible with \"multiple-cursors\""
  (interactive)
  (if (and (boundp 'popwin:popup-window)
           (eq (selected-window) popwin:popup-window))
      (call-interactively 'isearch-forward-regexp)
    (phi-search--initialize)))

;;;###autoload
(defun phi-search-backward ()
  "incremental search command compatible with \"multiple-cursors\""
  (interactive)
  (let ((phi-search--direction t))
    (phi-search)
    (setq phi-search--direction t)))

(defun phi-search-again-or-next ()
  "search again with the last query, or search next item"
  (interactive)
  (let ((str (phi-search--with-target-buffer
              phi-search--last-executed)))
    (if (not (string= (buffer-string) ""))
        (phi-search-next)
      (insert str))))

(defun phi-search-again-or-previous ()
  "search again with the last query, or search previous item"
  (interactive)
  (let ((str (phi-search--with-target-buffer
              phi-search--last-executed)))
    (if (not (string= (buffer-string) ""))
        (phi-search-previous)
      (insert str))))

(defun phi-search-abort ()
  "abort phi-search"
  (interactive)
  (phi-search--with-target-buffer
   (phi-search--with-sublimity
    (phi-search--delete-overlays)))
  (phi-search-complete))

(defun phi-search-complete (&optional cmd)
  "set repeatable command as \"this-command\" and quit phi-search.
if optional arg command is non-nil, call it after that."
  (interactive)
  (phi-search--with-target-buffer
   (let ((command
          (cond ((null phi-search--selection)
                 (phi-search--command-do-nothing))
                ((string= query phi-search--original-region)
                 (phi-search--command-do-search
                  nil phi-search--selection cmd 'use-region))
                (t
                 (phi-search--command-do-search
                  query phi-search--selection cmd)))))
     (setq this-command command
           this-original-command command)
     (when (boundp 'mc--this-command)
       (setq mc--this-command command)))
   ;; clear overlays *without moving back cursor*
   (save-excursion (phi-search--delete-overlays)))
  (phi-search--clean)
  (when cmd (call-interactively cmd)))

;; * phi-search-maybe-xxxx

(defun phi-search-maybe-next-line ()
  "quit phi-search with next-line"
  (interactive)
  (condition-case err
      (call-interactively 'next-line)
    (error
     (phi-search-complete 'next-line))))

(defun phi-search-maybe-previous-line ()
  "quit phi-search with previous-line"
  (interactive)
  (condition-case err
      (call-interactively 'previous-line)
    (error
     (phi-search-complete 'previous-line))))

(defun phi-search-maybe-forward-char ()
  "quit phi-search with forward-char"
  (interactive)
  (condition-case err
      (call-interactively 'forward-char)
    (error
     (phi-search-complete 'forward-char))))

;; * provide

(provide 'phi-search)

;;; phi-search.el ends here
