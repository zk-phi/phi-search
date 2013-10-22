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
;; Version: 2.0.1

;;; Commentary:

;; Add following expression in your init file :
;;
;;   (require 'phi-search)
;;
;; and bind command "phi-search"
;;
;;   (global-set-key (kbd "C-s") 'phi-search)
;;   (global-set-key (kbd "C-r") 'phi-search-backward)

;; In *phi-search* buffer, following commands are available.

;; - phi-search-again-or-next (replaces "phi-search")
;;
;;   Move to the next matching item. If query is blank, use the last
;;   query.

;; - phi-search-again-or-previous (replaces "phi-search-backward")
;;
;;   Similar to phi-search-again-or-next, but move to the previous item.

;; - phi-search-scroll-up (replaces "scroll-up")
;;
;;   Scroll the target window up, to check candidates.

;; - phi-search-scroll-down (replaces "scroll-down")
;;
;;   Scroll the target window down.

;; - phi-search-recenter (replaces "recenter")
;;
;;   Recenter the target window.

;; - phi-search-yank-word (replaces "kill-region")
;;
;;   Expand query by yanking one word from the target buffer.

;; - [RET] phi-search-complete
;;
;;   Finish searching.

;; - [C-RET] phi-search-complete-at-beginning
;;
;;   Finish searching at the beginning of the match.

;; - [C-c C-c] phi-search-unlimit
;;
;;   Force update results regardless of "phi-search-limit"

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
;; 1.1.4 fixed a bug in adjacent matches
;; 1.1.5 added a hook
;; 1.1.6 added an option phi-search-case-sensitive
;; 1.1.7 added phi-search-recenter, phi-search-yank-word
;; 1.1.8 added phi-search-scroll-up/down
;; 1.1.9 improved fallback behavior when called with region
;;       fixed bug on invoking multiple-cursors just after phi-search
;; 1.2.0 added command "phi-search-complete-at-beginning"
;; 1.2.1 use "remap" for default keybindings
;; 2.0.0 divided into two files ("phi-search-core.el")
;;       added "phi-search-unlimit" command
;; 2.0.1 added phi-search-init-hook

;;; Code:

(require 'phi-search-core)

;; + constants

(defconst phi-search-version "2.0.0")

;; + customs

(defcustom phi-search-case-sensitive nil
  "when non-nil, phi-search will be case sensitive"
  :group 'phi-search)

(defcustom phi-search-init-hook nil
  "hook run after initialization of phi-search"
  :group 'phi-search)

(defcustom phi-search-additional-keybinds
  '(([remap next-line] . 'phi-search-maybe-next-line)
    ([remap previous-line] . 'phi-search-maybe-previous-line)
    ([remap forward-char] . 'phi-search-maybe-forward-char)
    ((kbd "C-<return>") . 'phi-search-complete-at-beginning))
  "additional bindings used in phi-search"
  :group 'phi-search)

(defcustom phi-search-backward-additional-keybinds
  (append
   '(([remap phi-search] . 'phi-search-again-or-previous)
     ([remap phi-search-backward] . 'phi-search-again-or-next)
     ([remap phi-search-again-or-next] . 'phi-search-again-or-previous)
     ([remap phi-search-again-or-previous] . 'phi-search-again-or-next))
   phi-search-additional-keybinds)
  "additional bindings used in phi-search-backward"
  :group 'phi-search)

(defcustom phi-search-mode-line-format
  '(" *phi-search*"
    (:eval (let (total selection)
             (phi-search--with-target-buffer
              (setq selection phi-search--selection
                    total (length phi-search--overlays)))
             (when selection
               (format " [ %d / %d ]" (1+ selection) total)))))
  "mode-line-format for phi-search(-backward)"
  :group 'phi-search)

;; + variables

(defvar phi-search--original-region nil
  "stores region substring this search started with.")
(make-variable-buffer-local 'phi-search--original-region)

;; + generate repeatable commands

(defvar phi-search--region-query nil
  "query for a generated command, must be cursor-local")
(eval-after-load "multiple-cursors"
  '(add-to-list 'mc/cursor-specific-vars 'phi-search--region-query))

(defun phi-search--generate-command (query n &optional filter cmd use-region)
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
         (unless (phi-search--search-forward ,query nil ,filter (zerop n))
           (goto-char (point-min))
           (phi-search--search-forward ,query nil ,filter t)))
       ,post-process)))

;; + start/end phi-search

(defun phi-search--backward-after-update-function ()
  (when phi-search--selection
    (or (phi-search--select (1- phi-search--selection))
        (phi-search--select (1- (length phi-search--overlays))))))

(defun phi-search--complete-function (&optional cmd)
  (phi-search--with-target-buffer
   ;; generate a repeatable command for multiple-cursors
   (let ((command
          (cond ((null phi-search--selection)
                 (lambda () nil))
                ((string= query phi-search--original-region)
                 (phi-search--generate-command
                  nil phi-search--selection nil cmd 'use-region))
                (t
                 (phi-search--generate-command
                  query phi-search--selection nil cmd)))))
     (setq this-command command
           this-original-command command)
     (when (and (boundp 'multiple-cursors-mode) multiple-cursors-mode)
       (setq mc--this-command command)))
   ;; move cursor back to the selection
   (when phi-search--selection
     (phi-search--select phi-search--selection))
   ;; run command
   (when cmd (call-interactively cmd))
   ;; clean-up variable
   (setq phi-search--original-region nil)))

(defun phi-search--search-initialize (&optional backward)
  (when (and (use-region-p)
             (not (= (region-beginning) (region-end))))
    (setq phi-search--original-region
          (buffer-substring (region-beginning) (region-end)))
    (deactivate-mark))
  (let ((str phi-search--original-region))
    (phi-search--initialize
     (not phi-search-case-sensitive)
     phi-search-mode-line-format
     (if backward
         phi-search-backward-additional-keybinds
       phi-search-additional-keybinds)
     nil
     (when backward 'phi-search--backward-after-update-function)
     'phi-search--complete-function)
    (when str (insert str)))
  (run-hooks 'phi-search-init-hook))

;; + commands

;;;###autoload
(defun phi-search (&optional use-isearch)
  "incremental search command compatible with \"multiple-cursors\""
  (interactive "P")
  (if (and (not use-isearch)
           (or (not (boundp 'popwin:popup-window))
               (not (eq (selected-window) popwin:popup-window))))
      (phi-search--search-initialize nil)
    (call-interactively 'isearch-forward-regexp)
    (when (use-region-p)
      (let ((string
             (buffer-substring (region-beginning) (region-end))))
        (deactivate-mark)
        (isearch-yank-string string)))))

;;;###autoload
(defun phi-search-backward (&optional use-isearch)
  "incremental search command compatible with \"multiple-cursors\""
  (interactive "P")
  (if (and (not use-isearch)
           (or (not (boundp 'popwin:popup-window))
               (not (eq (selected-window) popwin:popup-window))))
      (phi-search--search-initialize t)
    (call-interactively 'isearch-backward-regexp)
    (when (use-region-p)
      (let ((string
             (buffer-substring (region-beginning) (region-end))))
        (deactivate-mark)
        (isearch-yank-string string)))))

(defun phi-search-complete-at-beginning ()
  (interactive)
  (phi-search-complete
   `(lambda ()
      (interactive)
      (when (looking-back ,(buffer-string))
        (goto-char (match-beginning 0))))))

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

;; + provide

(provide 'phi-search)

;;; phi-search.el ends here
