;;; phi-replace.el --- another replace command building on phi-search

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
;; Version: 1.0.8

;;; Commentary:

;; Add following expression in your init file :
;;
;;   (require 'phi-replace)
;;
;; and bind command "phi-replace" or "phi-replace-query"
;;
;;   (global-set-key (kbd "M-%") 'phi-replace)

;; For more details, see "Readme".

;;; Change Log:

;; 1.0.0 first released
;; 1.0.1 added weight for phi-replace
;; 1.0.2 use "sublimity" not "nurumacs"
;; 1.0.3 better integration with sublimity
;; 1.0.4 added a hook
;; 1.0.5 added some commands
;; 1.0.6 better handling of narrowed buffer
;; 1.0.7 fixed bug on completing replace without matches
;; 1.0.8 use "remap" for default keybindings

;;; Code:

(require 'phi-search)
(defconst phi-replace-version "1.0.8")

(defvar phi-replace-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-s") 'phi-search-again-or-next)
    (define-key map (kbd "C-r") 'phi-search-again-or-previous)
    (define-key map [remap phi-search] 'phi-search-again-or-next)
    (define-key map [remap phi-search-backward] 'phi-search-again-or-previous)
    (define-key map [remap keyboard-quit] 'phi-replace-abort)
    (define-key map [remap scroll-up] 'phi-search-scroll-up)
    (define-key map [remap pager-page-down] 'phi-search-scroll-up)
    (define-key map [remap scroll-down] 'phi-search-scroll-down)
    (define-key map [remap pager-page-up] 'phi-search-scroll-down)
    (define-key map [remap recenter] 'phi-search-recenter)
    (define-key map [remap kill-region] 'phi-search-yank-word)
    (define-key map [remap phi-rectangle-kill-region] 'phi-search-yank-word)
    (define-key map (kbd "RET") 'phi-replace-complete)
    map)
  "keymap for the phi-search prompt buffers")

(defvar phi-replace-mode-hook nil
  "hook run when entering phi-replace-mode")

(defvar phi-replace-weight 0.02
  "weight for \"phi-replace\"")

;; + target buffer

(defvar phi-replace--original-restriction nil)
(make-variable-buffer-local 'phi-replace--original-restriction)

;; + prompt buffer

(define-minor-mode phi-replace-mode
  "minor mode for phi-replace prompt buffer"
  :init-value nil
  :global nil
  :keymap phi-replace-mode-map
  (if phi-replace-mode
      (progn
       (add-hook 'after-change-functions 'phi-search--update nil t)
       (run-hooks 'phi-replace-mode-hook)
       (when (fboundp 'sublimity-mode) (sublimity-mode -1)))
    (remove-hook 'after-change-functions 'phi-search--update t)))

(defvar phi-replace--query-mode nil)
(make-variable-buffer-local 'phi-replace--query-mode)

(defvar phi-replace--mode-line-format
  '(" *phi-replace*"
    (:eval (phi-search--with-target-buffer
            (format " [ %d ]" (length phi-search--overlays))))))

;; + start/end phi-replace

(defun phi-replace--initialize (&optional mode)
  ;; store point
  (setq phi-search--original-position (point))
  ;; narrow to region
  (when (use-region-p)
    (setq phi-replace--original-restriction
          (cons (point-min) (point-max)))
    (narrow-to-region (region-beginning) (region-end))
    (deactivate-mark))
  ;; make prompt buffer and window
  (let ((target (cons (selected-window) (current-buffer))))
    (select-window (split-window-vertically -4))
    (switch-to-buffer (generate-new-buffer "*phi-replace*"))
    (phi-replace-mode 1)
    (setq phi-replace--query-mode mode
          phi-search--target target
          mode-line-format phi-replace--mode-line-format)))

(defun phi-replace--clean ()
  (let ((wnd (car phi-search--target))
        (str (buffer-string)))
    ;; delete prompt buffer
    (kill-buffer (current-buffer))
    (delete-window (selected-window))
    (select-window wnd)
    ;; widen
    (narrow-to-region (car phi-replace--original-restriction)
                      (cdr phi-replace--original-restriction))
    ;; clear variables
    (setq phi-search--original-position nil
          phi-search--overlays nil
          phi-search--last-executed str
          phi-replace--original-restriction nil)))

;; + commands

;;;###autoload
(defun phi-replace ()
  "replace command using phi-search"
  (interactive)
  (if (and (boundp 'popwin:popup-window)
           (eq (selected-window) popwin:popup-window))
      (call-interactively 'replace-regexp)
    (phi-replace--initialize)))

;;;###autoload
(defun phi-replace-query ()
  "replace command using phi-search"
  (interactive)
  (if (and (boundp 'popwin:popup-window)
           (eq (selected-window) popwin:popup-window))
      (call-interactively 'query-replace-regexp)
    (phi-replace--initialize 'query)))

(defun phi-replace-abort ()
  "abort phi-replace"
  (interactive)
  (phi-search--with-target-buffer
   (phi-search--with-sublimity
    (phi-search--delete-overlays)))
  (phi-replace-complete))

(defun phi-replace-complete ()
  "execute phi-replace"
  (interactive)
  ;; if the query is blank, use the last query
  (when (and (string= (buffer-string) "")
             phi-search--last-executed)
    (insert phi-search--last-executed))
  (let ((force (not phi-replace--query-mode))
        str orig-cursor)
    (phi-search--with-target-buffer
     (setq orig-cursor (make-overlay phi-search--original-position
                                     (1+ phi-search--original-position)))
     (when phi-search--overlays
       (setq str (read-from-minibuffer "replace with ? "))
       (if force
           ;; replace all
           (dotimes (n (length phi-search--overlays))
             (phi-search--with-sublimity
              (phi-search--select n))
             (sit-for phi-replace-weight)
             (let ((ov (nth n phi-search--overlays)))
               (goto-char (overlay-start ov))
               (delete-region (overlay-start ov)
                              (overlay-end ov))
               (insert str)))
         ;; query replace
         (dotimes (n (length phi-search--overlays))
           (phi-search--with-sublimity
            (phi-search--select n))
           (let ((ov (nth n phi-search--overlays)))
             (when (y-or-n-p (format "replace with %s ? " str))
               (goto-char (overlay-start ov))
               (delete-region (overlay-start ov)
                              (overlay-end ov))
               (insert str))))))
     ;; clear overlays and back to the original position
     (phi-search--with-sublimity
      (phi-search--delete-overlays)
      (goto-char (overlay-start orig-cursor)))))
  (phi-search--clean))

;; provide

(provide 'phi-replace)

;;; phi-replace.el ends here
