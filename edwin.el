;;; edwin --- Dynamic window manager for Emacs -*- lexical-binding: t -*-

;;; Copyright © 2019 Alex Griffin <a@ajgrf.com>
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'seq)

(defvar edwin-layout 'edwin-tall-layout
  "The current Edwin layout.
A layout is a function that takes a list of panes, and arranges them into
a window configuration.")

(defvar edwin-nmaster 1
  "The number of windows to put in the Edwin master area.")

(defvar edwin-mfact 0.55
  "The size of the master area in proportion to the stack area.")

(defvar edwin--window-fields
  '(buffer start hscroll point prev-buffers)
  "List of window fields to save and restore.")

(defvar edwin--window-params
  '(delete-window quit-restore)
  "List of window parameters to save and restore.")

(defun edwin-pane (window)
  "Create pane from WINDOW.
A pane is Edwin's internal window abstraction, an association list containing
a buffer and other information."
  (let ((pane '()))
    (dolist (field edwin--window-fields)
      (let* ((getter (intern (concat "window-" (symbol-name field))))
             (value (funcall getter window)))
        (push (cons field value) pane)))
    (dolist (param edwin--window-params)
      (let ((value (window-parameter window param)))
        (push (cons param value) pane)))
    pane))

(defun edwin-restore-pane (pane)
  "Restore PANE in the selected window."
  (dolist (field edwin--window-fields)
    (let ((setter (intern (concat "set-window-" (symbol-name field))))
          (value  (alist-get field pane)))
      (funcall setter nil value)))
  (dolist (param edwin--window-params)
    (set-window-parameter nil param (alist-get param pane)))
  (unless (window-parameter nil 'delete-window)
    (set-window-parameter nil 'delete-window #'edwin-delete-window)))

(defun edwin--window-list (&optional frame)
  "Return a list of windows on FRAME in layout order."
  (window-list frame nil (frame-first-window frame)))

(defun edwin-arrange ()
  "Arrange windows according to Edwin's current layout."
  (interactive)
  (let* ((windows (edwin--window-list))
         (selected-window-index (seq-position windows (selected-window)))
         (panes (mapcar #'edwin-pane windows)))
    (delete-other-windows)
    (funcall edwin-layout panes)
    (select-window (nth selected-window-index
                        (edwin--window-list)))))

(defun edwin--display-buffer (display-buffer &rest args)
  "Apply DISPLAY-BUFFER to ARGS and arrange windows.
Meant to be used as advice :around `display-buffer'."
  (let* ((window (apply display-buffer args))
         (windows (edwin--window-list))
         (window-index (seq-position windows window)))
    (edwin-arrange)
    (nth window-index
         (edwin--window-list))))

(defun edwin-stack-layout (panes)
  "Edwin layout that stacks PANES evenly on top of each other."
  (let ((split-height (ceiling (/ (window-height)
                                  (length panes)))))
    (edwin-restore-pane (car panes))
    (dolist (pane (cdr panes))
      (select-window
       (split-window nil split-height 'below))
      (edwin-restore-pane pane))))

(defun edwin--mastered (side layout)
  "Add a master area to LAYOUT.
SIDE has the same meaning as in `split-window', but putting master to the
right or bottom is not supported."
  (lambda (panes)
    (let ((master (seq-take panes edwin-nmaster))
          (stack  (seq-drop panes edwin-nmaster))
          (msize  (ceiling (* -1
                              edwin-mfact
                              (if (memq side '(left right t))
                                  (frame-width)
                                (frame-height))))))
      (when stack
        (funcall layout stack))
      (when master
        (when stack
          (select-window
           (split-window (frame-root-window) msize side)))
        (edwin-stack-layout master)))))

(defun edwin-tall-layout (panes)
  "Edwin layout with master and stack areas for PANES."
  (let* ((side (if (< (frame-width) 132) 'above 'left))
         (layout (edwin--mastered side #'edwin-stack-layout)))
    (funcall layout panes)))

(defun edwin-select-next-window ()
  "Move cursor to the next window in cyclic order."
  (interactive)
  (select-window (next-window)))

(defun edwin-select-previous-window ()
  "Move cursor to the previous window in cyclic order."
  (interactive)
  (select-window (previous-window)))

(defun edwin-swap-next-window ()
  "Swap the selected window with the next window."
  (interactive)
  (window-swap-states (selected-window)
                      (next-window)))

(defun edwin-swap-previous-window ()
  "Swap the selected window with the previous window."
  (interactive)
  (window-swap-states (selected-window)
                      (previous-window)))

(defun edwin-inc-nmaster ()
  "Increase the number of windows in the master area."
  (interactive)
  (setq edwin-nmaster (+ edwin-nmaster 1))
  (edwin-arrange))

(defun edwin-dec-nmaster ()
  "Decrease the number of windows in the master area."
  (interactive)
  (setq edwin-nmaster (- edwin-nmaster 1))
  (when (< edwin-nmaster 0)
    (setq edwin-nmaster 0))
  (edwin-arrange))

(defun edwin-inc-mfact ()
  "Increase the size of the master area."
  (interactive)
  (setq edwin-mfact (min (+ edwin-mfact 0.05)
                         0.95))
  (edwin-arrange))

(defun edwin-dec-mfact ()
  "Decrease the size of the master area."
  (interactive)
  (setq edwin-mfact (max (- edwin-mfact 0.05)
                         0.05))
  (edwin-arrange))

(defun edwin-delete-window (&optional window)
  "Delete WINDOW."
  (interactive)
  (let ((ignore-window-parameters t))
    (delete-window window)
    (edwin-arrange)))

(defvar edwin-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-r") 'edwin-arrange)
    (define-key map (kbd "M-j") 'edwin-select-next-window)
    (define-key map (kbd "M-k") 'edwin-select-previous-window)
    (define-key map (kbd "M-J") 'edwin-swap-next-window)
    (define-key map (kbd "M-K") 'edwin-swap-previous-window)
    (define-key map (kbd "M-i") 'edwin-inc-nmaster)
    (define-key map (kbd "M-d") 'edwin-dec-nmaster)
    (define-key map (kbd "M-h") 'edwin-dec-mfact)
    (define-key map (kbd "M-l") 'edwin-inc-mfact)
    (define-key map (kbd "M-C") 'edwin-delete-window)
    map)
  "Keymap for edwin-mode.")

(defvar edwin-mode-map-alist
  `((edwin-mode . ,edwin-mode-map))
  "Add to `emulation-mode-map-alists' to give bindings higher precedence.")

(define-minor-mode edwin-mode
  "Toggle Edwin mode on or off.
With a prefix argument ARG, enable Edwin mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is `toggle'.

Edwin mode is a global minor mode that provides dwm-like dynamic
window management for Emacs windows."
  :global t
  :lighter " edwin"
  (if edwin-mode
      (progn
        (add-to-list 'emulation-mode-map-alists
                     'edwin-mode-map-alist)
        (advice-add #'display-buffer :around #'edwin--display-buffer)
        (edwin-arrange))
    (advice-remove #'display-buffer #'edwin--display-buffer)))

(provide 'edwin)
;;; edwin.el ends here
