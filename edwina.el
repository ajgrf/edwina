;;; edwina.el --- Dynamic window manager -*- lexical-binding: t -*-

;; Author: Alex Griffin <a@ajgrf.com>
;; URL: https://gitlab.com/ajgrf/edwina
;; Version: 0.3.0-pre
;; Package-Requires: ((emacs "25"))
;; Keywords: convenience

;;; Copyright © 2019 Alex Griffin <a@ajgrf.com>
;;;
;;;
;;; This file is NOT part of GNU Emacs.
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

;; Edwina is a dynamic window manager for Emacs. It automatically arranges your
;; Emacs panes (called "windows" in Emacs parlance) into predefined layouts,
;; dwm-style.

;;; Code:

(require 'seq)

(defgroup edwina nil
  "A dynamic window manager for Emacs."
  :group 'convenience
  :prefix "edwina-")

(defcustom edwina-keymap-prefix (kbd "C-c C-w")
  "Prefix key for keybindings."
  :type 'string
  :group 'edwina)

(defcustom edwina-mode-line-format "%s "
  "String used for displaying the current layout in mode line."
  :type 'string
  :group 'edwina)

(defcustom edwina-nmaster 1
  "The number of windows to put in the Edwina master area."
  :type 'integer
  :group 'edwina)

(defcustom edwina-mfact 0.55
  "The size of the master area in proportion to the stack area."
  :type 'float
  :group 'edwina)

(defcustom edwina-narrow-threshold 132
  "Put master area on top if the frame is narrower than this."
  :type 'integer
  :group 'edwina)

(defvar edwina-layout 'edwina-tall-layout
  "The current Edwina layout.
A layout is a function that takes a list of panes, and arranges them into
a window configuration.")

(defvar edwina--window-fields
  '(buffer start hscroll vscroll point prev-buffers)
  "List of window fields to save and restore.")

(defvar edwina--window-params
  '(delete-window quit-restore)
  "List of window parameters to save and restore.")

(defun edwina-pane (window)
  "Create pane from WINDOW.
A pane is Edwina's internal window abstraction, an association list containing
a buffer and other information."
  (let ((pane '()))
    (dolist (field edwina--window-fields)
      (let* ((getter (intern (concat "window-" (symbol-name field))))
             (value (funcall getter window)))
        (push (cons field value) pane)))
    (dolist (param edwina--window-params)
      (let ((value (window-parameter window param)))
        (push (cons param value) pane)))
    pane))

(defun edwina-restore-pane (pane)
  "Restore PANE in the selected window."
  (dolist (field edwina--window-fields)
    (let ((setter (intern (concat "set-window-" (symbol-name field))))
          (value  (alist-get field pane)))
      (funcall setter nil value)))
  (dolist (param edwina--window-params)
    (set-window-parameter nil param (alist-get param pane)))
  (unless (window-parameter nil 'delete-window)
    (set-window-parameter nil 'delete-window #'edwina-delete-window)))

(defun edwina--window-list (&optional frame)
  "Return a list of windows on FRAME in layout order."
  (window-list frame nil (frame-first-window frame)))

(defun edwina-pane-list (&optional frame)
  "Return the current list of panes on FRAME in layout order."
  (mapcar #'edwina-pane (edwina--window-list frame)))

(defmacro edwina--respective-window (window &rest body)
  "Execute Edwina manipulations in BODY and return the respective WINDOW."
  (declare (indent 1))
  `(let* ((window ,window)
          (windows (edwina--window-list))
          (index (seq-position windows window)))
     ,@body
     (nth index (edwina--window-list))))

(defun edwina-arrange (&optional panes)
  "Arrange PANES according to Edwina's current layout."
  (interactive)
  (let* ((panes (or panes (edwina-pane-list))))
    (select-window
     (edwina--respective-window (selected-window)
       (delete-other-windows)
       (funcall edwina-layout panes)))))

(defun edwina--display-buffer (display-buffer &rest args)
  "Apply DISPLAY-BUFFER to ARGS and arrange windows.
Meant to be used as advice :around `display-buffer'."
  (edwina--respective-window (apply display-buffer args)
    (edwina-arrange)))

(defun edwina-stack-layout (panes)
  "Edwina layout that stacks PANES evenly on top of each other."
  (let ((split-height (ceiling (/ (window-height)
                                  (length panes)))))
    (edwina-restore-pane (car panes))
    (dolist (pane (cdr panes))
      (select-window
       (split-window nil split-height 'below))
      (edwina-restore-pane pane))))

(defun edwina--mastered (side layout)
  "Add a master area to LAYOUT.
SIDE is passed to `split-window' to position the stack area."
  (lambda (panes)
    (let ((master (seq-take panes edwina-nmaster))
          (stack  (seq-drop panes edwina-nmaster))
          (msize  (ceiling (* edwina-mfact
                              (if (memq side '(left right t))
                                  (frame-width)
                                (frame-height))))))
      (cond ((and master stack)
             (split-window nil msize side)
             (edwina-stack-layout master)
             (select-window (next-window))
             (funcall layout stack))
            (master (edwina-stack-layout master))
            (stack  (funcall layout stack))))))

(defun edwina-tall-layout (panes)
  "Edwina layout with master and stack areas for PANES."
  (let* ((side (if (< (frame-width) edwina-narrow-threshold) 'below 'right))
         (layout (edwina--mastered side #'edwina-stack-layout)))
    (funcall layout panes)))

(defun edwina-layout-name (layout)
  "Return the user-facing name of LAYOUT."
  (capitalize
   (replace-regexp-in-string "\\(edwina-\\|-layout\\)" ""
                             (symbol-name layout))))

(defun edwina-mode-line-indicator ()
  "Return a string to display in the mode line."
  (format edwina-mode-line-format
          (edwina-layout-name edwina-layout)))

(defun edwina-select-next-window ()
  "Move cursor to the next window in cyclic order."
  (interactive)
  (select-window (next-window)))

(defun edwina-select-previous-window ()
  "Move cursor to the previous window in cyclic order."
  (interactive)
  (select-window (previous-window)))

(defun edwina-swap-next-window ()
  "Swap the selected window with the next window."
  (interactive)
  (let ((cur  (edwina-pane (selected-window)))
        (next (edwina-pane (next-window))))
    (edwina-restore-pane next)
    (edwina-select-next-window)
    (edwina-restore-pane cur)))

(defun edwina-swap-previous-window ()
  "Swap the selected window with the previous window."
  (interactive)
  (let ((cur  (edwina-pane (selected-window)))
        (prev (edwina-pane (previous-window))))
    (edwina-restore-pane prev)
    (edwina-select-previous-window)
    (edwina-restore-pane cur)))

(defun edwina-dec-mfact ()
  "Decrease the size of the master area."
  (interactive)
  (setq edwina-mfact (max (- edwina-mfact 0.05)
                         0.05))
  (edwina-arrange))

(defun edwina-inc-mfact ()
  "Increase the size of the master area."
  (interactive)
  (setq edwina-mfact (min (+ edwina-mfact 0.05)
                         0.95))
  (edwina-arrange))

(defun edwina-dec-nmaster ()
  "Decrease the number of windows in the master area."
  (interactive)
  (setq edwina-nmaster (max (- edwina-nmaster 1)
                            0))
  (edwina-arrange))

(defun edwina-inc-nmaster ()
  "Increase the number of windows in the master area."
  (interactive)
  (setq edwina-nmaster (+ edwina-nmaster 1))
  (edwina-arrange))

(defun edwina-clone-window ()
  "Clone selected window."
  (interactive)
  (split-window-below)
  (edwina-arrange))

(defun edwina-delete-window (&optional window)
  "Delete WINDOW."
  (interactive)
  (let ((ignore-window-parameters t))
    (delete-window window))
  (when (or (null window)
            (and (boundp 'edwina-mode) edwina-mode))
    (edwina-arrange)))

(defun edwina-zoom ()
  "Zoom/cycle the selected window to/from master area."
  (interactive)
  (if (eq (selected-window) (frame-first-window))
      (edwina-swap-next-window)
    (let ((pane (edwina-pane (selected-window))))
      (edwina-delete-window)
      (edwina-arrange (cons pane (edwina-pane-list))))))

(defvar edwina-mode-map
  (let ((map (make-sparse-keymap))
        (prefix-map (make-sparse-keymap)))
    (define-key prefix-map (kbd "r") 'edwina-arrange)
    (define-key prefix-map (kbd "C-r") 'edwina-arrange)
    (define-key prefix-map (kbd "n") 'edwina-select-next-window)
    (define-key prefix-map (kbd "C-n") 'edwina-select-next-window)
    (define-key prefix-map (kbd "SPC") 'edwina-select-next-window)
    (define-key prefix-map (kbd "p") 'edwina-select-previous-window)
    (define-key prefix-map (kbd "C-p") 'edwina-select-previous-window)
    (define-key prefix-map (kbd "N") 'edwina-swap-next-window)
    (define-key prefix-map (kbd "C-S-n") 'edwina-swap-next-window)
    (define-key prefix-map (kbd "P") 'edwina-swap-previous-window)
    (define-key prefix-map (kbd "C-S-p") 'edwina-swap-previous-window)
    (define-key prefix-map (kbd "%") 'edwina-dec-mfact)
    (define-key prefix-map (kbd "{") 'edwina-dec-mfact)
    (define-key prefix-map (kbd "[") 'edwina-dec-mfact)
    (define-key prefix-map (kbd "^") 'edwina-inc-mfact)
    (define-key prefix-map (kbd "}") 'edwina-inc-mfact)
    (define-key prefix-map (kbd "]") 'edwina-inc-mfact)
    (define-key prefix-map (kbd "d") 'edwina-dec-nmaster)
    (define-key prefix-map (kbd "C-d") 'edwina-dec-nmaster)
    (define-key prefix-map (kbd "i") 'edwina-inc-nmaster)
    (define-key prefix-map (kbd "k") 'edwina-delete-window)
    (define-key prefix-map (kbd "C-k") 'edwina-delete-window)
    (define-key prefix-map (kbd "RET") 'edwina-zoom)
    (define-key prefix-map (kbd "<return>") 'edwina-zoom)
    (define-key prefix-map (kbd "c") 'edwina-clone-window)
    (define-key prefix-map (kbd "C-c") 'edwina-clone-window)
    (define-key map edwina-keymap-prefix prefix-map)
    map)
  "Keymap for command `edwina-mode'.")

(defvar edwina-mode-map-alist
  `((edwina-mode . ,edwina-mode-map))
  "Add to `emulation-mode-map-alists' to give bindings higher precedence.")

(defvar edwina-dwm-key-alist
  '(("r" edwina-arrange)
    ("j" edwina-select-next-window)
    ("k" edwina-select-previous-window)
    ("S-j" edwina-swap-next-window)
    ("J" edwina-swap-next-window)
    ("S-k" edwina-swap-previous-window)
    ("K" edwina-swap-previous-window)
    ("h" edwina-dec-mfact)
    ("l" edwina-inc-mfact)
    ("d" edwina-dec-nmaster)
    ("i" edwina-inc-nmaster)
    ("S-c" edwina-delete-window)
    ("C" edwina-delete-window)

    ("RET" edwina-zoom t)
    ("return" edwina-zoom t)
    ("S-RET" edwina-clone-window t)
    ("S-return" edwina-clone-window t))

  "A list of keys to bind with a prefix. Used in
  `edwina-setup-dwm-keys'")

(defun edwina-setup-dwm-keys (&optional modifier)
  "Set up dwm-like keybindings. MODIFIER is the mod-key to use,
and must be a either \'super or \'hyper. With no argument,
use meta."
  (let ((mod-prefix
	 (cond
	  ((equal 'super modifier)
	   "s-")
	  ((equal 'hyper modifier)
	   "H-")
	  (t "M-"))))
    (dolist (key-and-function edwina-dwm-key-alist)
      (define-key edwina-mode-map
	(if (cddr key-and-function)
	    (kbd (format "<%s%s>"
			 mod-prefix
			 (car key-and-function)))
	  (kbd (format "%s%s"
		       mod-prefix
		       (car key-and-function))))
	(cadr key-and-function)))))

(defun edwina--init ()
  "Initialize command `edwina-mode'."
  (add-to-list 'emulation-mode-map-alists
               'edwina-mode-map-alist)
  (advice-add #'display-buffer :around #'edwina--display-buffer)
  (unless (assoc 'edwina-mode mode-line-misc-info)
    (push '(edwina-mode (:eval (edwina-mode-line-indicator)))
          (cdr (last mode-line-misc-info))))
  (edwina-arrange))

(defun edwina--clean-up ()
  "Clean up when disabling command `edwina-mode'."
  (advice-remove #'display-buffer #'edwina--display-buffer))

;;;###autoload
(define-minor-mode edwina-mode
  "Toggle Edwina mode on or off.
With a prefix argument ARG, enable Edwina mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is `toggle'.

Edwina mode is a global minor mode that provides dwm-like dynamic
window management for Emacs windows."
  :global t
  (if edwina-mode
      (edwina--init)
    (edwina--clean-up)))

(provide 'edwina)
;;; edwina.el ends here
