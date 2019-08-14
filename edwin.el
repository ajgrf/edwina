;;; edwin --- Dynamic window manager for Emacs -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'seq)

(defvar edwin-layout 'edwin-stack-layout
  "The current Edwin layout.
A layout is a function that takes a list of buffers, and arranges them into
a window configuration.")

(defun edwin-arrange ()
  "Arrange windows according to Edwin's current layout."
  (interactive)
  (let* ((windows (edwin-window-list))
         (selected-window-index (seq-position windows (selected-window)))
         (buffers (mapcar #'window-buffer windows)))
    (delete-other-windows)
    (funcall edwin-layout buffers)
    (select-window (nth selected-window-index
                        (edwin-window-list)))))

(defun edwin-window-list (&optional frame)
  "Return a list of windows on FRAME in layout order."
  (window-list frame nil (frame-first-window frame)))

(defun edwin-stack-layout (buffers)
  "Edwin layout that stacks BUFFERS evenly on top of each other."
  (dolist (buffer buffers)
    (switch-to-buffer buffer)
    (split-window)
    (edwin-select-next-window))
  (delete-window)
  (balance-windows (window-parent)))

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

(defvar edwin-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-r") 'edwin-arrange)
    (define-key map (kbd "M-j") 'edwin-select-next-window)
    (define-key map (kbd "M-k") 'edwin-select-previous-window)
    (define-key map (kbd "M-J") 'edwin-swap-next-window)
    (define-key map (kbd "M-K") 'edwin-swap-previous-window)
    map)
  "Keymap for edwin-mode.")

(define-minor-mode edwin-mode
  "Toggle Edwin mode on or off.
With a prefix argument ARG, enable Edwin mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is `toggle'.

Edwin mode is a global minor mode that provides dwm-like dynamic
window management for Emacs windows."
  :global t
  :lighter " edwin"
  :keymap 'edwin-mode-map
  (edwin-arrange))

(provide 'edwin)
;;; edwin.el ends here
