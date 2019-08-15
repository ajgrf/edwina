;;; edwin --- Dynamic window manager for Emacs -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'seq)

(defvar edwin-layout 'edwin-tall-layout
  "The current Edwin layout.
A layout is a function that takes a list of buffers, and arranges them into
a window configuration.")

(defvar edwin-nmaster 1
  "The number of windows to put in the Edwin master area.")

(defvar edwin-mfact 0.55
  "The size of the master area in proportion to the stack area.")

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

(defun edwin-add-master-left (layout)
  "Add a master area to the left of LAYOUT."
  (lambda (buffers)
    (let ((master (seq-take buffers edwin-nmaster))
          (stack (seq-drop buffers edwin-nmaster)))
      (funcall layout stack)
      (when master
        (select-window
         (split-window (frame-root-window)
                       (ceiling (* -1 edwin-mfact (frame-width)))
                       'left))
        (edwin-stack-layout master)))))

(defun edwin-tall-layout (buffers)
  "Edwin layout with master and stack areas for BUFFERS."
  (let ((layout (edwin-add-master-left #'edwin-stack-layout)))
    (funcall layout buffers)))

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
  (setq edwin-mfact (+ edwin-mfact 0.05))
  (when (> edwin-mfact 0.95)
    (setq edwin-mfact 0.95))
  (edwin-arrange))

(defun edwin-dec-mfact ()
  "Decrease the size of the master area."
  (interactive)
  (setq edwin-mfact (- edwin-mfact 0.05))
  (when (< edwin-mfact 0.05)
    (setq edwin-mfact 0.05))
  (edwin-arrange))

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
