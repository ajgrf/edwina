;;; edwin.el --- Dynamic window manager -*- lexical-binding: t -*-

;; Author: Alex Griffin <a@ajgrf.com>
;; URL: https://gitlab.com/ajgrf/edwina
;; Version: 0.3.0-pre
;; Package-Requires: ((emacs "25"))

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

;; Edwin is a dynamic window manager for Emacs. It automatically arranges your
;; Emacs panes (called "windows" in Emacs parlance) into predefined layouts,
;; dwm-style.

;;; Code:

(require 'edwina)

(defvaralias 'edwin-layout 'edwina-layout)
(defvaralias 'edwin-nmaster 'edwina-nmaster)
(defvaralias 'edwin-mfact 'edwina-mfact)
(defalias #'edwin-pane #'edwina-pane)
(defalias #'edwin-restore-pane #'edwina-restore-pane)
(defalias #'edwin-pane-list #'edwina-pane-list)
(defalias #'edwin-arrange #'edwina-arrange)
(defalias #'edwin-stack-layout #'edwina-stack-layout)
(defvaralias 'edwin-narrow-threshold 'edwina-narrow-threshold)
(defalias #'edwin-tall-layout #'edwina-tall-layout)
(defalias #'edwin-select-next-window #'edwina-select-next-window)
(defalias #'edwin-select-previous-window #'edwina-select-previous-window)
(defalias #'edwin-swap-next-window #'edwina-swap-next-window)
(defalias #'edwin-swap-previous-window #'edwina-swap-previous-window)
(defalias #'edwin-dec-mfact #'edwina-dec-mfact)
(defalias #'edwin-inc-mfact #'edwina-inc-mfact)
(defalias #'edwin-dec-nmaster #'edwina-dec-nmaster)
(defalias #'edwin-inc-nmaster #'edwina-inc-nmaster)
(defalias #'edwin-clone-window #'edwina-clone-window)
(defalias #'edwin-delete-window #'edwina-delete-window)
(defalias #'edwin-zoom #'edwina-zoom)
(defvaralias 'edwin-mode-map 'edwina-mode-map)
(defvaralias 'edwin-mode-map-alist 'edwina-mode-map-alist)

;;;###autoload
(defvaralias 'edwin-mode 'edwina-mode)
;;;###autoload
(defalias #'edwin-mode #'edwina-mode)

(warn "Edwin has been renamed Edwina. Please update your configuration!")

(provide 'edwin)
;;; edwin.el ends here
