;;; org-krita.el --- Krita support for Org Mode -*- lexical-binding: t; -*-

;; Copyright (c) 2020 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav@lepisma.xyz>
;; Version: 0.0.1
;; Package-Requires: ((emacs "26"))
;; URL: https://github.com/lepisma/org-krita

;;; Commentary:

;; Krita support for Org Mode
;; This file is not a part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'archive-mode)
(require 'filenotify)
(require 'f)
(require 'org)

(org-add-link-type "krita" #'org-krita-link-action #'org-krita-export)

(defun org-krita-edit (png-path &optional full-mode)
  "Edit given PNG-PATH in krita canvasonly mode.

If FULL-MODE is not null, run full krita."
  (let ((kra-path (expand-file-name (f-swap-ext png-path "kra"))))
    (when (f-exists-p kra-path)
      (if full-mode
          (call-process "krita" nil 0 nil kra-path)
        (call-process "krita" nil 0 nil "--canvasonly" "--nosplash" kra-path)))))

(defun org-krita-link-action (path)
  (org-krita-edit path))

(defun org-krita-export (_path _desc _backend)
  (error "Krita export not implemented yet."))

(defun org-krita-extract-png (kra-path)
  "Extract png from given KRA-PATH and return data."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (archive-zip-extract kra-path "mergedimage.png")
    (buffer-string)))

;; TODO Add krita file watching
;; (defun org-krita-callback (event)
;;   (let ((kra-path "./still.kra"))
;;     (org-krita-extract-png kra-path (f-swap-ext kra-path "png"))
;;     (with-current-buffer "test.org"
;;       (org-redisplay-inline-images))))

;; (file-notify-rm-watch "./still.kra")
;; (file-notify-add-watch "./still.kra" '(change) 'org-krita-callback)

(defun org-krita-get-links ()
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (link)
      (when (string-equal (org-element-property :type link) "krita")
        link))))

(defun org-krita-show-link (link)
  (let* ((start (org-element-property :begin link))
         (end (org-element-property :end link))
         (overlay (make-overlay start end))
         (img (create-image (org-krita-extract-png (org-element-property :path link)) 'png t)))
    (overlay-put overlay 'display img)))

(defun org-krita-hide-all ()
  (dolist (overlay (append (car (overlay-lists)) (cdr (overlay-lists))))
    (delete-overlay overlay)))

(defun org-krita-enable ()
  (dolist (link (org-krita-get-links))
    (org-krita-show-link link)))

(defun org-krita-disable ()
  (dolist (link (org-krita-get-links))
    (file-notify-rm-watch (org-element-property :path link)))
  (org-krita-hide-all))

;;;###autoload
(define-minor-mode org-krita-mode
  "Mode for displaying editable krita images within Org Mode."
  :initial-value nil
  (if org-krita-mode (org-krita-enable) (org-krita-disable)))

(provide 'org-krita)

;;; org-krita.el ends here
