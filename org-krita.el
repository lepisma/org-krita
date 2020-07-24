;;; org-krita.el --- Krita support for Org Mode -*- lexical-binding: t; -*-

;; Copyright (c) 2020 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav@lepisma.xyz>
;; Version: 0.1.0
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

(require 'arc-mode)
(require 'filenotify)
(require 'f)
(require 'cl-lib)
(require 'org)

(org-add-link-type "krita" #'org-krita-edit #'org-krita-export)

;; NOTE: Only single reference for a file supported as of now.

(defvar-local org-krita-watchers nil
  "A-list mapping file names to change watcher descriptors.")

(defvar-local org-krita-overlays nil
  "A-list mapping file names to overlay.")

(defun org-krita-export (_path _desc _backend)
  (error "Krita export not implemented yet."))

(defun org-krita-extract-png (kra-path)
  "Extract png from given KRA-PATH and return data."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (archive-zip-extract kra-path "mergedimage.png")
    (buffer-string)))

(defun org-krita-get-links ()
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (link)
      (when (string-equal (org-element-property :type link) "krita")
        link))))

(defun org-krita-event-file-path (event)
  (if (eq (nth 1 event) 'renamed)
      (nth 3 event)
    (nth 2 event)))

(defun org-krita-watcher-callback (event)
  "Callback that runs after krita files are modified."
  (let* ((kra-path (org-krita-event-file-path event))
         (links (org-krita-get-links))
         (paths (mapcar (lambda (it) (expand-file-name (org-element-property :path it))) links))
         (idx (cl-position kra-path paths :test #'string-equal)))
    (when idx (org-krita-show-link (nth idx links)))))

(defun org-krita-add-watcher (kra-path)
  "Setup auto-refreshing watcher for given krita LINK."
  (let ((desc (file-notify-add-watch kra-path '(change) #'org-krita-watcher-callback)))
    (unless (alist-get kra-path org-krita-watchers nil nil #'string-equal)
      (push (cons kra-path desc) org-krita-watchers))))

(defun org-krita-edit (png-path &optional full-mode)
  "Edit given PNG-PATH in krita canvasonly mode.

If FULL-MODE is not null, run full krita."
  (let ((kra-path (expand-file-name (f-swap-ext png-path "kra"))))
    (when (f-exists-p kra-path)
      (if full-mode
          (call-process "krita" nil 0 nil kra-path)
        (call-process "krita" nil 0 nil "--canvasonly" "--nosplash" kra-path))
      (org-krita-add-watcher kra-path))))

(defun org-krita-hide-link (link)
  (let ((overlay (alist-get (org-element-property :path link) org-krita-overlays nil nil #'string-equal)))
    (when overlay (delete-overlay overlay))))

(defun org-krita-show-link (link)
  (org-krita-hide-link link)
  (let* ((start (org-element-property :begin link))
         (end (org-element-property :end link))
         (overlay (make-overlay start end))
         (kra-path (org-element-property :path link)))
    (overlay-put overlay 'display (create-image (org-krita-extract-png kra-path) 'png t))
    (push (cons kra-path overlay) org-krita-overlays)))

(defun org-krita-hide-all ()
  (dolist (link (org-krita-get-links))
    (org-krita-hide-link link)))

(defun org-krita-enable ()
  (dolist (link (org-krita-get-links))
    (org-krita-show-link link)))

(defun org-krita-disable ()
  "Disable watchers and hide krita images."
  (dolist (watcher org-krita-watchers)
    (file-notify-rm-watch (cdr watcher)))
  (setq org-krita-watchers nil)
  (org-krita-hide-all))

;;;###autoload
(define-minor-mode org-krita-mode
  "Mode for displaying editable krita images within Org file."
  :initial-value nil
  (if org-krita-mode (org-krita-enable) (org-krita-disable)))

(provide 'org-krita)

;;; org-krita.el ends here
