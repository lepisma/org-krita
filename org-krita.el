;;; org-krita.el --- Krita support for Org Mode -*- lexical-binding: t; -*-

;; Copyright (c) 2020 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav@lepisma.xyz>
;; Version: 0.2.0
;; Package-Requires: ((emacs "26") (f "0.20.0") (org "9.3"))
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

(org-link-set-parameters "krita" :follow #'org-krita-edit :export #'org-krita-export)

;; NOTE: Only single reference for a file supported as of now.

(defgroup org-krita nil
  "Org-krita customization."
  :group 'org
  :package-version '(org-krita . "0.2.0"))

(defcustom org-krita-append-ext-kra t
  "Append automatically .kra extension."
  :group 'org-krita
  :type 'boolean
  :package-version '(org-krita . "0.2.0"))

(defcustom org-krita-get-new-filepath (lambda () (read-file-name "New krita file: "))
  "Function returning filepath of new created image."
  :group 'org-krita
  :type 'function
  :package-version '(org-krita . "0.2.0"))

(defcustom org-krita-get-new-desc (lambda () (read-string "Description: "))
  "Function returning description of new created image."
  :group 'org-krita
  :type 'function
  :package-version '(org-krita . "0.2.0"))

(defvar-local org-krita-watchers nil
  "A-list mapping file names to change watcher descriptors.")

(defvar-local org-krita-overlays nil
  "A-list mapping file names to overlay.")

(defconst org-krita-dir (file-name-directory load-file-name)
  "Base directory for package.")

(defun org-krita-resource (file)
  "Return full path of a resource FILE."
  (expand-file-name file (file-name-as-directory (concat org-krita-dir "resources"))))

(defun org-krita-export (_path _desc _backend)
  "Export krita canvas _PATH from Org files.
Argument _DESC refers to link description.
Argument _BACKEND refers to export backend."
  (let ((png-path (f-swap-ext _path "png")))
    (cl-case _backend
      (html (format "<img src=\"%s\">"
                    (prog1 png-path
                      (org-krita-save-image _path png-path))))
      (ascii (format "%s (%s)" (or _desc _path) _path))
      (latex (format "\\includegraphics[width=\\textheight,height=\\textwidth,keepaspectratio]{%s}"
                     (prog1 png-path
                       (org-krita-save-image _path png-path)))))))

(defun org-krita-save-image (kra-path png-path)
  "Extract from KRA-PATH a .png and write it to PNG-PATH."
  (let ((image (create-image (org-krita-extract-png kra-path) 'png t)))
    (with-temp-buffer
      (insert (plist-get (cdr image) :data))
      (write-region (point-min) (point-max) png-path))))

(defun org-krita-make-new-image (output-kra-path &optional width height)
  "Create a new image based on a template at OUTPUT-KRA-PATH."
  (let ((template (org-krita-resource "template.kra")))
    ;; TODO: Change image width and height based on provided argument
    (f-copy template output-kra-path)))

(defun org-krita-extract-png (kra-path)
  "Extract png from given KRA-PATH and return data."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (archive-zip-extract (expand-file-name kra-path) "mergedimage.png")
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

(defun org-krita-edit (path &optional full-mode)
  "Edit given PATH in krita canvasonly mode.

If FULL-MODE is not null, run full krita."
  (let ((kra-path (expand-file-name path)))
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

(defun org-krita-validate-path (path)
  "Validate the file PATH as a krita path."
  (if (f-ext-p path "kra")
      path
    (if org-krita-append-ext-kra
        (concat path ".kra")
      path)))

;;;###autoload
(defun org-krita-insert-new-image (output-kra-path desc)
  "Insert new image in current buffer."
  (interactive
   (let ((output-kra-path (funcall org-krita-get-new-filepath))
         (desc (funcall org-krita-get-new-desc)))
     (list (org-krita-validate-path output-kra-path) desc)))
  (org-krita-make-new-image output-kra-path)
  (org-insert-link nil (concat "krita:" output-kra-path) desc)
  ;; TODO: Enable only the new image
  (org-krita-enable))

;;;###autoload
(define-minor-mode org-krita-mode
  "Mode for displaying editable krita images within Org file."
  :init-value nil
  (if org-krita-mode (org-krita-enable) (org-krita-disable)))

(provide 'org-krita)

;;; org-krita.el ends here
