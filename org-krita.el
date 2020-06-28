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

(defun org-krita-extract-png (kra-path output-path)
  "Extract png from given KRA-PATH and save in OUTPUT-PATH"
  (with-temp-file output-path
    (archive-zip-extract kra-path "mergedimage.png")))

(defun org-krita-callback (event)
  (let ((kra-path "./still.kra"))
    (org-krita-extract-png kra-path (f-swap-ext kra-path "png"))
    (with-current-buffer "test.org"
      (org-redisplay-inline-images))))

(defun org-krita-edit (png-path)
  "Edit given png-path in krita."
  (let ((kra-path (expand-file-name (f-swap-ext png-path "kra"))))
    (when (f-exists-p kra-path)
      (call-process "krita" nil 0 nil "--canvasonly" "--nosplash" kra-path))))

(file-notify-rm-watch "./still.kra")
(file-notify-add-watch "./still.kra" '(change) 'org-krita-callback)

(provide 'org-krita)

;;; org-krita.el ends here
