;;; rails-lib.el ---

;; Copyright (C) 2006 Galinsky Dmitry <dima dot exe at gmail dot com>

;; Keywords: ruby rails languages oop
;; $URL$
;; $Id$

;;; License

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;; Code:

(defun rails-lib:goto-file-with-menu (dir title &optional ext no-inflector)
  "Make menu to choose files and find-file it"
  (let* (file
         files
         (ext (if ext ext "rb"))
         (ext (concat "\\." ext "$"))
         (root (rails-core:root))
         (dir (concat root dir))
         (mouse-coord (if (functionp 'posn-at-point) ; mouse position at point
                         (nth 2 (posn-at-point))
                       (cons 200 100))))
    (message dir)
    (message ext)
    (message files)
    (setq files (find-recursive-directory-relative-files dir "" ext))
    (setq files (sort files 'string<))
    (setq files (reverse files))
    (setq files (mapcar
                 (lambda(f)
                   (cons (if no-inflector
                             f
                           (rails-core:class-by-file f)) f))
                 files))
    (setq file (x-popup-menu
                (list (list (car mouse-coord) (cdr mouse-coord)) (selected-window))
                (list title (cons title files ))))
    (if file
        (find-file (concat dir file)))))

(defun rails-lib:goto-controllers ()
  "Goto Controller"
  (interactive)
  (rails-lib:goto-file-with-menu "app/controllers/" "Go to controller.."))

(defun rails-lib:goto-models ()
  "Goto Model"
  (interactive)
  (rails-lib:goto-file-with-menu "app/models/" "Go to model.."))

(defun rails-lib:goto-helpers ()
  "Goto helper"
  (interactive)
  (rails-lib:goto-file-with-menu "app/helpers/" "Go to helper.."))

(defun rails-lib:goto-layouts ()
  "Goto layout"
  (interactive)
  (rails-lib:goto-file-with-menu "app/views/layouts/" "Go to layout.." "rhtml" t))

(defun rails-lib:goto-stylesheets ()
  "Goto layout"
  (interactive)
  (rails-lib:goto-file-with-menu "public/stylesheets/" "Go to stylesheet.." "css" t))

(defun rails-lib:goto-javascripts ()
  "Goto layout"
  (interactive)
  (rails-lib:goto-file-with-menu "public/javascripts/" "Go to stylesheet.." "js" t))

(defun rails-lib:goto-migrate ()
  "Goto layout"
  (interactive)
  (rails-lib:goto-file-with-menu "db/migrate/" "Go to migrate.." "rb" t))


(provide 'rails-lib)