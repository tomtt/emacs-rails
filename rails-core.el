;;; rails-core.el --- core helper functions and macros for emacs-rails

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

(defun rails-core:root ()
  "Return RAILS_ROOT if this file is a part of rails
   application, else return nil"
  (let ((curdir default-directory)
        (max 10)
        (found nil))
    (while (and (not found) (> max 0))
      (progn
        (if (file-exists-p (concat curdir "config/environment.rb"))
            (progn
              (setq found t))
          (progn
            (setq curdir (concat curdir "../"))
            (setq max (- max 1))))))
    (if found (expand-file-name curdir))))

(defmacro* rails-core:with-root ((root) &body body)
 "If you use ``rails-core:root'' or functions ralated on it
  several times in block of code you can optimize you code by using
  this macro. Also block of code will be executed only if rails-root exist.
 (rails-core:with-root (root)
    (foo root)
    (bar (rails-file \"some/path\")))
 "
 `(let ((,root (rails-core:root)))
    (when ,root
      (flet ((rails-core:root () ,root))
        ,@body))))

(defun rails-core:class-by-file (filename)
  "Return Class associated for FILENAME"
  (let* ((case-fold-search nil)
         (path (capitalize (replace-regexp-in-string "\\(.*app/\\(controllers\\|models\\)/\\)?\\([^\.]+\\)\\(.*\\)?" "\\3" filename)))
         (path (replace-regexp-in-string "/" "::" path)))
    (replace-regexp-in-string "_" "" path)))

(defun rails-core:file-by-class (classname &optional do-not-append-ext)
  "Return filename associated for CLASSNAME,
   if optional parameter DO-NOT-APPEND-EXT is set
   this function not append \".rb\" to result"
  (let* ((case-fold-search nil)
         (path (replace-regexp-in-string "::" "/" classname))
         (path (replace-regexp-in-string "\\([A-Z]+\\)\\([A-Z][a-z]\\)" "\\1_\\2" path))
         (path (replace-regexp-in-string "\\([a-z\\d]\\)\\([A-Z]\\)" "\\1_\\2" path)))
    (concat (downcase path)
            (unless do-not-append-ext ".rb"))))

(defun rails-core:get-controller-file (controller-class)
  "Return file contains controller CONTROLLER-CLASS"
  (let ((file (rails-core:file-by-class controller-class))
        (root (rails-core:root)))
    (if (file-exists-p (concat root "app/controllers/" file))
        (concat root "app/controllers/" file))))

(defun rails-core:get-model-file (model-class)
  "Return file contains model MODEL-CLASS"
  (let ((file (rails-core:file-by-class model-class))
        (root (rails-core:root)))
    (if (file-exists-p (concat root "app/models/" file))
        (concat root "app/models/" file))))

(defun rails-core:get-view-files (controller-class action)
  "Retrun list contains views for CONTROLLER-CLASS#ACTON"
  (let ((controller (rails-core:file-by-class controller-class)))
    (rails-core:with-root
     (root)
     (directory-files (concat root
                              "app/views/"
                              (replace-regexp-in-string "_controller\\.rb$" "/"
                                                        controller))
                      t
                      (concat "^" action (rails-core:regex-for-match-view))))))

(defun rails-core:regex-for-match-view ()
  "Return regex to match rails view templates.
   File extension for view located in rails-templates-list"
  (let ((reg-string "\\.\\("))
    (mapcar (lambda (it) (setq reg-string (concat reg-string it "\\|"))) rails-templates-list)
    (concat (substring reg-string 0 -1) ")$")))

(defmacro rails-core:add-to-rails-menubar (item &rest prefix)
  "Add to ITEM local rails menubar
   ITEM is (cons \"Menu title\" 'func)"
  `(local-set-key [menu-bar file ,@prefix]  ,item))

(provide 'rails-core)
