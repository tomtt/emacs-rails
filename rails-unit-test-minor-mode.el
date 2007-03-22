;;; rails-unit-test-minor-mode.el --- minor mode for RubyOnRails unit tests

;; Copyright (C) 2006 Galinsky Dmitry <dima dot exe at gmail dot com>

;; Keywords: ruby rails languages oop
;; $URL: svn+ssh://rubyforge/var/svn/emacs-rails/trunk/rails-for-rhtml.el $
;; $Id: rails-for-rhtml.el 58 2006-12-17 21:47:39Z dimaexe $

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

(defun rails-unit-test-minor-mode:switch-to-model ()
  (interactive)
  (rails-core:find-file-if-exist
   (rails-core:model-file (rails-core:current-model))))

(defun rails-unit-test-minor-mode:switch-with-menu ()
  (interactive)
  (let* ((item)
         (model (rails-core:current-model))
         (model-file (rails-core:model-file model)))
    (setq item
          (rails-core:menu
           (list (concat "Unit test " model)
                 (cons "Please select.."
                       (list (cons "Model" model-file))))))
    (when item
      (rails-core:find-file-if-exist item))))

(define-minor-mode rails-unit-test-minor-mode
  "Minor mode for RubyOnRails unit tests."
  nil
  " unit-test"
  nil
  (setq rails-primary-switch-func 'rails-unit-test-minor-mode:switch-to-model)
  (setq rails-secondary-switch-func 'rails-unit-test-minor-mode:switch-with-menu))

(provide 'rails-unit-test-minor-mode)