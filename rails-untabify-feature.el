;;; rails-untabify-feature.el ---

;; Copyright (C) 2006 Dmitry Galinsky <dima dot exe at gmail dot com>

;; Authors: Dmitry Galinsky <dima dot exe at gmail dot com>,

;; Keywords: ruby rails languages oop
;; $URL: svn+ssh://rubyforge/var/svn/emacs-rails/trunk/rails.el $
;; $Id: rails.el 149 2007-03-29 15:07:49Z dimaexe $

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

(require 'rails-project)

(defvar rails-untabify-feature:enabled-p nil)

(defvar rails-untabify-feature:exclude-list
  '("Makefile$"))

(defun rails-untabify-feature:untabify ()
  (when (and (eq this-command 'save-buffer)
             (not (find nil
                        rails-untabify-feature:exclude-list
                        :if #'(lambda (r)
                                (string-match r (buffer-name))))))
    (save-excursion
      (untabify (point-min) (point-max))
      (delete-trailing-whitespace))))

(defun rails-untabify-feature:install ()
  "Strip all trailing whitespaces and untabify buffer before
save."
  (add-hook 'find-file-hook
            (lambda ()
              (rails-project:with-root
               (root)
               (unless rails-untabify-feature:enabled-p
                 (set (make-local-variable 'rails-untabify-feature:enabled-p) t)
                 (add-hook 'local-write-file-hooks 'rails-untabify-feature:untabify))))))

(provide 'rails-untabify-feature)
