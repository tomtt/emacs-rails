;;; rails-rake.el --- emacs-rails integraions with rake tasks.

;; Copyright (C) 2006 Dmitry Galinsky <dima dot exe at gmail dot com>

;; Authors: Dmitry Galinsky <dima dot exe at gmail dot com>,

;; Keywords: ruby rails languages oop
;; $URL: svn+ssh://rubyforge/var/svn/emacs-rails/trunk/rails-scripts.el $
;; $Id: rails-scripts.el 117 2007-03-25 23:37:37Z dimaexe $

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

(eval-when-compile
  (require 'rails-scripts))

(defvar rails-rake:history (list))

(defvar rails-rake:tasks-regexp "^rake \\([^ ]*\\).*# \\(.*\\)"
  "Regexp to match tasks list in `rake --tasks` output.")

(defun rails-rake:create-tasks-cache (file-name)
  "Create a cache file from rake --tasks output."
  (let ((tasks (loop for str in (split-string (rails-cmd-proxy:shell-command-to-string "rake --tasks") "\n")
                     for task = (when (string-not-empty str)
                                  (string=~ rails-rake:tasks-regexp str $1))
                     when task collect task)))
    (write-string-to-file file-name (prin1-to-string tasks))
    tasks))

(defun rails-rake:tasks-list ()
  "Return all tasks list and create tasks cache file."
  (rails-core:in-root
   (let* ((cache-file (rails-core:file "tmp/.tasks-cache")))
     (if (file-exists-p cache-file)
         (read-from-file cache-file)
       (rails-rake:create-tasks-cache cache-file)))))

(defun rails-rake:task (task &optional major-mode)
  "Run a Rake task in RAILS_ROOT with MAJOR-MODE."
  (interactive (rails-completing-read "What task run" (rails-rake:tasks-list)
                                      'rails-rake:history nil))
  (when task
    (rails-script:run "rake" (list task) major-mode)))

(provide 'rails-rake)
