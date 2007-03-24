;;; rails-remote-cmd.el ---

;; Copyright (C) 2006 Galinsky Dmitry <dima dot exe at gmail dot com>

;; Authors: Galinsky Dmitry <dima dot exe at gmail dot com>,
;;          Rezikov Peter <crazypit13 (at) gmail.com>

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

(defvar rails-cmd-proxy:directories-list
  (list
   (list "^y:" "/mnt/www" "-t @server-cmd")))

(defvar rails-cmd-proxy:remote-cmd
  "plink")

(defun rails-cmd-proxy:get-remote ()
  (rails-core:with-root
   (root)
   (loop for (local remote args) in rails-cmd-proxy:directories-list
         when (string-match local root)
         do (return
             (list (replace-regexp-in-string local remote root)
                   args)))))

(defun rails-cmd-proxy:apply-remote (path command command-args)
  (let (cmd)
    (setq cmd (concat cmd (format "cd %s && %s " path command)))
    (dolist (it (car command-args))
      (setq cmd (concat cmd " " it)))
    (concat "\"" cmd "\"")))

(defun rails-cmd-proxy:start-process (name buffer command command-args)
  (let ((remote (rails-cmd-proxy:get-remote)))
    (if remote
        (let* ((remote-path (car remote))
               (remote-args (car (cdr remote)))
               (remote-cmd-args (list remote-args
                                      (rails-cmd-proxy:apply-remote remote-path command command-args))))
           (start-process
                  name
                  buffer
                  rails-cmd-proxy:remote-cmd
                  remote-cmd-args))
      (start-process-shell-command name
                                   buffer
                                   command
                                   command-args))))

(provide 'rails-cmd-proxy)