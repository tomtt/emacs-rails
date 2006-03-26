;;; rails-webkick.el --- functions for manadge webrick

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

(defvar rails-webrick:buffer-name "*WEBrick*")
(defvar rails-webrick:port "3000")
(defvar rails-webrick:default-env "development")
(defvar rails-webrick:open-url (concat "http://localhost:" rails-webrick:port))
(defvar rails-webrick:use-mongrel nil "Non nil using Mongrel, else WEBrick")

(defun rails-webrick:status()
  "Return t if webrick process is running"
  (let ((status (get-buffer-process rails-webrick:buffer-name)))
    (if status t nil)))

(defun rails-webrick:sentinel-proc (proc msg)
  (if (memq (process-status proc) '(exit signal))
      (message
       (concat
        (if rails-webrick:use-mongrel "Mongrel" "WEBrick") " stopped"))))

(defun rails-webrick:toggle-use-mongrel()
  "Toggle rails-webrick:use-mongrel on/off"
  (interactive)
  (setq rails-webrick:use-mongrel (not rails-webrick:use-mongrel)))

(defun rails-webrick:start(&optional env)
  "Start Webrick process with ENV environment
   if ENV not set using rails-webrick:default-env"
  (rails-core:with-root
   (root)
   (let ((proc (get-buffer-process rails-webrick:buffer-name))
         (dir default-directory))
     (unless proc
       (progn
         (setq default-directory root)
         (unless env
           (setq env (rails-webrick:default-env)))
         (if rails-webrick:use-mongrel
             (setq proc
                   (apply 'start-process-shell-command
                          "mongrel_rails"
                          rails-webrick:buffer-name
                          "mongrel_rails"
                          (list "start" "0.0.0.0" rails-webrick:port)))
           (setq proc
                 (apply 'start-process-shell-command
                        rails-ruby-command
                        rails-webrick:buffer-name
                        rails-ruby-command
                        (list (concat root "script/server")
                              (concat " -e " env)
                              (concat " -p " rails-webrick:port)))))
         (set-process-sentinel proc 'rails-webrick:sentinel-proc)
         (setq default-directory dir)
         (message (format "%s (%s) starting with port %s"
                          (if rails-webrick:use-mongrel "Mongrel" "Webrick")
                          env
                          rails-webrick:port)))))))

(defun rails-webrick:stop()
  "Stop Webrick process"
  (interactive)
  (let ((proc (get-buffer-process rails-webrick:buffer-name)))
    (if proc
        (kill-process proc))))

(defun rails-webrick:open-browser()
  (interactive)
  (browse-url rails-webrick:open-url))

(provide 'rails-webrick)