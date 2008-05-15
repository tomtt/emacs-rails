;;; rails-shoulda.el --- emacs-rails integraions with should plugin.

;; Copyright (C) 2006 Dmitry Galinsky <dima dot exe at gmail dot com>
;; Authors: Rob Christie  <robchristie at gmail dot com>,
;; Keywords: ruby rails languages oop

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

(defun rails-shoulda:current-test ()
  "Return the test name based on point"
  (save-excursion
    (ruby-end-of-block)
    (let ((should (when (search-backward-regexp "^[ ]*should \"\\([a-z0-9_ ]+\\)\"[ ]*do" nil t)
                    (match-string-no-properties 1)))
          (context (when (search-backward-regexp "^[ ]*context \"\\([a-z0-9_ ]+\\)\"[ ]*do" nil t)
                     (match-string-no-properties 1))))
      (when (and should context)
        (concat context " should " should)))))

(defun rails-shoulda:current-context ()
  "Return the shoulda context name based on point"
  (save-excursion
    (ruby-end-of-block)
    (when (search-backward-regexp "^[ ]*context \"\\([a-z0-9_ ]+\\)\"[ ]*do" nil t)
      (match-string-no-properties 1))))

(defun rails-shoulda:run-current-should ()
  "Run should assertion based on the location of point."
  (interactive)
  (let ((file (substring (buffer-file-name) (length (rails-project:root))))
        (method (replace-regexp-in-string "[\+\. \'\"\(\)]" "." (rails-shoulda:current-test))))
    (when method
      (rails-test:run-single-file file (format "--name=/%s/" method)))))

(defun rails-shoulda:run-current-context ()
  "Run tests associated with the context based on the location of point."
  (interactive)
  (let ((file (substring (buffer-file-name) (length (rails-project:root))))
        (method (replace-regexp-in-string "[\+\. \'\"\(\)]" "." (rails-shoulda:current-context))))
    (when method
      (rails-test:run-single-file file (format "--name=/%s/" method)))))

(provide 'rails-shoulda)
