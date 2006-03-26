;;; rails-for-controller.el ---

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

(defun rails-controller:switch-to-view()
  (interactive)
  (save-excursion
    (let ((root (rails-core:root))
          action controller files)
      (goto-char (line-end-position))
      (search-backward-regexp "^[ ]*def \\([a-z_]+\\)")
      (setq action (match-string 1))
      (search-backward-regexp "^[ ]*class \\([a-zA-Z0-9_:]+\\)[ ]+<")
      (setq controller (match-string 1))
      (setq files (rails-core:get-view-files controller action))

      (if (= 1 (list-length files)) ;; one view
          (progn
            (find-file (car files))
            (message (concat controller "#" action))))

      (if (< 1 (list-length files)) ;; multiple views
          (let ((items (list))
                (tmp files)
                (mouse-coord (if (functionp 'posn-at-point) ; mouse position at point
                                 (nth 2 (posn-at-point))
                               (cons 200 100)))
                file)
            (while (car tmp)
              (add-to-list 'items (cons (replace-regexp-in-string "\\(.*/\\)\\([^/]+\\)$" "\\2" (car tmp)) (car tmp)))
              (setq tmp (cdr tmp)))

            (setq file
                  (x-popup-menu
                   (list (list (car mouse-coord) (cdr mouse-coord)) (selected-window))
                   (list "Please select.." (cons "Please select.." items ))))
            (if file
                (progn
                  (find-file file)
                  (message (concat controller "#" action))))))

      (if (> 1 (list-length files)) ;; view not found
          (if (y-or-n-p (format "View for %s#%s not found, create %s.rhtml? " controller action action))
              (let ((file (concat root "app/views/"
                                  (replace-regexp-in-string "_controller" ""
                                                            (rails-core:file-by-class controller t)))))
                (make-directory file t)
                (find-file (format "%s/%s.rhtml" file action))))))))


(defun rails-for-controller ()
  (interactive)
  (local-set-key (kbd "C-c <up>") 'rails-controller:switch-to-view))

(provide 'rails-for-controller)