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

(defun rails-controller:get-current-controller-and-action ()
  "Return list contains current controller and action"
  (save-excursion
    (let (action controller)
      (goto-char (line-end-position))
      (search-backward-regexp "^[ ]*def \\([a-z_]+\\)" nil t)
      (setq action (match-string-no-properties 1))
      (search-backward-regexp "^[ ]*class \\([a-zA-Z0-9_:]+\\)[ ]+<" nil t)
      (setq controller (match-string-no-properties 1))
      (list controller action))))

(defun rails-controller:switch-to-view()
  (interactive)
  (let* ((calist (rails-controller:get-current-controller-and-action))
         (controller (nth 0 calist))
         (action (nth 1 calist)))

    (if action
        (let ((root (rails-core:root))
              files)
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
                    (find-file (format "%s/%s.rhtml" file action)))))))))

(defun rails-controller:switch-with-menu ()
  (interactive)
  (let* ((root (rails-core:root))
         (menu (list))
         (views (list))
         (calist (rails-controller:get-current-controller-and-action))
         (controller (nth 0 calist))
         (action (nth 1 calist))
         (files (rails-core:get-view-files controller nil))
         (helper (rails-core:helper-file controller))
         (test (rails-core:functional-test-file controller))
         file pos)
    (add-to-list 'menu (cons "Functional test" (concat root test)))
    (add-to-list 'menu (cons "Helper" (concat root helper)))
    (while (car files)
      (add-to-list 'menu (cons (replace-regexp-in-string "\\(.*/\\)\\([^/]+\\)$" "View\: \\2" (car files)) (car files)))
      (setq files (cdr files)))
    (setq pos (if (functionp 'posn-at-point) ; mouse position at point
                  (nth 2 (posn-at-point))
                (cons 200 100)))
    (setq file
          (x-popup-menu
           (list (list (car pos) (cdr pos)) (selected-window))
           (list "Please select.." (cons "Please select.." menu ))))
    (if file
        (find-file file))))

(defun rails-for-controller ()
  (interactive)
  (setq rails-secondary-switch-func 'rails-controller:switch-with-menu)
  (setq rails-primary-switch-func 'rails-controller:switch-to-view))


(defun rails-for-controller:views-for-current-action ()
  (mapcar (lambda (view-file)
	    (list (replace-regexp-in-string "\\(.*/\\)\\([^/]+\\)$" "View\: \\2" view-file)
		  view-file))
	  (rails-core:get-view-files (rails-core:current-controller)
				     (rails-core:current-action))))

(defun rails-for-controller:switch-by-current-controller (to-what file-func)
  (let ((controller (rails-core:current-controller)))
    (rails-core:find-or-ask-to-create
     (format "%s for controller %s does not exist, create it? " to-what controller)
     (funcall file-func controller))))

(defun rails-for-controller:switch-to-functional-test ()
  (rails-for-controller:switch-by-current-controller
   "Functional test" 'rails-core:functional-test-file))

(defun rails-for-controller:switch-to-helper ()
  (rails-for-controller:switch-by-current-controller
   "Helper file" 'rails-core:helper-file))

(defun rails-for-controller:switch-to-view2 ()
  (rails-core:open-controller+action
   :view (rails-core:current-controller) (rails-core:current-action)))

(defun rails-for-controller:switch-to-controller ()
  (rails-core:open-controller+action
   :controller (rails-core:current-controller) nil))

(defun rails-for-controller:switch-to-views ()
  (rails-core:open-controller+action
   :view (rails-core:current-controller) nil))


(provide 'rails-for-controller)
