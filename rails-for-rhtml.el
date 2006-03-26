;;; rails-for-rhtml.el ---

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

(defun rails-rhtml:create-partial-from-selection ()
  "Create partial from selection"
  (interactive)
  (if (and mark-active
           transient-mark-mode)
      (save-excursion
        (let ((name (read-string "Partial name? "))
              (content (buffer-substring-no-properties (region-beginning) (region-end))))
          (kill-region (region-beginning) (region-end))
          (insert (concat "<%= render :partial => \"" name "\" %>"))
          (split-window-vertically)
          (other-window 1)
          (find-file (concat "_" name ".rhtml"))
          (goto-char (point-min))
          (insert content)
          (other-window -1)
          (mmm-parse-region (line-beginning-position) (line-end-position))))))

(defun rails-rhtml:create-helper-from-block (&optional helper-name)
  "Create helper function from current erb block (<% .. %>)"
  (interactive)
  (rails-core:with-root
   (root)
   (let ((current-pos (point))
         (file buffer-file-name)
         begin-pos end-pos)
     (save-excursion
       (setq begin-pos (search-backward "<%" nil t))
       (setq end-pos (search-forward "%>" nil t)))
     (if (and begin-pos
              end-pos
              (> current-pos begin-pos)
              (< current-pos end-pos)
              (string-match "app/views/\\(.*\\)/\\([a-z_]+\\)\.[a-z]+$" file))
         (let* ((helper-file (concat root "app/helpers/" (match-string 1 file) "_helper.rb"))
                (content (buffer-substring-no-properties begin-pos end-pos))
                (helper-alist (if helper-name helper-name (read-string "Enter helper function name with args: ")))
                (helper-alist (split-string helper-alist)))
           (if (file-exists-p helper-file)
               (let (start-point-in-helper helper-func-name)
                 (setq helper-func-name (concat "def " (car helper-alist) " ("))
                 (setq helper-alist (cdr helper-alist))
                 (mapcar (lambda (arg) (setq helper-func-name (concat helper-func-name arg ", "))) helper-alist)
                 (setq helper-func-name (concat (substring helper-func-name 0 -2) ")" ))
                 (kill-region begin-pos end-pos)
                 (insert (concat "<%= " helper-func-name " -%>" ))
                 (mmm-parse-region (line-beginning-position) (line-end-position))
                 (split-window-vertically)
                 (other-window 1)
                 (find-file helper-file)
                 (goto-char (point-min))
                 (search-forward-regexp "module +[a-zA-Z0-9:]+")
                 (newline)
                 (setq start-point-in-helper (point))
                 (insert helper-func-name)
                 (ruby-indent-command)
                 (newline)
                 (insert content)
                 (insert "\nend\n")
;;                  (while (and (re-search-forward "\\(<%=?\\|-?%>\\)" nil t)
;;                              (< (point) start-point-in-helper))
;;                    (replace-match "" nil nil))
                 (replace-regexp "\\(<%=?\\|-?%>\\)" "" nil start-point-in-helper (point))
                 (goto-char start-point-in-helper)
                 (ruby-indent-exp)
                 (other-window -1))
             (message "helper not found")))
       (message "block not found")))))

(defun rails-rhtml:switch-to-action()
  (interactive)
  (let ((file buffer-file-name))
    (rails-core:with-root
     (root)
     (if (string-match "app/views/\\(.*\\)/\\([a-z_]+\\)\.[a-z]+$" file)
         (let ((controller (concat root "app/controllers/" (match-string 1 file) "_controller.rb"))
               (action (match-string 2 file)))
           (if (file-exists-p controller)
               (let ((controller-class-name (rails-core:class-by-file controller)))
                 (find-file controller)
                 (goto-char (point-min))
                 (message (concat controller-class-name "#" action))
                 (if (search-forward-regexp (concat "^[ ]*def[ ]*" action))
                     (recenter)))))))))

(defun rails-for-rhtml ()
  (interactive)
  (local-set-key (kbd "C-c <up>") 'rails-rhtml:switch-to-action)
  (local-set-key (kbd "\C-c p") 'rails-rhtml:create-partial-from-selection)
  (local-set-key (kbd "\C-c b") 'rails-rhtml:create-helper-from-block))

(provide 'rails-for-rhtml)