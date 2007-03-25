;;; rails-scripts.el --- emacs-rails integraions with rails script/* scripts

;; Copyright (C) 2006 Dmitry Galinsky <dima dot exe at gmail dot com>

;; Authors: Dmitry Galinsky <dima dot exe at gmail dot com>,
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

(eval-when-compile
  (require 'inf-ruby)
  (require 'ruby-mode))

(defvar rails-script:generation-buffer-name "*RailsGeneration*")
(defvar rails-script:rake-tests-alist
  '(("all"         . "test")
    ("recent"      . "test:recent")
    ("units"       . "test:units")
    ("functionals" . "test:functionals")
    ("integraion"  . "test:integration")))

(defvar rails-script:rake-recent-test-alist nil)

(defvar rails-script:generators-list
  '("controller" "model" "scaffold" "migration" "plugin" "mailer" "observer" "resource"))

(defvar rails-script:destroy-list rails-script:generators-list)

(defvar rails-script:generate-params-list
  '("-f")
  "Add parameters to script/generate.
For example -s to keep existing files and -c to add new files into svn.")

(defvar rails-script:destroy-params-list
  '("-f")
  "Add parameters to script/destroy.
For example -c to remove files from svn.")

(defvar rails-script:buffer-name "*Rails Script*")
(defvar rails-script:running-script-name nil
  "Curently running the script name")

;; output-mode

(defconst rails-script:output-mode-font-lock-ketwords
  (list
   '(" \\(rm\\|rmdir\\) "                  1 font-lock-warning-face)
   '(" \\(missing\\|notempty\\|exists\\) " 1 font-lock-constant-face)
   '(" \\(create\\|dependency\\) "         1 font-lock-function-name-face)))

(defconst rails-script:output-mode-link-regexp
  " \\(create\\) + \\([^ ]+\\.\\w+\\)")

(defun rails-script:output-mode-make-links (start end len)
  (save-excursion
    (let ((buffer-read-only nil))
      (goto-char start)
      (while (re-search-forward rails-script:output-mode-link-regexp end t)
        (make-button (match-beginning 2) (match-end 2)
                     :type 'rails-button
                     :rails:file-name (match-string 2))))))

(define-derived-mode rails-script:output-mode fundamental-mode "Script Output"
  "Simple mode to Rails Script Output."
  (set (make-local-variable 'font-lock-defaults)
       '((rails-script:output-mode-font-lock-ketwords) nil t))
  (buffer-disable-undo)
  (rails-script:output-mode-make-links (point-min) (point-max) (point-max))
  (setq buffer-read-only t)
  (make-local-variable 'after-change-functions)
  (add-hook 'after-change-functions 'rails-script:output-mode-make-links))

(defun rails-script:running-p ()
  (get-buffer-process rails-script:buffer-name))

(defun rails-script:sentinel-proc (proc msg)
  (let ((name rails-script:running-script-name)
        (buf (current-buffer)))
    (when (memq (process-status proc) '(exit signal))
      (setq rails-script:running-script-name nil
            msg (format "%s was stopped (%s)." name msg)))
    (unless rails-script:running-script-name
      (unless (buffer-visible-p rails-script:buffer-name)
        (display-buffer rails-script:buffer-name t))
      (pop-to-buffer (get-buffer rails-script:buffer-name))
      (goto-char (point-min))
      (let ((button (next-button 1)))
        (if button
            (push-button (button-start button))
          (pop-to-buffer buf)))
      (shrink-window-if-larger-than-buffer (get-buffer-window rails-script:buffer-name)))
  (message
   (replace-regexp-in-string "\n" "" msg))))

(defun rails-script:run (command parameters)
  "Run a Rails script with PARAMETERS  using
MESSAGE-FORMAT to format the output."
  (rails-core:with-root
   (root)
   (let ((proc (rails-script:running-p)))
     (if proc
         (message "Only one instance rails-script allowed")
       (let* ((default-directory root)
              (proc (rails-cmd-proxy:start-process rails-script:buffer-name
                                                   rails-script:buffer-name
                                                   command
                                                   (strings-join " " parameters))))
         (with-current-buffer (get-buffer rails-script:buffer-name)
           (let ((buffer-read-only nil))
             (kill-region (point-min) (point-max))
             (goto-char (point-min)))
           (rails-script:output-mode))
         (setq rails-script:running-script-name
               (format "%s %s"
                       (first parameters)
                       (first (cdr parameters))))
         (set-process-sentinel proc 'rails-script:sentinel-proc)
         (message "Starting %s." rails-script:running-script-name))))))

;;;;;;;;;; Destroy stuff ;;;;;;;;;;

(defun rails-script:run-destroy (what &rest parameters)
  "Run the destroy script using WHAT and PARAMETERS."
  (rails-script:run rails-ruby-command
                    (append (list (format "script/destroy %s"  what))
                            parameters
                            rails-script:destroy-params-list)))

(defun rails-script:destroy (&optional what)
  "Run destroy WHAT"
  (interactive (list (completing-read "What destroy? (use autocomplete): " rails-script:destroy-list)))
  (let ((name (intern (concat "rails-script:destroy-" what))))
    (when (fboundp name)
      (call-interactively name))))

(defmacro rails-script:gen-destroy-function (name &optional completion completion-arg)
  (let ((func (intern (format "rails-script:destroy-%s" name)))
        (param (intern (concat name "-name"))))
    `(defun ,func (&optional ,param)
       (interactive
        (list (completing-read ,(concat "Destroy " name ": ")
                               ,(if completion
                                    `(list->alist
                                      ,(if completion-arg
                                           `(,completion ,completion-arg)
                                         `(,completion)))
                                  nil))))
       (when (string-not-empty ,param)
         (rails-script:run-destroy ,name ,param)))))

(rails-script:gen-destroy-function "controller" rails-core:controllers t)
(rails-script:gen-destroy-function "model"      rails-core:models)
(rails-script:gen-destroy-function "scaffold")
(rails-script:gen-destroy-function "migration"  rails-core:migrations)
(rails-script:gen-destroy-function "mailer"     rails-core:mailers)
(rails-script:gen-destroy-function "plugin"     rails-core:plugins)
(rails-script:gen-destroy-function "observer"   rails-core:observers)
(rails-script:gen-destroy-function "resource")

;;;;;;;;;; Generators stuff ;;;;;;;;;;

(defun rails-script:run-generate (what &rest parameters)
  "Run the generate script using WHAT and PARAMETERS."
  (rails-script:run rails-ruby-command
                    (append (list (format "script/generate %s" what))
                            parameters
                            rails-script:generate-params-list)))

(defun rails-script:generate (&optional what)
  "Run generate WHAT"
  (interactive (list (completing-read "What generate? (use autocomplete): " rails-script:generators-list)))
  (let ((name (intern (concat "rails-script:generate-" what))))
    (when (fboundp name)
      (call-interactively name))))

(defmacro rails-script:gen-generate-function (name &optional completion completion-arg)
  (let ((func (intern (format "rails-script:generate-%s" name)))
        (param (intern (concat name "-name"))))
    `(defun ,func (&optional ,param)
       (interactive
        (list (completing-read ,(concat "Generate " name ": ")
                               ,(if completion
                                    `(list->alist
                                      ,(if completion-arg
                                           `(,completion ,completion-arg)
                                         `(,completion)))
                                  nil))))
       (when (string-not-empty ,param)
         (rails-script:run-generate ,name ,param)))))


(defun rails-script:generate-controller (&optional controller-name actions)
  "Generate a controller and open the controller file."
  (interactive (list
                (completing-read "Controller name (use autocomplete) : "
                                 (list->alist (rails-core:controllers-ancestors)))
                (read-string "Actions (or return to skip): ")))
  (when (string-not-empty controller-name)
    (rails-script:run-generate "controller" controller-name actions)))

(defun rails-script:generate-scaffold (&optional model-name controller-name actions)
  "Generate a scaffold and open the controller file."
  (interactive
   "MModel name: \nMController (or return to skip): \nMActions (or return to skip): ")
  (when (string-not-empty model-name)
    (if (string-not-empty controller-name)
        (rails-script:run-generate "scaffold" model-name controller-name actions)
      (rails-script:run-generate "scaffold" model-name))))

(rails-script:gen-generate-function "model"     rails-core:models-ancestors)
(rails-script:gen-generate-function "migration")
(rails-script:gen-generate-function "plugin")
(rails-script:gen-generate-function "mailer")
(rails-script:gen-generate-function "observer")
(rails-script:gen-generate-function "resource")

;;;;;;;;;; Rails create project ;;;;;;;;;;

(defun rails-script:create-project (dir)
  "Create a new project in a directory named DIR."
  (interactive "FNew project directory: ")
  (shell-command (concat "rails " dir)
                 rails-script:generation-buffer-name)
  (flet ((rails-core:root () (concat dir "/") ))
    (rails-log-add
     (format "\nCreating project %s\n%s"
             dir (buffer-string-by-name rails-script:generation-buffer-name))))
  (find-file dir))

;;;;;;;;;; Shells ;;;;;;;;;;

(defun run-ruby-in-buffer (cmd buf)
  "Run CMD as a ruby process in BUF if BUF does not exist."
  (let ((abuf (concat "*" buf "*")))
    (if (not (comint-check-proc abuf))
  (set-buffer (make-comint buf rails-ruby-command nil cmd)))
    (inferior-ruby-mode)
    (make-local-variable 'inferior-ruby-first-prompt-pattern)
    (make-local-variable 'inferior-ruby-prompt-pattern)
    (setq inferior-ruby-first-prompt-pattern "^>> "
          inferior-ruby-prompt-pattern "^>> ")
    (pop-to-buffer abuf)))

(defun rails-interactive-buffer-name (name)
  "Return a buffer name in the format
*rails-<project-name>-<name>*."
  (format "rails-%s-%s" (rails-core:project-name) name))

(defun rails-run-interactive (name script)
  "Run an interactive shell with SCRIPT in a buffer named
*rails-<project-name>-<name>*."
  (rails-core:with-root
   (root)
   (run-ruby-in-buffer (rails-core:file script)
                       (rails-interactive-buffer-name name))
   (rails-minor-mode t)))

(defun rails-run-console ()
  "Run script/console."
  (interactive)
  (rails-run-interactive "console" "script/console"))

(defun rails-run-breakpointer ()
  "Run script/breakpointer."
  (interactive)
  (rails-run-interactive "breakpointer" "script/breakpointer"))

;;;; Rake ;;;;

(defun rails-rake-create-cache (file-name)
  "Create a cache file from rake --tasks output."
  (write-string-to-file file-name
   (prin1-to-string
    (loop for str in (split-string (shell-command-to-string "rake --tasks") "\n")
          for task = (when (string-not-empty str)
                       (string-match "^rake \\([^ ]*\\).*# \\(.*\\)" str)
                       (match-string 1 str))
          when task collect task))))

(defun rails-rake-tasks ()
  "Return all tasks in the main Rails Rakefile."
  (rails-core:in-root
   (let ((cache-file (rails-core:file ".rake-tasks-cache")))
     (unless (file-exists-p cache-file)
       (rails-rake-create-cache cache-file))
     (read-from-file cache-file))))

(defun rails-rake (&optional task message)
  "Run a Rake task in RAILS_ROOT."
  (interactive (list (completing-read "Rake task (use autocomplete): " (list->alist (rails-rake-tasks)))))
  (rails-core:in-root
   (save-some-buffers)
   (message (or message (format "Running rake task \"%s\"" task)))
   (shell-command (concat "rake " task) "*Rails Rake Output*" "*Rails Rake Errors*")))

(defun rails-rake-tests (&optional what)
  "Run Rake tests in RAILS_ROOT."
  (interactive (list (completing-read (concat "What test run?"
                                              (when rails-script:rake-recent-test-alist
                                                (concat " (" rails-script:rake-recent-test-alist  ")") )
                                              ": ")
                                      rails-script:rake-tests-alist
                                      nil nil nil nil
                                      (caar rails-script:rake-tests-alist))))
  (unless what
    (setq what rails-script:rake-recent-test-alist))
  (when what
    (let ((task (cdr (assoc what rails-script:rake-tests-alist))))
      (setq rails-script:rake-recent-test-alist what)
      (make-local-variable 'compilation-error-regexp-alist)
      (save-excursion
        (save-some-buffers)
        (setq default-directory (rails-core:root))
        (compile (format "rake %s" task))))))

(provide 'rails-scripts)