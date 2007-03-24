;;; rails-scripts.el --- emacs-rails integraions with rails script/* scripts

;; Copyright (C) 2006 Galinsky Dmitry <dima dot exe at gmail dot com>

;; Authors: Galinsky Dmitry <dima dot exe at gmail dot com>,
;;          Rezikov Peter <crazypit13 (at) gmail.com>

;; Keywords: ruby rails languages oop
;; $URL: svn+ssh://crazypit@rubyforge.org/var/svn/emacs-rails/trunk/rails-core.el $
;; $Id: rails-navigation.el 23 2006-03-27 21:35:16Z crazypit $

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

(defun rails-script:run (script buffer parameters &optional message-format)
  "Run a Rails script with PARAMETERS in BUFFER using
MESSAGE-FORMAT to format the output."
  (rails-core:with-root
   (root)
   (let ((default-directory root))
     (rails-logged-shell-command
      (apply #'concat (format "script/%s " script)
             (mapcar #'(lambda (str)
                         (if str (concat str " ") ""))
                     parameters))
      buffer))
  (when message-format
    (message message-format (capitalize (first parameters))
             (second parameters)))))

;;;;;;;;;; Destroy stuff ;;;;;;;;;;

(defun rails-script:run-destroy (&rest parameters)
  "Run the destroy script using PARAMETERS."
  (rails-script:run "destroy" rails-script:generation-buffer-name
                    (append parameters rails-script:destroy-params-list)
                    "%s %s destroyed."))

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

;; (setq a (macroexpand
;;          '(rails-script:gen-destroy-function "controller" rails-core:controllers)))

;;;;;;;;;; Generators stuff ;;;;;;;;;;
(defun rails-script:run-generate (&rest parameters)
  "Run the generate script using PARAMETERS."
  (rails-script:run "generate"
                    rails-script:generation-buffer-name
                    (append parameters rails-script:generate-params-list)
                    "%s %s generated."))

(defun rails-script:generate (&optional what)
  "Run generate WHAT"
  (interactive (list (completing-read "What generate? (use autocomplete): " rails-script:generators-list)))
  (let ((name (intern (concat "rails-script:generate-" what))))
    (when (fboundp name)
      (call-interactively name))))

(defun rails-script:generate-controller (&optional controller-name actions)
  "Generate a controller and open the controller file."
  (interactive (list
                (completing-read "Controller name (use autocomplete) : "
                                 (list->alist (rails-core:controllers-ancestors)))
                (read-string "Actions (or return to skip): ")))
  (when (string-not-empty controller-name)
    (rails-script:run-generate "controller" controller-name actions)
    (rails-core:find-file-if-exist (rails-core:controller-file controller-name))))

(defun rails-script:generatemodel (&optional model-name)
  "Generate a model and open the model file."
  (interactive
   (list (completing-read "Model name: " (list->alist (rails-core:models-ancestors)))))
  (when (string-not-empty model-name)
    (rails-script:run-generate "model" model-name)
    (rails-core:find-file-if-exist (rails-core:model-file model-name))))

(defun rails-script:generatescaffold (&optional model-name controller-name actions)
  "Generate a scaffold and open the controller file."
  (interactive
   "MModel name: \nMController (or return to skip): \nMActions (or return to skip): ")
  (when (string-not-empty model-name)
    (if (string-not-empty controller-name)
        (progn
          (rails-script:run-generate "scaffold" model-name controller-name actions)
          (rails-core:find-file-if-exist (rails-core:controller-file controller-name)))
      (progn
        (rails-script:run-generate "scaffold" model-name)
        (rails-core:find-file-if-exist (rails-core:controller-file model-name))))))

(defun rails-script:generatemigration (migration-name)
  "Generate a migration and open the migration file."
  (interactive "MMigration name: ")
  (when (string-not-empty migration-name)
    (rails-script:run-generate "migration" migration-name)
    (rails-core:find-file-if-exist
     (save-excursion
       (set-buffer rails-script:generation-buffer-name)
       (goto-line 2)
       (search-forward-regexp "\\(db/migrate/[0-9a-z_]+.rb\\)")
       (match-string 1)))))

(defun rails-script:generateplugin (plugin-name)
  "Generate a plugin and open the init.rb file."
  (interactive "MPlugin name: ")
  (when (string-not-empty plugin-name)
    (rails-script:run-generate "plugin" plugin-name)
    (rails-core:find-file-if-exist (concat "vendor/plugins/" plugin-name "/init.rb"))))

(defun rails-script:generatemailer (mailer-name)
  "Generate a mailer and open the mailer file"
  (interactive "MMailer name: ")
  (when (string-not-empty mailer-name)
    (rails-script:run-generate "mailer" mailer-name)
    (rails-core:find-file-if-exist (concat (rails-core:model-file mailer-name)))))

(defun rails-script:generateobserver (observer-name)
  "Generate a observer and open the observer file"
  (interactive "MObserver name: ")
  (when (string-not-empty observer-name)
    (rails-script:run-generate "observer" observer-name)
    (unless (string-match "[Oo]bserver$" observer-name)
      (setq observer-name (concat observer-name "_observer")))
    (rails-core:find-file-if-exist (concat (rails-core:model-file observer-name)))))

(defun rails-script:generateresource (resource-name)
  "Generate a resource and open the resource file"
  (interactive "MResource name: ")
  (when (string-not-empty resource-name)
    (rails-script:run-generate "resource" resource-name)
    ;; pluralize bug
    (rails-core:find-file-if-exist (concat (rails-core:controller-file resource-name)))))

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