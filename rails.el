;;; rails.el --- minor mode for editing RubyOnRails code

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

(eval-when-compile
  (require 'speedbar)
  (require 'ruby-mode))

(require 'rails-core)
(require 'rails-lib)
(require 'rails-webrick)

(require 'ansi-color)
(require 'snippet)
(require 'etags)
(require 'find-recursive)

;;;;;;;;;; Variable definition ;;;;;;;;;;

(defvar rails-version "0.3")
(defvar rails-ruby-command "ruby")
(defvar rails-templates-list '("rhtml" "rxml" "rjs"))
(defvar rails-chm-file nil "Path to CHM file or nil")
(defvar rails-use-another-define-key nil )

(defvar rails-for-alist
  '(
    ("rhtml" rails-for-rhtml)
    ("rb" rails-for-controller (lambda (root) (string-match (concat root "app/controllers") buffer-file-name)))
    ))

;;;;;;;; hack ;;;;

;; replace in autorevert.el
(defun auto-revert-tail-handler ()
  (let ((size (nth 7 (file-attributes buffer-file-name)))
        (modified (buffer-modified-p))
        buffer-read-only    ; ignore
        (file buffer-file-name)
        buffer-file-name)   ; ignore that file has changed
    (when (> size auto-revert-tail-pos)
      (undo-boundary)
      (save-restriction
        (widen)
        (save-excursion
          (let ((cur-point (point-max)))
            (goto-char (point-max))
            (insert-file-contents file nil auto-revert-tail-pos size)
            (ansi-color-apply-on-region cur-point (point-max)))))
      (undo-boundary)
      (setq auto-revert-tail-pos size)
      (set-buffer-modified-p modified)))
  (set-visited-file-modtime))

;;;;;;;;;; Some init code ;;;;;;;;;;

(unless (boundp 'html-mode-abbrev-table)
  (setq html-mode-abbrev-table (make-abbrev-table)))
(unless (boundp 'html-helper-mode-abbrev-table)
  (setq html-helper-mode-abbrev-table (make-abbrev-table)))

;;;;;;;;;; Interface ;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Rails snips ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-snips (ruby-mode-abbrev-table)
  ("ra"      "render :action => \"$${action}\""
             "render (action)")
  ("ral"     "render :action => \"$${action}\", :layout => \"$${layoutname}\""
             "render (action,layout)")
  ("rf"      "render :file => \"$${filepath}\""
             "render (file)")
  ("rfu"     "render :file => \"$${filepath}\", :use_full_path => $${false}"
             "render (file,use_full_path)")
  ("ri"      "render :inline => \"$${<%= 'hello' %>}\""
             "render (inline)")
  ("ril"     "render :inline => \"$${<%= 'hello' %>}\", :locals => { $${name} => \"$${value}\" }"
             "render (inline,locals)")
  ("rit"     "render :inline => \"$${<%= 'hello' %>}\", :type => :$${rxml})"
             "render (inline,type)")
  ("rl"      "render :layout => \"$${layoutname}\""
             "render (layout)")
  ("rn"      "render :nothing => $${true}"
             "render (nothing)")
  ("rns"     "render :nothing => $${true}, :status => $${401}"
             "render (nothing,status)")
  ("rp"      "render :partial => \"$${item}\""
             "render (partial)")
  ("rpc"     "render :partial => \"$${item}\", :collection => $${items}"
             "render (partial,collection)")
  ("rpl"     "render :partial => \"$${item}\", :locals => { :$${name} => \"$${value}\"}"
             "render (partial,locals)")
  ("rpo"     "render :partial => \"$${item}\", :object => $${object}"
             "render (partial,object)")
  ("rps"     "render :partial => \"$${item}\", :status => $${500}"
             "render (partial,status)")
  ("rt"      "render :text => \"$${Text here...}\""
             "render (text)")
  ("rtl"     "render :text => \"$${Text here...}\", :layout => \"$${layoutname}\""
             "render (text,layout)")
  ("rtlt"    "render :text => \"$${Text here...}\", :layout => $${true}"
             "render (text,layout => true)")
  ("rts"     "render :text => \"$${Text here...}\", :status => $${401}"
             "")
  ("rcea"    "render_component :action => \"$${index}\""
             "render_component (action)")
  ("rcec"    "render_component :controller => \"$${items}\""
             "render_component (controller)")
  ("rceca"   "render_component :controller => \"$${items}\", :action => \"$${index}\""
             "render_component (controller, action)")
  ("rea"     "redirect_to :action => \"$${index}\""
             "redirect_to (action)")
  ("reai"    "redirect_to :action => \"$${show}\", :id => $${@item}"
             "redirect_to (action, id)")
  ("rec"     "redirect_to :controller => \"$${items}\""
             "redirect_to (controller)")
  ("reca"    "redirect_to :controller => \"$${items}\", :action => \"$${list}\""
             "redirect_to (controller, action)")
  ("recai"   "redirect_to :controller => \"$${items}\", :action => \"$${show}\", :id => $${@item}"
             "redirect_to (controller, action, id)")

  ;; Environment
  ("flash"   "flash[:$${notice}] = \"$${Text here...}\""
             "flash[...]")
  ("logi"    "logger.info \"$${Text here...}\""
             "logger.info")
  ("par"     "params[:$${id}]"
             "params[...]")
  ("ses"     "session[:$${user}]"
             "session[...]")

  ;; Models
  ("bt"      "belongs_to :$${model}, :class_name => \"$${class}\", :foreign_key => \"$${key}\""
             "belongs_to (class_name,foreign_key)")
  ("hm"      "has_many :$${model}, :class_name => \"$${class}\", :foreign_key => \"$${key}\", :dependent => :$${destroy}"
             "has_many (class_name,foreign_key,dependent)")
  ("ho"      "has_one :$${model}, :class_name => \"$${class}\", :foreign_key => \"$${key}\", :dependent => :$${destroy}"
             "has_one (class_name,foreign_key,dependent)")
  ("vp"      "validates_presence_of :$${attr}"
             "validates_presence_of")
  ("vu"      "validates_uniqueness_of :$${attr}"
             "validates_uniqueness_of")
  ("vn"     "validates_numericality_of :$${attr}"
             "validates_numericality_of")

  ;; Migrations
  ("mct"     "create_table :$${name} do |t|\n$>$.\nend"
             "create_table (table)")
  ("mctf"    "create_table :$${name}, :force => true do |t|\n$>$.\nend"
             "create_table (table, force)")
  ("mdt"     "drop_table :$${name}"
             "drop_table (table)")
  ("mtcl"    "t.column \"$${name}\", :$${type}"
             "t.column (column)")
  ("mac"     "add_column :$${table_name}, :$${column_name}, :$${type}"
             "add_column (table, column, type)")
  ("mcc"     "change_column :$${table_name}, :$${column_name}, :$${type}"
             "change_column (table, column, type)")
  ("mrec"    "rename_column :$${table_name}, :$${column_name}, :$${new_column_name}"
             "rename_column (table, name, new_name)")
  ("mrmc"    "remove_column :$${table_name}, :$${column_name}"
             "remove_column (table, column)")
  ("mai"     "add_index :$${table_name}, :$${column_name}"
             "add_index (table, column)")
  ("mait"    "add_index :$${table_name}, :$${column_name}, :$${index_type}"
             "add_index (table, column, type)")
  ("mrmi"    "remove_index :$${table_name}, :$${column_name}"
             "remove_index (table, column)"))

;;;; ERB Snips ;;;;

(def-snips (html-mode-abbrev-table html-helper-mode-abbrev-table)
  ("ft"      "<%= form_tag :action => \"$${update}\" %>\n$.\n<%= end_form_tag %>"
             "form_tag")
  ("lia"     "<%= link_to \"$${title}\", :action => \"$${index}\" %>"
             "link_to (action)")
  ("liai"    "<%= link_to \"$${title}\", :action => \"$${edit}\", :id => $${@item} %>"
             "link_to (action, id)")
  ("lic"     "<%= link_to \"$${title}\", :controller => \"$${items}\" %>"
             "link_to (controller)")
  ("lica"    "<%= link_to \"$${title}\", :controller => \"$${items}\", :action => \"$${index}\" %>"
             "link_to (controller, action)")
  ("licai"   "<%= link_to \"$${title}\", :controller => \"$${items}\", :action => \"$${edit}\", :id => $${@item} %>"
             "link_to (controller, action, id)")
  ("%h"      "<%=h $${@item} %>"
             "<% h ... %>")
  ("%if"     "<% if $${cond} -%>\n$.\n<% end -%>"
             "<% if/end %>")
  ("%ifel"   "<% if $${cond} -%>\n$.\n<% else -%>\n<% end -%>"
             "<% if/else/end %>")
  ("%unless" "<% unless $${cond} -%>\n$.\n<% end -%>"
             "<% unless/end %>")
  ("%"       "<% $. -%>"
             "<% ... %>")
  ("%%"      "<%= $. %>"
             "<%= ... %>"))

(defvar rails-minor-mode-menu-bar-map
  (let ((map (make-sparse-keymap)))
    (define-key map [rails] (cons "RubyOnRails" (make-sparse-keymap "RubyOnRails")))
    (define-key map [rails svn-status]
      '(menu-item "SVN status" rails-svn-status-into-root
                  :enable (rails-core:root)))
    (define-key map [rails tag] '("Update TAGS file" . rails-create-tags))
    (define-key map [rails ri] '("Search documentation" . rails-search-doc))
    (define-key map [rails separator] '("--"))

    (define-key map [rails snip] (cons "Snippets" (make-sparse-keymap "Snippets")))
    (define-key map [rails snip render] (cons "render" (make-sparse-keymap "render")))
    (define-key map [rails snip render sk-ra]  (snippet-menu-line 'ruby-mode-abbrev-table "ra"))
    (define-key map [rails snip render sk-ral] (snippet-menu-line 'ruby-mode-abbrev-table "ral"))
    (define-key map [rails snip render sk-rf]  (snippet-menu-line 'ruby-mode-abbrev-table "rf"))
    (define-key map [rails snip render sk-rfu] (snippet-menu-line 'ruby-mode-abbrev-table "rfu"))
    (define-key map [rails snip render sk-ri]  (snippet-menu-line 'ruby-mode-abbrev-table "ri"))
    (define-key map [rails snip render sk-ril] (snippet-menu-line 'ruby-mode-abbrev-table "ril"))
    (define-key map [rails snip render sk-rit] (snippet-menu-line 'ruby-mode-abbrev-table "rit"))
    (define-key map [rails snip render sk-rl]  (snippet-menu-line 'ruby-mode-abbrev-table "rl"))
    (define-key map [rails snip render sk-rn]  (snippet-menu-line 'ruby-mode-abbrev-table "rn"))
    (define-key map [rails snip render sk-rns] (snippet-menu-line 'ruby-mode-abbrev-table "rns"))
    (define-key map [rails snip render sk-rp]  (snippet-menu-line 'ruby-mode-abbrev-table "rp"))
    (define-key map [rails snip render sk-rpc] (snippet-menu-line 'ruby-mode-abbrev-table "rpc"))
    (define-key map [rails snip render sk-rpl] (snippet-menu-line 'ruby-mode-abbrev-table "rpl"))
    (define-key map [rails snip render sk-rpo] (snippet-menu-line 'ruby-mode-abbrev-table "rpo"))
    (define-key map [rails snip render sk-rps] (snippet-menu-line 'ruby-mode-abbrev-table "rps"))
    (define-key map [rails snip render sk-rt] (snippet-menu-line 'ruby-mode-abbrev-table "rt"))
    (define-key map [rails snip render sk-rtl] (snippet-menu-line 'ruby-mode-abbrev-table "rtl"))
    (define-key map [rails snip render sk-rtlt] (snippet-menu-line 'ruby-mode-abbrev-table "rtlt"))
    (define-key map [rails snip render sk-rcea] (snippet-menu-line 'ruby-mode-abbrev-table "rcea"))
    (define-key map [rails snip render sk-rcec] (snippet-menu-line 'ruby-mode-abbrev-table "rcec"))
    (define-key map [rails snip render sk-rceca] (snippet-menu-line 'ruby-mode-abbrev-table "rceca"))

    (define-key map [rails snip redirect_to] (cons "redirect_to" (make-sparse-keymap "redirect_to")))
    (define-key map [rails snip redirect_to sk-rea] (snippet-menu-line 'ruby-mode-abbrev-table "rea"))
    (define-key map [rails snip redirect_to sk-reai] (snippet-menu-line 'ruby-mode-abbrev-table "reai"))
    (define-key map [rails snip redirect_to sk-rec] (snippet-menu-line 'ruby-mode-abbrev-table "rec"))
    (define-key map [rails snip redirect_to sk-reca] (snippet-menu-line 'ruby-mode-abbrev-table "reca"))
    (define-key map [rails snip redirect_to sk-recai] (snippet-menu-line 'ruby-mode-abbrev-table "recai"))

    (define-key map [rails snip environment] (cons "environment" (make-sparse-keymap "environment")))
    (define-key map [rails snip environment sk-flash] (snippet-menu-line 'ruby-mode-abbrev-table "flash"))
    (define-key map [rails snip environment sk-logi] (snippet-menu-line 'ruby-mode-abbrev-table "logi"))
    (define-key map [rails snip environment sk-params] (snippet-menu-line 'ruby-mode-abbrev-table "par"))
    (define-key map [rails snip environment sk-session] (snippet-menu-line 'ruby-mode-abbrev-table "ses"))

    (define-key map [rails snip model] (cons "model" (make-sparse-keymap "model")))
    (define-key map [rails snip model sk-bt] (snippet-menu-line 'ruby-mode-abbrev-table "bt"))
    (define-key map [rails snip model sk-hm] (snippet-menu-line 'ruby-mode-abbrev-table "hm"))
    (define-key map [rails snip model sk-ho] (snippet-menu-line 'ruby-mode-abbrev-table "ho"))
    (define-key map [rails snip model sk-vp] (snippet-menu-line 'ruby-mode-abbrev-table "vp"))
    (define-key map [rails snip model sk-vu] (snippet-menu-line 'ruby-mode-abbrev-table "vu"))
    (define-key map [rails snip model sk-vn] (snippet-menu-line 'ruby-mode-abbrev-table "vn"))
    (define-key map [rails snip migrations] (cons "migrations" (make-sparse-keymap "model")))
    (define-key map [rails snip migrations mct] (snippet-menu-line 'ruby-mode-abbrev-table "mct"))
    (define-key map [rails snip migrations mctf] (snippet-menu-line 'ruby-mode-abbrev-table "mctf"))
    (define-key map [rails snip migrations mdt] (snippet-menu-line 'ruby-mode-abbrev-table "mdt"))
    (define-key map [rails snip migrations mtcl] (snippet-menu-line 'ruby-mode-abbrev-table "mtcl"))
    (define-key map [rails snip migrations mac] (snippet-menu-line 'ruby-mode-abbrev-table "mac"))
    (define-key map [rails snip migrations mcc] (snippet-menu-line 'ruby-mode-abbrev-table "mcc"))
    (define-key map [rails snip migrations mrec] (snippet-menu-line 'ruby-mode-abbrev-table "mrec"))
    (define-key map [rails snip migrations mrmc] (snippet-menu-line 'ruby-mode-abbrev-table "mrmc"))
    (define-key map [rails snip migrations mai] (snippet-menu-line 'ruby-mode-abbrev-table "mai"))
    (define-key map [rails snip migrations mait] (snippet-menu-line 'ruby-mode-abbrev-table "mait"))
    (define-key map [rails snip migrations mrmi] (snippet-menu-line 'ruby-mode-abbrev-table "mrmi"))

    (define-key map [rails snip rhtml] (cons "rhtml" (make-sparse-keymap "rhtml")))
    (define-key map [rails snip rhtml sk-erb-ft] (snippet-menu-line 'html-mode-abbrev-table "ft"))
    (define-key map [rails snip rhtml sk-erb-lia] (snippet-menu-line 'html-mode-abbrev-table "lia"))
    (define-key map [rails snip rhtml sk-erb-liai] (snippet-menu-line 'html-mode-abbrev-table "liai"))
    (define-key map [rails snip rhtml sk-erb-lic] (snippet-menu-line 'html-mode-abbrev-table "lic"))
    (define-key map [rails snip rhtml sk-erb-lica] (snippet-menu-line 'html-mode-abbrev-table "lica"))
    (define-key map [rails snip rhtml sk-erb-licai] (snippet-menu-line 'html-mode-abbrev-table "licai"))
    (define-key map [rails snip rhtml sk-erb-h] (snippet-menu-line 'html-mode-abbrev-table "%h"))
    (define-key map [rails snip rhtml sk-erb-if] (snippet-menu-line 'html-mode-abbrev-table "%if"))
    (define-key map [rails snip rhtml sk-erb-unless] (snippet-menu-line 'html-mode-abbrev-table "%unless"))
    (define-key map [rails snip rhtml sk-erb-ifel] (snippet-menu-line 'html-mode-abbrev-table "%ifel"))
    (define-key map [rails snip rhtml sk-erb-block] (snippet-menu-line 'html-mode-abbrev-table "%"))
    (define-key map [rails snip rhtml sk-erb-echo-block] (snippet-menu-line 'html-mode-abbrev-table "%%"))

    (define-key map [rails log] (cons "Open log" (make-sparse-keymap "Open log")))
    (define-key map [rails log test]
      '("test.log" . (lambda() (interactive) (rails-open-log "test"))))
    (define-key map [rails log pro]
      '("production.log" . (lambda() (interactive) (rails-open-log "production"))))
    (define-key map [rails log dev]
      '("development.log" . (lambda() (interactive) (rails-open-log "development"))))

    (define-key map [rails config] (cons "Configuration" (make-sparse-keymap "Configuration")))
    (define-key map [rails config routes]
      '("routes.rb" .
        (lambda()
          (interactive)
          (let ((root (rails-core:root)))
            (if root (find-file (concat root "config/routes.rb")))))))

    (define-key map [rails config environment]
      '("environment.rb" .
        (lambda()
          (interactive)
          (let ((root (rails-core:root)))
            (if root (find-file (concat root "config/environment.rb")))))))
    (define-key map [rails config database]
      '("database.yml" .
        (lambda()
          (interactive)
          (let ((root (rails-core:root)))
            (if root (find-file (concat root "config/database.yml")))))))
    (define-key map [rails config boot]
      '("boot.rb" .
        (lambda()
          (interactive)
          (let ((root (rails-core:root)))
            (if root (find-file (concat root "config/boot.rb")))))))

    (define-key map [rails config env] (cons "environments" (make-sparse-keymap "environments")))
    (define-key map [rails config env test]
      '("test.rb" .
        (lambda()
          (interactive)
          (let ((root (rails-core:root)))
            (if root (find-file (concat root "config/environments/test.rb")))))))
    (define-key map [rails config env production]
      '("production.rb" .
        (lambda()
          (interactive)
          (let ((root (rails-core:root)))
            (if root (find-file (concat root "config/environments/production.rb")))))))
    (define-key map [rails config env development]
      '("development.rb" .
        (lambda()
          (interactive)
          (let ((root (rails-core:root)))
            (if root (find-file (concat root "config/environments/development.rb")))))))

    (define-key map [rails webrick] (cons "WEBrick" (make-sparse-keymap "WEBrick")))

    (define-key map [rails webrick mongrel]
      '(menu-item "Use Mongrel" rails-webrick:toggle-use-mongrel
                  :enable (not (rails-webrick:status))
                  :button (:toggle
                           . (and (boundp 'rails-webrick:use-mongrel)
                                  rails-webrick:use-mongrel))))

    (define-key map [rails webrick separator] '("--"))

    (define-key map [rails brows] '("Open browser..." . rails-webrick:open-browser-on-controller))

    (define-key map [rails auto-brows]
      '("Open browser on current action" . rails-webrick:auto-open-browser))

    (define-key map [rails webrick url]
      '(menu-item "Open browser"
                  rails-webrick:open-browser
                  :enable (rails-webrick:status)))
    (define-key map [rails webrick stop]
      '(menu-item "Stop"
                  rails-webrick:stop
                  :enable (rails-webrick:status)))
    (define-key map [rails webrick test]
      '(menu-item "Start test"
                  (lambda() (interactive) (rails-webrick:start "test"))
                  :enable (not (rails-webrick:status))))
    (define-key map [rails webrick production]
      '(menu-item "Start production"
                  (lambda() (interactive) (rails-webrick:start "production"))
                  :enable (not (rails-webrick:status))))
    (define-key map [rails webrick development]
      '(menu-item "Start development"
                  (lambda() (interactive) (rails-webrick:start "development"))
                  :enable (not (rails-webrick:status))))

    (define-key map [rails separator2] '("--"))

    (define-key map [rails goto-models] '("Goto models" . rails-lib:goto-models))
    (define-key map [rails goto-controllers] '("Goto controllers" . rails-lib:goto-controllers))
    (define-key map [rails goto-helpers] '("Goto helpers" . rails-lib:goto-helpers))
    (define-key map [rails goto-layouts] '("Goto layouts" . rails-lib:goto-layouts))
    (define-key map [rails goto-stylesheets] '("Goto stylesheets" . rails-lib:goto-stylesheets))
    (define-key map [rails goto-javascripts] '("Goto javascripts" . rails-lib:goto-javascripts))
    (define-key map [rails goto-migrate] '("Goto migrate" . rails-lib:goto-migrate))
    map))

(define-keys rails-minor-mode-map
  ([menu-bar] rails-minor-mode-menu-bar-map)
  ;; Goto
  ((kbd "\C-c g m") 'rails-lib:goto-models)
  ((kbd "\C-c g c") 'rails-lib:goto-controllers)
  ((kbd "\C-c g h") 'rails-lib:goto-helpers)
  ((kbd "\C-c g l") 'rails-lib:goto-layouts)
  ((kbd "\C-c g s") 'rails-lib:goto-stylesheets)
  ((kbd "\C-c g j") 'rails-lib:goto-javascripts)
  ((kbd "\C-c g g") 'rails-lib:goto-migrate)

  ;; Browser
  ((kbd "\C-c <f5>") 'rails-webrick:auto-open-browser)
  ;;; Doc
  ([f1]  'rails-search-doc)
  ([f9]  'rails-svn-status-into-root))

(defun rails-svn-status-into-root ()
  (interactive)
  (rails-core:with-root (root)
                        (svn-status root)))

;; helper functions/macros
(defun rails-open-log (env)
  (rails-core:with-root
   (root)
   (if (file-exists-p (concat root "/log/" env ".log"))
       (progn
         (find-file (concat root "/log/" env ".log"))
         (set-buffer-file-coding-system 'utf-8)
         (ansi-color-apply-on-region (point-min) (point-max))
         (set-buffer-modified-p nil)
         (rails-minor-mode t)
         (goto-char (point-max))
         (setq auto-revert-interval 0.5)
         (auto-revert-set-timer)
         (setq auto-window-vscroll t)
         (auto-revert-tail-mode t)))))


(defun rails-search-doc (&optional item)
  (interactive)
  (setq item (if item item (thing-at-point 'sexp)))
  (unless item
    (setq item (read-string "Search symbol: ")))
  (if item
      (if (and rails-chm-file
               (file-exists-p rails-chm-file))
          (start-process "keyhh" "*keyhh*" "keyhh.exe" "-#klink"
                         (format "'%s'" item)  rails-chm-file)
        (let ((buf (buffer-file-name)))
          (unless (string= buf "*ri*")
            (switch-to-buffer-other-window "*ri*"))
          (setq buffer-read-only nil)
          (kill-region (point-min) (point-max))
          (message (concat "Please wait..."))
          (call-process "ri" nil "*ri*" t item)
          (setq buffer-read-only t)
          (local-set-key [return] 'rails-search-doc)
          (goto-char (point-min))))))

(defun rails-create-tags()
  "Create tags file"
  (interactive)
  (rails-core:with-root
   (root)
   (let ((dir default-directory)
         (cmd "ctags -e -a --Ruby-kinds=-f -o %s -R %s"))
    (message "Creating TAGS, please wait...")
    (setq default-directory root)
    (shell-command (format cmd tags-file-name (concat root "app")))
    (setq default-directory dir)
    (visit-tags-table tags-file-name))))

(defun rails-run-for-alist(root)
  (let ((ret nil)
        (alist rails-for-alist))
    (while (car alist)
      (let* ((it (car alist))
             (ext (concat "\\." (nth 0 it) "$"))
             (for-func (nth 1 it))
             (for-lambda (nth 2 it)))
        (if (string-match ext buffer-file-name)
            (progn
              (if (and for-lambda
                       (apply for-lambda (list root)))
                  (progn
                    (setq alist nil)
                    (require for-func)
                    (apply for-func nil)
                    (setq ret t)))
              (unless for-lambda
                (progn
                  (setq alist nil)
                  (require for-func)
                  (apply for-func nil)
                  (setq ret t))))))
      (setq alist (cdr alist)))
    ret))

(define-minor-mode rails-minor-mode
  "RubyOnRails"
  nil
  " RoR"
  rails-minor-mode-map

  (abbrev-mode -1)
  (make-local-variable 'tags-file-name)
  (setq tags-file-name (concat (rails-core:root) "TAGS")))

(add-hook 'ruby-mode-hook
          (lambda()
            (require 'rails-ruby)
            (local-set-key (kbd "C-.") 'complete-tag)
            (local-set-key (if rails-use-another-define-key (kbd "TAB") (kbd "<tab>")) 'ruby-indent-or-complete)
            (local-set-key (if rails-use-another-define-key (kbd "RET") (kbd "<return>")) 'ruby-newline-and-indent)))

(add-hook 'speedbar-mode-hook
          (lambda()
            (speedbar-add-supported-extension "\\.rb")))

(add-hook 'find-file-hooks
          (lambda()
            (rails-core:with-root
             (root)
             (progn
               (add-hook 'local-write-file-hooks
                         '(lambda()
                            (save-excursion
                              (untabify (point-min) (point-max))
                              (delete-trailing-whitespace))))
               (rails-minor-mode t)
               (rails-run-for-alist root)
               (local-set-key (if rails-use-another-define-key "TAB" (kbd "<tab>"))
                              '(lambda() (interactive)
                                 (if snippet
                                     (snippet-next-field)
                                   (if (looking-at "\\>")
                                       (hippie-expand nil)
                                     (indent-for-tab-command)))))))))

(provide 'rails)