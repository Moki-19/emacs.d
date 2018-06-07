;;; init.el --- Loic's Emacs configuration file
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentary:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:
;;
;;
;;--------------------------------------------------------------------
;; VARIABLES DECLARATION
;;--------------------------------------------------------------------
;; Internal
(defvar poa-rebuild)
(defvar cscope-update-db-path)
(defvar eslint-path)
(defvar chromium-path)

;; External
(defvar mac-option-key-is-meta)
(defvar mac-right-option-modifier)
(defvar cscope-option-do-not-update-database)
(defvar flycheck-check-syntax-automatically)
(defvar tramp-default-method)
(defvar yascroll:delay-to-hide)
(defvar recentf-max-menu-items)
(defvar gud-gdb-command-name)
(defvar buffer-move-behavior)
(defvar dashboard-items)
(defvar js2-mode-show-parse-errors)
(defvar js2-mode-show-strict-warnings)

;;--------------------------------------------------------------------
;; LISP
;;--------------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/lisp/")

;;--------------------------------------------------------------------
;; PACKAGES
;;--------------------------------------------------------------------
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;;--------------------------------------------------------------------
;; WIN conf
;;--------------------------------------------------------------------
(when (eq system-type 'windows-nt)
  (setq find-program "C:\\msys64\\usr\\bin\\find.exe")
  (defvar cscope-update-db-path "F:/Travail/Scripts/update_cscope.bat")
  (defvar poa-rebuild "F:/Travail/Scripts/rebuild_postop.bat")
  (defvar eslint-path "node_modules/.bin/eslint.cmd")
  (defvar chromium-path "C:/Users/loic/AppData/Local/Chromium/Application/chrome.exe"))

;;--------------------------------------------------------------------
;; MAC conf
;;--------------------------------------------------------------------
(when (eq system-type 'darwin)
  (load "osx_gud.el")
  (setq exec-path (append '("/usr/local/bin")
                          exec-path))
  (setq exec-path (append '("/Users/loic/.emacs.d/bin")
                          exec-path))
  (defvar cscope-update-db-path "/Users/Loic/Travail/Scripts/update_cscope")
  (defvar poa-rebuild "/Users/Loic/Travail/Scripts/rebuild_postop")
  (defvar eslint-path "node_modules/eslint/bin/eslint.js")
  (defvar chromium-path "chrome")
  (setq mac-option-key-is-meta t)
  (setq mac-right-option-modifier nil)
  (exec-path-from-shell-initialize))

;;--------------------------------------------------------------------
;; C++
;;--------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;;--------------------------------------------------------------------
;; PHP
;;--------------------------------------------------------------------
;;(require 'php-mode)
(autoload 'geben "geben" "DBGp protocol frontend, a script debugger" t)

;;--------------------------------------------------------------------
;; WEB
;;--------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))

(add-hook 'js2-mode-hook #'js2-refactor-mode)
(add-hook 'xref-backend-functions #'xref-js2--xref-backend t)
(add-hook 'js2-mode-hook (lambda ()
 			   (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

;; Disable JS2 syntax checking as we use flycheck one
(setq js2-mode-show-parse-errors nil)
(setq js2-mode-show-strict-warnings nil)

;; Refactor key bindings
(js2r-add-keybindings-with-prefix "C-c r")

;; js-mode (which js2 is based on) binds "M-." which conflicts with xref
;; and tide, so unbind it.
(define-key js-mode-map (kbd "M-.") nil)
(define-key js2-mode-map (kbd "M-.") nil)

;; Intelligent kill zone
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;; Disable completion keybindings, as we use xref-js2 instead
;;(require 'tern)
;;(define-key tern-mode-keymap (kbd "M-.") nil)
;;(define-key tern-mode-keymap (kbd "M-,") nil)

;; Load add-node-modules-path
;; 18/05/2018 : hangs with with last version of js2-mode, so replace it
;; by use-eslint-from-node-modules function (see FLYCHECK part)
;;(eval-after-load 'js2-mode
;;  '(add-hook 'js2-mode-hook #'add-node-modules-path))

;; No smart indentation on continuation lines
;; https://emacs.stackexchange.com/questions/29973/stop-javascript-mode-from-lining-up-function-parameters-after-newline/34534#34534
(defun js--proper-indentation-custom (parse-status)
  "Return the proper indentation for the current line according to PARSE-STATUS argument."
  (save-excursion
    (back-to-indentation)
    (cond ((nth 4 parse-status)    ; inside comment
           (js--get-c-offset 'c (nth 8 parse-status)))
          ((nth 3 parse-status) 0) ; inside string
          ((eq (char-after) ?#) 0)
          ((save-excursion (js--beginning-of-macro)) 4)
          ;; Indent array comprehension continuation lines specially.
          ((let ((bracket (nth 1 parse-status))
                 beg)
             (and bracket
                  (not (js--same-line bracket))
                  (setq beg (js--indent-in-array-comp bracket))
                  ;; At or after the first loop?
                  (>= (point) beg)
                  (js--array-comp-indentation bracket beg))))
          ((js--ctrl-statement-indentation))
          ((nth 1 parse-status)
           ;; A single closing paren/bracket should be indented at the
           ;; same level as the opening statement. Same goes for
           ;; "case" and "default".
           (let ((same-indent-p (looking-at "[]})]"))
                 (switch-keyword-p (looking-at "default\\_>\\|case\\_>[^:]"))
                 (continued-expr-p (js--continued-expression-p))
                 (original-point (point))
                 (open-symbol (nth 1 parse-status)))
             (goto-char (nth 1 parse-status)) ; go to the opening char
             (skip-syntax-backward " ")
             (when (eq (char-before) ?\)) (backward-list))
             (back-to-indentation)
             (js--maybe-goto-declaration-keyword-end parse-status)
             (let* ((in-switch-p (unless same-indent-p
                                   (looking-at "\\_<switch\\_>")))
                    (same-indent-p (or same-indent-p
                                       (and switch-keyword-p
                                            in-switch-p)))
                    (indent
                     (cond (same-indent-p
                            (current-column))
                           (continued-expr-p
                            (goto-char original-point)
                            ;; Go to beginning line of continued expression.
                            (while (js--continued-expression-p)
                              (forward-line -1))
                            ;; Go to the open symbol if it appears later.
                            (when (> open-symbol (point))
                              (goto-char open-symbol))
                            (back-to-indentation)
                            (+ (current-column)
                               js-indent-level
                               js-expr-indent-offset))
                           (t
                            (+ (current-column) js-indent-level
                               (pcase (char-after (nth 1 parse-status))
                                 (?\( js-paren-indent-offset)
                                 (?\[ js-square-indent-offset)
                                 (?\{ js-curly-indent-offset)))))))
               (if in-switch-p
                   (+ indent js-switch-indent-offset)
                 indent))))
          ((js--continued-expression-p)
           (+ js-indent-level js-expr-indent-offset))
          (t 0))))

(advice-add 'js--proper-indentation :override 'js--proper-indentation-custom)

;;--------------------------------------------------------------------
;; TIDE
;;--------------------------------------------------------------------
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1)
  (local-set-key (kbd "M-?") 'tide-references))

(add-hook 'js2-mode-hook #'setup-tide-mode)

;;--------------------------------------------------------------------
;; YAML
;;--------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;;--------------------------------------------------------------------
;; OBJECTIVE-C
;;--------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.mm\\'" . c++-mode))

;;--------------------------------------------------------------------
;; JAVA
;;--------------------------------------------------------------------
(add-hook 'java-mode-hook (function cscope-minor-mode))

;;--------------------------------------------------------------------
;; MAKEFILE
;;--------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.mingw\\'" . makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("\\.osx\\'" . makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("\\.common\\'" . makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("\\.gcc\\'" . makefile-gmake-mode))

;;--------------------------------------------------------------------
;; DSVN
;;--------------------------------------------------------------------
(autoload 'svn-status "dsvn" "Run `svn status'." t)
(autoload 'svn-update "dsvn" "Run `svn update'." t)

;;--------------------------------------------------------------------
;; CSCOPE
;;--------------------------------------------------------------------
(cscope-setup)
(setq cscope-option-do-not-update-database t)

(defun cscope-update-db()
      "Update cscope database (external script)"
      (interactive)
      (async-shell-command cscope-update-db-path))

;;--------------------------------------------------------------------
;; FLYCHECK
;;--------------------------------------------------------------------
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-check-syntax-automatically nil)

;; Use eslint with js2-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'js2-mode)

;; Use local eslint
(defun use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name eslint-path root))))
    (when (and eslint (file-exists-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(add-hook 'flycheck-mode-hook #'use-eslint-from-node-modules)

;;--------------------------------------------------------------------
;; TRAMP
;;--------------------------------------------------------------------
(when (eq system-type 'windows-nt)
  (setq tramp-default-method "plinkx"))

;;--------------------------------------------------------------------
;; FONT
;;--------------------------------------------------------------------
(if (> (x-display-pixel-width) 1280)
    (set-face-attribute 'default nil :height (if (eq system-type 'windows-nt) 110 120))
  (set-face-attribute 'default nil :height (if (eq system-type 'windows-nt) 100 110)))
  
(set-face-font 'default "-outline-Consolas-normal-r-normal-normal-*-*-96-96-c-*-iso8859-1")
(set-face-font 'bold "-outline-Consolas-bold-r-normal-normal-*-*-96-96-c-*-iso8859-1")
(set-face-font 'italic "-outline-Consolas-normal-i-normal-normal-*-*-96-96-c-*-iso8859-1")
(set-face-font 'bold-italic "-outline-Consolas-bold-i-normal-normal-*-*-96-96-c-*-iso8859-1")

;;--------------------------------------------------------------------
;; DISABLE SOUND
;;--------------------------------------------------------------------
(setq visible-bell 1)

;;--------------------------------------------------------------------
;; THEMES
;;--------------------------------------------------------------------
(load-theme 'spolsky t)

(eval-after-load 'xcscope
  '(progn
     (set-face-attribute 'cscope-function-face nil
 			 :foreground "#FD971F")
     (set-face-attribute 'cscope-file-face nil
			 :foreground "#A6E22E"
			 :weight 'bold)
     (set-face-attribute 'cscope-line-number-face nil
			 :foreground "#FF80F4")
     (set-face-attribute 'cscope-separator-face nil
			 :foreground "#8C8C8C")
     (set-face-attribute 'cscope-mouse-face nil
			 :foreground "#DEDEDE")))

;;--------------------------------------------------------------------
;; SCROLL BARS
;;--------------------------------------------------------------------
(scroll-bar-mode -1)
(global-yascroll-bar-mode 1)
(setq yascroll:delay-to-hide nil)

;;--------------------------------------------------------------------
;; TOOLBAR
;;--------------------------------------------------------------------
(tool-bar-mode -1)

;;--------------------------------------------------------------------
;; RECENT FILES
;;--------------------------------------------------------------------
(recentf-mode 1)
(setq recentf-max-menu-items 25)

;;--------------------------------------------------------------------
;; INDENTATION
;;--------------------------------------------------------------------
(setq-default indent-tabs-mode nil)

;; C/C++/Java
(setq c-default-style "linux"
      c-basic-offset 4)
(c-set-offset 'comment-intro 0)

;; Web mode
(setq js2-basic-offset 2)

;; adjust indents for web-mode to 2 spaces
(defun my-web-mode-hook ()
  "Hooks for Web mode. Adjust indents"
  ;;; http://web-mode.org/
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook  'my-web-mode-hook)

;;--------------------------------------------------------------------
;; COMPANY
;;--------------------------------------------------------------------
(require 'company)

(setq company-idle-delay 0.2)
(setq company-backends-base '(company-dabbrev-code
                              company-dabbrev
                              company-capf
                              company-yasnippet
                              company-files
                              company-keywords
                              company-oddmuse))
(setq company-backends company-backends-base)

(add-hook 'web-mode-hook (lambda ()
                           (make-local-variable 'company-backends)
                           (setq company-backends
                                 (append '(company-web-html)
                                         company-backends-base))))

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (make-local-variable 'company-backends)
                                  (setq company-backends
                                        (append '(company-elisp)
                                                company-backends-base))))

;;(add-hook 'js2-mode-hook 'tern-mode)
(add-hook 'after-init-hook 'global-company-mode)

;; (eval-after-load 'company
;;   '(progn
;;      (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
;;      (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)))

;; (eval-after-load 'company
;;   '(progn
;;      (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
;;      (define-key company-active-map (kbd "<backtab>") 'company-select-previous)))

;;--------------------------------------------------------------------
;; YASNIPPET
;;--------------------------------------------------------------------
(yas-global-mode 1)

;;--------------------------------------------------------------------
;; COMPILATION
;;--------------------------------------------------------------------
(defvar compilation-scroll-output 'first-error)
(setq compile-command "dmake")
(defvar compile-poa-lib-command compile-command)
(defvar compile-poa-soft-command compile-command)
;;(setq compilation-always-kill t)

(defun compile-poa (repo compile-poa-command)
  "Base compilation function for poa-lib and poa-soft according \
to REPO and COMPILE-POA-COMMAND arguments"
  (let ((default-directory (concat (locate-dominating-file (buffer-file-name)
				   "Makefile") (concat "../" repo))))
    (setq compile-command compile-poa-command)
    (call-interactively #'compile)
    (setq compile-poa-command compile-command)))

(defun compile-poa-lib()
  (interactive); "spoa-lib compile command: ")
  (compile-poa "poa-lib/" compile-poa-lib-command)
  (setq compile-poa-lib-command compile-command))

(defun compile-poa-soft()
  (interactive); "spoa-soft compile command: ")
  (compile-poa "poa-soft/" compile-poa-soft-command)
  (setq compile-poa-soft-command compile-command))

(defun rebuild-poa()
  "poa full rebuild"
  (interactive)
  (compile poa-rebuild))

;;--------------------------------------------------------------------
;; GDB
;;--------------------------------------------------------------------
(setq gud-gdb-command-name "gdb -i=mi ../../../poa-soft/PostOpAnalyser/bin/PostOpAnalyser")

;;--------------------------------------------------------------------
;; INDIUM
;;--------------------------------------------------------------------
(require 'indium)
(add-hook 'js2-mode-hook #'indium-interaction-mode)
(setq indium-chrome-executable chromium-path)

;; Chromium start for debug with indium
(defun indium-start-chrome()
      "Start chromium on http://localhost:3000"
      (interactive)
      (indium-run-chrome "http://localhost:3000"))

;;--------------------------------------------------------------------
;; WINDMOVE
;;--------------------------------------------------------------------
(windmove-default-keybindings 'meta)

;;--------------------------------------------------------------------
;; BUFFERMOVE
;;--------------------------------------------------------------------
(setq buffer-move-behavior 'move)

;;--------------------------------------------------------------------
;; NEOTREE
;;--------------------------------------------------------------------
;; (use-package neotree
;;   :ensure t
;;   :bind (([f8] . neotree-toggle))
;;   :config
;;   (setq neo-autorefresh t)
;;   (setq neo-force-change-root t)
;;   (setq neo-show-hidden-files t)
;;   (setq neo-smart-open t)
;;   (setq neo-window-width 33)
;;   ;;(setq projectile-switch-project-action 'neotree-projectile-action)
;;   )

;; (neotree)

;;--------------------------------------------------------------------
;; DASHBOARD
;;--------------------------------------------------------------------
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents  . 15)
			  (projects . 5))))

;;--------------------------------------------------------------------
;; PROJECTILE
;;--------------------------------------------------------------------
(projectile-mode t)
(setq projectile-indexing-method 'alien)
(setq projectile-enable-caching t)
(setq projectile-mode-line '(:eval (format " Prj[%s]" (projectile-project-name))))

;; On Projectile doesn't care about find-program configuration, so this
;; is usefull on Windows
(setq projectile-generic-command (concat find-program ". -type f -print0"))

(setq projectile-globally-ignored-directories
      (append '("*.svn"
                "*.git")
              projectile-globally-ignored-directories))

;; Only works if index method is 'alien
(setq projectile-globally-ignored-file-suffixes
      (append '(".o"
                ".gz"
                ".jar"
                ".tar.gz"
                ".tgz"
                ".zip"
                ".png"
                ".gif"
                ".odt"
                ".pdf"
                ".DS_Store"
                "~")
              projectile-globally-ignored-file-suffixes))

;;--------------------------------------------------------------------
;; MAXIMIZE EMACS ON STARTUP
;;--------------------------------------------------------------------
(defun maximize-frame ()
  "Maximizes the active frame in Windows."
  (interactive)
  ;; Send a `WM_SYSCOMMAND' message to the active frame with the
  ;; `SC_MAXIMIZE' parameter.
  (when (eq system-type 'windows-nt)
    (w32-send-sys-command 61488)))
(add-hook 'window-setup-hook 'maximize-frame t)

;;--------------------------------------------------------------------
;; GLOBAL SHORTCUTS
;;--------------------------------------------------------------------
(global-set-key (kbd "<mouse-4>") 'mouse-yank-primary)
(global-set-key (kbd "M-S-<up>") 'buf-move-up)
(global-set-key (kbd "M-S-<down>") 'buf-move-down)
(global-set-key (kbd "M-S-<left>") 'buf-move-left)
(global-set-key (kbd "M-S-<right>") 'buf-move-right)
(global-set-key (kbd "C-<pause>") 'previous-buffer)
(global-set-key (kbd "M-<pause>") 'next-buffer)
(global-set-key (kbd "C-<tab>") 'dabbrev-expand)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "M-<backspace>") 'delete-indentation)
(global-set-key (kbd "C-c C-k") 'delete-horizontal-space)
(global-set-key (kbd "C-c C-j") 'just-one-space)
(global-set-key (kbd "C-c C-i") 'string-inflection-all-cycle)
(global-set-key (kbd "C-c i c") 'string-inflection-camelcase)
(global-set-key (kbd "C-c i l") 'string-inflection-lower-camelcase)
(global-set-key (kbd "C-c i u") 'string-inflection-underscore)
(global-set-key (kbd "C-c i U") 'string-inflection-upcase)

;;--------------------------------------------------------------------
;; C/C++ SHORTCUTS
;;--------------------------------------------------------------------
(defun c-mode-shortcuts ()
  "Local shortcuts for use in c-mode-common-hook."
  (local-set-key (kbd "C-c C-x C-c") 'uncomment-region)
  (local-set-key (kbd "C-c b l") 'compile-poa-lib)
  (local-set-key (kbd "C-c b s") 'compile-poa-soft)
  (local-set-key (kbd "C-c b r") 'recompile)
  (local-set-key (kbd "C-c b a") 'rebuild-poa)
  (local-set-key (kbd "C-c s x") 'cscope-update-db))

(add-hook 'c-mode-common-hook 'c-mode-shortcuts)

;;--------------------------------------------------------------------
;; QUICK EMACS CONF FILE OPENNING
;;--------------------------------------------------------------------
(defun open-emacs-init-file ()
  "Open the Emacs configuration file"
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;;--------------------------------------------------------------------
;; FRAME TITLE
;;--------------------------------------------------------------------
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
        '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;;--------------------------------------------------------------------
;; OPEN FILES IN THE CURRENT INSTANCE
;;--------------------------------------------------------------------
(server-start)

;;--------------------------------------------------------------------
;; AUTO RELOAD FILE IF CHANGED ON DISK
;;--------------------------------------------------------------------
(global-auto-revert-mode t)

;;--------------------------------------------------------------------
;; OTHERS
;;--------------------------------------------------------------------
(put 'upcase-region 'disabled nil)
(delete-selection-mode 1)

;; (defun shell-quote-argument (argument)
;;   "Quote ARGUMENT for passing as argument to an inferior shell.

;; This function is designed to work with the syntax of your system's
;; standard shell, and might produce incorrect results with unusual shells.
;; See Info node `(elisp)Security Considerations'."
;;   argument)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" "3cc2385c39257fed66238921602d8104d8fd6266ad88a006d0a4325336f5ee02" default)))
 '(gdb-create-source-file-list nil)
 '(package-selected-packages
   (quote
    (magit indium tide string-inflection rjsx-mode flycheck all-the-icons-dired neotree use-package company-web dsvn add-node-modules-path geben yasnippet-snippets projectile yascroll dashboard auto-complete chess yaml-mode php-mode buffer-move color-theme zenburn-theme dracula-theme company-tern json-mode ag js2-refactor s xref-js2 exec-path-from-shell)))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
