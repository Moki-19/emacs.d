;;; init.el --- Loic's Emacs configuration file
;;
;;; Commentary:
;;
;;; Code:
;;
;;--------------------------------------------------------------------------------------------------
;; VARIABLES DECLARATION
;;--------------------------------------------------------------------------------------------------
;; Internal
(defvar font-height-medium-screen)
(defvar font-height-large-screen)
(defvar default-max-line-length 100)

;; External
(defvar mac-option-key-is-meta)
(defvar mac-right-option-modifier)
(defvar flycheck-check-syntax-automatically)
(defvar tramp-default-method)
(defvar yascroll:delay-to-hide)
(defvar recentf-max-menu-items)
(defvar gud-gdb-command-name)
(defvar buffer-move-behavior)
(defvar dashboard-items)
(defvar js2-mode-show-parse-errors)
(defvar js2-mode-show-strict-warnings)

;;--------------------------------------------------------------------------------------------------
;; LISP
;;--------------------------------------------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/lisp/")

;;--------------------------------------------------------------------------------------------------
;; PACKAGES
;;--------------------------------------------------------------------------------------------------
(prefer-coding-system 'utf-8)

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

;; Try to uncomment next line if a package download fails (probably not longer needed with Emacs >= 26.3)
;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;;--------------------------------------------------------------------------------------------------
;; WIN conf
;;--------------------------------------------------------------------------------------------------
(when (eq system-type 'windows-nt)
  (setq find-program "C:\\msys64\\usr\\bin\\find.exe")
  ;; Projectile doesn't care about find-program configuration, so this
  ;; is usefull on Windows
  (defvar projectile-generic-command (concat find-program ". -type f -print0"))
  (defvar app-rebuild "F:/Travail/Scripts/rebuild_postop.bat")
  (defvar eslint-path "node_modules/.bin/eslint.cmd")
  (defvar jest-path "node_modules/.bin/jest.cmd")
  (defvar chrome-path "C:/Users/loic/AppData/Roaming/chrlauncher/64/bin/chrome.exe")
  (defvar org-work-notes-path "F:/Travail/Docs/org-work-notes")
  (defvar font-height-medium-screen 100)
  (defvar font-height-large-screen 110)
  (set-face-font 'default "-outline-Consolas-normal-r-normal-normal-*-*-96-96-c-*-iso8859-1")
  (set-face-font 'bold "-outline-Consolas-bold-r-normal-normal-*-*-96-96-c-*-iso8859-1")
  (set-face-font 'italic "-outline-Consolas-normal-i-normal-normal-*-*-96-96-c-*-iso8859-1")
  (set-face-font 'bold-italic "-outline-Consolas-bold-i-normal-normal-*-*-96-96-c-*-iso8859-1"))

;;--------------------------------------------------------------------------------------------------
;; MAC conf
;;--------------------------------------------------------------------------------------------------
(when (eq system-type 'darwin)
  ;; (load "osx_gud.el")
  (setq exec-path (append '("/usr/local/bin")
                          exec-path))
  (defvar app-rebuild "~/Travail/Scripts/rebuild_postop")
  (defvar eslint-path "node_modules/eslint/bin/eslint.js")
  (defvar jest-path "node_modules/jest/bin/jest.js")
  (defvar chrome-path "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")
  (defvar org-work-notes-path "~/Travail/Docs/org-work-notes")
  (setq mac-option-key-is-meta t)
  (setq mac-right-option-modifier nil)
  (global-set-key (kbd "<s-left>") 'move-beginning-of-line)
  (global-set-key (kbd "<s-right>") 'move-end-of-line)
  (global-set-key (kbd "<s-up>") 'beginning-of-buffer)
  (global-set-key (kbd "<s-down>") 'end-of-buffer)
  (defvar font-height-medium-screen 110)
  (defvar font-height-large-screen 150)
  (set-face-attribute 'default nil
                      :font "Iosevka"))

;;--------------------------------------------------------------------------------------------------
;; LINUX conf
;;--------------------------------------------------------------------------------------------------
(when (eq system-type 'gnu/linux)
  ;; (setq exec-path (append '("/usr/local/bin")
  ;;                         exec-path))
  ;; (defvar app-rebuild "~/Travail/Scripts/rebuild_postop")
  (defvar eslint-path "node_modules/eslint/bin/eslint.js")
  (defvar jest-path "node_modules/jest/bin/jest.js")
  ;; (defvar chrome-path "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")
  (defvar org-work-notes-path "~/Travail/Docs/org-work-notes")
  (defvar font-height-medium-screen 110)
  (defvar font-height-large-screen 150))


;;--------------------------------------------------------------------------------------------------
;; EXEC PATH FROM SHELL
;;--------------------------------------------------------------------------------------------------;;
(use-package exec-path-from-shell
  :ensure t
  :if (eq system-type 'darwin)
  :init
  (exec-path-from-shell-initialize))

;;--------------------------------------------------------------------------------------------------
;; FONT
;;--------------------------------------------------------------------------------------------------
;; (set-face-attribute 'default nil :height (if (<= (x-display-pixel-width) 1280)
;;                                              font-height-medium-screen
;;                                            font-height-large-screen))
;; FIXME: find which package override font size just after initialization (putting previous line
;; at the end of this file does not work). This issue occurs since a massive package update.
(add-hook 'emacs-startup-hook (lambda () (set-face-attribute 'default nil :height
                                                             (if (<= (x-display-pixel-width) 1280)
                                                                 font-height-medium-screen
                                                               font-height-large-screen))))

;;--------------------------------------------------------------------------------------------------
;; Disable SOUND
;;--------------------------------------------------------------------------------------------------
(setq visible-bell 1)

;;--------------------------------------------------------------------------------------------------
;; THEMES
;;--------------------------------------------------------------------------------------------------
(use-package sublime-themes
  :ensure t
  :init (load-theme 'spolsky t))

;;--------------------------------------------------------------------------------------------------
;; SCROLL BARS
;;--------------------------------------------------------------------------------------------------
(use-package yascroll
  :ensure t
  :init
  (scroll-bar-mode -1)
  :config
  (global-yascroll-bar-mode 1)
  (setq yascroll:delay-to-hide nil))

;;--------------------------------------------------------------------------------------------------
;; TOOLBAR
;;--------------------------------------------------------------------------------------------------
(tool-bar-mode -1)

;;--------------------------------------------------------------------------------------------------
;; WHITESPACE
;;--------------------------------------------------------------------------------------------------
(use-package whitespace
  :bind ("C-c w m" . whitespace-mode)
  :init
  (setq whitespace-style '(face tabs lines-tail trailing))
  (add-hook 'prog-mode-hook
            #'(lambda ()
                (set (make-local-variable 'whitespace-line-column)
                     (pcase (projectile-project-name)
                       ;; ("django-api" 79)
                       (_ default-max-line-length)))
                (whitespace-mode 1)))
  :config
  (setq-default fill-column default-max-line-length))

;;--------------------------------------------------------------------------------------------------
;; WS-BUTLER
;;--------------------------------------------------------------------------------------------------
(use-package ws-butler
  :ensure t
  :config
  (setq ws-butler-keep-whitespace-before-point nil)
  (ws-butler-global-mode t))

;;--------------------------------------------------------------------------------------------------
;; HIGHTLIGHT INDENTATION
;;--------------------------------------------------------------------------------------------------
(use-package highlight-indentation
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-indentation-current-column-mode)
  (set-face-background 'highlight-indentation-face "gray10")
  (set-face-background 'highlight-indentation-current-column-face "gray20"))

;;--------------------------------------------------------------------------------------------------
;; TOO LONG LINES
;;--------------------------------------------------------------------------------------------------
(require 'too-long-lines-mode)
(setq too-long-lines-special-buffer-modes '(ag-mode))
(too-long-lines-mode)

;;--------------------------------------------------------------------------------------------------
;; IBUFFER
;;--------------------------------------------------------------------------------------------------
(use-package ibuffer
  :bind (("C-x C-b" . ibuffer)))

(use-package ibuffer-projectile
  :ensure t
  :after projectile
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-projectile-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

;;--------------------------------------------------------------------------------------------------
;; SMERGE
;;--------------------------------------------------------------------------------------------------
(setq smerge-command-prefix (kbd "C-c ="))

;;--------------------------------------------------------------------------------------------------
;; C++
;;--------------------------------------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;;--------------------------------------------------------------------------------------------------
;; PHP
;;--------------------------------------------------------------------------------------------------
(autoload 'geben "geben" "DBGp protocol frontend, a script debugger" t)

;;--------------------------------------------------------------------------------------------------
;; PYTHON
;;--------------------------------------------------------------------------------------------------
(use-package elpy
  :ensure t
  :bind ((:map compilation-mode-map ("C-c C-p" . compile-to-inferior-python))
         (:map python-mode-map ("C-c ! f" . elpy-autopep8-fix-code))
         (:map python-mode-map ("C-c <kp-subtract>" . elpy-nav-indent-shift-left))
         (:map python-mode-map ("C-c <kp-add>" . elpy-nav-indent-shift-right)))
  :init (elpy-enable)
  :config
  (setq elpy-test-runner 'elpy-test-django-runner)
  (setq elpy-django-command "./manage.py")
  (setenv "DJANGO_SETTINGS_MODULE" "doqboard.settings.dev")
  (setq elpy-rpc-python-command "python3")
  (setq python-shell-interpreter "python3"
        python-shell-interpreter-args "-i")

  ;; Use Flycheck instead of Flymake
  (when (require 'flycheck nil t)
    (remove-hook 'elpy-modules 'elpy-module-flymake)
    (add-hook 'elpy-mode-hook 'flycheck-mode))

  ;; Customize pdb command for Django
  (setq gud-pdb-command-name "python -m pdb")

  ;; Fix some keys conflicts
  (define-key elpy-mode-map (kbd "<C-down>") nil)
  (define-key elpy-mode-map (kbd "<C-up>") nil)
  (define-key elpy-mode-map (kbd "<M-down>") nil)
  (define-key elpy-mode-map (kbd "<M-up>") nil)
  (define-key elpy-mode-map (kbd "<M-left>") nil)
  (define-key elpy-mode-map (kbd "<M-right>") nil)

  (defun compile-to-inferior-python ()
      "Turn a compilation buffer into Inferior Python mode"
      (interactive)
      (if (string= major-mode "compilation-mode")
          (progn
            (read-only-mode -1)
            (let ((python-shell--interpreter nil)
                  (python-shell--interpreter-args nil))
              (inferior-python-mode)))
        (message "Buffer mode needs to be compilation-mode"))))

;;--------------------------------------------------------------------------------------------------
;; PO-MODE
;;--------------------------------------------------------------------------------------------------
(use-package po-mode
  :ensure t)

;;--------------------------------------------------------------------------------------------------
;; XML
;;--------------------------------------------------------------------------------------------------
(defun xml-format-buffer ()
  "XML format buffer."
  (interactive)
  (xml-format t))

(defun xml-format-region ()
  "XML format region."
  (interactive)
  (xml-format nil))

(defun xml-format (extend-selection-to-whole-buffer)
  "XML format base function : EXTEND-SELECTION-TO-WHOLE-BUFFER is nil for a region, t for the entire buffer."
  (save-excursion
    (when extend-selection-to-whole-buffer (mark-whole-buffer))
    (shell-command-on-region (mark) (point) "xmllint --format -" (buffer-name) t)))

;;--------------------------------------------------------------------------------------------------
;; WEB-MODE
;;--------------------------------------------------------------------------------------------------
(use-package web-mode
  :ensure t
  :defer t
  :mode (("\\.phtml\\'" . web-mode)
	 ("\\.[agj]sp\\'" . web-mode)
	 ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
	 ("\\.djhtml\\'" . web-mode)
	 ("\\.as[cp]x\\'" . web-mode))
  :config (progn
            (setq web-mode-markup-indent-offset 2
		  web-mode-css-indent-offset 2
		  web-mode-code-indent-offset 2)))

;;--------------------------------------------------------------------------------------------------
;; JS2-MODE
;;--------------------------------------------------------------------------------------------------
;; RJSX-MODE and JS2-MODE are not required starting from Emacs 27, but they seem to do a better job
;; than the embedded support (commenting JSX, ...), so keeping it for now.
(use-package js2-mode
  :ensure t
  :bind (:map js2-mode-map (("C-c ! f" . eslint-fix-file-and-revert)
                            ("C-c ! F" . eslint-fix-all-files)))
  :mode (("\\.js\\'" . rjsx-mode))
  :config (progn
            ;; Disable JS2 syntax checking as we use flycheck one
            (setq js2-mode-show-parse-errors nil)
            (setq js2-mode-show-strict-warnings nil)

            ;; Indentation
            (setq js2-basic-offset 2)
            (add-hook 'js2-mode-hook
                      (lambda ()
                        (setq js-switch-indent-offset 2)))

            ;; js-mode (which js2 is based on) binds "M-." which conflicts with xref
            ;; and tide, so unbind it.
            (define-key js-mode-map (kbd "M-.") nil)
            (define-key js2-mode-map (kbd "M-.") nil)

            ;; Load add-node-modules-path
            ;; 18/05/2018 : hangs with with last version of js2-mode, so replace it
            ;; by use-eslint-from-node-modules function (see FLYCHECK part)
            ;; (add-hook 'js2-mode-hook #'add-node-modules-path)
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

            ;; https://github.com/felipeochoa/rjsx-mode/issues/112
            (defun +rjsx-electric-gt-fragment-a (n)
              (if (or (/= n 1) (not (and (eq (char-before) ?<) (eq (char-after) ?/)))) 't
                (insert ?> ?<)
                (backward-char)))

            (advice-add #'rjsx-electric-gt :before-while #'+rjsx-electric-gt-fragment-a))

  (defun eslint-fix-file ()
    (interactive)
    (let ((eslint (concat (projectile-project-root) eslint-path))
          (options (list "--fix" buffer-file-name)))
      (let ((inhibit-message t))
        (message (concat eslint " " (string-join options " "))))
      (message "eslint --fixing the file" buffer-file-name)
      (apply #'call-process eslint nil "*ESLint Errors*" nil options)
      (message "done")))

  (defun eslint-fix-file-and-revert ()
    (interactive)
    (save-buffer)
    (eslint-fix-file)
    (revert-buffer t t))

  (defun eslint-fix-all-files ()
    (interactive)
    (let ((eslint (concat (projectile-project-root) eslint-path))
          (options (list (concat (projectile-project-root) "src/**/*.js")
  			 (concat (projectile-project-root) "src/**/*.jsx") "--fix")))
      (let ((inhibit-message t))
        (message (concat eslint " " (string-join options " "))))
      (message "eslint --fixing all project files")
      (apply #'call-process eslint nil "*ESLint Errors*" nil options)
      (message "done"))))

(use-package js2-refactor
  :ensure t
  :bind (("C-M-<down>" . js2r-move-line-down)
         ("C-M-<up>" . js2r-move-line-up))
  :after js2-mode
  :init
  (add-hook 'js2-mode-hook 'js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c r")
  ;; Unbind conflicting keys
  (define-key js2-refactor-mode-map (kbd "<C-S-down>") nil)
  (define-key js2-refactor-mode-map (kbd "<C-S-up>") nil))

(use-package xref-js2
  :ensure t
  :init (add-hook 'js2-mode-hook
                  (lambda ()
                    (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
  :bind (("M-C-." . xref-find-definitions)
         ("M-C-?" . xref-find-references)))

(use-package rjsx-mode
  :ensure t)

;;--------------------------------------------------------------------------------------------------
;; LATEX
;;--------------------------------------------------------------------------------------------------
(use-package tex
  :ensure auctex)

(use-package company-auctex
  :ensure t
  :after tex
  :config (company-auctex-init))

;;--------------------------------------------------------------------------------------------------
;; JSON
;;--------------------------------------------------------------------------------------------------
(use-package json-mode
  :ensure t
  :bind (("C-c f j" . json-pretty-print)))

(use-package json-navigator
  :ensure t
  :bind (("C-c n j r" . json-navigator-navigate-region)
         ("C-c n j a" . json-navigator-navigate-after-point)))

;;--------------------------------------------------------------------------------------------------
;; RESTCLIENT
;;--------------------------------------------------------------------------------------------------
(use-package restclient
  :ensure t
  :mode (("\\.rstclt\\'" . restclient-mode))
  :config
  (setq restclient-same-buffer-response-name "*restclient HTTP response*")
  (defvar api-restclient-token nil)
  ;;(add-to-list 'restclient-content-type-modes '("application/json" . json-mode))
  (defun api-restclient-hook ()
    "Update token from a request."
    (save-excursion
      (save-match-data
        ;; update regexp to extract required data
        (when (re-search-forward "\"token\":\"\\(.*?\\)\"" nil t)
          (setq api-restclient-token (match-string 1))))))
  (add-hook 'restclient-response-received-hook #'api-restclient-hook)
  (add-hook 'js-mode-hook 'flycheck-disable-on-temp-buffers))

;;--------------------------------------------------------------------------------------------------
;; TIDE
;;--------------------------------------------------------------------------------------------------
(use-package tide
  :ensure t
  :after (js2-mode company flycheck)
  :bind ((:map js2-mode-map
               ("C-c j" . tide-jsdoc-template)))
  :config
  (setq company-tooltip-align-annotations t)
  :hook ((js2-mode . (lambda ()
                   (tide-setup)
                   (eldoc-mode)
                   (tide-hl-identifier-mode)
                   (company-mode)
                   (local-set-key (kbd "M-?") 'tide-references)))))


;;--------------------------------------------------------------------------------------------------
;; JEST
;;--------------------------------------------------------------------------------------------------
(use-package jest
  :ensure t
  :after js2-mode
  :bind (("C-c t f" . jest-file)
         ("C-c t c" . jest-function)
         ("C-c t p" . jest)
         ("C-c t r" . jest-repeat))
  :config
  (setq jest-executable jest-path))

;;--------------------------------------------------------------------------------------------------
;; YAML
;;--------------------------------------------------------------------------------------------------
(use-package yaml-mode
  :ensure t
  :mode ("\\.ya?ml\\'" . yaml-mode))

;;--------------------------------------------------------------------------------------------------
;; OBJECTIVE-C
;;--------------------------------------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.mm\\'" . c++-mode))

;;--------------------------------------------------------------------------------------------------
;; MAKEFILE
;;--------------------------------------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.mingw\\'" . makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("\\.osx\\'" . makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("\\.common\\'" . makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("\\.gcc\\'" . makefile-gmake-mode))

;;--------------------------------------------------------------------------------------------------
;; MARKDOWN
;;--------------------------------------------------------------------------------------------------
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;--------------------------------------------------------------------------------------------------
;; GTAGS
;;--------------------------------------------------------------------------------------------------
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string= web-mode-engine "php") (ggtags-mode 1))))
(setq ggtags-auto-jump-to-match nil)
(setq ggtags-global-output-format 'cscope)
(global-set-key (kbd "C-c M-t") 'ggtags-grep)

;;--------------------------------------------------------------------------------------------------
;; FLYCHECK
;;--------------------------------------------------------------------------------------------------
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  ;;(setq flycheck-check-syntax-automatically nil)
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (setq flycheck-python-flake8-executable "python3")
  (add-hook 'flycheck-mode-hook #'use-eslint-from-node-modules)
  :config
  (defun flycheck-disable-on-temp-buffers ()
    (unless (and buffer-file-name (file-exists-p buffer-file-name)) (flycheck-mode -1)))

  (defun use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name eslint-path root))))
      (when (and eslint (file-exists-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint)))))

;;--------------------------------------------------------------------------------------------------
;; MAGIT
;;--------------------------------------------------------------------------------------------------
(use-package magit
  :ensure t
  :bind (("C-c m s s" . magit-status)
         ("C-c m c i" . magit-commit-create)
         ("C-c m c o" . magit-checkout)
         ("C-c m b c" . magit-branch-and-checkout)
         ("C-c m b d" . magit-branch-delete)
         ("C-c m p h" . magit-push)
         ("C-c m p t" . magit-push-tag)
         ("C-c m p l" . magit-pull)
         ("C-c m m" . magit-merge)
         ("C-c m s h" . magit-stash)
         ("C-c m s l" . magit-stash-list)
         ("C-c m s a" . magit-stash-apply)
         ("C-c m s c" . magit-stash-clear)
         ("C-c m s m" . magit-stage-modified)
         ("C-c m s r" . magit-show-refs)
         ("C-c m r b" . magit-rebase-branch)
         ("C-c m d" . magit-diff-working-tree)
         ("C-c m l" . magit-log-head)
         ("C-c m a a" . magit-blame-addition)
         ("C-c m a q" . magit-blame-quit)
         ("C-c m f f" . magit-find-file)
         ("C-c m f w" . magit-find-file-other-window)
         ("C-c m r r" . magit-reset)
         ("C-c m t" . magit-tag)
         ("C-c m f a" . magit-fetch-all))
  :config
  (setq auto-revert-check-vc-info t))

;;--------------------------------------------------------------------------------------------------
;; TRAMP
;;--------------------------------------------------------------------------------------------------
(use-package tramp
  :bind (("C-c b s u" . tramp-browse-server-user)
         ("C-c b s s" . tramp-browse-server-sudo))
  :config
  (when (eq system-type 'windows-nt)
    (setq tramp-default-method "plinkx"))

  (defun tramp-browse-server (server path &optional sudo)
    "Browse the SERVER at PATH with Tramp with optional SUDO"
    (find-file (concat
                (concat
                 (concat
                  (concat "/ssh:" server) (if sudo
                                              (concat "|sudo:" server)
                                            "")) ":") (or path ""))))

  (defun tramp-browse-server-sudo (server path)
    "Browse the SERVER at PATH with Tramp in sudo mode"
    (interactive "sServer connection string: \nsRemote path:")
    (tramp-browse-server server path t))

  (defun tramp-browse-server-user (server path &optional sudo)
    "Browse the SERVER at PATH with Tramp"
    (interactive "sServer connection string: \nsRemote path:")
    (tramp-browse-server server path nil)))

;;--------------------------------------------------------------------------------------------------
;; RECENT FILES
;;--------------------------------------------------------------------------------------------------
(use-package init-open-recentf
  :ensure t
  :init
  (recentf-mode 1)
  (setq recentf-max-menu-items 25)
  :config
  (init-open-recentf))

;;--------------------------------------------------------------------------------------------------
;; INDENTATION
;;--------------------------------------------------------------------------------------------------
(setq-default indent-tabs-mode nil)

;; C/C++/Java
(setq c-default-style "linux"
      c-basic-offset 4)
(setq c-offsets-alist '((comment-intro . 0)
                        (inline-open . 0)
                        (inlambda . 0)
                        (case-label . +)))

;;--------------------------------------------------------------------------------------------------
;; IVY
;;--------------------------------------------------------------------------------------------------
(use-package ivy
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-x b" . ivy-switch-buffer)
         ("C-x C-r" . counsel-recentf)
         ("<C-return>" . ivy-minibuffer-immediate-done)
         ("C-?" . counsel-mark-ring)
         ("C-M-s" . swiper-isearch)
         ("C-M-r" . swiper-isearch-backward)
         ("C-S-s" . swiper-isearch-thing-at-point)
         ("<M-S-return>" . show-current-buffer-other-window))
  :config
  (ivy-mode 1)
  ;; no regexp by default
  (setq ivy-initial-inputs-alist nil)

  (defun ivy-minibuffer-immediate-done ()
    "Same as ivy-immediate-done but do nothing outside the minibuffer"
    (interactive)
    (when (minibufferp (current-buffer)) (ivy-immediate-done)))

  (defun show-current-buffer-other-window ()
    (interactive)
    (ivy--switch-buffer-other-window-action (buffer-name))))

(use-package counsel-projectile
  :ensure t
  :config (counsel-projectile-mode))

;;--------------------------------------------------------------------------------------------------
;; COMPANY
;;--------------------------------------------------------------------------------------------------
(use-package company
  :config

  (setq company-idle-delay 0.2)
  (setq company-backends-base '(company-capf
                                company-yasnippet
                                company-dabbrev-code
                                company-dabbrev
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
                                    (setq company-backends company-backends-base)))

  (add-hook 'org-mode-hook (lambda ()
                             (make-local-variable 'company-backends)
                             (setq company-backends company-backends-base)))

  (add-hook 'c-mode-common-hook (lambda ()
                                  (make-local-variable 'company-backends)
                                  (setq company-backends
                                        (append '(company-gtags)
                                                company-backends-base))))
  (add-hook 'js2-mode-hook (lambda ()
                             (make-local-variable 'company-backends)
                             (setq company-backends
                                   (append '(company-tide)
                                           company-backends-base))))

  (add-hook 'after-init-hook 'global-company-mode))

;;--------------------------------------------------------------------------------------------------
;; YASNIPPET
;;--------------------------------------------------------------------------------------------------
(use-package yasnippet
  :ensure t
  :config
  (setq yas-expand-only-for-last-commands '(self-insert-command))
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)

(use-package ivy-yasnippet
  :ensure t
  :bind ("C-c y" . ivy-yasnippet))

;;--------------------------------------------------------------------------------------------------
;; INFLEXION
;;--------------------------------------------------------------------------------------------------
(use-package string-inflection
  :ensure t
  :bind (("C-c s a" . string-inflection-all-cycle)
         ("C-c s c" . string-inflection-camelcase)
         ("C-c s L" . string-inflection-lower-camelcase)
         ("C-c s l" . string-inflection-lisp)
         ("C-c s u" . string-inflection-underscore)
         ("C-c s U" . string-inflection-upcase)))

;;--------------------------------------------------------------------------------------------------
;; COMPILATION
;;--------------------------------------------------------------------------------------------------
(defvar compilation-scroll-output 'first-error)
(setq compile-command "dmake")
(defvar compile-app-lib-command compile-command)
(defvar compile-app-soft-command compile-command)
;;(setq compilation-always-kill t)

(defun compile-app (repo compile-app-command)
  "Base compilation function for app-lib and app-soft according \
to REPO and COMPILE-APP-COMMAND arguments"
  (let ((default-directory (concat (locate-dominating-file (buffer-file-name)
                                                           "Makefile") (concat "../" repo))))
    (setq compile-command compile-app-command)
    (call-interactively #'compile)
    (setq compile-app-command compile-command)))

(defun compile-app-lib()
  "Compile app-lib."
  (interactive)
  (compile-app "app-lib/" compile-app-lib-command)
  (setq compile-app-lib-command compile-command))

(defun compile-app-soft()
  "Compile app-soft."
  (interactive)
  (compile-app "app-soft/" compile-app-soft-command)
  (setq compile-app-soft-command compile-command))

(defun rebuild-app()
  "App full rebuild."
  (interactive)
  (compile app-rebuild))

;;--------------------------------------------------------------------------------------------------
;; GDB
;;--------------------------------------------------------------------------------------------------
(setq gud-gdb-command-name "gdb -i=mi ../../../app-soft/PostOpAnalyser/bin/PostOpAnalyser")

;;--------------------------------------------------------------------------------------------------
;; REAL-GUD
;;--------------------------------------------------------------------------------------------------
(use-package realgud
  :ensure t
  :bind (("C-c b t" . realgud-track-mode)
         ("C-c b a" . realgud:attach-cmd-buffer)
         ("C-c b d" . realgud-cmdbuf-toggle-in-debugger?))
  :commands (realgud-track-mode realgud-cmdbuf-toggle-in-debugger? realgud:attach-cmd-buffer))

;;--------------------------------------------------------------------------------------------------
;; INDIUM
;;--------------------------------------------------------------------------------------------------
(use-package indium
  :ensure t
  :after js2-mode
  :bind (("C-c i c" . indium-connect)
         ("C-c i l" . indium-launch)
         ("C-c i q" . indium-quit)
         ("C-c i k" . indium-quit)
         ("C-c i r" . indium-debugger-resume)
         ("C-c i i" . indium-debugger-step-into)
         ("C-c i o" . indium-debugger-step-out)
         ("C-c i SPC" . indium-debugger-step-over))
  :hook (js2-mode . indium-interaction-mode)
  :config
  (setq indium-chrome-executable chrome-path)
  (setq indium-chrome-use-temporary-profile t)
  (setq indium-chrome-data-dir (expand-file-name "indium-chrome" user-emacs-directory))
  (make-directory indium-chrome-data-dir t))

;;--------------------------------------------------------------------------------------------------
;; WINDMOVE & FRAMEMOVE
;;--------------------------------------------------------------------------------------------------
(require 'framemove)
(windmove-default-keybindings 'meta)
(setq framemove-hook-into-windmove t)

;;--------------------------------------------------------------------------------------------------
;; BUFFERMOVE
;;--------------------------------------------------------------------------------------------------

(use-package buffer-move
  :ensure t
  :bind (("M-S-<up>" . buf-move-up)
         ("M-S-<down>" . buf-move-down)
         ("M-S-<left>" . buf-move-left)
         ("M-S-<right>" . buf-move-right))
  :config
  (setq buffer-move-behavior 'move))


;;--------------------------------------------------------------------------------------------------
;; TREEMACS
;;--------------------------------------------------------------------------------------------------
(use-package treemacs
  :ensure t
  :defer t
  :init
  :config
  (progn
    (setq treemacs-collapse-dirs              0
          treemacs-file-event-delay           5000
          treemacs-follow-after-init          nil
          treemacs-follow-recenter-distance   0.1
          treemacs-goto-tag-strategy          'refetch-index
          treemacs-indentation                2
          treemacs-indentation-string         " "
          treemacs-is-never-other-window      nil
          treemacs-no-png-images              t
          treemacs-project-follow-cleanup     nil
          treemacs-recenter-after-file-follow nil
          treemacs-recenter-after-tag-follow  nil
          treemacs-show-hidden-files          t
          treemacs-silent-filewatch           nil
          treemacs-silent-refresh             nil
          treemacs-sorting                    'alphabetic-desc
          treemacs-tag-follow-cleanup         t
          treemacs-tag-follow-delay           1.5
          treemacs-width                      35)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'extended))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

;;--------------------------------------------------------------------------------------------------
;; DASHBOARD
;;--------------------------------------------------------------------------------------------------
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents  . 15)
                          (projects . 5)
                          (agenda . 15)))
  (setq show-week-agenda-p t))

;;--------------------------------------------------------------------------------------------------
;; AG
;;--------------------------------------------------------------------------------------------------
(use-package ag
  :ensure t
  :bind (("C-c a p" . ag-project)
         ("C-c a r" . ag-project-regexp)
         ("C-c a a" . ag-app_v2)
         ("C-c a l" . ag-find-in-libs))
  :config
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers t)
  (setq-default ag-ignore-list '("**.min.*"))

  (defun ag-app_v2 (string)
    "Find string in app_v2"
    (interactive (list (ag/read-from-minibuffer "Search string")))
    (ag/search string (concat (projectile-project-root) "/..")))

  (defun ag-find-in-libs (string)
     "Find string in project libraries"
    (interactive (list (ag/read-from-minibuffer "Search string")))
    (let ((dir (pcase (projectile-project-name)
                 ("web-app" (concat (projectile-project-root) "/node_modules"))
                 ("django-api" python-shell-virtualenv-root)
                 (_ (if last-ag-find-in-libs-path
                        last-ag-find-in-libs-path
                      (projectile-project-root))))))
      (setq last-ag-find-in-libs-path dir)
      (ag/search string dir))))

;;--------------------------------------------------------------------------------------------------
;; PROJECTILE
;;--------------------------------------------------------------------------------------------------
(use-package projectile
  :ensure t
  :bind (("C-c p" . projectile-command-map)
         ("C-c p x p" . run-project)
         ("C-c p x b" . run-project-in-debug)
         ("C-c p x d f" . deploy-doqboard-front)
         ("C-c p x d b" . deploy-doqboard-back)
         ("C-c p x i" . start-django-shell)
         ("C-c p x c" . save-pg-database-to-file)
         ("C-c p x r" . restore-django-pg-database-from-file))
  :init
  :config
  (projectile-global-mode)
  (setq projectile-indexing-method 'alien)
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'ivy)
  (setq projectile-mode-line-function '(lambda () (format " Prj[%s]" (projectile-project-name))))
  (add-to-list 'projectile-other-file-alist '("json" "json"))

  (setq projectile-globally-ignored-directories
        (append '("*.svn"
                  "*.git"
                  "ext")
                projectile-globally-ignored-directories))

  ;; Only works if index method is 'alien
  (setq projectile-globally-ignored-file-suffixes
        (append '(".o"
                  ".gz"
                  ".z"
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

  ;; (defun run-doqboard-project()
  ;;   "Run Doqboard project (frontend and backend)"
  ;;   (interactive)
  ;;   (let ((current-project-name (projectile-default-project-name (projectile-project-root))))
  ;;     (setq projectile-switch-project-action 'run-project)
  ;;     (projectile-switch-project-by-name "django-api")
  ;;     (projectile-switch-project-by-name "web-app")
  ;;     ;; (if current-project-name (projectile-switch-project-by-name current-project-name))
  ;;     (setq projectile-switch-project-action 'projectile-find-file)))

  (defun run-project()
    "Run a project"
    (interactive)
    (let ((cmd (pcase (projectile-project-name)
                 ("web-app" "yarn start")
                 ("django-api" "workon doqboard && ./manage.py runserver")
                 (_ nil)))
          (cmd-buffer-name (concat (projectile-project-name) " running")))
      (if cmd
          (progn
            (let* ((cmd-buffer (get-buffer cmd-buffer-name))
                   (cmd-buffer-already-displayed (equal cmd-buffer (current-buffer)))
                   (kill-buffer-query-functions nil))
              (if (and cmd-buffer (not cmd-buffer-already-displayed))
                  (pop-to-buffer cmd-buffer nil t)
                (when cmd-buffer (kill-buffer cmd-buffer))
                (projectile-with-default-dir (projectile-ensure-project (projectile-project-root))
                  (async-shell-command cmd cmd-buffer-name)))))
        (message "No running command set for %s project" (projectile-project-name)))))


  (defun run-project-in-debug()
    "Run a project in debug mode"
    (interactive)
    (pcase (projectile-project-name)
      ("django-api" (projectile-with-default-dir
                        (projectile-ensure-project (projectile-project-root))
                      (realgud:pdb "python -m pdb manage.py runserver --noreload")))
      (_ (message "No debugging command set for %s project" (projectile-project-name)))))

  (defun deploy-doqboard-app(end cmd)
    (let ((project "django-api"))
      (if (string= (projectile-project-name) project)
          (projectile-with-default-dir (projectile-ensure-project (projectile-project-root))
            (async-shell-command (concat "workon doqboard && " cmd) (concat end " deployment")))
        (message "You must be on %s project first." project))))

  (defun deploy-doqboard-front()
    "Deploy Doqboard frontend."
    (interactive)
    (deploy-doqboard-app "web-app" "fab update_front_staging"))

  (defun deploy-doqboard-back()
    "Deploy Doqboard backend."
    (interactive)
    (deploy-doqboard-app "django-api" "fab update_back_staging"))

  (defun find-other-file-goto-same-line (&optional flex-matching)
  "Switch to other file at same line than the current buffer"
  (interactive "P")
  (let ((line (line-number-at-pos)))
    (projectile--find-other-file flex-matching)
    (when (<= line (count-lines (point-min) (point-max)))
      (goto-line line))))

  (advice-add 'projectile-find-other-file :override #'find-other-file-goto-same-line)

  (defun start-django-shell ()
    "Start a django shell"
    (interactive)
    (let* ((django-shell-name "Django shell")
          (django-shell-buffer (get-buffer django-shell-name)))
      (if django-shell-buffer
          (pop-to-buffer django-shell-buffer nil t)
        (if (string= (projectile-project-name) "django-api")
            (progn
              (async-shell-command (concat (projectile-project-root) "manage.py shell_plus") django-shell-name)
              (pop-to-buffer django-shell-name))
          (message "Please switch to the django-api project first.")))))

;; From https://stackoverflow.com/questions/13901955/how-to-avoid-pop-up-of-async-shell-command-buffer-in-emacs
(defun async-shell-command-no-window (command)
  "Run a async COMMAND without displaying a shell window."
  (let ((display-buffer-alist
         (list
          (cons
           "\\*Async Shell Command\\*.*"
           (cons #'display-buffer-no-window nil)))))
    (async-shell-command command)))

(defun save-pg-database-to-file (bk-file &optional db-name)
  "Save the DB-NAME postgresql database in BK-FILE."
  (interactive "FBackup file path: \nsDatabase name (doqboard): ")
  (let ((db-name (if (and db-name (not (string-empty-p db-name))) db-name  "doqboard")))
    (when (or (not (file-exists-p bk-file))
              (yes-or-no-p (format "Please confirm that you want to overwrite %s file ?" bk-file)))
      (async-shell-command-no-window
       (format "pg_dump -U llemaitre -Fc -b -f %s %s" bk-file db-name)))))

(defun restore-django-pg-database-from-file (bk-file &optional db-name)
  "Restore the DB-NAME postgresql database from BK-FILE."
  (interactive "fBackup file path: \nsDatabase name (doqboard): ")
  (let* ((user "llemaitre")
         (db-name (if (and db-name (not (string-empty-p db-name))) db-name  "doqboard"))
         (drop-schema-cmd (format
                           "psql -U %s %s -c \"DROP SCHEMA IF EXISTS public CASCADE;\""
                           user db-name))
         (create-schema-cmd (format
                             "psql -U %s %s -c \"CREATE SCHEMA public AUTHORIZATION %s;\""
                             user db-name "doqboard"))
         (pg-restore-cmd (format
                         "pg_restore -U %s -d %s -n public --if-exists -c -e -j 5 \"%s\""
                         user db-name bk-file))
         ;; (migrate-cmd (format "%smanage.py migrate" (projectile-project-root)))
         (cmd (concat drop-schema-cmd " && " create-schema-cmd " && " pg-restore-cmd)))

    (if (string= (projectile-project-name) "django-api")
        (async-shell-command-no-window cmd)
      (message "You must be in django-api project first.")))))

;;--------------------------------------------------------------------------------------------------
;; MAXIMIZE EMACS ON STARTUP
;;--------------------------------------------------------------------------------------------------
;; (defun maximize-frame ()
;;   "Maximizes the active frame in Windows."
;;   (interactive)
;;   ;; Send a `WM_SYSCOMMAND' message to the active frame with the
;;   ;; `SC_MAXIMIZE' parameter.
;;   (when (eq system-type 'windows-nt)
;;     (w32-send-sys-command 61488)))
;; (add-hook 'window-setup-hook 'maximize-frame t)
(toggle-frame-maximized)

;;--------------------------------------------------------------------------------------------------
;; GLOBAL SHORTCUTS
;;--------------------------------------------------------------------------------------------------
(when (eq system-type 'darwin)
  (global-set-key (kbd "<mouse-4>") 'mouse-yank-primary))
(global-set-key (kbd "C-<pause>") 'previous-buffer)
(global-set-key (kbd "M-<pause>") 'next-buffer)
(global-set-key (kbd "C-<tab>") 'dabbrev-expand)
(global-set-key (kbd "C-S-<tab>") 'hippie-expand)
(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "M-<backspace>") 'delete-indentation)
(global-set-key (kbd "M-<delete>") 'delete-horizontal-space)
(global-set-key (kbd "M-S-<delete>") 'just-one-space)
(global-set-key (kbd "C-c b r") 'revert-buffer)
(global-set-key (kbd "C-c o i") 'open-emacs-init-file)
(global-set-key (kbd "C-c b b") 'browse-url-of-buffer)
(global-set-key (kbd "C-c b u") 'browse-url)
(global-set-key (kbd "C-x SPC") 'rectangle-mark-mode)
(global-set-key (kbd "C-c b d") 'diff-buffer-with-file)
(global-set-key (kbd "<f13>") 'kmacro-start-macro)
(global-set-key (kbd "<f14>") 'kmacro-end-and-call-macro)
(global-set-key (kbd "C-c *") 'comint-send-invisible)
(global-set-key (kbd "C-c q l") 'sql-postgres)
(global-set-key (kbd "C-c q m") 'sql-mode)

;;--------------------------------------------------------------------------------------------------
;; XML SHORTCUTS
;;--------------------------------------------------------------------------------------------------
(defun nxml-mode-shortcuts ()
  "Local shortcuts for use in 'nxml-mode-hook'."
  (local-set-key (kbd "C-c x f") 'xml-format-buffer)
  (local-set-key (kbd "C-c x r") 'xml-format-region))

(add-hook 'nxml-mode-hook 'nxml-mode-shortcuts)

;;--------------------------------------------------------------------------------------------------
;; C/C++ SHORTCUTS
;;--------------------------------------------------------------------------------------------------
(defun c-mode-shortcuts ()
  "Local shortcuts for use in ‘c-mode-common-hook’."
  (local-set-key (kbd "C-c b l") 'compile-app-lib)
  (local-set-key (kbd "C-c b s") 'compile-app-soft)
  (local-set-key (kbd "C-c b r") 'recompile)
  (local-set-key (kbd "C-c b a") 'rebuild-app))

(add-hook 'c-mode-common-hook 'c-mode-shortcuts)

;;--------------------------------------------------------------------------------------------------
;; SQL
;;--------------------------------------------------------------------------------------------------
(use-package sqlformat
  :ensure t
  :bind (("C-c q f r" . sqlformat-region)
         ("C-c q f b" . sqlformat-buffer))
  :config
  (setq sqlformat-command 'pgformatter)
  (setq sqlformat-args '("-s2" "-g")))

(add-hook 'sql-interactive-mode-hook '(lambda () (toggle-truncate-lines 1)))
(setq sql-user "doqboard")
(setq sql-database "doqboard")

;; From https://www.emacswiki.org/emacs/SqlMode
(add-hook 'sql-login-hook 'my-sql-login-hook)
(defun my-sql-login-hook ()
  "Custom SQL log-in behaviours.  See 'sql-login-hook'."
  ;; n.b. If you are looking for a response and need to parse the
  ;; response, use `sql-redirect-value' instead of `comint-send-string'.
  (when (eq sql-product 'postgres)
    (let ((proc (get-buffer-process (current-buffer))))
      ;; Output each query before executing it. (n.b. this also avoids
      ;; the psql prompt breaking the alignment of query results.)
      (comint-send-string proc "\\set ECHO queries\n"))))

;;--------------------------------------------------------------------------------------------------
;; UNFILL
;;--------------------------------------------------------------------------------------------------
(use-package unfill
  :ensure t
  :bind (("C-c u r" . unfill-region)))

;;--------------------------------------------------------------------------------------------------
;; HIDESHOW
;;--------------------------------------------------------------------------------------------------
(use-package hideshow
  :hook ((prog-mode . hs-minor-mode))
  :bind (("C-<" . hs-toggle-hiding)
         ("C->" . hs-show-all)
         ("C-M-<" . hs-hide-all)))

;;--------------------------------------------------------------------------------------------------
;; GOOGLE TRANSLATE
;;--------------------------------------------------------------------------------------------------
(use-package google-translate
  :ensure t
  :defer t
  :bind (("C-c g q" . google-translate-query-translate)
         ("C-c g Q" . google-translate-query-translate-reverse)
         ("C-c g p" . google-translate-at-point)
         ("C-c g P" .  google-translate-at-point-reverse))
  :init
  (setq google-translate-default-source-language "fr")
  (setq google-translate-default-target-language "en")
  ;; Emacs default backend is broken for now so use the curl one
  (setq google-translate-backend-method 'curl))

;;--------------------------------------------------------------------------------------------------
;; ORG
;;--------------------------------------------------------------------------------------------------
(use-package org
  :ensure t
  :bind (("C-c o w" . open-current-work-notes)
         ("C-c o W" . open-previous-work-notes)
         ("C-c o t l" . org-todo-list)
         ("C-c o c l" . org-agenda-show-custom)
         ("C-c <left>" . org-metaleft)
         ("C-c <right>" . org-metaright)
         ("C-c o e s" . org-slack-export-to-clipboard-as-slack)
         ("C-c o n d" . org-notes-new-day))
  :config
  (setq org-log-done 'time)
  (setq org-startup-folded nil)
  (setq org-agenda-files (list org-work-notes-path))
  (setq org-todo-keywords
        '((sequence "TODO" "SUSPENDED" "|" "DONE" "CANCELLED")))
  (setq org-agenda-custom-commands
        '(("c" "Custom agenda view"
           ((agenda "")
            (alltodo "")))))
  (defun org-agenda-show-custom (&optional arg)
    (interactive "P")
    (org-agenda arg "c"))

  ;; Unbind conflicting keys
  (define-key org-mode-map (kbd "<M-left>") nil)
  (define-key org-mode-map (kbd "<M-right>") nil)
  (define-key org-mode-map (kbd "<M-S-left>") nil)
  (define-key org-mode-map (kbd "<M-S-right>") nil)
  (define-key org-mode-map (kbd "<S-left>") nil)
  (define-key org-mode-map (kbd "<S-right>") nil)
  (define-key org-mode-map (kbd "<S-up>") nil)
  (define-key org-mode-map (kbd "<S-down>") nil)

  (defun open-work-notes (year)
    "Open work notes file of YEAR."
    (interactive)
    (find-file (format "%s/%s.org" org-work-notes-path year)))

  (defun open-current-work-notes ()
    "Open current work notes file."
    (interactive)
    (open-work-notes (format-time-string "%Y")))

  (defun open-previous-work-notes ()
    "Open previous work notes file."
    (interactive)
    (open-work-notes (number-to-string (- (string-to-number(format-time-string "%Y")) 1))))

  (defun org-notes-new-day ()
      "Add new day entry to org notes"
      (interactive)
      (insert "....................................................................................................")
      (newline)
      (insert "* ")
      (org-insert-time-stamp (current-time))))

(use-package ox-gfm
  :ensure t)

(require 'ox-slack)

;;--------------------------------------------------------------------------------------------------
;; TELEPHONE MODE LINE
;;--------------------------------------------------------------------------------------------------
(use-package telephone-line
  :ensure t
  :config
  (telephone-line-mode 1))

;;--------------------------------------------------------------------------------------------------
;; CRUX
;;--------------------------------------------------------------------------------------------------
(use-package crux
  :ensure t
  :bind (("C-c c r" . crux-rename-file-and-buffer)))

;;--------------------------------------------------------------------------------------------------
;; QUICK EMACS CONF FILE OPENNING
;;--------------------------------------------------------------------------------------------------
(defun open-emacs-init-file ()
  "Open the Emacs configuration file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;;--------------------------------------------------------------------------------------------------
;; SHELL
;;--------------------------------------------------------------------------------------------------
;; Prevent large shell output to be slow
(setq comint-move-point-for-output nil)
(setq comint-scroll-show-maximum-output nil)
(setq shell-font-lock-keywords nil)
;; Force shell-command to load .bashrc
(unless (eq system-type 'gnu/linux) ;; Conflicting with Projectile on Linux
  (setq shell-command-switch "-ic"))

;;--------------------------------------------------------------------------------------------------
;; MISC
;;--------------------------------------------------------------------------------------------------
(server-start) ;; Start emacs as server
(global-auto-revert-mode t) ;; Auto reload file if changed on disk
(fset 'yes-or-no-p 'y-or-n-p) ;; Enable Y/N answers
(put 'upcase-region 'disabled nil)
(delete-selection-mode 1) ;; Ensure to delete a selected region when hitting a key
(define-key isearch-mode-map (kbd "<backspace>") 'isearch-del-char) ;; More natural behaviour for
;; backspace in isearch
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))
(setq set-mark-command-repeat-pop t) ;; C-SPC reverts C-u C-SPC before add marks
(defun get_nb_months_since(month year)
  "Return the number of months since the date MONTH/YEAR."
  (let ((current_year (nth 5 (decode-time (current-time))))
        (current_month (nth 4 (decode-time (current-time)))))
    (+ (* 12 (- current_year year)) (- current_month month))))

;;--------------------------------------------------------------------------------------------------
;; GENERATED BY EMACS
;;--------------------------------------------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" "3cc2385c39257fed66238921602d8104d8fd6266ad88a006d0a4325336f5ee02" default))
 '(gdb-create-source-file-list nil)
 '(org-agenda-files nil)
 '(package-selected-packages
   '(unfill sqlformat company-auctex auctex telephone-line powerline smart-mode-line ws-butler kurecolor flycheck-css-colorguard rainbow-mode js2-refactor js2-mode org po-mode realgud-ipdb markdown-mode jest json-navigator realgud multi-term restclient indium dashboard rjsx-mode build-helper elpy diminish ibuffer-projectile ivy-yasnippet yasnippet highlight-indentation birds-of-paradise-plus-theme php-mode counsel-gtags counsel-projectile counsel all-the-icons-ivy google-translate web-mode powershell ggtags init-open-recentf treemacs-projectile treemacs magit tide string-inflection flycheck all-the-icons-dired use-package company-web add-node-modules-path geben yasnippet-snippets projectile yascroll auto-complete chess yaml-mode buffer-move zenburn-theme dracula-theme company-tern json-mode ag s exec-path-from-shell))
 '(paradox-github-token t)
 '(safe-local-variable-values
   '((eval pyvenv-workon "django-api-env")
     (mocha-reporter . "spec")
     (mocha-project-test-directory . "test")
     (mocha-options . "--recursive --exit")
     (mocha-environment-variables . "set NODE_ENV=test &&")
     (buffer-file-coding-system . utf-8-unix)
     (eval setq-local projectile-project-compilation-cmd
           (if
               (eq system-type 'windows-nt)
               "mvnw.cmd" "mvnw"))
     (eval setq-local projectile-project-compilation-cmd "mvnw.cmd")
     (eval setq projectile-project-compilation-cmd "mvnw.cmd")
     `(projectile-project-compilation-cmd \,
                                          (if
                                              (eq system-type 'windows-nt)
                                              "mvnw.cmd" "msvw"))
     (projectile-project-compilation-cmd \,
                                         (if
                                             (eq system-type 'windows-nt)
                                             "mvnw.cmd" "msvw"))
     (projectile-project-compilation-cmd cons
                                         (if
                                             (eq system-type 'windows-nt)
                                             "mvnw.cmd" "msvw"))
     (projectile-project-compilation-cmd \`
                                         (if
                                             (eq system-type 'windows-nt)
                                             "mvnw.cmd" "msvw"))
     (projectile-project-compilation-cmd if
                                         (eq system-type 'windows-nt)
                                         "mvnw.cmd" "msvw")
     (projectile-project-compilation-cmd . "mvnw.cmd")))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))

;;; init.el ends here
