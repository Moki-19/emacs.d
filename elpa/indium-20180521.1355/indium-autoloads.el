;;; indium-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "indium-backend" "indium-backend.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from indium-backend.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "indium-backend" '("indium-")))

;;;***

;;;### (autoloads nil "indium-breakpoint" "indium-breakpoint.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from indium-breakpoint.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "indium-breakpoint" '("indium-breakpoint-")))

;;;***

;;;### (autoloads nil "indium-chrome" "indium-chrome.el" (0 0 0 0))
;;; Generated autoloads from indium-chrome.el

(autoload 'indium-run-chrome "indium-chrome" "\
Start chrome/chromium with remote debugging enabled.
Open URL if provided.

\(fn URL)" t nil)

(autoload 'indium-connect-to-chrome "indium-chrome" "\
Open a connection to a v8 tab.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "indium-chrome" '("indium-chrome-")))

;;;***

;;;### (autoloads nil "indium-debugger" "indium-debugger.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from indium-debugger.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "indium-debugger" '("indium-debugger-")))

;;;***

;;;### (autoloads nil "indium-debugger-frames" "indium-debugger-frames.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from indium-debugger-frames.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "indium-debugger-frames" '("indium-debugger-")))

;;;***

;;;### (autoloads nil "indium-debugger-litable" "indium-debugger-litable.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from indium-debugger-litable.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "indium-debugger-litable" '("indium-debugger-litable-")))

;;;***

;;;### (autoloads nil "indium-debugger-locals" "indium-debugger-locals.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from indium-debugger-locals.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "indium-debugger-locals" '("indium-debugger-locals")))

;;;***

;;;### (autoloads nil "indium-inspector" "indium-inspector.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from indium-inspector.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "indium-inspector" '("indium-inspector-")))

;;;***

;;;### (autoloads nil "indium-interaction" "indium-interaction.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from indium-interaction.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "indium-interaction" '("indium-")))

;;;***

;;;### (autoloads nil "indium-list-scripts" "indium-list-scripts.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from indium-list-scripts.el

(autoload 'indium-list-scripts "indium-list-scripts" "\
Display a list of parsed scripts.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "indium-list-scripts" '("indium-list-scripts-")))

;;;***

;;;### (autoloads nil "indium-nodejs" "indium-nodejs.el" (0 0 0 0))
;;; Generated autoloads from indium-nodejs.el

(autoload 'indium-run-node "indium-nodejs" "\
Start a NodeJS process.

Execute COMMAND, adding the `--inspect' flag.  When the process
is ready, open an Indium connection on it.

If `indium-nodejs-inspect-brk' is set to non-nil, break the
execution at the first statement.

If a connection is already open, close it.

\(fn COMMAND)" t nil)

(autoload 'indium-restart-node "indium-nodejs" "\
Restart the current nodejs process, and connect to it.

If no process has been started, or if it was not started using
`indium-run-node', do nothing.

\(fn)" t nil)

(autoload 'indium-connect-to-nodejs "indium-nodejs" "\
Open a connection to HOST:PORT/PATH.

\(fn HOST PORT PATH)" t nil)

(autoload 'indium-nodejs-connect-to-url "indium-nodejs" "\
Connect to a node process with a given URL.

\(fn URL)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "indium-nodejs" '("indium-nodejs-")))

;;;***

;;;### (autoloads nil "indium-render" "indium-render.el" (0 0 0 0))
;;; Generated autoloads from indium-render.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "indium-render" '("indium-")))

;;;***

;;;### (autoloads nil "indium-repl" "indium-repl.el" (0 0 0 0))
;;; Generated autoloads from indium-repl.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "indium-repl" '("indium-" "company-indium-repl")))

;;;***

;;;### (autoloads nil "indium-scratch" "indium-scratch.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from indium-scratch.el

(autoload 'indium-scratch "indium-scratch" "\
Pop to the scratch buffer.
If no scratch buffer exists for the current connection, create
one first.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "indium-scratch" '("indium-scratch-")))

;;;***

;;;### (autoloads nil "indium-script" "indium-script.el" (0 0 0 0))
;;; Generated autoloads from indium-script.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "indium-script" '("indium-")))

;;;***

;;;### (autoloads nil "indium-sourcemap" "indium-sourcemap.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from indium-sourcemap.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "indium-sourcemap" '("indium-")))

;;;***

;;;### (autoloads nil "indium-structs" "indium-structs.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from indium-structs.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "indium-structs" '("indium-" "unless-indium-connected" "when-indium-connected")))

;;;***

;;;### (autoloads nil "indium-v8" "indium-v8.el" (0 0 0 0))
;;; Generated autoloads from indium-v8.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "indium-v8" '("indium-")))

;;;***

;;;### (autoloads nil "indium-workspace" "indium-workspace.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from indium-workspace.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "indium-workspace" '("indium-workspace")))

;;;***

;;;### (autoloads nil nil ("indium-faces.el" "indium-pkg.el" "indium-seq-fix.el"
;;;;;;  "indium.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; indium-autoloads.el ends here
