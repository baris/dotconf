;; -*- mode: lisp -*-

(setenv "PATH" (concat (getenv "PATH") ":" "/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

(defvar my-conf-dir "$HOME/repos/dotconf")
(setq my-dot-emacs-file (concat my-conf-dir
				"/emacs.d/dotemacs.el"))
(load-file my-dot-emacs-file)
