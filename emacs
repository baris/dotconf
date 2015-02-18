(setenv "PATH" (concat (getenv "PATH") ":" "/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))
(load-file "$HOME/repos/emacs/dotemacs.el")
