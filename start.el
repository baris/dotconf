;; Baris Metin <baris@metin.org>
;;
;; emacs is such a slut, there's nothing she won't do for you ;)

(add-to-list 'load-path (file-name-directory load-file-name))
(require 'cl)
(require 'variables)
(require 'functions)

(tool-bar-mode nil)
(menu-bar-mode t)
(setq scroll-bar-mode-explicit t)
(blink-cursor-mode t)
(setq line-number-mode t)
(setq column-number-mode t)
(setq show-trailing-whitespace t)
(setq frame-title-format "%b (%m)") ;; filename (mode)
(global-hl-line-mode -1)
(setq inhibit-startup-message t)
(setq make-backup-files nil)
(setq scroll-step 1)
(setq show-paren-style 'mixed)
(setq show-paren-mode t)(show-paren-mode t)
(setq transient-mark-mode t) ;; highlight selected region
(setq-default indent-tabs-mode nil)
(setq visible-bell t) ;; don't beep 
(setq default-line-spacing 0.1) ;; same line-spacing with TextMate
(setq default-input-method "rfc1345")
(setq current-language-environment "UTF-8")

(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-enable-last-directory-history nil)

(setq dired-listing-switches "-l")
(setq dired-recursive-copies 'top)
(setq dired-recursive-deletes 'top)

;; use a saner indentation style
(setq c-default-style
      '((java-mode . "java") (other . "stroustrup")))

;; lazy load additional settings
(idle-exec
 ;; make same buffer/file names unique
 (when (require-maybe 'uniquify)
   (setq uniquify-buffer-name-style 'reverse)
   (setq uniquify-separator " -> "))

 (setq gdb-many-windows t)  ;; prety gdb mode
 (which-function-mode 1)  ;; show functions in the mode line.
 (setq tramp-default-method "ssh")
 (setq compilation-scroll-output t)

 (dolist (elt (list 'python-mode-hook
                    'c++-mode-hook 'c-mode-hook 'objc-mode-hook
                    'java-mode-hook
                    'javascript-mode-hook
                    'emacs-lisp-mode-hook 'lisp-mode-hook))
   (add-hook elt
             (lambda ()
               (font-lock-add-keywords nil
                                       '(("\\<\\(FIXME\\):" 1 font-lock-warning-face t)
                                         ("\\<\\(TODO\\):" 1 font-lock-warning-face t)))
               (local-set-key (kbd "C-o") 'open-line-keeping-indent)
               (local-set-key (kbd "C-y") 'yank-keeping-indent)
               (local-set-key (kbd "C-j") 'newline-and-indent-with-curline-indent)
               (outline-minor-mode)
               (imenu-add-menubar-index)))) ; generate index

 ;; org-mode stuff
 (setq org-todo-keywords
       '((sequence "TODO" "FEEDBACK" "VERIFY" "PROGRESS" "|" "DONE" "DELEGATED" "CANCELED")))

 (setq org-todo-keyword-faces
       '(("TODO"      . org-warning)
         ("PROGRESS"  . shadow)
         ("CANCELED"  . (:foreground "green" :weight bold))))

 ;; erc stuff
 (autoload 'erc-select "erc" t)
 (setq erc-server "irc.freenode.net"
       erc-port 6667
       erc-nick "barismetin"
       erc-user-full-name "Baris Metin"
       erc-email-userid "baris"
       erc-prompt-for-nickserv-password t
       erc-echo-notices-in-current-buffer t
       erc-max-buffer-size 30000
       erc-auto-query t
       erc-send-wihespace-lines nil
       erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT"))

 (require 'pabbrev)
 ;;(setq pabbrev-read-only-error nil)
 (global-pabbrev-mode)

 (require 'myplc)
 (require 'mywiki))

(defalias 'yes-or-no-p 'y-or-n-p)
(require 'keys)

(provide 'start)
