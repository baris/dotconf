;; Baris Metin <baris@metin.org>
;;
;; emacs is such a slut, there's nothing she won't do for you ;)

(add-to-list 'load-path (file-name-directory load-file-name))
(require 'cl)
(require 'variables)
(require 'functions)
(let ((__curdir (getenv "HOME"))) ;; FIXME: get current working dir
      (progn (cd emacs-root)
             (normal-top-level-add-subdirs-to-load-path)
             (cd __curdir)))

(if window-system
    (progn
;;      (Pardus
;;       (add-to-list 'default-frame-alist '(font . "monospace-10")))
      (Darwin
       (create-fontset-from-fontset-spec
        (concat
         "-apple-monaco-medium-r-normal--11-*-*-*-*-*-fontset-monaco,"
         "ascii:-apple-monaco-medium-r-normal--11-100-*-*-m-100-mac-roman,"
         "latin-iso8859-1:-apple-monaco-medium-r-normal--11-100-*-*-m-100-mac-roman"))
       (add-to-list 'default-frame-alist '(font . "fontset-monaco"))
       (add-to-list 'default-frame-alist '(alpha . 95)))))

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
(setq default-input-method "rfc1345")
(setq visible-bell t) ;; don't beep 

(setq default-line-spacing 0.1) ;; same line-spacing with TextMate
(setq current-language-environment "UTF-8")
(Windows
 (set-keyboard-coding-system 'iso8859-9)
 (setq current-language-environment "iso8859-9"))

;;(global-font-lock-mode 0)
;; my color themes
(require 'theme)
(wombat-theme)

(Emacs22+
 (ido-mode t)
 (setq ido-enable-flex-matching t)
 (setq ido-enable-last-directory-history nil))

(setq compilation-scroll-output t)

(setq tramp-default-method "ssh")

(setq dired-listing-switches "-l")
(setq dired-recursive-copies 'top)
(setq dired-recursive-deletes 'top)

;; make same buffer/file names unique
(when (require-maybe 'uniquify)
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator " -> "))

;; prety gdb mode
(setq gdb-many-windows t)

;; show functions in the mode line.
(which-function-mode 1)

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

;; use a saner indentation style
(setq c-default-style
      '((java-mode . "java") (other . "stroustrup")))

(defalias 'yes-or-no-p 'y-or-n-p)
(require 'mywiki)
(require 'myplc)
(require 'keys)

(require 'magit)                        ; fantastic git mode.

(require 'pabbrev)
(global-pabbrev-mode)
;;(setq pabbrev-read-only-error nil)

(provide 'start)
