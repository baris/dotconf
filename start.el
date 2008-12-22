;; Baris Metin <baris@metin.org>
;;
;; emacs is such a slut, there's nothing she won't do for you ;)

(add-to-list 'load-path (file-name-directory load-file-name))
(require 'variables)
(require 'functions)
(require 'mywiki)

(setup-initial-frame-parameters)

;;;;;;;;;;;
;; Style ;;
;;;;;;;;;;;
(tool-bar-mode nil)
(menu-bar-mode t)
;;(set-scroll-bar-mode nil)
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

;; (setq line-spacing 0.1) ;; same line-spacing with TextMate
(setq current-language-environment "UTF-8")
(Windows
 (set-keyboard-coding-system 'iso8859-9)
 (setq current-language-environment "iso8859-9"))

;; my color theme...
;; (global-font-lock-mode 0)
(set-cursor-color "#000000")
(custom-set-faces
 '(default ((t (:background "#f0f0f0" :foreground "#000000"))))
;; '(ido-first-match ((t (:foreground "#33ff33"))))
;; '(ido-subdir ((t (:foreground "#4444ff"))))
 '(font-lock-comment-face ((t (:foreground "#229955"))))
 '(font-lock-string-face ((t (:foreground "#4444ff"))))
 '(font-lock-keyword-face ((t (:bold nil :foreground "#851565"))))
 '(font-lock-constant-face ((t (:bold t :foreground "#000000"))))
 '(font-lock-builtin-face ((t (:foreground "#851565"))))
 '(font-lock-type-face ((t (:foreground "#851565"))))
 '(font-lock-variable-name-face ((t (:foreground "#000000"))))
 '(font-lock-function-name-face ((t (:bold t :foreground "#000000"))))
 '(font-lock-warning-face ((t (:underline t :foreground "VioletRed"))))
 '(font-lock-negation-char-face ((t (:bold t :foreground "#000000"))))
 '(font-lock-preprocessor-face ((t (:bold t :foreground "#000000")))))

;;;;;;;;;;;
;; Modes ;;
;;;;;;;;;;;
(add-to-list 'load-path 3rd_party-root)

(setq tramp-default-method "ssh")

(Emacs22+
 (ido-mode t)
 (setq ido-enable-flex-matching t)
 (setq ido-enable-last-directory-history nil))

;; use English dictionary by default
(Pardus
 (setq ispell-program-name "aspell-en"))
(Darwin
 (setq ispell-program-name "aspell"))

(setq dired-listing-switches "-l")
(setq dired-recursive-copies 'top)
(setq dired-recursive-deletes 'top)

(setq compilation-scroll-output t)

(Linux
 (setq browse-url-browser-function 'browse-url-firefox))
(Darwin
 (setq browse-url-browser-function 'browse-url-default-macosx-browser))

;; make same buffer/file names unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " -> ")

;; use c++-mode for header files
(add-to-list 'auto-mode-alist
             '("\\.h\\'" . c++-mode))

;; use text-mode for lex/yacc until I find a better mode.
(add-to-list 'auto-mode-alist
             '("\\.lex\\'" . text-mode))
(add-to-list 'auto-mode-alist
             '("\\.yacc\\'" . text-mode))

;; use a saner indentation style
(setq c-default-style
      '((java-mode . "java") (other . "stroustrup")))

;; javascript.el, a sane mode for .js files
(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
(autoload 'javascript-mode "javascript" nil t)

;; css-mode.el
(autoload 'css-mode "css-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))

;; lua-mode
(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

;; textmate parens
(load-3rd_party-file "textmate.el")

;; prety gdb mode
(setq gdb-many-windows t)

;; show functions in the mode line.
(which-function-mode 1)

;; ChangeLog files (C-4-a)
(setq add-log-full-name "Barış Metin"
      add-log-mailing-address "baris@metin.org")

;;;;;;;;;;;;;;;;
;; Mode Hooks ;;
;;;;;;;;;;;;;;;;

(dolist (elt (list 'python-mode-hook
                   'lua-mode-hook
                   'c++-mode-hook 'c-mode-hook
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
              (outline-minor-mode)
              (textmate-mode)
              (imenu-add-menubar-index)))) ; generate index

(dolist (elt (list 'c++mode-hook 'c-mode-hook
                   'java-mode-hook))
  (add-hook elt
            (lambda ()
              (local-set-key (kbd "C-j") 'newline-and-indent-with-curline-indent))))

;; python-mode-hook
(add-hook 'python-mode-hook
          (lambda ()
            (setq outline-regexp " *\\(def \\|clas\\|#hea\\)")))

;; Open the files designated by emacsclient in their own frame
(add-hook 'server-switch-hook
          (lambda nil
            (let ((server-buf (current-buffer)))
              (bury-buffer)
              (switch-to-buffer-other-frame server-buf))))

;; Cleanup things
(add-hook 'server-done-hook
          (lambda nil
            (kill-buffer nil)
            (delete-frame)))


(require 'yasnippet-bundle)
(require 'magit) ; fantastic git mode.

(defalias 'yes-or-no-p 'y-or-n-p)
(require 'keys)

(provide 'start)

