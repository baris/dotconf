;; Copyright (c) 2005-2007 Baris Metin <baris@metin.org>
;;
;; emacs is such a slut, there's nothing she won't do for you.

(add-to-list 'load-path (file-name-directory load-file-name))
(require 'variables)
(require 'functions)

;;(server-start)

;;;;;;;;;;;
;; Style ;;
;;;;;;;;;;;
(tool-bar-mode nil)
(menu-bar-mode t)
(set-scroll-bar-mode t)
;;  (setq scroll-bar-mode-explicit t)
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

(setq current-language-environment "UTF-8")
(Windows
 (set-keyboard-coding-system 'iso8859-9)
 (setq current-language-environment "iso8859-9"))


;; my color theme...
;; (global-font-lock-mode 0)
(set-cursor-color "#000000")
(custom-set-faces
 '(default ((t (:background "#f0f0f0" :foreground "#000000"))))
 
 '(ido-first-match ((t (:foreground "#99FF00"))))
 '(ido-subdir ((t (:foreground "#6BCFF7"))))

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

(setq compilation-scroll-output t)

(Emacs22+
 (require 'blog))

;; use English dictionary by default
(Pardus
 (setq ispell-program-name "aspell-en"))
(Darwin
 (setq ispell-program-name "aspell"))

(use-ido-mode)

(setq dired-listing-switches "-l")
(setq dired-recursive-copies 'top)
(setq dired-recursive-deletes 'top)

(Darwin
 (setq browse-url-browser-function 'browse-url-safari))
(Linux
 (setq browse-url-browser-function 'browse-url-firefox))

;; make same buffer/file names unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " -> ")

;; ibuffer
(Emacs21
  (load-3rd_party-file "ibuffer.el"))
(autoload 'ibuffer "ibuffer" "List Buffers" t)

;; tramp for remote access
(Emacs22+
 ;; no need to load as of version 22
 (require 'tramp))
(setq tramp-default-method "ssh")

;; javascript.el, a sane mode for .js files
(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
(autoload 'javascript-mode "javascript" nil t)

;; css-mode.el
(autoload 'css-mode "css-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))

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

;; ChangeLog files (C-4-a)
(setq add-log-full-name "Barış Metin"
      add-log-mailing-address "baris@metin.org")

;; org-mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)

;; textmate parens
(load-3rd_party-file "textmate.el")

;; prety gdb mode
(setq gdb-many-windows t)

;; show functions in the mode line.
(which-function-mode 1)

;;;;;;;;;;;;;;;;
;; Mode Hooks ;;
;;;;;;;;;;;;;;;;
;; Don't use tabs in any text-mode
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
              (local-set-key (kbd "C-j") 'newline-and-indent)
              (outline-minor-mode)
              (imenu-add-menubar-index)))) ; generate index


;; python-mode-hook
(add-hook 'python-mode-hook
          (lambda ()
            (setq outline-regexp " *\\(def \\|clas\\|#hea\\)")))

;; lua-mode
(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

;; nxml mode use spaces instead of tabs
(add-to-list 'auto-mode-alist
             '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\|html\\)\\'" . nxml-mode))
(fset 'xml-mode 'nxml-mode)


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
(require 'keys)
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'shell 'new-shell)

;; For Bakery
(setq tags-table-list '("/studio/resource/rnd"))

(defun ccmd (&optional d) (cd (concat "~/work/cmd/" d)))
(defun clib (&optional d) (cd (concat "~/work/lib/" d)))
(defun cdso (&optional d) (cd (concat "~/work/dso/" d)))

;;;;;;;;;;;;;;;;;;;;;;;
;; Abandoned modules ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; scroll before the end/beginning of the buffer
;; (Emacs22+
;;  (require 'smooth-scrolling))

;; show white space
;;(require 'show-wspace)
;;(toggle-tabs-font-lock)
;;(toggle-hardspace-font-lock)


;; CMake
;;(load-3rd_party-file "cmake-mode.el")
;;(add-to-list 'auto-mode-alist '("CMakeLists.txt" . cmake-mode))


;; highlight the symbol under point in the
;;(require 'light-symbol)


;; show line numbers like vim does
;;(require 'setnu)


;; ;;setup gnus for Gmail
;; (setq user-mail-address "baris@metin.org")
;; (setq user-full-name "Baris Metin")
;; (setq gnus-user-agent '(emacs gnus))
;; (setq gnus-select-method '(nnimap "metin"
;;                                   (nnimap-address "imap.gmail.com")
;;                                   (nnimap-server-port 993)
;;                                   (nnimap-stream ssl)))
;; (setq smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
;;       smtpmail-smtp-server "smtp.gmail.com"
;;       smtpmail-default-smtp-server "smtp.gmail.com"
;;       send-mail-function 'smtpmail-send-it
;;       message-send-mail-function 'smtpmail-send-it
;;       smtpmail-smtp-service 587
;;       smtpmail-auth-credentials '(("smtp.gmail.com" 587 "baris@metin.org"  nil)))
;; (require 'starttls)
;; (require 'smtpmail)
;; ;;Gnus keys
;; ;;(define-key gnus-summary-mode-map (kbd "d") '(lambda )


;; Ask if I want to use untabify at save
;; (add-hook 'before-save-hook
;;           (lambda ()
;;             (save-excursion
;;               (save-restriction
;;                 (goto-char (point-min))
;;                 (if (search-forward "\t" nil t)
;;                     (if (y-or-n-p "Tabs detected! Untabify file? ")
;;                         (untabify (point-min) (point-max))))))))

(provide 'start)

