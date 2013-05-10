; -*- compile-command: "find . -name '*.el' | xargs etags -a" -*-
;
; Baris Metin <baris@metin.org>

(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;
;; Helper Functions ;;
;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defmacro Darwin (&rest body)
  (list 'if (eq system-type 'darwin)
        (cons 'progn body)))

;;;###autoload
(defmacro Linux (&rest body)
  (list 'if (eq system-type 'gnu/linux)
        (cons 'progn body)))

;;;###autoload
(defmacro Windows (&rest body)
  (list 'if (string= window-system "w32")
        (cons 'progn body)))

;;;###autoload
(defmacro require-maybe (feature &optional file)
  `(require ,feature ,file 'noerror))

;;;###autoload
(defmacro when-available (func foo)
  `(when (fboundp ,func) ,foo))

;;;###autoload
(defmacro idle-exec (&rest body)
  "Run body with when emacs is idle"
  (list
   'run-with-idle-timer 0.5 nil
   (list 'lambda nil (cons 'progn body))))

(defun shell-command-on-buffer (command &optional replace)
  "Run a shell command on current buffer"
  (interactive "sShell Command: ")
  (shell-command-on-region (point-min) (point-max) command nil replace))

(defun json-beautify-buffer ()
  (interactive)
  (shell-command-on-buffer "python -m json.tool" t))

;;;;;;;;;;;;;;;;;;;;;;
;; Install packages ;;
;;;;;;;;;;;;;;;;;;;;;;
(setq external-packages-list '(dash             ; a modern list library for elisp
                               s                ; string manipulation library
                               python-mode      ; Python mode
                               go-mode          ; Go mode
                               lua-mode         ; Lua mode
                               js2-mode         ; JavaScript mode
                               json-mode        ; JSON mode
                               magit            ; git repository management
                               mo-git-blame     ; git-blame mode
                               ahg              ; mercurial repository management
                               paredit          ; parentheses editing (for lisp modes)
                               company          ; Complete Anything
                               full-ack         ; search with ack
                               browse-kill-ring ; interactively select items from kill-ring
                               ))

(if (>= emacs-major-version 24)
    (progn
      (require 'package)
      (add-to-list 'package-archives
                   '("marmalade" .
                     "http://marmalade-repo.org/packages/"))
      (package-initialize)

      (mapc
       (lambda (pkg)
         (if (package-installed-p pkg)
             (require pkg)
           (package-install pkg)))
       external-packages-list)))

;;;;;;;;;;;;;;;;;
;; Load addons ;;
;;;;;;;;;;;;;;;;;
(when load-in-progress
  (setq *emacs-addon-dir* (concat (file-name-directory load-file-name) "addon")))

(dolist (elt (ignore-errors (directory-files *emacs-addon-dir* t ".*\.el$")))
  (load-file elt))

;;;;;;;;;;;;;;;;;
;; Basic Setup ;;
;;;;;;;;;;;;;;;;;
(setq make-backup-files nil)
(setq inhibit-startup-message t)
(setq line-number-mode t)
(setq column-number-mode t)
(setq scroll-step 1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq frame-title-format "%b (%m)") ;; filename (mode)
(setq show-paren-style 'mixed)
(setq show-paren-mode t)(show-paren-mode t)
(setq transient-mark-mode t) ;; highlight selected region
(setq-default indent-tabs-mode nil)
(setq visible-bell t) ;; don't beep
(set-default 'cursor-type 'box)

(setq default-line-spacing 0.1) ;; same line-spacing with TextMate
(setq current-language-environment "UTF-8")
(setq tab-width 4)

;(setq ispell-program-name "aspell")
;(ispell-change-dictionary "english" 1)

(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-enable-last-directory-history nil)

(setq dired-listing-switches "-l")
(setq dired-recursive-copies 'top)
(setq dired-recursive-deletes 'top)

;; make same buffer/file names unique
(when (require-maybe 'uniquify)
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator " -> "))

(setq tramp-default-method "ssh")
(setq gdb-many-windows t)  ;; prety gdb mode
(which-function-mode 1)  ;; show functions in the mode line.
(setq compilation-scroll-output t)


;;;;;;;;;;;;;;;;
;; Mode Hooks ;;
;;;;;;;;;;;;;;;;
(add-hook 'text-mode-hook (lambda ()
                            (flyspell-mode)))

(setq *lisp-prog-mode-hooks* (list
                       'emacs-lisp-mode-hook
                       'ielm-mode-hook
                       'eval-expression-minibuffer-setup-hook))

(setq *other-prog-mode-hooks* (list
                               'python-mode-hook
                               'c-mode-hook
                               'lua-mode-hook
                               'js2-mode-hook
                               'json-mode-hook
                               'c++-mode-hook
                               'objc-mode-hook
                               'go-mode-hook))

(setq *prog-mode-hooks* (append *other-prog-mode-hooks* *lisp-prog-mode-hooks*))

(dolist (*mode-hook* *prog-mode-hooks*)
  (add-hook *mode-hook*
            (lambda ()
              (font-lock-add-keywords
               nil
               '(("\\<\\(FIXME\\):" 1 font-lock-warning-face t)
                 ("\\<\\(TODO\\):" 1 font-lock-warning-face t)))
              (setq show-trailing-whitespace 1)
              (outline-minor-mode)
              (setq c-basic-offset 4)
              (flyspell-prog-mode))))

(dolist (*mode-hook* *lisp-prog-mode-hooks*)
        (add-hook *mode-hook* #'enable-paredit-mode))

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook
          (lambda ()
            (slime-js-minor-mode 1)))

(add-hook 'css-mode-hook
          (lambda ()
            (define-key css-mode-map "\M-\C-x" 'slime-js-refresh-css)
            (define-key css-mode-map "\C-c\C-r" 'slime-js-embed-css)))


;; Enable company mode globally
(add-hook 'after-init-hook 'global-company-mode)

;;;;;;;;;;;;;;;;;;;;;;
;; Function Aliases ;;
;;;;;;;;;;;;;;;;;;;;;;
(idle-exec
 (defalias 'yes-or-no-p 'y-or-n-p))

(idle-exec
 (when (require-maybe 'magit)
   (defalias 'm 'magit-status)))

