;; Baris Metin <baris@metin.org>
(require 'cl)

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


;;;;;;;;;;;;;;;;;
;; Load addons ;;
;;;;;;;;;;;;;;;;;
(when load-in-progress
  (setq *emacs-addon-dir* (concat (file-name-directory load-file-name) "addon")))

(dolist (elt (ignore-errors (directory-files *emacs-addon-dir* t ".*\.el$")))
  (load-file elt))

;;;;;;;;;;;;;;;;;;;;;;
;; Install packages ;;
;;;;;;;;;;;;;;;;;;;;;;
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
       '(dash s magit ahg go-mode))))


;;;;;;;;;;;;;;;;;
;; Basic Setup ;;
;;;;;;;;;;;;;;;;;
(if window-system
    (progn
      (tool-bar-mode -1)
      (wm))
  (progn
    (require 'mouse)
    (xterm-mouse-mode t)
    (global-set-key [mouse-4] '(lambda ()
                                 (interactive)
                                 (scroll-down 1)))
    (global-set-key [mouse-5] '(lambda ()
                                 (interactive)
                                 (scroll-up 1)))))

(Darwin
 (setq x-select-enable-clipboard t)
 (setq mac-option-modifier 'meta)
 (setq mac-command-modifier 'meta)
 ;; re-assign other-frame to Command-` (s-`) since we take s- away
 ;; we'll need a way to switch frames.
 (global-set-key (kbd "M-`") 'other-frame)
)

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

;;(setq default-line-spacing 0) ;; same line-spacing with TextMate
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

(dolist (*mode-hook* (list 'python-mode-hook 'c-mode-hook 'c++-mode-hook 'objc-mode-hook 'go-mode-hook))
        (add-hook *mode-hook*
                  (lambda ()
                    (font-lock-add-keywords nil
                                            '(("\\<\\(FIXME\\):" 1 font-lock-warning-face t)
                                              ("\\<\\(TODO\\):" 1 font-lock-warning-face t)))q
                    (setq show-trailing-whitespace 1)
                    (outline-minor-mode)
                    (setq c-basic-offset 4)
                    (flyspell-prog-mode))))

(add-hook 'text-mode-hook (lambda () (flyspell-mode)))

(idle-exec
 (defalias 'yes-or-no-p 'y-or-n-p))

(idle-exec
 (when (require-maybe 'magit)
   (defalias 'm 'magit-status)))


;;;;;;;;;;;;;;;;
;; Keys Setup ;;
;;;;;;;;;;;;;;;;

(idle-exec
 (global-set-key (kbd "C-x .") 'hippie-expand)

 ;; use Meta + arrow keys to switch windows.
 ;; (windmove-default-keybindings 'meta)

 ;; use meta + {-,+} to resize windows.
 (global-unset-key (kbd "M--")) (global-set-key (kbd "M--") (lambda () (interactive) (enlarge-window -2)))
 (global-set-key (kbd "<M-wheel-down>")   (lambda () (interactive) nil))
 (global-set-key (kbd "<M-double-wheel-down>")   (lambda () (interactive) (enlarge-window -1)))

 (global-unset-key (kbd "M-=")) (global-set-key (kbd "M-=") (lambda () (interactive) (enlarge-window 2)))
 (global-set-key (kbd "<M-wheel-up>") (lambda () (interactive) nil))
 (global-set-key (kbd "<M-double-wheel-up>") (lambda () (interactive) (enlarge-window 1)))

 (global-unset-key (kbd "M-_")) (global-set-key (kbd "M-_") (lambda () (interactive) (enlarge-window -2 t)))
 (global-set-key (kbd "<C-M-wheel-down>") (lambda () (interactive) (enlarge-window -2 t)))

 (global-unset-key (kbd "M-+")) (global-set-key (kbd "M-+") (lambda () (interactive) (enlarge-window 2 t)))
 (global-set-key (kbd "<C-M-wheel-up>") (lambda () (interactive) (enlarge-window 2 t)))

 ;; scroll window... vim had this nice thing too.
 (global-set-key (kbd "M-n") (lambda () (interactive) (scroll-up 1)))
 (global-set-key (kbd "M-p") (lambda () (interactive) (scroll-down 1)))

 ;; file completions
 (global-set-key (kbd "C-'") 'ff-find-other-file)

 ;; switching buffers
 (global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
 (global-set-key (kbd "C-x b") 'ibuffer)

 ;; dired
 (global-set-key (kbd "C-x d") 'ido-dired)
 (global-set-key (kbd "C-x C-d") 'ido-dired)

 ;; make CTRL+C f complete filename
 (global-set-key (kbd "C-c f") 'comint-dynamic-complete-filename)

 ;; use find-file-in-repository by default
 (global-set-key (kbd "C-x C-r") 'find-file-in-repository)

 ;; shell
 (global-set-key (kbd "C-c t") 'toggle-shell)
 (global-set-key (kbd "C-c s") 'switch-to-shell))
