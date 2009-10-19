;; Baris Metin <baris@metin.org>

(require 'cl)

;;;;;;;;;;;;;;;
;; Functions ;;
;;;;;;;;;;;;;;;

;; Platform macros
(defmacro Darwin (&rest body)
  (list 'if (eq system-type 'darwin)
        (cons 'progn body)))

(defmacro Linux (&rest body)
  (list 'if (eq system-type 'gnu/linux)
        (cons 'progn body)))

(defmacro Windows (&rest body)
  (list 'if (string= window-system "w32")
        (cons 'progn body)))

(defmacro require-maybe (feature &optional file)
  `(require ,feature ,file 'noerror)) 

(defmacro when-available (func foo)
  `(when (fboundp ,func) ,foo)) 

(defmacro idle-exec (&rest body)
  "Run body with when emacs is idle"
  (list
   'run-with-idle-timer 0.001 nil
   (list 'lambda nil (cons 'progn body))))

;;;###autoload
(defun error-message (msg)
  "Print out an error message"
  (message "Error: %s" (propertize msg 'face 'error-face)))

;;;###autoload
(defun count-words ()
  "Count words in a region or buffer"
  (interactive)
  (let ((b (if mark-active (mark) (point-min)))
        (e (if mark-active (point) (point-max))))
    (message (int-to-string (how-many "\\w+" b e)))))


;;;;;;;;;;;;;;;;;
;; Basic Setup ;;
;;;;;;;;;;;;;;;;;
(tool-bar-mode nil)
(blink-cursor-mode t)
(setq make-backup-files nil)
(setq inhibit-startup-message t)
(setq line-number-mode t)
(setq column-number-mode t)
(setq scroll-step 1)
(setq show-trailing-whitespace nil)
(setq frame-title-format "%b (%m)") ;; filename (mode)
(global-hl-line-mode -1)
(setq show-paren-style 'mixed)
(setq show-paren-mode t)(show-paren-mode t)
(setq transient-mark-mode t) ;; highlight selected region
(setq-default indent-tabs-mode nil)
(setq visible-bell t) ;; don't beep 
(setq default-line-spacing 0.1) ;; same line-spacing with TextMate
(setq current-language-environment "UTF-8")

(setq ispell-program-name "aspell")
(ispell-change-dictionary "english" 1)

(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-enable-last-directory-history nil)

(setq dired-listing-switches "-l")
(setq dired-recursive-copies 'top)
(setq dired-recursive-deletes 'top)

;; use a saner indentation style
(setq c-default-style
      '((java-mode . "java") (other . "stroustrup")))

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
              (flyspell-prog-mode)
              (outline-minor-mode)
              (imenu-add-menubar-index)))) ; generate index

(add-hook 'text-mode-hook
          (lambda ()
            (flyspell-mode)))

(defalias 'yes-or-no-p 'y-or-n-p)

;;;;;;;;;;;;;;;;
;; Keys Setup ;;
;;;;;;;;;;;;;;;;

;; make ALT+backspace work
(define-key global-map [(meta backspace)] 'backward-kill-word)

; use Meta + arrow keys to switch windows.
(windmove-default-keybindings 'meta)

;; scroll window... vim had this nice thing too.
(global-set-key (kbd "M-n") (lambda () (interactive) (scroll-up 1)))
(global-set-key (kbd "M-p") (lambda () (interactive) (scroll-down 1)))

;;; file completions
(global-set-key (kbd "C-'") 'ff-find-other-file)  

;; switching buffers
(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
(global-set-key (kbd "C-x b") 'ibuffer)

;; dired
(global-set-key (kbd "C-x d") 'ido-dired)
(global-set-key (kbd "C-x C-d") 'ido-dired)

; make CTRL+C f complete filename
(global-set-key (kbd "C-c f") 'comint-dynamic-complete-filename)

(global-set-key (kbd "<f1>") 'switch-to-latest-non-shell)
(global-set-key (kbd "<f2>") 'switch-to-shell)
(global-set-key (kbd "<f3>") 'new-shell)
(global-set-key (kbd "<f6>") 'compile)
(global-set-key (kbd "<f7>") 'recompile)

(Darwin
 (setq mac-command-modifier 'meta)
 (setq x-select-enable-clipboard t))

;;;;;;;;;;;;;;;;;
;; Load addons ;;
;;;;;;;;;;;;;;;;;
(when (file-name-directory load-file-name)
  (let ((addon-dir (concat (file-name-directory load-file-name) "addon")))
    (dolist (elt (ignore-errors (directory-files addon-dir t ".*\.el$")))
          (load-file elt))))

