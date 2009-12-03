;; Baris Metin <baris@metin.org>
(require 'cl)

;;;;;;;;;;;;;;;
;; Functions ;;
;;;;;;;;;;;;;;;

;; Platform macros

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
(tool-bar-mode -1)
(setq make-backup-files nil)
(setq inhibit-startup-message t)
(setq line-number-mode t)
(setq column-number-mode t)
(setq scroll-step 1)
(setq frame-title-format "%b (%m)") ;; filename (mode)
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


;; make same buffer/file names unique
(when (require-maybe 'uniquify)
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator " -> "))

(setq tramp-default-method "ssh")
(setq gdb-many-windows t)  ;; prety gdb mode
(which-function-mode 1)  ;; show functions in the mode line.
(setq compilation-scroll-output t)

(add-hook 'text-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(FIXME\\):" 1 font-lock-warning-face t)
                                      ("\\<\\(TODO\\):" 1 font-lock-warning-face t)))
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

(Darwin
 (setq mac-option-modifier 'meta)
 (setq mac-command-modifier 'meta)
 (setq x-select-enable-clipboard t))

;;;;;;;;;;;;;;;;;
;; Load addons ;;
;;;;;;;;;;;;;;;;;
(when (file-name-directory load-file-name)
  (setq *emacs-addon-dir* (concat (file-name-directory load-file-name) "addon")))
(idle-exec
 (dolist (elt (ignore-errors (directory-files *emacs-addon-dir* t ".*\.el$")))
   (load-file elt)))

