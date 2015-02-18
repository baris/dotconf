;;; -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;
;; Keys Setup ;;
;;;;;;;;;;;;;;;;
(defvar baris-keys-minor-mode-map (make-keymap) "baris-keys-minor-mode keymap.")

(defvar default-mode-key "*default-mode-key*")
(defun conditional-behavior (mode-behavior-map)
  (let ((default-behavior (gethash default-mode-key mode-behavior-map (lambda () nil)))
        (call-default 't))
    (maphash (lambda (name behavior)
               (if (and
                    (string-match name mode-name)
                    (not (string= name default-mode-key)))
                   (progn
                     (funcall behavior)
                     (setq call-default nil))))
             mode-behavior-map)
    (if call-default
        (funcall default-behavior))))

(define-key baris-keys-minor-mode-map (kbd "M-{") 'previous-buffer)
(define-key baris-keys-minor-mode-map (kbd "M-}") 'next-buffer)
(define-key baris-keys-minor-mode-map (kbd "C-M-b") 'ido-switch-buffer)
(define-key baris-keys-minor-mode-map (kbd "C-M-o") 'other-window)

(if (boundp 'company-mode)
    (define-key baris-keys-minor-mode-map (kbd "C-x .") 'company-complete)
  (define-key baris-keys-minor-mode-map (kbd "C-x .") 'hippie-expand))

;; use meta + {-,+} to resize windows.
(define-key baris-keys-minor-mode-map (kbd "M--") (lambda () (interactive) (enlarge-window -2)))
(define-key baris-keys-minor-mode-map (kbd "<M-wheel-down>")   (lambda () (interactive) nil))
(define-key baris-keys-minor-mode-map (kbd "<M-double-wheel-down>")   (lambda () (interactive) (enlarge-window -1)))

(define-key baris-keys-minor-mode-map (kbd "M-=") (lambda () (interactive) (enlarge-window 2)))
(define-key baris-keys-minor-mode-map (kbd "<M-wheel-up>") (lambda () (interactive) nil))
(define-key baris-keys-minor-mode-map (kbd "<M-double-wheel-up>") (lambda () (interactive) (enlarge-window 1)))

(define-key baris-keys-minor-mode-map (kbd "M-_") (lambda () (interactive) (enlarge-window -2 t)))
(define-key baris-keys-minor-mode-map (kbd "<C-M-wheel-down>") (lambda () (interactive) (enlarge-window -2 t)))

(define-key baris-keys-minor-mode-map (kbd "M-+") (lambda () (interactive) (enlarge-window 2 t)))
(define-key baris-keys-minor-mode-map (kbd "<C-M-wheel-up>") (lambda () (interactive) (enlarge-window 2 t)))

;; scroll window... vim had this nice thing too.
(add-hook 'minibuffer-setup-hook (lambda () (baris-keys-minor-mode nil)))

(let ((behavior-map (make-hash-table :test 'equal)))
  (puthash ".*Minibuffer.*" '(lambda () (next-history-element 1)) behavior-map)
  (puthash "Shell$" '(lambda () (comint-next-input 1)) behavior-map)
  (puthash "IELM" '(lambda () (comint-next-input 1)) behavior-map)
  (puthash default-mode-key '(lambda () (scroll-up 1)) behavior-map)
  (define-key baris-keys-minor-mode-map (kbd "M-n")
    (lambda () (interactive) (conditional-behavior behavior-map))))

(let ((behavior-map (make-hash-table :test 'equal)))
  (puthash ".*Minibuffer.*" '(lambda () (previous-history-element 1)) behavior-map)
  (puthash "Shell$" '(lambda () (comint-previous-input 1)) behavior-map)
  (puthash "IELM" '(lambda () (comint-previous-input 1)) behavior-map)
  (puthash default-mode-key '(lambda () (scroll-down 1)) behavior-map)
  (define-key baris-keys-minor-mode-map (kbd "M-p")
    (lambda () (interactive) (conditional-behavior behavior-map))))

;; file completions
(define-key baris-keys-minor-mode-map (kbd "C-'") 'ff-find-other-file)

;; switching buffers
;(define-key baris-keys-minor-mode-map (kbd "C-x C-b") 'ido-switch-buffer)
(define-key baris-keys-minor-mode-map (kbd "C-x C-b") 'ibuffer)
;(define-key baris-keys-minor-mode-map (kbd "C-x b") 'ibuffer)

;; dired
(define-key baris-keys-minor-mode-map (kbd "C-x d") 'ido-dired)
(define-key baris-keys-minor-mode-map (kbd "C-x C-d") 'ido-dired)

;; make CTRL+C f complete filename
(define-key baris-keys-minor-mode-map (kbd "C-c f") 'comint-dynamic-complete-filename)

;; use find-file-in-repository by default
(define-key baris-keys-minor-mode-map (kbd "C-x C-r") 'find-file-in-repository)

;; shell
(define-key baris-keys-minor-mode-map (kbd "C-c t") 'toggle-shell)
(define-key baris-keys-minor-mode-map (kbd "C-c s") 'switch-to-shell)

(define-minor-mode baris-keys-minor-mode
  "A minor mode for my key bindings."
  t " baris-keys" 'baris-keys-minor-mode-map)

;; Initialize
(baris-keys-minor-mode 1)
