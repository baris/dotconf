;;;;;;;;;;;;;;;;
;; Keys Setup ;;
;;;;;;;;;;;;;;;;

(defvar baris-keys-minor-mode-map (make-keymap) "baris-keys-minor-mode keymap.")

(define-key baris-keys-minor-mode-map (kbd "C-x .") 'hippie-expand)

;; use Meta + arrow keys to switch windows.
;; (windmove-default-keybindings 'meta)
(define-key baris-keys-minor-mode-map (kbd "C-M-o") 'other-window)

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

(define-key baris-keys-minor-mode-map (kbd "M-n")
  (lambda ()
    (interactive)
    (if (string-match ".*Minibuffer.*" mode-name)
        (next-history-element 1)
      (scroll-down 1))))
(define-key baris-keys-minor-mode-map (kbd "M-p")
  (lambda ()
    (interactive)
    (if (string-match ".*Minibuffer.*" mode-name)
        (previous-history-element 1)
      (scroll-up 1))))

;; file completions
(define-key baris-keys-minor-mode-map (kbd "C-'") 'ff-find-other-file)

;; switching buffers
(define-key baris-keys-minor-mode-map (kbd "C-x C-b") 'ido-switch-buffer)
(define-key baris-keys-minor-mode-map (kbd "C-x b") 'ibuffer)

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

(baris-keys-minor-mode 1)
