;; Baris Metin <baris@metin.org>

                                        ; make ALT+backspace work
(define-key global-map [(meta backspace)] 'backward-kill-word)

;; window and frame keybindings
(windmove-default-keybindings 'meta) ; use Meta + arrow keys to switch windows.
(global-unset-key [C-S-up])
(global-unset-key [C-S-down])
(global-unset-key [C-S-left])
(global-unset-key [C-S-right])
(global-set-key [C-S-left] '(lambda () (interactive) (frame-move-x -30)))
(global-set-key [C-S-right] '(lambda () (interactive) (frame-move-x 30)))
(global-set-key [C-S-up] '(lambda () (interactive) (frame-move-y -10)))
(global-set-key [C-S-down] '(lambda () (interactive) (frame-move-y 10)))

(global-set-key [M-return] 'frame-toggle-fullscreen)

(global-set-key (kbd "C-|") 'enlarge-current-window-to-max)

;; use Control keys instead of M-* keys like M-x (Yegge tip)
(global-set-key (kbd "C-x C-g") 'execute-extended-command)
(define-key minibuffer-local-map "\C-n" 'next-complete-history-element)
(define-key minibuffer-local-map "\C-p" 'previous-complete-history-element)

;; scroll window... vim had this nice thing too.
(global-set-key (kbd "M-n") (lambda () (interactive) (scroll-up 1)))
(global-set-key (kbd "M-p") (lambda () (interactive) (scroll-down 1)))

;;; completions
(global-set-key (kbd "C-/") 'hippie-expand)
(global-set-key (kbd "C-'") 'ff-find-other-file)  

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x b") 'ido-switch-buffer)

;; dired
(global-set-key (kbd "C-x d") 'ido-dired)
(global-set-key (kbd "C-x C-d") 'ido-dired)

; make CTRL+C f complete filename
(global-set-key (kbd "C-c f") 'comint-dynamic-complete-filename)

(global-set-key (kbd "C-c s") 'speedbar)

(global-set-key (kbd "<f1>") 'switch-to-latest-non-shell)
(global-set-key (kbd "<f2>") 'switch-to-shell)
(global-set-key (kbd "<f3>") 'new-shell)
(global-set-key (kbd "<f6>") 'compile)
(global-set-key (kbd "<f7>") 'recompile)
;;(global-set-key (kbd "<f8>") 'rnd_make_call)
;;(global-set-key (kbd "<f9>") 'rnd_make)

(Darwin
 (setq mac-command-modifier 'meta)
 (setq x-select-enable-clipboard t))

(provide 'keys)
