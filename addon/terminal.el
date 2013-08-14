;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Terminal setup management ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun setup-terminal-modeline-colors ()
  (ignore-errors (set-face-foreground 'modeline "green"))
  (ignore-errors (set-face-foreground 'modeline-inactive "black"))
  (ignore-errors (set-face-background 'modeline "black"))
  (ignore-errors (set-face-background 'modeline-inactive "white")))

;; Initialization
(if (not window-system)
    (progn
      (require 'mouse)
      (xterm-mouse-mode t)
      (global-set-key [mouse-4] '(lambda ()
                                   (interactive)
                                   (scroll-down 1)))
      (global-set-key [mouse-5] '(lambda ()
                                   (interactive)
                                   (scroll-up 1)))
      (setup-terminal-modeline-colors)
      (load-theme 'wombat)))
