;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Terminal setup management ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun setup-modeline-colors ()
  (set-face-foreground 'modeline "green")
  (set-face-foreground 'modeline-inactive "black")
  (set-face-background 'modeline "black")
  (set-face-background 'modeline-inactive "white"))

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
      (load-theme 'wombat)))
