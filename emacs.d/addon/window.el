;;;;;;;;;;;;;;;;;;;;;;;
;; Window management ;;
;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun fs ()
  "toggle fullscreen frame"
  (interactive)
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen)
                                           nil
                                         'fullboth)))

;;;###autoload
(defun setup-window-modeline-colors ()
  (set-face-foreground 'modeline "black")
  (set-face-foreground 'modeline-inactive "white")
  (set-face-background 'modeline "yellow")
  (set-face-background 'modeline-inactive "black"))

;;;###autoload
(defun setup-window (width height left top)
  (if window-system
      (progn
        (set-frame-size (selected-frame) width height)
        (set-frame-position (selected-frame) left top)
        (if (>= emacs-major-version 24)
            (load-theme 'wombat)))))

;;;###autoload
(defun setup-main-window ()
  (interactive)
  (let
      ((width (floor (/ (* 0.9 (x-display-pixel-width)) (frame-char-width))))
       (height (floor (/ (* 0.85 (x-display-pixel-height)) (frame-char-height))))
       (top 25)
       (left (floor (* 0.05 (x-display-pixel-width)))))
    (progn
      (setup-window width height left top))))

;;;###autoload
(defun setup-side-window (left)
  (let
      ((width (floor (/ (* 0.48 (x-display-pixel-width)) (frame-char-width))))
       (height (floor (/ (* 0.90 (x-display-pixel-height)) (frame-char-height))))
       (top 25))
    (progn
      (setup-window width height left top))))

;;;###autoload
(defun setup-left-window ()
  (interactive)
  (setup-side-window 0))

;;;###autoload
(defun setup-right-window ()
  (interactive)
  (setup-side-window (floor (* 0.50 (x-display-pixel-width)))))

(defalias 'wm 'setup-main-window)
(defalias 'wl 'setup-left-window)
(defalias 'wr 'setup-right-window)

;; Initialization
(if window-system
    (progn
      (setup-main-window)
      (ignore-errors (setup-window-modeline-colors))
      (tool-bar-mode -1)))
