;; Baris Metin <baris@metin.org>

(defun emacs-major-version ()
  "extract major emacs version from emacs-version and return as integer"
  (progn
    (string-match "\\(^[0-9]+\\)\\.\\([0-9]+\\)" emacs-version)
    (string-to-number (match-string 1 emacs-version))))

;; Platform macros
(defmacro Emacs22+ (&rest body)
  (list 'if (>= emacs-major-version 22)
        (cons 'progn body)))

(defmacro Emacs21 (&rest body)
  (list 'if (= emacs-major-version 21)
        (cons 'progn body)))

(defmacro Darwin (&rest body)
  (list 'if (eq system-type 'darwin)
        (cons 'progn body)))

(defmacro Linux (&rest body)
  (list 'if (eq system-type 'gnu/linux)
        (cons 'progn body)))

(defmacro Pardus (&rest body)
  (list 'if (file-regular-p "/etc/pardus-release")
        (cons 'progn body)))

(defmacro !Pardus (&rest body)
  (list 'if (not (file-regular-p "/etc/pardus-release"))
        (cons 'progn body)))

(defmacro Windows (&rest body)
  (list 'if (string= window-system "w32")
        (cons 'progn body)))

(defmacro require-maybe (feature &optional file)
  `(require ,feature ,file 'noerror)) 

(defmacro when-available (func foo)
  `(when (fboundp ,func) ,foo)) 

(defun load-3rd_party-file (filename)
  (load-file (concat 3rd_party-root "/" filename)))

(defun error-message (msg)
  "Print out an error message"
  (message "Error: %s" (propertize msg 'face 'error-face)))

(defun info-message (msg)
  "Print out a message"
  (message "%s" (propertize msg 'face 'message-face)))

(defun count-words ()
  (interactive)
  (let ((b (if mark-active (mark) (point-min)))
        (e (if mark-active (point) (point-max))))
    (message (int-to-string (how-many "\\w+" b e)))))
;; start IRC client
;;;###autoload
(defun start-erc ()
  (interactive "")
  (autoload 'erc-select "erc" t)
  (setq erc-server "irc.freenode.net"
        erc-port 6667
        erc-nick "barismetin"
        erc-user-full-name "Baris Metin"
        erc-email-userid "baris"
        erc-prompt-for-nickserv-password t
        erc-echo-notices-in-current-buffer t
        erc-max-buffer-size 30000
        erc-auto-query t
        erc-send-wihespace-lines nil
        erc-pals '("cartman" "kartman" "caglar10ur" "meren" "madcat")
        erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT"))
  (erc-select))

;;;###autoload
(defun enlarge-current-window-to-max ()
  "enlarge the current window to maximum available heigth"
  (interactive)
  (let ((old-height (window-height (selected-window)))
        (new-height (window-height (selected-window)))
        (old-width (window-width (selected-window)))
        (new-width (window-width (selected-window)))
        (first-run t))
    (while (or first-run
               (or (> new-height old-height) (> new-width old-width)))
      (enlarge-window 1)
      (condition-case err
          (enlarge-window-horizontally 1)
        (error
         nil))
      (setq old-height new-height
            new-height (window-height (selected-window))
            old-width new-width
            new-width (window-width (selected-window)))
      (setq first-run nil))))

;;;###autoload
(defun newline-and-indent-with-curline-indent ()
  (interactive)
  (progn
    (indent-according-to-mode)
    (newline-and-indent)))

;;;###autoload
(defun open-line-keeping-indent ()
  (interactive)
  (progn
    (move-beginning-of-line nil)
    (open-line 1)
    (indent-according-to-mode)))

;;;###autoload
(defun yank-keeping-indent ()
  (interactive)
  (progn
    (set-mark (point))
    (yank)
    (indent-region (mark) (point))))

;;;###autoload
(defun setup-initial-frame-parameters ()
  (if window-system
      (progn 
        (set-frame-position (selected-frame)
                            (frame-parameter (selected-frame) 'left)
                            0)
        (set-frame-width (selected-frame) 120))))

(defun frame-toggle-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen)
                                           nil
                                         'fullboth)))

(defun frame-alpha (alpha-value)
  (interactive "sAlpha Value: ")
  (set-frame-parameter nil 'alpha (string-to-int alpha-value)))

(defun frame-move-x (x)
  (interactive)
  (let ((cur-left (frame-parameter (selected-frame) 'left))
        (max-left (x-display-pixel-width))
        (min-left 0))
    (let ((new-left (+ cur-left x)))
      (if (not (or (> new-left max-left)
                   (< new-left min-left)))
          (set-frame-parameter (selected-frame) 'left new-left)))))

(defun frame-move-y (y)
  (interactive)
  (set-frame-parameter (selected-frame) 'top
                       (+ (frame-parameter (selected-frame) 'top) y)))

;; Manage shells inside emacs.
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(defconst my-shell-prefix "*shell-")
(defconst my-shell-postfix "*")
(defconst my-default-shell-name "default")
(defvar my-latest-non-shell-buffer nil)
(defvar my-preferred-shell 'shell) ;;  eshell?

(defun new-shell (name &optional ansi)
  (interactive "sNew Shell Name: ")
  (if ansi
      (ansi-term "/bin/bash")
    (apply my-preferred-shell nil))
  (if (eq name nil)
      (setq name my-default-shell-name))
  (rename-buffer (make-shell-buffer-name name)))

(defun make-shell-buffer-name (name)
  (concat my-shell-prefix  name my-shell-postfix))

(defun get-name-from-shell-buffer-name (shell-buffer-name)
  (let ((prefix-len (length my-shell-prefix))
        (postfix-len (length my-shell-postfix)))
    (if (string= my-shell-prefix (ignore-errors (substring shell-buffer-name 0 prefix-len)))
        (substring shell-buffer-name prefix-len (- postfix-len))
      nil)))

(defun shell-buffers ()
  (interactive)
  (let ((all-shell-buffers ))
    (dolist (elt (buffer-list))
      (if (get-name-from-shell-buffer-name (buffer-name elt))
          (add-to-list 'all-shell-buffers (get-name-from-shell-buffer-name (buffer-name elt)))))
    all-shell-buffers))

(defun switch-to-shell ()
  (interactive)
  (if (not (get-name-from-shell-buffer-name (buffer-name)))
      (setq my-latest-non-shell-buffer (buffer-name)))
  (let ((buffers (shell-buffers)))
    (let ((buffers-len (safe-length buffers)))
      (if (< buffers-len 2)
          (if (= buffers-len 1)
              (if (string= (buffer-name) (make-shell-buffer-name my-default-shell-name))
                  (new-shell (read-string "New Shell Name: "))
                (switch-to-buffer (make-shell-buffer-name (car buffers))))
            (new-shell nil))
        (switch-to-buffer (make-shell-buffer-name (ido-completing-read "Switch to Shell: " buffers)))))))

(defun switch-to-latest-non-shell ()
  (interactive)
  (if (not (null my-latest-non-shell-buffer))
      (switch-to-buffer (get-buffer my-latest-non-shell-buffer))))

(provide 'functions)

;; A silly function to cascade all frame which is not very useful.
;;
;; ;;;###autoload
;; (defun cascade-my-frames ()
;;   (interactive)
;;   (if window-system
;;       (let ((num-frames (length (frames-on-display-list)))
;;             (screen-width (x-display-pixel-width))
;;             (num-max-columns 2)
;;             (force-height 0)
;;             (force-width 120)
;;             (start-top 0)
;;             (start-left 0))
;;         ;; define height of a frame
;;         (if (> num-frames num-max-columns)
;;             (setq force-height
;;                   (- (/ (/ (x-display-pixel-height) (frame-char-height))
;;                         (ceiling (/ (float num-frames)
;;                                     (float num-max-columns))))
;;                      ;; -4 for windowmanager decorations, menubar and modeline
;;                      4))
;;           (setq force-height
;;                 (- (/ (x-display-pixel-height) (frame-char-height)) 5)))
;;         ;; define width of a frame
;;         (if (> num-frames 1)
;;             (setq force-width
;;                   (- (/ (/ (x-display-pixel-width) (frame-char-width)) 2)
;;                      ;; -6 for window manager decorations
;;                      6)))
;;         ;; resize frames and position them
;;         (dolist (elt (frames-on-display-list))
;;           (progn
;;             (set-frame-width elt force-width)
;;             (set-frame-height elt force-height)))
;;         (dolist (elt (frames-on-display-list))
;;           (progn
;;             (sleep-for 0 100)
;;             (if (eq num-frames 1)
;;                 (set-frame-position elt
;;                                     (- (x-display-pixel-width) (frame-pixel-width elt))
;;                                     0)
;;               (set-frame-position elt start-left start-top))
;;             (if (eq start-left 0)
;;                 (setq start-left (+ start-left
;;                                     (frame-pixel-width elt) 10))
;;               (progn
;;                 (setq start-left 0)
;;                 (setq start-top
;;                       (+ start-top
;;                          (frame-pixel-height elt) 10)))))))))
