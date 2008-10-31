;; Baris Metin <baris@metin.org>

(defun emacs-major-version ()
  "extract major emacs version from emacs-version and return as integer"
  (progn
    (string-match "\\(^[0-9]+\\)\\.\\([0-9]+\\)" emacs-version)
    (string-to-int (match-string 1 emacs-version))))


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

(defun load-3rd_party-file (filename)
  (load-file (concat 3rd_party-root "/" filename)))

(defun error-message (msg)
  "Print out an error message"
  (message "Error: %s" (propertize msg 'face 'error-face)))

(defun info-message (msg)
  "Print out a message"
  (message "%s" (propertize msg 'face 'message-face)))

(defun use-ido-mode ()
  (Emacs22+
   (autoload 'ido-mode "ido" t))
  (Emacs21
   ;; older version of emacs. load ido.el
   (load-3rd_party-file "ido.el"))
  (ido-mode t)
  (setq ido-enable-last-directory-history nil))

(defun preview-tex-file ()
  "Save the current buffer and run pdflatex on file.
Using this with KPDF works fine."
  (interactive "")
  (save-buffer)
  (shell-command
   (format "%s %s" (executable-find "pdflatex") (buffer-file-name))))

(defun read-real-file ()
  "Open the actual file instead of the symlink"
  (let ((orig-fname buffer-file-name))
    (if (file-symlink-p orig-fname)
        (let ((fname (file-truename orig-fname)))
          (message (format "%s bağı yerine %s dosyasını açtım..." orig-fname fname))
          (find-alternate-file fname)))))
(add-hook 'find-file-hooks 'read-real-file)

;; start IRC client
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
        erc-send-wihespace-lines nil)
  (setq erc-pals '("cartman" "kartman" "caglar10ur" "meren" "madcat"))
  (erc-select))



(defun log-keys ()
  "log keys in a buffer using mwe-log-commands"
  (interactive)
  (progn
    (require 'mwe-log-commands)
    (save-selected-window
      (mwe:log-keyboard-commands)
      (mwe:open-command-log-buffer)
      (let ((w-height (window-height (selected-window))))
        (shrink-window (- w-height 10))))))


(defun switch-c-to-h ()
  "switch c to h"
  (interactive)
  (when (string-match "\\(.*\\)\\(\\..*\\)\\'" buffer-file-name)
    (let ((other-file
           (file-expand-wildcards
            (concat (match-string 1 buffer-file-name)
                    (if (string-match
                         "\\.c\\(c\\|pp\\|xx\\|\\+\\+\\)?\\|\\.CC\\'"
                         (match-string 2 buffer-file-name))
                        ".[hH]*"
                      ".[cC]*"))
            t)))
      (if other-file (find-file (car other-file))))))


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

(defun newline-and-indent-with-curline-indent ()
  (interactive)
  (progn
    (indent-according-to-mode)
    (newline-and-indent)))

(defun open-line-keeping-indent ()
  (interactive)
  (progn
    (move-beginning-of-line nil)
    (open-line 1)
    (indent-according-to-mode)))

(defun yank-keeping-indent ()
  (interactive)
  (progn
    (set-mark (point))
    (yank)
    (indent-region (mark) (point))))


(defun cascade-my-frames ()
  (interactive)
  (if (not (eq window-system nil))
      (let ((num-frames (length (frames-on-display-list)))
            (screen-width (x-display-pixel-width))
            (num-max-columns 2)
            (force-height 0)
            (force-width 120)
            (start-top 0)
            (start-left 0))
        ;; define height of a frame
        (if (> num-frames num-max-columns)
            (setq force-height
                  (- (/ (/ (x-display-pixel-height) (frame-char-height))
                        (ceiling (/ (float num-frames)
                                    (float num-max-columns))))
                     ;; -4 for windowmanager decorations, menubar and modeline
                     4))
          (setq force-height
                (- (/ (x-display-pixel-height) (frame-char-height)) 5)))
        ;; define width of a frame
        (if (> num-frames 1)
            (setq force-width
                  (- (/ (/ (x-display-pixel-width) (frame-char-width)) 2)
                     ;; -6 for window manager decorations
                     6)))
        ;; resize frames and position them
        (dolist (elt (frames-on-display-list))
            (progn
              (set-frame-width elt force-width)
              (set-frame-height elt force-height)))
        (dolist (elt (frames-on-display-list))
          (progn
            (sleep-for 0 100)
            (if (eq num-frames 1)
                (set-frame-position elt
                                    (- (x-display-pixel-width) (frame-pixel-width elt))
                                    0)
              (set-frame-position elt start-left start-top))
            (if (eq start-left 0)
                (setq start-left (+ start-left
                                    (frame-pixel-width elt) 10))
              (progn
                (setq start-left 0)
                (setq start-top
                      (+ start-top
                         (frame-pixel-height elt) 10)))))))))


(defun rnd_make_call (&optional has_args)
  (interactive)
  (progn 
    (setq rnd_make_args "")
    (if has_args
        (setq rnd_make_args (read-string "rnd_make arguments: ")))
    (compile (concat "rnd_make " rnd_make_args))))

(defun rnd_make ()
  (interactive)
  (rnd_make_call t))


(defun start-caml ()
  (interactive)
  (add-to-list 'load-path (concat 3rd_party-root "/caml-mode"))
  (setq auto-mode-alist
        (cons '("\\.ml[iylp]?$" . caml-mode) auto-mode-alist))
  (autoload 'caml-mode "caml" "Major mode for editing Caml code." t)
  (require 'caml-font))

;; Moving in the window quickly
(defun u ()
  (interactive)
  (windmove-up))
(defun d ()
  (interactive)
  (windmove-down))
(defun l ()
  (interactive)
  (windmove-left))
(defun r ()
  (interactive)
  (windmove-right))

(defun new-shell (name)
  (interactive "sName for shell: ")
  (eshell)
  (rename-buffer (format "*shell-%s*" name)))

(defun ff (filename)
  (find-file filename))

(provide 'functions)
