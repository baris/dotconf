;; Baris Metin <baris@metin.org>

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

;;
;; Shell management
;;
(idle-exec
 (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
 (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
 (defconst my-shell-prefix "*shell-")
 (defconst my-shell-postfix "*")
 (defconst my-default-shell-name "default")
 (defvar my-latest-non-shell-buffer nil)
 (defvar my-preferred-shell 'shell)) ;;  eshell?

;;;###autoload
(defun new-shell (name &optional ansi)
  (interactive "sNew Shell Name: ")
  (if ansi
      (ansi-term "/bin/bash")
    (apply my-preferred-shell nil))
  (if (eq name nil)
      (setq name my-default-shell-name))
  (rename-buffer (make-shell-buffer-name name)))

;;;###autoload
(defun make-shell-buffer-name (name)
  (concat my-shell-prefix  name my-shell-postfix))

;;;###autoload
(defun get-name-from-shell-buffer-name (shell-buffer-name)
  (let ((prefix-len (length my-shell-prefix))
        (postfix-len (length my-shell-postfix)))
    (if (string= my-shell-prefix (ignore-errors (substring shell-buffer-name 0 prefix-len)))
        (substring shell-buffer-name prefix-len (- postfix-len))
      nil)))

;;;###autoload
(defun shell-buffers ()
  (interactive)
  (let ((all-shell-buffers ))
    (dolist (elt (buffer-list))
      (if (get-name-from-shell-buffer-name (buffer-name elt))
          (add-to-list 'all-shell-buffers (get-name-from-shell-buffer-name (buffer-name elt)))))
    all-shell-buffers))

;;;###autoload
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

;;;###autoload
(defun switch-to-latest-non-shell ()
  (interactive)
  (if (not (null my-latest-non-shell-buffer))
      (switch-to-buffer (get-buffer my-latest-non-shell-buffer))))

(provide 'functions)
