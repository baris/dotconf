;; Baris Metin <baris@metin.org>

;;;;;;;;;;;;;;;;;;;;;;
;; Shell management ;;
;;;;;;;;;;;;;;;;;;;;;;

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
  (let ((new-buffer-name (make-shell-buffer-name name)))
    (rename-buffer (make-shell-buffer-name name))
    new-buffer-name))

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
    (if (>= (safe-length buffers) 1)
        (let ((buffer-to-switch (ido-completing-read "Switch to Shell: " buffers)))
          (if (member buffer-to-switch buffers)
              (switch-to-buffer (make-shell-buffer-name buffer-to-switch))
            (switch-to-buffer (new-shell buffer-to-switch))))
      (new-shell nil))))

;;;###autoload
(defun switch-to-latest-non-shell ()
  (interactive)
  (if (not (null my-latest-non-shell-buffer))
      (switch-to-buffer (get-buffer my-latest-non-shell-buffer))))

;;;###autoload
(defun toggle-shell ()
  (interactive)
  (if (string-prefix-p my-shell-prefix (buffer-name))
      (switch-to-latest-non-shell)
    (switch-to-shell)))

(defalias 'sh 'switch-to-shell)
(defalias 'nh 'new-shell)
(defalias 'sb 'switch-to-latest-non-shell)
