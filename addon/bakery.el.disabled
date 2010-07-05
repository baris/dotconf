;; handy functions for The Bakery...

(setq tags-table-list '("/studio/resource/rnd"))


(defvar *bakery-system-users* '()
  "")


(defun read-shell-command-output ()
  (save-current-buffer
    (progn
      (set-buffer (get-buffer "*Shell Command Output*"))
      (buffer-substring 1 (buffer-size)))))


(defun populate-bakery-system-users ()
  (save-window-excursion
    (save-excursion
      (save-current-buffer
        (progn
          (setq *bakery-system-users* '())
          (message "running-cmd")
          (shell-command "sys_usr_list")
          (set-buffer (get-buffer "*Shell Command Output*"))
          (goto-char 0))
        (let ((*continue* t))
          (while *continue*
            (let ((*line-start* (search-forward-regexp "^" (buffer-size) t))
                  (*line-end* (search-forward-regexp "$" (buffer-size) t)))
              (if (or (eq *line-start* nil) (eq *line-end* nil))
                  (setq *continue* nil)
                (if (eq *line-start* *line-end*)
                    (goto-char (+ *line-end* 1))
                  (progn
                    (add-to-list '*bakery-system-users*
                                 (list (buffer-substring-no-properties *line-start* *line-end*) 1))
                    (goto-char *line-end*)))))))))))


(defun yo ()
  (interactive)
  (progn
    (populate-bakery-system-users)
    (shell-command
     (concat (executable-find "yo")
             " "
             (completing-read "To user: " *bakery-system-users*)
             " -m "
             (shell-quote-argument (read-string "Message (single-line): "))))))


(defun v_get (varname)
  (shell-command (concat "v_get -e " varname))
  (read-shell-command-output))

(defun daily (&optional *date*)
  (interactive)
  (shell-command
   (concat (executable-find "daily_append")
           " "
           (if *date*
               (concat "-date " *date* " ")
             "")
           (completing-read "Type: " '(("dev" 1) ("sup" 2) ("misc" 1)) nil t)
           " \"" (shell-quote-argument (read-string "Project: ")) "\" "
           (read-string "Percent: ") 
           " \"" (shell-quote-argument (read-string "Title: ")) "\" "
           " \"" (shell-quote-argument (read-string "Notes: ")) "\" ")))

(defun daily-date ()
  (interactive)
  (daily (read-string "Date: ")))

(defun open-daily ()
  (interactive)
  (shell-command "date +%Y_%m_%d")
  (let ((*date* (read-shell-command-output)))
    (find-file (concat (v_get "v_resource_dir") "/usr_update/" *date* "_" (v_get "v_usr_name")))))


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


;; commands for eshell
(defun ccmd (&optional d) (cd (concat (v_get "v_usr_home_dir") "/work/cmd/" d)))
(defun clib (&optional d) (cd (concat (v_get "v_usr_home_dir") "/work/lib/" d)))
(defun cdso (&optional d) (cd (concat (v_get "v_usr_home_dir") "/work/dso/" d)))
(defun cdaily ()
  (let ((directory ""))
    (cd (concat (v_get "v_resource_dir") "/usr_update"))
    (shell-command "v_get -e v_resource_dir")))


(require 'derived)
(require 'cc-mode)

(defvar bakery-pod-indentation
  '("bakery"
    (c-basic-offset . 4)
    (c-comment-only-line-offset . 4)))

(define-derived-mode bakery-pod-mode c-mode "Bakery POD"
  "Major mode to edit POD files"
  (font-lock-add-keywords
   nil
   '(("true" . font-lock-constant-face)
     ("false" . font-lock-constant-face)
     ("\\([a-zA-Z][a-zA-Z0-9_:\.]*\\)[ \t]+[a-zA-Z][a-zA-Z0-9_:\.]*[ \t]*=" 1 font-lock-builtin-face)
     ("[a-zA-Z][a-zA-Z0-9_:\.]*[ \t]+\\([a-zA-Z][a-zA-Z0-9_:\.]*\\)[ \t]*=" 1 font-lock-keyword-face)
     ("\\([a-zA-Z][a-zA-Z0-9_:\.]*\\)[ \t]*=" 1 font-lock-keyword-face)
     ("[^_]\\([e]?[-]?[0-9]+\\)" 1 font-lock-constant-face)))
  
  (setq bakery-pod-mode-imenu-expression
        '((nil "[ \t\n\r{};]+\\([a-zA-Z][a-zA-Z0-9_:\.]*\\)[ \t]*=" 1)))
  (setq imenu-generic-expression bakery-pod-mode-imenu-expression)
  (imenu-add-menubar-index)
  )

(dolist (elt (list '("\\.pod" . bakery-pod-mode)
                   '("\\.mud" . bakery-pod-mode)
                   '("\\.gop" . bakery-pod-mode)
                   '("\\.gos" . bakery-pod-mode)
                   '("var" . bakery-pod-mode)))
  (add-to-list 'auto-mode-alist elt))

(add-to-list 'c-default-style
             '(bakery-pod-mode . "bakery"))


(provide 'bakery)
