;; Baris Metin <baris@metin.org>

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


;; commands for eshell
(defun ccmd (&optional d) (cd (concat (v_get "v_usr_home_dir") "/work/cmd/" d)))
(defun clib (&optional d) (cd (concat (v_get "v_usr_home_dir") "/work/lib/" d)))
(defun cdso (&optional d) (cd (concat (v_get "v_usr_home_dir") "/work/dso/" d)))
(defun cdaily ()
  (let ((directory ""))
    (cd (concat (v_get "v_resource_dir") "/usr_update"))
    (shell-command "v_get -e v_resource_dir")))


(provide 'bakery)
