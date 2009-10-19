;; Helper functions for PLCAPI
;; Baris metin <tmetin ~at~ sophia.inria.fr>
;;
;; Basic setup
;; (setq myplc-server "https://baris.onelab.eu")
;; (setq myplc-username "tmetin@sophia.inria.fr")

(require 'xml-rpc)


(defvar myplc-server "https://www.planet-lab.eu"
  "URL for you PLC server")

(defvar myplc-username "username")
(defvar myplc-password nil)

(setq *myplc-methods* '())
(setq *myplc-help-buffer-name* "*myplc-help*")

(defun myplc-server-api ()
  (concat myplc-server "/PLCAPI/"))

(defun myplc-populate-methods ()
  (save-window-excursion
    (save-excursion
      (progn
        (dolist (elt (xml-rpc-method-call (myplc-server-api) "system.listMethods"))
          (add-to-list '*myplc-methods* elt))))))

(defun myplc-authentication ()
  (if (not myplc-password) (setq myplc-password (read-passwd "MyPLC Password: ")))
  (list (cons "Username" myplc-username)
        (cons "AuthString" myplc-password)
        (cons "AuthMethod"  "password")
        (cons "Role" "user")))

(defun myplc-help (&optional method-name)
  (interactive "")
  (if (not *myplc-methods*)
      (myplc-populate-methods))
  (if (not method-name)
      (setq method-name (completing-read "Method Name: " *myplc-methods*)))
  (if (get-buffer *myplc-help-buffer-name*)
      (kill-buffer (get-buffer *myplc-help-buffer-name*)))
  (let ((b (get-buffer-create *myplc-help-buffer-name*)))
    (save-excursion
      (set-buffer b)
      (insert (xml-rpc-method-call (myplc-server-api)
                                   "system.methodHelp"
                                   method-name))
      (goto-line 0))
    (switch-to-buffer-other-window b)))


(defmacro myplc-call (&rest args)
  "first argument is the method name and the rest is parameters to the method"
  (setq *myplc-dummy-method-call* (list 'xml-rpc-method-call
                                        '(myplc-server-api)
                                        (car args))) ;; first argument is the method name
  (if (not (string= "system." (ignore-errors (substring (car args) 0 (length "system.")))))
      ;; need to add the authentication for non-system methods
      (add-to-list '*myplc-dummy-method-call* '(myplc-authentication) t))
  ;; now add the parameters to the method
  (dolist (elt (cdr args))
    (add-to-list '*myplc-dummy-method-call* elt t))
  *myplc-dummy-method-call*)


;; examples:
;; (myplc-call "GetNodes" '(("hostname" . "*")) '("hostname"))
;; (myplc-call "system.methodSignature" "GetNodes")


(provide 'myplc)
