;; Baris Metin <baris@metin.org>

(autoload 'xml-rpc-method-call "xml-rpc" t)

(defvar weblog-xmlrpc-url "http://www.metin.org/weblog/xml_rpc/"
  "weblog xml-rpc interface")

(defun weblog ()
  (interactive)
  (let ((blog-title (read-string "Title: "))
        (blog-tags-list (split-string
                         (read-string "Tags (comma separated): ")
                         ",")))
    (weblog-new-entry  blog-title  blog-tags-list)))

(defun weblog-tags ()
  (interactive)
  (progn
    (let ((tag-count 0)
          (tag-string ""))
      (dolist (var (weblog-get-tags))
          (progn
            (setq tag-count (1+ tag-count))
            (setq tag-string
                  (concat tag-string
                          (format "* %-15s"
                                  (propertize var 'face 'message-face))))
            (if (= tag-count 3)
                (progn
                  (setq tag-string (concat tag-string "\n"))
                  (setq tag-count 0))
              (setq tag-string (concat tag-string "\t")))))
      (message tag-string))))


(defun weblog-new-entry (blog-title blog-tags-list)
  (message (xml-rpc-method-call weblog-xmlrpc-url
                                'new_entry
                                (read-string "Username: ")
                                (read-passwd "Password: ")
                                blog-title
                                (buffer-string)
                                 blog-tags-list))) 

(defun weblog-get-tags ()
  (xml-rpc-method-call weblog-xmlrpc-url 'all_tags))


(provide 'blog)
