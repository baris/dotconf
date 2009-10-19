;; Baris Metin <baris@metin.org>

(require 'variables)
(require 'functions)

;; muse stuff
;;(setq muse-file-extension nil muse-mode-auto-p t)
(when (require-maybe 'muse-html)
  (require 'muse-wiki)

  (defvar mywiki-path (concat depo-root "/muse"))
  (defvar mywiki-html-path (concat mywiki-path "/html"))
  (defvar mywiki-remote-path "")

  (setq my-muse-wiki-style-sheet
        (concat "<link rel=\"stylesheet\" type=\"text/css\""
                " charset=\"utf-8\" media=\"all\""
                " href=\"wiki-style.css\" />"))

  (muse-derive-style "my-wiki-xhtml" "xhtml"
                     :footer (concat mywiki-path "/footer.html")
                     :header (concat mywiki-path "/header.html")
                     :style-sheet my-muse-wiki-style-sheet)

  (setq muse-project-alist
        (list (list "Wiki"
                    (list mywiki-path :default "index")
                    (list :base "my-wiki-xhtml" :path mywiki-html-path))))

  (defun publish-mywiki ()
    (interactive)
    (muse-project-publish "Wiki" t)
    (copy-file (concat mywiki-path "/wiki-style.css") mywiki-html-path t)
    (dolist (elt (directory-files mywiki-path))
      (if (string= ".png" (ignore-errors (substring elt -4)))
          (copy-file (concat mywiki-path "/" elt) mywiki-html-path t))))

  )

  (provide 'mywiki)