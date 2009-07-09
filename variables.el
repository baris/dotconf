;; Baris Metin <baris@metin.org>

(require 'functions)

(Linux
 (defvar depo-root (concat (getenv "HOME") "/work")
   "Personal repository's root directory"))

(Darwin
 (defvar depo-root (concat (getenv "HOME") "/work")
   "Personal repository's root directory"))

(Windows
 (defvar depo-root "c:/work"
   "Personal repository's root directory"))

(defvar emacs-root (concat depo-root "/emacs")
  "Personal emacs directory")

(defvar emacs-colors-initialized nil
  "color mode initialized?")

;; Faces
(defface error-face '((t (:foreground "black" :background "red" :bold t)))
  "error face")
(defface message-face '((t (:foreground "black" :background "green")))
  "message face")


(provide 'variables)
