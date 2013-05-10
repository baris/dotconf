;;;;;;;;;;;;;;;;;;;;;;;
;; Setup on Mac OS X ;;
;;;;;;;;;;;;;;;;;;;;;;;

(Darwin
 (setq x-select-enable-clipboard t)
 (setq mac-option-modifier 'meta)
 (setq mac-command-modifier 'meta)
 ;; re-assign other-frame to Command-` (s-`) since we take s- away
 ;; we'll need a way to switch frames.
 (global-set-key (kbd "M-`") 'other-frame))
