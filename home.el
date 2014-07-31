;; GNU Emacs 24.3 from "port install emacs-app"

;; Mac: Add MacPorts to path (because Emacs.app does not get launched
;; via a shell, so ~/.profile and similar files don't get evaluated.)
;;
;; http://cs.gmu.edu/~sean/stuff/emacs/
;; http://xahlee.org/emacs/emacs_env_var_paths.html
(setq exec-path (cons "/opt/local/bin" exec-path))

;; Mac: Use Command key as meta.
(setq ns-command-modifier (quote meta))

;; Window size.
(setq initial-frame-alist '((width . 80)
			    (height . 71)))
