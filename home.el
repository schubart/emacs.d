;; GNU Emacs 23.1:
;; Source from gnu.org
;; Compile with --with-ns
;; "make install"
;; Copy to /Applications

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

;; Load Haskell mode.
;(load "~/haskell-mode-2.4/haskell-site-file")
;; Tell Haskell mode which interpreter to use.
;(set-variable 'haskell-program-name "ghci")

;; Magit.
;(setq load-path (cons "~/magit/mainline" load-path))
;(autoload 'magit-status "magit" nil t)
