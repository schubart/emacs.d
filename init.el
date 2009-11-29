;; Don't start with *GNU Emacs* (about) buffer.
(setq inhibit-startup-message t)

;; Keep version controlled backups in separate directory.
;; http://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files
(setq backup-directory-alist `(("." . "~/.emacs_backups")))
(setq version-control t)
(setq delete-old-versions t)

;; Highlight matching parentheses.
(show-paren-mode t)

;; Color trailing whitespace.
(setq-default show-trailing-whitespace t)

;; Show column number in status bar.
(setq column-number-mode t)

;; ido mode for visiting files and switching buffers.
(ido-mode t)
;; Any item containing the entered characters matches.
(setq ido-enable-flex-matching t)

;; Settings specific to my home PC.
(if (file-exists-p "~/.emacs.d/home.el") (load-library "~/.emacs.d/home.el"))
;; Settings specific to my work PC.
(if (file-exists-p "~/.emacs.d/work.el") (load-library "~/.emacs.d/work.el"))