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

;; Don't use tabs.
;; http://www.jwz.org/doc/tabs-vs-spaces.html
(setq-default indent-tabs-mode nil)

;; ido mode for visiting files and switching buffers.
(ido-mode t)
;; Any item containing the entered characters matches.
(setq ido-enable-flex-matching t)

;; Replace default buffer menu (C-x C-b) with ibuffer.
(defalias 'list-buffers 'ibuffer)

;; Smooth scrolling.
;; http://www.emacswiki.org/emacs/SmoothScrolling
(setq scroll-step 1
      scroll-conservatively 10000)

;; Add plugins directory to load path.
(setq load-path (cons "~/.emacs.d/plugins" load-path))

;;
;; org-mode
;;
;; Default:  Here:
;; --------  -------
;; * Foo     * Foo
;; ** Bar     * Bar
;; *** Baz     * Baz
(setq org-hide-leading-stars t)

;;
;; C++
;;

;; Default:     Here:
;; -----------  ------------
;; if (a == b)  if (a == b)
;;   {          {
;;     foo();     foo();
;;   }          }
(c-set-offset 'substatement-open 0)

;;
;; Perl
;;

;; cperl-mode is supposedly superior to perl-mode:
;; http://www.emacswiki.org/emacs/CPerlMode
(defalias 'perl-mode 'cperl-mode)
;; Indent with 4 chars as per perlstyle(1).
(setq cperl-indent-level 4)

;;
;; Haskell
;;

(load "~/.emacs.d/plugins/haskell-mode-2.7.0/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;; Choose exactly one of three indentation modes.
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

;;
;; ParEdit for lisps.
;;
(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode +1)))
(add-hook 'lisp-mode-hook             (lambda () (paredit-mode +1)))
(add-hook 'scheme-mode-hook           (lambda () (paredit-mode +1)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))

;;
;; Home vs. work.
;;

;; Settings specific to my home PC.
(if (file-exists-p "~/.emacs.d/home.el") (load-library "~/.emacs.d/home.el"))
;; Settings specific to my work PC.
(if (file-exists-p "~/.emacs.d/work.el") (load-library "~/.emacs.d/work.el"))