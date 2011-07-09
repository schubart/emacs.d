;; Don't start with *GNU Emacs* (about) buffer.
(setq inhibit-startup-message t)

;; Keep version controlled backups in separate directory.
;; http://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files
(setq backup-directory-alist `(("." . "~/.emacs_backups")))
(setq version-control t)
(setq delete-old-versions t)

;; Highlight matching parentheses.
(show-paren-mode t)
;; http://www.emacswiki.org/emacs/ShowParenMode#toc1
(defadvice show-paren-function
      (after show-matching-paren-offscreen activate)
      "If the matching paren is offscreen, show the matching line in the
        echo area. Has no effect if the character before point is not of
        the syntax class ')'."
      (interactive)
      (if (not (minibuffer-prompt))
          (let ((matching-text nil))
            ;; Only call `blink-matching-open' if the character before point
            ;; is a close parentheses type character. Otherwise, there's not
            ;; really any point, and `blink-matching-open' would just echo
            ;; "Mismatched parentheses", which gets really annoying.
            (if (char-equal (char-syntax (char-before (point))) ?\))
                (setq matching-text (blink-matching-open)))
            (if (not (null matching-text))
                (message matching-text)))))

;; Color trailing whitespace.
(setq-default show-trailing-whitespace t)

;; Make the whole comment red, not just the leading // or ;; or #.
;; Seems relevant ony when running in a terminal.
(set-face-foreground 'font-lock-comment-face "red")

;; Show column number in status bar.
(setq column-number-mode t)

;; Don't use tabs.
;; http://www.jwz.org/doc/tabs-vs-spaces.html
(setq-default indent-tabs-mode nil)

;; ido mode for visiting files and switching buffers.
;(ido-mode t)
;; Any item containing the entered characters matches.
;(setq ido-enable-flex-matching t)

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
;; Common Lisp
;;
(setq lisp-indent-function 'common-lisp-indent-function)

;;
;; Smex: https://github.com/nonsequitur/smex/
;;
;(require 'smex)
;(smex-initialize)
;(global-set-key (kbd "M-x") 'smex)
;(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;;
;; Magit
;;

(setq load-path (cons "~/.emacs.d/plugins/magit-1.0.0" load-path))
(load-library "magit")
(global-set-key (kbd "M-m") 'magit-status)
;; Default seems to be black and white, make it a bit more colorful.
(set-face-foreground 'magit-diff-add "green")
(set-face-foreground 'magit-diff-del "red")

;;
;; Compilation
;;
(global-set-key (kbd "M-c") 'recompile)
(setq compilation-scroll-output 'first-error)

;;
;; Yasnippet
;;
(setq load-path (cons "~/.emacs.d/plugins/yasnippet-0.6.1c" load-path))
(require 'yasnippet)
(yas/initialize)
(setq yas/root-directory '("~/.emacs.d/snippets"
                           "~/.emacs.d/plugins/yasnippet-0.6.1c/snippets"))
(mapc 'yas/load-directory yas/root-directory)

;;
;; Anything
;;
(setq load-path (cons "~/.emacs.d/plugins/anything-config" load-path))
(require 'anything-startup)
(global-set-key "\M-1" 'anything)
(global-set-key (kbd "M-x")     'anything-M-x)
(global-set-key (kbd "C-x C-f") 'anything-for-files)
(global-set-key (kbd "C-x b")   'anything-for-buffers)

(setq anything-sources
      '(anything-c-source-buffers+
        anything-c-source-recentf
        anything-c-source-files-in-current-dir+))

; Defaults:
;    (setq anything-sources
;          '(anything-c-source-buffers+
;            anything-c-source-recentf
;            anything-c-source-files-in-current-dir+))

;; Very complete list of sources from
;; http://code.google.com/p/dea/source/browse/trunk/my-lisps/anything-settings.el
;    (setq anything-sources
;          '(;; Buffer:
;            anything-c-source-buffers
;            anything-c-source-buffer-not-found
;            anything-c-source-buffers+
;            ;; File:
;            anything-c-source-file-name-history
;            anything-c-source-files-in-current-dir
;            anything-c-source-files-in-current-dir+
;            anything-c-source-file-cache
;            anything-c-source-locate
;            anything-c-source-recentf
;            anything-c-source-ffap-guesser
;            anything-c-source-ffap-line
;            ;; Help:
;            anything-c-source-man-pages
;            anything-c-source-info-pages
;            anything-c-source-info-elisp
;            anything-c-source-info-cl
;            ;; Command:
;            anything-c-source-complex-command-history
;            anything-c-source-extended-command-history
;            anything-c-source-emacs-commands
;            ;; Function:
;            anything-c-source-emacs-functions
;            anything-c-source-emacs-functions-with-abbrevs
;            ;; Variable:
;            anything-c-source-emacs-variables
;            ;; Bookmark:
;            anything-c-source-bookmarks
;            anything-c-source-bookmark-set
;            anything-c-source-bookmarks-ssh
;            anything-c-source-bookmarks-su
;            anything-c-source-bookmarks-local
;            ;; Library:
;            anything-c-source-elisp-library-scan
;            ;; Programming:
;            anything-c-source-imenu
;            anything-c-source-ctags
;            anything-c-source-semantic
;            anything-c-source-simple-call-tree-functions-callers
;            anything-c-source-simple-call-tree-callers-functions
;            anything-c-source-commands-and-options-in-file
;            ;; Color and Face:
;            anything-c-source-customize-face
;            anything-c-source-colors
;            ;; Search Engine:
;            anything-c-source-tracker-search
;            anything-c-source-mac-spotlight
;            ;; Kill ring:
;            anything-c-source-kill-ring
;            ;; Mark ring:
;            anything-c-source-global-mark-ring
;            ;; Register:
;            anything-c-source-register
;            ;; Headline Extraction:
;            anything-c-source-fixme
;            anything-c-source-rd-headline
;            anything-c-source-oddmuse-headline
;            anything-c-source-emacs-source-defun
;            anything-c-source-emacs-lisp-expectations
;            anything-c-source-emacs-lisp-toplevels
;            anything-c-source-org-headline
;            anything-c-source-eev-anchor
;            ;; Misc:
;            anything-c-source-evaluation-result
;            anything-c-source-calculation-result
;            anything-c-source-google-suggest
;            anything-c-source-call-source
;            anything-c-source-occur
;            anything-c-source-create
;            anything-c-source-minibuffer-history
;            ;; System:
;            anything-c-source-emacs-process))

;;
;; Home vs. work.
;;

;; Settings specific to my home PC.
(if (file-exists-p "~/.emacs.d/home.el") (load-library "~/.emacs.d/home.el"))
;; Settings specific to my work PC.
(if (file-exists-p "~/.emacs.d/work.el") (load-library "~/.emacs.d/work.el"))