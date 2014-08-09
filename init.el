;; Don't start with *GNU Emacs* (about) buffer.
(setq inhibit-startup-message t)

;;
;; Package management.
;;

(require 'package)
(setq package-archives '(("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

;; Installs the package if it's not installed.
(defun schubart-package-ensure-installed (package)
  ;; Make sure we have the list of available packages.
  (unless package-archive-contents
    (package-refresh-contents))
  ;; Install package if it's not installed.
  (unless (package-installed-p package)
    (package-install package)))

;; Packages that don't require any further initialisation:
(schubart-package-ensure-installed 'fill-column-indicator)
(schubart-package-ensure-installed 'lua-mode)
(schubart-package-ensure-installed 'yaml-mode)

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

;; Replace default buffer menu (C-x C-b) with ibuffer.
(defalias 'list-buffers 'ibuffer)

;; Use bits of path to make buffer names unique.
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; Smooth scrolling.
;; http://www.emacswiki.org/emacs/SmoothScrolling
(setq scroll-step 1
      scroll-conservatively 10000)

;;
;; org-mode
;;
(require 'org-install)
; Key bindings as recommended by the manual.
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-default-notes-file "~/org/notes.org")
(setq org-agenda-files (list "~/org/notes.org"))

;; Default:  Here:
;; --------  -------
;; * Foo     * Foo
;; ** Bar     * Bar
;; *** Baz     * Baz
(setq org-hide-leading-stars t)

;;
;; Ido
;;

;; ido mode for visiting files and switching buffers.
(ido-mode t)
;; Everywhere.
(schubart-package-ensure-installed 'ido-ubiquitous)
(ido-ubiquitous-mode 1)
;; flx (not flex) matching.
(schubart-package-ensure-installed 'flx-ido)
(flx-ido-mode 1)
;; Disable ido faces to see flx highlights.
(setq ido-use-faces nil)
;; Any item containing the entered characters matches.
(setq ido-enable-flex-matching t)
;; Vertical.
(schubart-package-ensure-installed 'ido-vertical-mode)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)

;;
;; Smex: Like ido for M-x
;;

(schubart-package-ensure-installed 'smex)
(global-set-key (kbd "M-x") 'smex)

;;
;; Projectile
;;

(schubart-package-ensure-installed 'projectile)
(projectile-global-mode)

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

;; Default:         Here:
;; ---------------  ------------
;; namespace foo {  namespace foo {
;;   bar() {}       bar() {}
;; }                }
(c-set-offset 'innamespace 0)

;;
;; GNU Global (gtags)
;;
;; http://www.gnu.org/software/global/
;; https://github.com/leoliu/ggtags
;;
(schubart-package-ensure-installed 'ggtags)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))))

;;
;; Perl
;;

;; cperl-mode is supposedly superior to perl-mode:
;; http://www.emacswiki.org/emacs/CPerlMode
(defalias 'perl-mode 'cperl-mode)
;; Indent with 4 chars as per perlstyle(1).
(setq cperl-indent-level 4)

;;
;; Magit
;;
(schubart-package-ensure-installed 'magit)
(global-set-key (kbd "M-m") 'magit-status)

;;
;; git-gutter+: https://github.com/nonsequitur/git-gutter-plus
;;
(schubart-package-ensure-installed 'git-gutter+)
;; Turn it on.
(global-git-gutter+-mode t)
;; Hide gutter if there are no changes.
(setq git-gutter+-hide-gutter t)

;;
;; w3m: Command line web browser.
;;
;; http://w3m.sourceforge.net/index.en.html
;; http://emacs-w3m.namazu.org/
;; http://beatofthegeek.com/2014/02/my-setup-for-using-emacs-as-web-browser.html
;;
(schubart-package-ensure-installed 'w3m)
(setq browse-url-browser-function 'w3m-goto-url-new-session)

;;
;; Compilation
;;
(global-set-key (kbd "M-c") 'recompile)
(setq compilation-scroll-output 'first-error)

;;
;; Yasnippet
;;
(schubart-package-ensure-installed 'yasnippet)
(yas-global-mode 1)

;;
;; Desktop mode with automatic saving when Emacs is idle
;;
;; http://www.emacswiki.org/emacs/DeskTop
(require 'desktop)
(desktop-save-mode 1)
(defun my-desktop-save ()
  (interactive)
  ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
  (if (eq (desktop-owner) (emacs-pid))
      (desktop-save desktop-dirname)))
(add-hook 'auto-save-hook 'my-desktop-save)

;;
;; Perforce
;;

(schubart-package-ensure-installed 'p4)
(require 'p4)

;;
;; Home vs. work.
;;

;; Settings specific to my home PC.
(if (file-exists-p "~/.emacs.d/home.el") (load-library "~/.emacs.d/home.el"))
;; Settings specific to my work PC.
(if (file-exists-p "~/.emacs.d/work.el") (load-library "~/.emacs.d/work.el"))

;; Customize.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cperl-array-face ((t (:background "black" :foreground "yellow" :weight bold))))
 '(cperl-hash-face ((t (:background "black" :foreground "Red" :slant italic :weight bold))))
 '(flx-highlight-face ((t (:inherit font-lock-variable-name-face))))
 '(magit-diff-add ((t (:foreground "green"))))
 '(magit-diff-del ((t (:foreground "red"))))
 '(magit-item-highlight ((t (:underline t)))))
