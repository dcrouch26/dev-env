(require 'package)
(package-initialize)
(setq package-archives '(("marmalade" . "http://marmalade-repo.org/packages/")
                         ("tromey" . "http://tromey.com/elpa/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; The packages you want installed. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:
(defvar my-packages
  '(paredit
    clojure-mode
    clojure-mode-extra-font-locking
    cider
    smex
    projectile
    rainbow-delimiters
    tagedit
    ido-completing-read+))

;; on OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(if (eq system-type 'darwin)
    (add-to-list 'my-packages 'exec-path-from-shell))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH")))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(global-set-key (kbd "M-i") 'previous-line)
(global-set-key (kbd "M-k") 'next-line)
(global-set-key (kbd "M-j") 'left-char)
(global-set-key (kbd "M-u") 'left-word)
(global-set-key (kbd "M-S-j") 'move-beginning-of-line)
(global-set-key (kbd "M-l") 'right-char)
(global-set-key (kbd "M-o") 'right-word)
(global-set-key (kbd "M-S-l") 'move-end-of-line)
(global-set-key (kbd "M-SPC") 'set-mark-command)
(global-set-key (kbd "C-x C-x") 'kill-sexp)
(global-set-key (kbd "<f1>") 'eshell)
(global-set-key (kbd "<f2>") 'rgrep)
(global-set-key (kbd "<home>") 'move-beginning-of-line)
(global-set-key (kbd "<end>") 'move-end-of-line)
(global-set-key (kbd "<next>") 'forward-paragraph)
(global-set-key (kbd "<prior>") 'backward-paragraph)
(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "C-z") 'undo)

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(add-to-list 'load-path "~/.emacs.d/customizations/")
(load "navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; Hard-to-categorize customizations
(load "misc.el")

;; For editing lisps
(load "elisp-editing.el")
(load "setup-clojure.el")
(load "setup-js.el")

(split-window-below)
(find-file "~/notes/todo")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(package-selected-packages
   (quote
    (clj-refactor tagedit smex rainbow-delimiters projectile paredit exec-path-from-shell clojure-mode-extra-font-locking cider ido-completing-read+))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
