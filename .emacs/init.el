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

(exec-path-from-shell-initialize)

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(exec-path-from-shell-initialize)

(global-set-key (kbd "<tab>") #'company-indent-or-complete-common)
(global-set-key (kbd "RET") #'newline-and-indent)

(global-unset-key (kbd "C-p"))
(global-unset-key (kbd "C-n"))
(global-unset-key (kbd "C-j"))
(global-unset-key (kbd "C-l"))
(global-unset-key (kbd "M-i"))
(global-unset-key (kbd "M-k"))
(global-unset-key (kbd "M-l"))
(global-unset-key (kbd "M-i"))
(define-key key-translation-map (kbd "C-i") (kbd "<up>"))
(define-key key-translation-map (kbd "C-k") (kbd "<down>"))
(define-key key-translation-map (kbd "C-j") (kbd "<left>"))
(define-key key-translation-map (kbd "C-l") (kbd "<right>"))

(global-set-key (kbd "M-SPC") 'set-mark-commandl)
(global-set-key (kbd "<f1>") 'eshell)
(global-set-key (kbd "<f2>") 'rgrep)
(global-set-key (kbd "<f3>") 'occur)
(global-set-key (kbd "<f4>") 'just-one-space)
(global-set-key (kbd "C-a") 'kill-sexp)

(global-unset-key (kbd "C-e"))
(global-unset-key (kbd "M-}"))
(global-unset-key (kbd "M-{"))
(global-unset-key (kbd "<C-down>"))
(global-unset-key (kbd "<C-up>"))

(global-unset-key (kbd "<M-left>"))
(global-set-key (kbd "M-j") 'left-word)
(global-unset-key (kbd "<M-right>"))
(global-set-key (kbd "M-l") 'right-word)
(global-set-key (kbd "<home>") 'move-beginning-of-line)
(global-set-key (kbd "M-u") 'move-beginning-of-line)
(global-set-key (kbd "<end>") 'move-end-of-line)
(global-set-key (kbd "M-o") 'move-end-of-line)

(global-set-key (kbd "C-k") 'kill-line)
(global-set-key (kbd "M-<down>") 'forward-paragraph)
(global-set-key (kbd "<next>") 'forward-paragraph)
(global-unset-key (kbd "C-i"))
(global-set-key (kbd "M-<up>") 'backward-paragraph)
(global-set-key (kbd "<prior>") 'backward-paragraph)

(global-unset-key (kbd "C-y"))
(global-set-key (kbd "C-v") 'yank)
(global-unset-key (kbd "C-/"))
(global-set-key (kbd "C-z") 'undo)
;Rebound paredit C-j to C-q?
;(global-unset-key (kbd "C-i"))
;(global-unset-key (kbd "C-j"))
(setq tab-always-indent 'complete)

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

(define-key global-map (kbd "C-;") nil)
(define-key global-map (kbd "M-f") nil)
(define-key paredit-mode-map (kbd "C-d") 'paredit-backward-kill-word)
(define-key paredit-mode-map (kbd "C-M-d") 'paredit-forward-kill-word)
(define-key paredit-mode-map (kbd "M-<up>") nil)
(define-key paredit-mode-map (kbd "M-<down>") nil)
(define-key paredit-mode-map (kbd "M-{") 'paredit-wrap-curly)
(define-key paredit-mode-map (kbd "M-[") 'paredit-wrap-square)
(define-key paredit-mode-map (kbd "M-<left>") 'paredit-backward-up)
(define-key paredit-mode-map (kbd "M-<right>") 'paredit-forward-down)
(define-key paredit-mode-map (kbd "M-j") 'paredit-backward)
(define-key paredit-mode-map (kbd "M-l") 'paredit-forward)
(define-key paredit-mode-map (kbd "C-;") 'paredit-close-curly-and-newline)
(define-key paredit-mode-map (kbd "M-o") 'paredit-forward-slurp-sexp)
(define-key paredit-mode-map (kbd "M-O") 'paredit-forward-barf-sexp)
(define-key paredit-mode-map (kbd "M-u") 'paredit-backward-slurp-sexp)
(define-key paredit-mode-map (kbd "M-U") 'paredit-backward-barf-sexp)
(define-key emacs-lisp-mode-map (kbd "C-M-i") nil)
(define-key cider-mode-map (kbd "C-M-i") nil)
(define-key cider-mode-map (kbd "C-c C-k") nil)
(define-key cider-mode-map (kbd "C-c k") 'cider-load-buffer)
(define-key cider-mode-map (kbd "C-c j") 'cider-repl-set-ns)
(define-key cider-mode-map (kbd "C-c l") 'cider-ns-refresh)
(define-key paredit-mode-map (kbd "C-j") nil)
(define-key cider-repl-mode-map (kbd "<up>") 'cider-repl-backward-input)
(define-key cider-repl-mode-map (kbd "<down>") 'cider-repl-forward-input)
(split-window-below)
(find-file "~/notes/todo")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(company-minimum-prefix-length 2)
 '(package-selected-packages
   (quote
    (company-quickhelp company json-mode clj-refactor tagedit smex rainbow-delimiters projectile paredit exec-path-from-shell clojure-mode-extra-font-locking cider ido-completing-read+))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
