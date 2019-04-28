(require 'package)
(setq package-enable-at-startup nil)    ; avoid loading packages again after processing this file (we are calling package-initialize already)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package exec-path-from-shell       ; use user's $PATH rather than system-wide's $PATH in OS X
  :ensure
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

(menu-bar-mode -1)                      ; don't show menu bar
(tool-bar-mode -1)                      ; don't show tool bar
(scroll-bar-mode -1)                    ; don't show scroll bars
(column-number-mode 1)                  ; show column number in the mode line
(toggle-indicate-empty-lines)           ; distinguish lines after the buffer en
(show-paren-mode 1)                     ; highlight matching parenthesis
(save-place-mode 1)                     ; move point to where it was last time
(global-auto-revert-mode t)             ; revert buffers automatically when underlying files are changed externally
(ido-mode 1)                            ; enable ido mode
(ido-everywhere 1)                      ; ???
(setq inhibit-startup-screen t          ; don't show "welcome to emacs" screen
      ring-bell-function (lambda () (message "*beep*")) ; disable annoying beep noise and show a message instead
      backup-directory-alist `(("." . ,(concat user-emacs-directory ; don't place backups files all over
                                               "backups")))
      require-final-newline t           ; add newline when saving file
      load-prefer-newer t               ; load most recent when several versions of the same file exist
      apropos-do-all t                  ; search more extensively but maybe slower
      save-interprogram-paste-before-kill t ; put clipboard into kill ring before replacing it
      ido-enable-flex-matching t        ; use flex matching in ido mode
      frame-resize-pixelwise t          ; wihtout this frame size is *proportional* to char size
      default-frame-alist '((internal-border-width . 0)) ; no black border — not sure if the next line is needed
      initial-frame-alist '((internal-border-width . 0)))

(add-hook 'before-save-hook             ; clean whitespace when saving
          'delete-trailing-whitespace)

(defalias 'yes-or-no-p 'y-or-n-p)       ; no need to type yes or no

(use-package ns-auto-titlebar
  :ensure
  :if (eq system-type 'darwin)
  :config
  (ns-auto-titlebar-mode))

(use-package ace-window
  :ensure
  :bind ("C-x o" . ace-window)
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package add-node-modules-path      ; flycheck needs this to pick up eslint
  :ensure
  :config
  (add-hook 'js-mode-hook #'add-node-modules-path))

(use-package flycheck
  :ensure
  :config
  (add-hook 'js-mode-hook #'flycheck-mode))

(use-package uniquify                   ; include path for buffers with the same name
  :config
  (setq uniquify-buffer-name-style 'forward))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(use-package hydra :ensure)

(use-package dumb-jump
  :ensure)

(defhydra dumb-jump-hydra (:color blue :columns 3)
  "Dumb Jump"
  ("j" dumb-jump-go "Go")
  ("o" dumb-jump-go-other-window "Other window")
  ("e" dumb-jump-go-prefer-external "Go external")
  ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
  ("i" dumb-jump-go-prompt "Prompt")
  ("l" dumb-jump-quick-look "Quick look")
  ("b" dumb-jump-back "Back"))

(global-set-key (kbd "C-; j") 'dumb-jump-hydra/body)
(setq dumb-jump-force-searcher 'ag)
(setq dumb-jump-selector 'ivy)

(use-package counsel
  :ensure
  :bind
  (("C-; a" . counsel-ag)
   ("C-; i" . counsel-imenu)
   ("C-; p" . counsel-git)))

(use-package ivy
  :demand                               ; loaded by counsel
  :config
  (setq ivy-use-virtual-buffers t       ; add recentf-mode and bookmarks to ivy-switch-buffer
        ivy-count-format "%d/%d "))     ; display candidates filtered / total count

(use-package swiper
  :demand                               ; loaded by counsel
  :bind (("C-; s" . swiper)))

(use-package rainbow-delimiters
  :ensure
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(setq org-hide-leading-stars t)

(setq js-indent-level 2)
(setq css-indent-offset 2)
(setq-default indent-tabs-mode nil)

(use-package imenu-list
  :ensure
  :config
  (setq imenu-list-auto-resize t
        imenu-list-focus-after-activation t
        imenu-list-mode-line-format nil)
  :bind
  (("C-; C-i" . imenu-list-smart-toggle)))

(use-package company :ensure)

(use-package rjsx-mode
  :ensure
  :mode ("\\.jsx?\\'"))

(use-package web-mode
  :ensure
  :mode ("\\.html?\\'" "\\.jsx\\'")
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-attr-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

(use-package prettier-js :ensure)

(use-package clojure-mode :ensure)

(use-package inf-clojure :ensure)

(defun lumo-repl()
  (interactive)
  (inf-clojure "lumo -d"))

(use-package paredit
  :ensure
  :config
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode))

(use-package markdown-mode
  :ensure
  :mode ("\\.md\\'" "\\.markdown\\'"))

(use-package magit
  :ensure
  :bind ("C-; g" . magit-status))

(use-package diff-hl
  :ensure
  :init
  (global-diff-hl-mode t)               ; enable highlight in all buffers
  (diff-hl-flydiff-mode t)              ; highlight changes on the fly
  :config
  (setq diff-hl-side 'right             ; show highlight in the right fringe
        diff-hl-draw-borders nil)       ; solid highlight
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)) ; refresh highlight after commit in magit

;; git config --global github.user <github-user-name>
;; git config --global github.oauth-token <access-token-with-gist-scope>
;; token can be generated from https://github.com/settings/tokens
(use-package gist
  :ensure
  :config
  (setq gist-ask-for-description t))    ; ask for gist when creating gist

(use-package browse-at-remote
  :ensure
  :config
  (setq browse-at-remote-prefer-symbolic nil))

(load (expand-file-name "user.el" user-emacs-directory) 'noerror) ; load file ignored by git

(when (memq window-system '(mac ns))    ; render emoji and nicer title bar
  (setq-default ns-right-alternate-modifier 'none)  ; right option is not meta
  (set-fontset-font t                   ; render emoji
                    'symbol
                    (font-spec :family "Apple Color Emoji")
                    nil
                    'prepend)
  (add-to-list 'default-frame-alist     ; paint title bar with background
               '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist     ; white title bar text
               '(ns-appearance . dark)))
