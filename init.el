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

(setq inhibit-startup-screen t)         ; don't show "welcome to emacs" screen
(setq ring-bell-function                ; disable annoying beep noise and show a message instead
  (lambda () (message "*beep*")))
(menu-bar-mode -1)                      ; don't show menu bar
(tool-bar-mode -1)                      ; don't show tool bar
(scroll-bar-mode -1)                    ; don't show scroll bars
(column-number-mode 1)                  ; show column number in the mode line
(show-paren-mode 1)                     ; highlight matching parenthesis
(setq ido-enable-flex-matching t)       ; use flex matching in ido mode
(ido-mode 1)                            ; enable ido mode
(ido-everywhere 1)                      ; ???
(setq indicate-empty-lines t)           ; distinguish lines after the buffer end


(defalias 'yes-or-no-p 'y-or-n-p)

(use-package uniquify                   ; include path for buffers with the same name
  :config
  (setq uniquify-buffer-name-style 'forward))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(when (display-graphic-p)
  (load-theme 'ion t)
  (set-frame-font "Menlo 15"))

;; (use-package rainbow-delimiters
;;   :ensure
;;   :config
;;   (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(setq js-indent-level 2)
(setq css-indent-offset 2)
(setq-default indent-tabs-mode nil)

(use-package web-mode
  :ensure
  :mode ("\\.html?\\'" "\\.jsx\\'")
  :config
  (progn
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-attr-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)))

(setq ns-right-alternate-modifier 'none)

(use-package markdown-mode
  :ensure
  :mode ("\\.md\\'" "\\.markdown\\'"))

(use-package elm-mode :ensure)
(use-package haskell-mode :ensure)
(use-package swift-mode :ensure)
(use-package stylus-mode :ensure)

(use-package magit
  :ensure
  :bind ("C-x g" . magit-status))

(use-package diff-hl
  :ensure
  :init
  (progn
    (global-diff-hl-mode t)
    (diff-hl-flydiff-mode))
  :config
  (setq diff-hl-side 'right))

(use-package tern ;; requires npm install --global tern
  :commands tern-mode
  :config
  (progn
    (add-hook 'js-mode-hook (lambda () (message "!") (tern-mode t)))
    (add-hook 'web-mode-hook (lambda () (tern-mode t)))))

(use-package avy
  :ensure
  :bind
  ("C-;" . avy-goto-char)
  ("C-'" . avy-goto-char-2)
  ("M-g f" . avy-goto-line)
  ("M-g w" . avy-goto-word-1)
  ("M-g e" . avy-goto-word-0))

(use-package deft ;; ln -s <dropbox-notes-dir> ~/.deft
  :ensure
  :init
  (progn
    (setq deft-extensions '("org" "md"))
    (setq deft-recursive t)
    (setq deft-use-filename-as-title t)
    (setq deft-use-filter-string-for-filename t)))
