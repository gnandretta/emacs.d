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

(use-package exec-path-from-shell
  :ensure
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

(setq-default
  inhibit-startup-screen t
  visible-bell t
  ring-bell-function (lambda () (message "*beep*")))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)
(show-paren-mode 1)
(ido-mode 1)
(ido-everywhere 1)
(setq ido-enable-flex-matching 1)

(defalias 'yes-or-no-p 'y-or-n-p)

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(use-package rainbow-delimiters
  :ensure
  :config
  (add-hook 'js-mode-hook 'rainbow-delimiters-mode))

(setq js-indent-level 2)
(setq css-indent-offset 2)
(setq-default indent-tabs-mode nil)

(use-package web-mode
  :ensure
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

(setq ns-right-alternate-modifier 'none)

(use-package markdown-mode
  :ensure
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
    (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))))
