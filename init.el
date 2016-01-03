(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(exec-path-from-shell
                      less-css-mode
                      markdown-mode
                      rainbow-delimiters
                      web-mode)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(when (memq window-system '(mac ns))
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

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(add-hook 'js-mode-hook 'rainbow-delimiters-mode)
(setq js-indent-level 2)
(setq css-indent-offset 2)
(setq-default indent-tabs-mode nil)


(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(setq ns-right-alternate-modifier 'none)

(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
