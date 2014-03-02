(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(clojure-mode
                      clojure-test-mode
                      cider
                      rainbow-delimiters
                      web-mode)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

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
(load-theme 'ibdknox t)

(add-to-list 'load-path "~/.emacs.d/vendor")
(require 'paredit)

(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'js-mode-hook 'rainbow-delimiters-mode)
(setq js-indent-level 2)
(setq css-indent-offset 2)
(setq-default indent-tabs-mode nil)


(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(setq ns-right-alternate-modifier 'none)
