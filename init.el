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


(defalias 'yes-or-no-p 'y-or-n-p)

(use-package uniquify                   ; include path for buffers with the same name
  :config
  (setq uniquify-buffer-name-style 'forward))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(use-package rainbow-delimiters
  :ensure
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

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
