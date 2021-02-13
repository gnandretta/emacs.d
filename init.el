(setq init-dir-path (file-name-directory (or load-file-name
					     (buffer-file-name))))
(setq config-file-path (expand-file-name "config.org" user-emacs-directory))
(org-babel-load-file config-file-path)
(load (expand-file-name "user.el" user-emacs-directory) 'noerror)
