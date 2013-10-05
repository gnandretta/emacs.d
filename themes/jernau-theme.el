(deftheme jernau "The Jernau color theme") ; ported from https://github.com/Misophistful/jernau-lighttable-theme


(let ((jernau-grey-1  "#999999")
      (jernau-grey    "#CCCCCC")
      (jernau-black+1 "#555555")
      (jernau-black   "#222222")
      (jernau-yellow  "#EEDDA9")
      (jernau-blue-1  "#90B5D3")
      (jernau-blue    "#ABCBFF")
      (jernau-green   "#A0CE8F")
      (jernau-purple  "#C9A4E8"))

  (custom-theme-set-faces
   'jernau
   `(default ((t (:foreground ,jernau-grey :background ,jernau-black))))

   `(font-lock-builtin-face ((t (:foreground ,jernau-green))))
   `(font-lock-comment-face ((t (:foreground ,jernau-blue-1)))) 
   `(font-lock-constant-face ((t (:foreground ,jernau-purple)))) 
   `(font-lock-doc-face ((t (:foreground ,jernau-yellow))))
   `(font-lock-function-name-face ((t (:foreground ,jernau-blue))))
   `(font-lock-keyword-face ((t (:foreground ,jernau-green))))
   `(font-lock-string-face ((t (:foreground ,jernau-yellow))))

   `(region ((t (:background ,jernau-black+1))))

   `(mode-line ((t (:foreground ,jernau-black :background ,jernau-grey :box nil))))
   `(mode-line-inactive ((t (:foreground ,jernau-black :background ,jernau-grey-1 :box nil))))
   `(minibuffer-prompt ((t (:foreground ,jernau-green))))
   `(ido-first-match ((t (:foreground ,jernau-purple :weight bold))))
   `(ido-only-match ((t (:foreground ,jernau-purple :weight bold))))
   `(ido-subdir ((t (:foreground ,jernau-blue-1))))


   `(show-paren-match ((t (:inverse-video t :weight bold))))




   `(rainbow-delimiters-depth-1-face ((t (:foreground "#74BEFF"))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground "#867EDB"))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground "#E676F3"))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground "#FDBC1B"))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground "#F0F047"))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground "#73EB47"))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground "#67F1E0")))))

  (provide-theme 'jernau))

