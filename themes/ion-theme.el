(deftheme ion  "")

(let ((bg "#473d41")
      (bg-highlight "#5a4e52")
      (fg "#c5c8c6")
      (blueish-green "#8abeb7")
      (violet "#b294bb")
      (blue "#81a2be")
      (orange "#de935f")
      (red "#cc6666")
      (grey "#969896")
      (yellow "#f0c674"))

  ;; black #373b41
  ;; green #b5bd68


  (custom-theme-set-faces
   'ion
   `(default ((t (:foreground ,fg :background ,bg))))

   `(font-lock-preprocessor-face ((t (:foreground "#ff0000"))))
   `(font-lock-type-face ((t (:foreground ,fg))))
   `(font-lock-builtin-face ((t (:foreground ,violet))))
   `(font-lock-comment-face ((t (:foreground ,grey))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,grey))))
   `(font-lock-constant-face ((t (:foreground ,blueish-green))))
   `(font-lock-doc-face ((t (:foreground ,blueish-green))))
   `(font-lock-function-name-face ((t (:foreground ,fg))))
   `(font-lock-keyword-face ((t (:foreground ,blue))))
   `(font-lock-string-face ((t (:foreground ,blueish-green))))
   `(font-lock-variable-name-face ((t (:foreground ,violet))))

   `(cursor ((t (:background ,fg))))
   `(region ((t (:background ,bg-highlight))))
   `(fringe ((t (:foreground ,fg :background ,bg))))
   `(mode-line ((t (:foreground ,bg :background ,fg :box nil))))
   `(mode-line-inactive ((t (:foreground "#716a6c" :background ,fg :box nil))))

   `(minibuffer-prompt ((t (:foreground ,blue))))
   `(ido-first-match ((t (:foreground ,blueish-green :weight bold))))
   `(ido-only-match ((t (:foreground ,blueish-green :weight bold))))
   `(ido-subdir ((t (:foreground ,blue))))

   `(show-paren-match ((t (:background nil :underline ,blueish-green))))
   `(show-paren-mismatch ((t (:foreground ,red :background nil :underline ,red))))

   `(rainbow-delimiters-depth-1-face ((t (:foreground ,fg))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,fg))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,fg))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,fg))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,fg))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,fg))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,fg))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,fg))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,fg))))
   `(rainbow-delimiters-unmatched-face ((t (:foreground ,red :background nil))))

   `(diff-hl-insert ((t (:foreground ,blueish-green :background ,blueish-green))))
   `(diff-hl-change ((t (:foreground ,yellow :background ,yellow))))
   `(diff-hl-delete ((t (:foreground ,red :background ,red))))

   `(web-mode-html-tag-bracket-face ((t (:foreground ,grey))))
   `(web-mode-html-tag-face ((t (:foreground ,fg))))
   `(web-mode-html-attr-name-face ((t (:foreground  ,blue))))
   `(web-mode-html-attr-value-face ((t (:foreground ,blueish-green)))))

  (provide-theme 'ion))
