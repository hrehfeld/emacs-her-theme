;;; her-theme.el --- her theme

;; Copyright (C) 2017 Hauke Rehfeld

;; Author: Hauke Rehfeld
;; URL: https://github.com/hrehfeld/emacs-her-theme
;; Version: 0.01

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:


(deftheme her "Her theme.")

(require 'color)

(defvar her-min-contrast 0.4)

(defun her-color-apply (f col &rest rest)
  (apply f (append col rest)))


(defun her-color-get-luminance (c) (nth 2 (her-color-apply 'color-rgb-to-hsl c)))
(defun her-color-set-luminance (rgb y) (let ((hsl (her-color-apply 'color-rgb-to-hsl rgb)))
                                         (color-hsl-to-rgb (nth 0 hsl) (nth 1 hsl) y)))

(defun her-color-luminance-diff (a b)
  (- (her-color-get-luminance a) (her-color-get-luminance b)))

(defun her-color-min-contrasted (bg fg min-contrast-y)
  (let* (
		 ;;(get-luminance (lambda (c) (nth 1 (her-color-apply 'color-srgb-to-xyz c))))
		 ;;
         ;; (set-luminance (lambda (rgb y) (let ((xyz (her-color-apply 'color-srgb-to-xyz rgb)))
		 ;;    							  (her-color-apply 'color-xyz-to-srgb (list (nth 0 xyz) y (nth 2 xyz))))))
		 ;;(hsl (color-rgb-to-hsl r g b))
		 (fg-y (her-color-get-luminance fg))
		 (bg-y (her-color-get-luminance bg))
		 (y-diff (- bg-y fg-y))
		 (min-legible-xyz (her-color-set-luminance fg
                                   (if (< y-diff 0)
                                       ;;fg is brighter
                                       (color-clamp (+ bg-y min-contrast-y))
                                     (color-clamp (- bg-y min-contrast-y)))))
		 ;;(darkest-rgb (mapcar 'color-clamp unclamped))
		 ;;(hex (apply 'color-rgb-to-hex darkest-rgb))
		 )
	min-legible-xyz
    ))


(setq her-fg "#000000")
(setq her-bg "#ffffff")
(setq her-num-bg-shades 8)
(setq her-shade-steps 3
      her-shade-sat-mul 0.9
      her-shade-sat-mul-step 0.7
      her-shade-sat-steps 4
      )

;;for faces that should not deviate too much from standard
;; be careful with these, they're against the point of everything above

(setq her-colors
      (let ((s 0.9)
            (l 0.4))
          (mapcar
           (lambda (h) (apply 'color-rgb-to-hex (color-hsl-to-rgb h s l)))
           (mapcar (lambda (h) (/ h 360.0)) (number-sequence -5 350 45))
           )))

(setq her-red 0)
(setq her-yellow 1)
(setq her-lightgreen 2)
(setq her-green 3)
(setq her-cyan 4)
(setq her-blue 5)
(setq her-purple 6)
(setq her-pink 7)

(setq her-color-names (list
                       her-cyan
                       her-pink
                       her-green
                       her-purple
                       her-lightgreen
                       her-blue
                       her-red
                       her-yellow
                       ))
;(setq her-colors (mapcar (lambda (i) (nth i her-colors)) her-color-names))
;(setq her-colors (mapcar (lambda (i) (nth i her-colors)) (number-sequence 0 (- (length her-color-names) 1))))

;(setq her-red (-elem-index her-red her-color-names))
;(setq her-yellow (-elem-index her-yellow her-color-names))
;(setq her-lightgreen (-elem-index her-lightgreen her-color-names))
;(setq her-green (-elem-index her-green her-color-names))
;(setq her-cyan (-elem-index her-cyan her-color-names))
;(setq her-blue (-elem-index her-blue her-color-names))
;(setq her-purple (-elem-index her-purple her-color-names))
;(setq her-pink (-elem-index her-pink her-color-names))


;;special color names
(setq
 her-primary her-cyan
 her-secondary her-blue
 her-tertiary her-green
 her-contrast her-purple
 her-contrast-large her-purple
 her-contrast-text her-blue
 her-alternative her-pink
 her-alternative-large her-pink
 her-error her-red
 her-success her-green
 her-warning her-yellow
 her-neutral her-blue
      )








(let* (
       (fg (color-name-to-rgb her-fg))
       (bg (color-name-to-rgb her-bg))

       (fg-bg-shades-rgb (color-gradient fg bg her-num-bg-shades))
       )
  (setq her-fg-bg-shades (append (list her-fg)
                                 (mapcar
                                  (lambda (c)
                                    (apply 'color-rgb-to-hex c))
                                  fg-bg-shades-rgb)
                                 (list her-bg)))
  
  (setq her-color-shades
        (mapcar
         (lambda (hex)
           (mapcar (lambda (isat)
                     (let* ((rgb (color-name-to-rgb hex))
                            (min-y-diff (min (abs (her-color-luminance-diff rgb fg))
                                             (abs (her-color-luminance-diff rgb bg))
                                             ))
                            (hsl (apply 'color-rgb-to-hsl rgb))
                            (y (nth 2 hsl))
                            (darkest-y (- y min-y-diff))
                            (lightest-y (+ y min-y-diff))
                            (sat (nth 1 hsl)) 
                            (darkest (color-hsl-to-rgb
                                      (nth 0 hsl)
                                      (* sat her-shade-sat-mul)
                                      darkest-y))
                            (lightest (color-hsl-to-rgb
                                       (nth 0 hsl)
                                       (* sat (1+ (1- her-shade-sat-mul)))
                                       lightest-y))
                            (l (append (color-gradient darkest rgb her-shade-steps)
                                       (list rgb)
                                       (color-gradient rgb lightest her-shade-steps)
                                       ))
                            (sat-mul (* isat (/ sat (1+ her-shade-sat-steps))))
                            (l (mapcar
                                (lambda (c)
                                  (let* ((hsl (apply 'color-rgb-to-hsl c))
                                         (hsl (list (nth 0 hsl)
                                                    (color-clamp (- (nth 1 hsl) sat-mul))
                                                    (nth 2 hsl))))
                                    (apply 'color-hsl-to-rgb hsl)))
                                l)))
                            (mapcar
                             (lambda (c)
                               (apply 'color-rgb-to-hex c))
                             (if (< y 0.5)
                                 l
                               (reverse l)))))
                   (number-sequence 0 her-shade-sat-steps)))
         her-colors))
  )

(defun her-safe-nth (i l)
  (let ((i (min i (- (length l) 1))))
    (nth i l)))

(defun her-color-shade (shades isat ival)
  (let* ((isat (- 0 isat))
         (ival (+ her-shade-steps ival)))
    (her-safe-nth ival (nth isat shades))))

(defun her-fg (&optional i)
  (let ((i (- (if i i 0))))
    (her-safe-nth i her-fg-bg-shades)))

;(her-fg -2)
;(her-bg 2)

(defun her-bg (&optional i)
  (let* ((i (- (if i i 0)))
         (j (- (length her-fg-bg-shades) 1 i)))
    (her-safe-nth j her-fg-bg-shades)))

;;(her-color)


(defun her-color (c &optional isat ival)
  (let ((isat (if isat isat 0))
        (ival (if ival ival 0)))
    (her-color-shade (nth c her-color-shades) isat ival)))

(defun her-bg-for (c)
  (let* ((hsl (apply 'color-rgb-to-hsl (color-name-to-rgb c)))
         (bg-hsl (apply 'color-rgb-to-hsl (color-name-to-rgb her-bg)))
         (fg-hsl (apply 'color-rgb-to-hsl (color-name-to-rgb her-fg)))
         (bg-v (nth 1 bg-hsl))
         (fg-v (nth 1 fg-hsl))
         (avg-v (/ (+ bg-v fg-v) 2))
         (is-light (< fg-v bg-v))
         (dark (if is-light her-fg her-bg))
         (light (if is-light her-bg her-fg))
         )
    (if (< (nth 1 hsl) avg-v)
        light
      dark
      )))

;;(her-color her-secondary 1)

(defun her-red (&rest rest) (apply 'her-color (append (list her-red) rest)))
(defun her-yellow (&rest rest) (apply 'her-color (append (list her-yellow) rest)))
(defun her-lightgreen (&rest rest) (apply 'her-color (append (list her-lightgreen) rest)))
(defun her-green (&rest rest) (apply 'her-color (append (list her-green) rest)))
(defun her-cyan (&rest rest) (apply 'her-color (append (list her-cyan) rest)))
(defun her-blue (&rest rest) (apply 'her-color (append (list her-blue) rest)))
(defun her-purple (&rest rest) (apply 'her-color (append (list her-purple) rest)))
(defun her-pink (&rest rest) (apply 'her-color (append (list her-pink) rest)))


(defvar her-reference-body-size 13)
(defun her-font-size (n) (/ n (float her-reference-body-size)))

(let* (
       (her-error-face (list :foreground (her-color her-red)))
       (her-neutral-face (list :foreground (her-color her-neutral)))
       )

  (apply
   'custom-theme-set-faces
   `(her

    ;;; color-theme mapping
     (default ((t (:background ,(her-bg) :foreground ,(her-fg)))))
     (cursor ((t (:background  ,(her-color her-secondary)))))



     (menu ((t (:inherit default))))
     (minibuffer-prompt ((t (:inherit default ;:foreground ,(her-color her-contrast 0 0)
                                      ))))
     
     (mode-line ((t (:inherit default :background ,(her-color her-primary -2) :inverse-video nil))))
     (mode-line-highlight ((t (:inherit mode-line :weight bold))))
     (mode-line-emphasis ((t (:inherit mode-line :background ,(her-color her-primary -2 1) :slant italic))))
     (mode-line-inactive ((t (:inherit mode-line
                                        :background ,(her-color her-primary -3 4)))))
     (mode-line-buffer-id ((t (:weight bold :background nil))))
     (which-func ((t (:inherit mode-line-highlight :inverse-video t :foreground ,(her-bg)))))
     (window-numbering-face ((t (:inherit mode-line-highlight :inverse-video t :box t :foreground ,(her-bg)))))
     
     
     ;;      `(mode-line-folder-face ((t (:foreground ,theme-bg-less1))))
     ;;      `(mode-line-modified-face ((t (:foreground ,theme-warning))))
     ;;      `(mode-line-buffer-name ((t (:inherit mode-line))))
     ;;      `(mode-line-mode-name ((t (:foreground ,theme-contrast))))
     ;;      `(mode-line-mode-string ((t (:foreground ,theme-contrast))))

     (region ((t ,(let ((col (her-color her-contrast-large -3 3)))
                    `(:background ,col :distant-foreground ,(her-bg-for col))))))
     ;;      `(secondary-selection ((t (:background ,theme-bg-less1))))


     

     ;; ;;; custom faces
     ;;      `(linum ((t (:foreground "#505050" :background ,theme-bg-less1 :weight normal))))
     ;;      `(tooltip ((t (:foreground ,theme-bg :background ,theme-fg))))
     ;;                                      ;`(fixed-pitch ((t (:family "Anka/Coder" :height 75))))
     ;;                                      ;`(variable-pitch ((t (:family "Helvetica Neue LT Std"))))

     ;;  ;;; whitespace
     ;;      `(whitespace-space ((t (:foreground ,theme-bg-less1))))
     ;;      `(whitespace-tab ((t (:foreground ,theme-bg-less1))))
     ;;      `(whitespace-trailing ((t (:background ,theme-yellow :weight bold))))
     ;;      `(whitespace-newline ((t (:foreground ,theme-bg-less1))))
     ;;      `(whitespace-empty ((t (:foreground ,theme-fg :background ,theme-bg))))

     ;; ;;; basic coloring
     (window-divider ((t (:background ,(her-fg)))))
     
     ;;border
     (vertical-border ((t (:foreground ,(her-bg -2)))))
     (fringe ((t (:inverse-video nil :background ,(her-bg)))))
     ;(lv-separator ((t(:background ,(her-red)))))
     ,(let* ((bg (her-color her-primary))
            (fg (her-bg-for bg)))
       `(header-line ((t (:inherit variable-pitch
                                   :foreground ,fg :background ,bg
                                   :box (:line-width 5 :color ,bg))))))
     (highlight ((t (:inverse-video t :foreground ,(her-color her-contrast)
                                    ;:distant-foreground ,(her-bg-for (her-color her-secondary))
                                    ))))
     (success ((t (:foreground ,(her-color her-success)))))
     (warning ((t (:foreground ,(her-color her-warning)))))
     (error ((t (:foreground ,(her-color her-error)))))
     ;;(table-cell ((t (:foreground ,(her-color her-success)))))

     ,(let* ((bg (her-yellow -2 2))
            (fg (her-bg-for bg)))
        `(tool-tip ((t (:foreground ,fg :background ,bg)))))

     

     ,(let* ((bg (her-bg -1))
            (fg (her-bg-for bg)))
       `(hl-line ((t (:background ,bg :distant-foreground ,fg )))))


     ;;      `(trailing-whitespace ((t (:underline t :foreground ,theme-red))))
     ,(let ((col (her-color her-alternative-large -2 3)))
        `(show-paren-match ((t (:background ,col :distant-foreground ,(her-bg-for col))))))
     (show-paren-mismatch ((t (:background ,(her-red)))))

     ;;next-error
     

     ;;button
     ;;buffer-menu-buffer

     ;;      ;; faces used by isearch
     ,@(let ((icol her-success))
       `((isearch ((t (:inverse-video t :foreground ,(her-color icol)))))
         (lazy-highlight ((t (:inherit isearch :foreground ,(her-color icol -2 2)))))))
     (match ((t (:inverse-video t :foreground ,(her-color her-neutral -2)))))
     (isearch-fail ((t (,@her-error-face :inverse-video t))))

     ;;phisearch
     (phi-search-selection-face ((t (:inherit isearch))))
     (phi-search-match-face ((t (:inherit lazy-highlight))))
     (phi-search-failpart-face ((t (:inherit isearch-fail))))
     (phi-replace-preview-face ((t (:box t :foreground ,(her-color her-success)))))


     ;;basic text
     ;;nobreak-space
     (escape-glyph ((t (:inherit font-lock-constant-face))))
     (glyphless-char ((t (:inherit font-lock-constant-face :box t))))

     (link ((t (:underline t :foreground ,(her-blue)))))
     (link-visited ((t (:inherit link :foreground ,(her-blue -2)))))


     ,@(let* ((outline-face `(:box (:line-width 5 :color ,(her-bg))))
              (subheading-size 15))
       `((outline-1 ((t (,@outline-face :foreground ,(her-fg) :height ,(her-font-size 16) :box (:line-width 5 :color ,(her-bg))))))
         (outline-2 ((t (,@outline-face :foreground ,(her-color 1) :height ,(her-font-size subheading-size)))))
         (outline-3 ((t (,@outline-face :foreground ,(her-color 2) :height ,(her-font-size (- subheading-size 1))))))
         (outline-4 ((t (,@outline-face :foreground ,(her-color 3) :height ,(her-font-size (- subheading-size 2))))))
         (outline-5 ((t (,@outline-face :foreground ,(her-color 4) :height ,(her-font-size (- subheading-size 3))))))
         (outline-6 ((t (,@outline-face :foreground ,(her-color 5) :height ,(her-font-size (- subheading-size 4))))))
         (outline-7 ((t (,@outline-face :foreground ,(her-color 6) ))))
         (outline-8 ((t (,@outline-face :foreground ,(her-color 7) ))))))
     
      ;;; font lock
     (font-lock-comment-face ((t (;,@theme-text-like-face
                                   :foreground ,(her-red) :weight normal
                                               ))))
     (font-lock-comment-delimiter-face ((t (:foreground ,(her-red)))))
     (font-lock-constant-face ((t (:foreground ,(her-cyan -2)))))
     (font-lock-doc-face ((t (:foreground ,(her-pink 1)))))
     (font-lock-doc-string-face ((t (:foreground ,(her-pink 2)))))
     (font-lock-function-name-face ((t (:foreground ,(her-blue)))))
     (font-lock-builtin-face ((t (:foreground ,(her-pink 0 1)))))
     (font-lock-keyword-face ((t (:foreground ,(her-purple 2)))))
     (font-lock-string-face ((t (:foreground ,(her-red -2 1)))))
     (font-lock-type-face ((t (:foreground ,(her-lightgreen 0 -1)))))
     (font-lock-variable-name-face ((t (:foreground ,(her-yellow 0 -1)))))
     (font-lock-warning-face ((t (;,@theme-warning-face
                                   ,(her-yellow)))))
     (font-lock-preprocessor-face ((t (:foreground ,(her-cyan)))))
     (font-lock-negation-char-face ((t ())))

     ;;org-mode

     (org-hide ((t (:foreground ,(her-bg) :inherit fixed-pitch))))
     (org-special-keyword ((t (:foreground ,(her-fg -4)))))
     (org-table ((t (:inherit fixed-pitch :foreground ,(her-color her-contrast-text)))))
     (org-tag ((t (:inherit org-special-keyword :background ,(her-bg -1) :height ,(her-font-size 10)))))
     (org-date ((t (:inherit org-table))))
     (org-checkbox ((t (:inherit (fixed-pitch)))))
     (org-agenda-date ((t (:inherit (variable-pitch outline-3) :inverse-video t :height ,(her-font-size 20) ))))
	 (org-agenda-calendar-event ((t (:inherit variable-pitch))))
	 (org-upcoming-deadline ((t (:inherit (org-warning variable-pitch) ))))
	 ;;for habits
	 (org-scheduled ((t (:inherit variable-pitch ,@ her-neutral-face))))
	 (org-scheduled-today ((t (:inherit (fixed-pitch) ,@ her-neutral-face))))
	 (org-scheduled-previously ((t (:inherit (org-scheduled org-warning)))))

     (highlight-symbol-face ((t (:background ,(her-cyan -3 4)))))

	 (lsp-face-highlight-read ((t (:inherit highlight-symbol-face))))
	 (lsp-face-highlight-textual ((t (:inherit font-lock-string-face :slant italic))))
	 (lsp-face-highlight-write ((t (:inherit highlight-symbol-face :slant italic))))


     )))

;;;###autoload
(when load-file-name
  (require 'color-theme-modern)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'her)

;;; her-theme.el ends here
