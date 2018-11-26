;;; her-theme.el --- her theme

;; Copyright (C) 2017-2018 Hauke Rehfeld

;; Author: Hauke Rehfeld
;; URL: https://github.com/hrehfeld/emacs-her-theme
;; Version: 0.02

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
(setq her-num-lightness-steps 5
      her-shade-sat-steps 5
      her-shade-lightness-sat-mul 1.0
      her-shade-sat-mul-step 0.7
      )

;;for faces that should not deviate too much from standard
;; be careful with these, they're against the point of everything above

(defun her-wrap (x min max)
  (let ((range (- max min)))
	(while (< x min)
	  (setq x (+ x range)))
	(while (> x max)
	  (setq x (- x range)))
	x))

(setq
 her-num-colors 8
 her-colors
	  ;;max saturated
      (let ((s 0.7)
			;;avg lightness
            (l 0.4))
		(mapcar
		 (lambda (h)
		   (apply 'color-rgb-to-hex (color-hsl-to-rgb h s l)))
		 (mapcar (lambda (h) (her-wrap h 0.0 1.1))
				 (let* ((from (/ -5 360))
						(to (+ 1 from)))
				   (-slice (number-sequence from to (/ (- to from) (float her-num-colors)))
						   0 -1)))
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


;;semantic color names
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


(defun her--min-diff (x a b)
  (min (abs (- x a))
	   (abs (- x b))))

(defun her--number-sequence-n (from to n)
  (let* ((n1 (- n 1))
		(stepsize (/ (- to from) n)))
	(mapcar (lambda (i) (+ from  (* i stepsize))) (number-sequence 0 n1))))

;;(her--number-sequence-n 0.1 0.5 2)
;;(her--number-sequence-n 1.0 2.25  2)

(defun her--color-saturation-mul (hsl sat-mul)
  (list (nth 0 hsl)
		(color-clamp (* (nth 1 hsl) sat-mul))
		(nth 2 hsl)))

(defun her--color-saturation-variants (base-sat shades)
  (let* ((s-minmax (list (/ 1.0 her-shade-sat-steps) 1.0))

		 (num-more-steps (round (* 0.5 (- her-shade-sat-steps 1))))
		 (num-less-steps (- her-shade-sat-steps 1 num-more-steps))
		 (num-steps (list num-less-steps num-more-steps))
		 
		 (s-ratios (mapcar (lambda (m) (/ m base-sat)) s-minmax))
		 ;(s-diffs (mapcar (lambda (m) (- m base-sat)) s-minmax))
		 (s-stepsize (map-apply (lambda (s n)
								  (/ s n
									 ))
								(-zip  s-ratios num-steps)))
		 (steps (append (her--number-sequence-n (first s-ratios) 1.0 num-less-steps)
						(list 1.0)
						(reverse
						 (her--number-sequence-n (nth 1 s-ratios) 1.0 num-more-steps))))
		 )
	;;reduce saturation for all shades
	(mapcar (lambda (sat-mul)
			  (mapcar
			   (lambda (hsl) (her--color-saturation-mul hsl sat-mul))
			   shades))
			steps)))
;;(her--color-saturation-variants 0.4 '((0.19999999999999998 0.054901960784313725 0.07712418300653595) (0.39999999999999997 0.10980392156862745 0.1542483660130719) (0.6 0.16470588235294117 0.23137254901960785) (0.7333333333333333 0.44313725490196076 0.4875816993464052) (0.8666666666666666 0.7215686274509804 0.7437908496732026)))

(defun her--map-apply-call (f l)
  (mapcar (lambda (c) (apply f c)) l))

(let* (
       (fg (color-name-to-rgb her-fg))
       (bg (color-name-to-rgb her-bg))
	   (hsl-fg (apply 'color-rgb-to-hsl fg))
	   (hsl-bg (apply 'color-rgb-to-hsl bg))

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
		   (let* ((rgb (color-name-to-rgb hex))
				  (hsl (apply 'color-rgb-to-hsl rgb))

				  (lightness-diff (apply 'her--min-diff
										 (mapcar (lambda (c) (nth 2 c)) (list hsl hsl-bg hsl-fg))))
				  (sat (nth 1 hsl)) 
				  (lightness (nth 2 hsl))
				  ;;(darkest-y (- lightness lightness-diff))
				  ;;(lightest-y (+ lightness lightness-diff))
				  (fg-l (nth 2 hsl-fg))
				  (bg-l (nth 2 hsl-bg))
				  (darkest-y (min fg-l bg-l))
				  (lightest-y (max fg-l bg-l))


				  ;;darkest and lightness receive saturation multipliers
				  (darkest (color-hsl-to-rgb
							(nth 0 hsl)
							(color-clamp (* sat her-shade-lightness-sat-mul))
							darkest-y))
				  (lightest (color-hsl-to-rgb
							 (nth 0 hsl)
							 (color-clamp (* sat (1+ (1- her-shade-lightness-sat-mul))))
							 lightest-y))
				  (shades (append (color-gradient darkest rgb (/ her-num-lightness-steps 2))
							 (list rgb)
							 (color-gradient rgb lightest (/ (- her-num-lightness-steps 1) 2))
							 )))
			 (let* ((hsl-shades (her--map-apply-call 'color-rgb-to-hsl shades))
					(hsl-desaturated-shades (her--color-saturation-variants sat hsl-shades)))
			   (mapcar
				(lambda (l)
				  (let ((rgbl (her--map-apply-call 'color-hsl-to-rgb l)))
					(her--map-apply-call 'color-rgb-to-hex rgbl)
					))
				hsl-desaturated-shades))
			 ))
         her-colors))

  )

(defun her-safe-nth (i l)
  (let ((i (min i (- (length l) 1))))
    (nth i l)))

(defun her-color-shade (shades isat_ ival_)
  (let* ((isat (+  (/ her-shade-sat-steps 2) isat_))
		 (ival (+ (/ her-num-lightness-steps 2) ival_))
		 (col (nth ival (nth isat shades))))
	(unless col (user-error "Wrong color index: %s %s" isat_ ival_))
	col
	))

;;(her-color-shade (nth 0 her-color-shades) 0 0)
;;(her-safe-nth 0 (nth 0 (nth her-num-lightness-steps her-color-shades)))

(defun her--float-fg-bg-index (i)
  (cond ((floatp i) (round (* i her-num-bg-shades)))
		((integerp i) i)
		(t 0)))

(defun her-fg (&optional i)
  (let* ((i (her--float-fg-bg-index i))
		 (i (- (or i 0))))
    (her-safe-nth i her-fg-bg-shades)))

										;(her-fg 1.0)
										;(her-bg 1.0)

(defun her-bg (&optional i)
  (let* ((i (her--float-fg-bg-index i))
		 (i (- i))
         (j (- (length her-fg-bg-shades) 1 i)))
    (her-safe-nth j her-fg-bg-shades)))



(defun her-color (c &optional isat ival)
  (let ((isat (or isat 0))
        (ival (or ival 0)))
    (her-color-shade (nth c her-color-shades) isat ival)))
;;(her-color her-blue 1 1)

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

;;(her-color her-secondary 4)

(defmacro her--defun-color (name)
  `(defun ,name (&rest rest) (apply 'her-color (append (list ,name) rest))))
(her--defun-color her-red)
(her--defun-color her-yellow)
(her--defun-color her-lightgreen)
(her--defun-color her-green)
(her--defun-color her-cyan)
(her--defun-color her-blue)
(her--defun-color her-purple)
(her--defun-color her-pink)
;;(her-blue 0 0)

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
     
     (mode-line ((t (:inherit default :background ,(her-color her-primary) :inverse-video nil))))
     (mode-line-highlight ((t (:inherit mode-line :weight bold))))
     (mode-line-emphasis ((t (:inherit mode-line :background ,(her-color her-primary 0 1
																		 ) :slant italic))))
     (mode-line-inactive ((t (:inherit mode-line
									   :background ,(her-color her-primary -1 2)))))
     (mode-line-buffer-id ((t (:weight bold :background nil))))
     (which-func ((t (:inherit mode-line-highlight :inverse-video t :foreground ,(her-bg)))))
     (window-numbering-face ((t (:inherit mode-line-highlight :inverse-video t :box t :foreground ,(her-bg)))))
     
     
     ;;      `(mode-line-folder-face ((t (:foreground ,theme-bg-less1))))
     ;;      `(mode-line-modified-face ((t (:foreground ,theme-warning))))
     ;;      `(mode-line-buffer-name ((t (:inherit mode-line))))
     ;;      `(mode-line-mode-name ((t (:foreground ,theme-contrast))))
     ;;      `(mode-line-mode-string ((t (:foreground ,theme-contrast))))

     (region ((t ,(let ((col (her-color her-contrast-large -2 2)))
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

     ,(let* ((bg (her-yellow 0 2))
			 (fg (her-bg-for bg)))
        `(tool-tip ((t (:foreground ,fg :background ,bg)))))

     

     ,(let* ((bg (her-bg -1))
			 (fg (her-bg-for bg)))
		`(hl-line ((t (:background ,bg :distant-foreground ,fg )))))


     ;;      `(trailing-whitespace ((t (:underline t :foreground ,theme-red))))
     ,(let ((col (her-color her-alternative-large 0 2)))
        `(show-paren-match ((t (:background ,col :distant-foreground ,(her-bg-for col))))))
     (show-paren-mismatch ((t (:inherit error :inverse-video t))))

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

	 ;; :box (:line-width 5 :color ,(her-bg))										  
     ,@(let* ((outline-face `(:weight bold :box (:line-width 5 :color ,(her-bg))))
              (subheading-size (her-font-size 15))
			  ;;(shades (color-gradient her-fg her-bg 15))
			  )
		 `((outline-1 ((t (,@outline-face :foreground ,(her-fg)))))
		   (outline-2 ((t (,@outline-face :foreground ,(her-color 1)))))
		   (outline-3 ((t (,@outline-face :foreground ,(her-color 2)))))
		   (outline-4 ((t (,@outline-face :foreground ,(her-color 3)))))
		   (outline-5 ((t (,@outline-face :foreground ,(her-color 4)))))
		   (outline-6 ((t (,@outline-face :foreground ,(her-color 5)))))
		   (outline-7 ((t (,@outline-face :foreground ,(her-color 6)))))
		   (outline-8 ((t (,@outline-face :foreground ,(her-color 7)))))))

      ;;; font lock
     (font-lock-builtin-face ((t (:foreground ,(her-pink)))))
     (font-lock-comment-face ((t (;,@theme-text-like-face
								  :foreground ,(her-red) :weight normal  :slant italic
											  ))))
     (font-lock-comment-delimiter-face ((t (:foreground ,(her-fg -.5)))))
     (font-lock-constant-face ((t (:foreground ,(her-cyan -1)))))
     (font-lock-doc-face ((t (:inherit font-lock-comment-face :foreground ,(her-pink 1)))))
     (font-lock-doc-string-face ((t (:inherit font-lock-doc-face :foreground ,(her-pink 2)))))
     (font-lock-function-name-face ((t (:foreground ,(her-blue 1) :weight bold))))
     (font-lock-builtin-face ((t (:foreground ,(her-pink 1)))))
     (font-lock-keyword-face ((t (:foreground ,(her-pink 2)))))
     (font-lock-string-face ((t (:foreground ,(her-red -2)))))
     (font-lock-type-face ((t (:foreground ,(her-green 1 -1)))))
     (font-lock-variable-name-face ((t (:foreground ,(her-yellow 1)))))
     (font-lock-warning-face ((t (;,@theme-warning-face
								  ,(her-yellow)))))
     (font-lock-preprocessor-face ((t (:foreground ,(her-cyan 1 -1)))))
     (font-lock-negation-char-face ((t ())))

	 ;;calendar
	 (diary ((t (:inherit font-lock-builtin-face))))

     
     ;;org-mode

     (org-hide ((t (:foreground ,(her-bg) :inherit fixed-pitch))))
     (org-special-keyword ((t (:foreground ,(her-fg -2)))))
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

	 ;;git
	 ;;(git-commit-comment-file ((t (:inherit fixed-pitch))))
	 (git-commit-pseudo-header ((t (:inherit fixed-pitch))))

     )))

;;;###autoload
(when load-file-name
  (require 'color-theme-modern)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'her)

;;; her-theme.el ends here
