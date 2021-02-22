;;; her-theme.el --- her theme

;; Copyright (C) 2017-2019 Hauke Rehfeld

;; Author: Hauke Rehfeld
;; URL: https://github.com/hrehfeld/emacs-her-theme
;; Version: 0.03

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
(setq her-num-lightness-steps 16
      her-shade-sat-steps 16
      her-shade-lightness-sat-mul 1.0
      her-shade-sat-mul-step 0.7
      )

;;for faces that should not deviate too much from standard
;; be careful with these, they're against the point of everything above

;; (her-wrap -58 -20 10)
;; (her-wrap2 -58 -20 10)
(defun her-wrap (x min max)
  (+ (mod (- x min) (- max min)) min))


(setq
 her-num-colors 8
 her-colors
	  ;;max saturated
      (let* ((s 0.7)
			;;avg lightness
            (l 0.4)
			(colorwheel-relative-start 1.05)
			(colorwheel-relative-end (+ colorwheel-relative-start 1)))
		(cl-flet ((hsl-to-hex (h) (apply 'color-rgb-to-hex (color-hsl-to-rgb h s l)))
				  (wrap (h) (her-wrap h 0.0 1.0)))
		  (mapcar
		   (lambda (x) (hsl-to-hex (wrap x)))
		   (let* ((from colorwheel-relative-start)
				  (to colorwheel-relative-end)
				  (range (- to from))
				  (inc  (/ range (float her-num-colors))))
			 (-butlast (number-sequence from to inc)))
		   ))))

(setq her-color-names
	  (-map-indexed
	   (lambda (i sym) (set sym i) (cons sym i))
	   '(her-red
		 her-yellow
		 her-lightgreen
		 her-green
		 her-cyan
		 her-blue
		 her-purple
		 her-pink
		 )))

;;semantic color names
(setq her-semantic-names
	  `((her-primary . ,her-cyan)
		(her-secondary . ,her-blue)
		(her-tertiary . ,her-green)
		(her-contrast . ,her-purple)
		(her-contrast-large . ,her-purple)
		(her-contrast-text . ,her-blue)
		(her-alternative . ,her-pink)
		(her-alternative-large . ,her-pink)
		(her-error . ,her-red)
		(her-success . ,her-green)
		(her-warning . ,her-yellow)
		(her-neutral . ,her-blue)))

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

(defun her-color-shade (shades isat ival)
  (let* ((isat (her--float-index (if (numberp isat) isat 0.5) (length shades)))
		 (values (nth isat shades))
         (ival (her--float-index (if (numberp ival) ival 0.5) (length values)))
		 (col (nth ival values)))
	col))

;;(her-color-shade (nth her-contrast-large her-color-shades) nil nil)
;;(her-color-shade (nth her-contrast-large her-color-shades) 0.0 0.0)
;;(her-color-shade (nth her-contrast-large her-color-shades) 0.5 0.0)
;;(her-color-shade (nth her-contrast-large her-color-shades) 0.5 1.0)

(defun her--float-index (i n)
  (cond ((floatp i) (max (min (round (* i (float n))) (- n 1)) 0))
		((integerp i) i)
		(t 0)))

(defun her-fg (&optional i)
    (let* ((n (length her-fg-bg-shades))
		 (i (her--float-index i n)))
	  (nth i her-fg-bg-shades)))

										;(her-fg)
										;(her-fg 0.5)
										;(her-fg 1.0)

(defun her-bg (&optional i)
  (let* ((n (length her-fg-bg-shades))
		 (i (her--float-index i n)))
	(nth (- n 1 i) her-fg-bg-shades)))
;;(her-bg)
;;(her-bg 0.5)
;;(her-bg 1.0)



(defun her-color (c &optional isat ival)
  (her-color-shade (nth c her-color-shades) isat ival))
;;(her-color her-pink)
;;(her-color her-pink 0.5 0.5)

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

(defmacro her--defun-color (name value)
  `(defun ,name (&optional sat val) (her-color ,value sat val)))

;;(her--defun-color 

(defmacro her--defun-color-list (plist)
  `(progn
	 ,@(mapcar
		(lambda (p)
		  (let* ((name (car p))
				 (value (cdr p)))
			(assert (symbolp name))
			(assert (numberp value))
			`(her--defun-color ,name ,value)))
		(eval plist))))
(her--defun-color-list her-color-names)
(her--defun-color-list her-semantic-names)
;;(her-primary 0.5 0.5)

(defvar her-reference-body-size 13)
(defun her-font-size (n) (/ n (float her-reference-body-size)))

(defface her-theme-hierarchy-1-face `((t (:foreground ,(her-fg)))) "Base for hierarchical faces 1")
(defface her-theme-hierarchy-2-face `((t (:foreground ,(her-color 1)))) "Base for hierarchical faces 2")
(defface her-theme-hierarchy-3-face `((t (:foreground ,(her-color 2)))) "Base for hierarchical faces 3")
(defface her-theme-hierarchy-4-face `((t (:foreground ,(her-color 3)))) "Base for hierarchical faces 4")
(defface her-theme-hierarchy-5-face `((t (:foreground ,(her-color 4)))) "Base for hierarchical faces 5")
(defface her-theme-hierarchy-6-face `((t (:foreground ,(her-color 5)))) "Base for hierarchical faces 6")
(defface her-theme-hierarchy-7-face `((t (:foreground ,(her-color 6)))) "Base for hierarchical faces 7")
(defface her-theme-hierarchy-8-face `((t (:foreground ,(her-color 7)))) "Base for hierarchical faces 8")
(defface her-theme-hierarchy-9-face `((t (:foreground ,(her-color 8)))) "Base for hierarchical faces 9")


(let* (
       (her-error-face (list :foreground (her-error)))
       (her-neutral-face (list :foreground (her-neutral)))
       )
  (apply
   'custom-theme-set-faces
   `(her

    ;;; color-theme mapping
	 ;; somehow breaks fixed pitch in variable-pitch-mode
	 ;;(fixed-pitch ((t ())))
     (default ((t (:background ,(her-bg) :foreground ,(her-fg)))))
     (cursor ((t (:background  ,(her-secondary)))))



     (menu ((t (:inherit default))))
     (minibuffer-prompt ((t (:inherit default ;:foreground ,(her-contrast 0.5 0.5)
                                      ))))
     (mode-line ((t (:inherit default :background ,(her-primary) :inverse-video nil))))
     (mode-line-highlight ((t (:inherit mode-line :weight bold))))
     (mode-line-emphasis ((t (:inherit mode-line :background ,(her-primary 0.5 0.7
																		 ) :slant italic))))
     (mode-line-inactive ((t (:inherit mode-line
									   :background ,(her-primary 0.2 0.9)))))
     (mode-line-buffer-id ((t (:weight bold :background nil))))
     (which-func ((t (:inherit mode-line-highlight :inverse-video t :foreground ,(her-bg)))))
     (window-numbering-face ((t (:inherit mode-line-highlight :inverse-video t :box t :foreground ,(her-bg)))))
     
     
     ;;      `(mode-line-folder-face ((t (:foreground ,theme-bg-less1))))
     ;;      `(mode-line-modified-face ((t (:foreground ,theme-warning))))
     ;;      `(mode-line-buffer-name ((t (:inherit mode-line))))
     ;;      `(mode-line-mode-name ((t (:foreground ,theme-contrast))))
     ;;      `(mode-line-mode-string ((t (:foreground ,theme-contrast))))

     (region ((t ,(let ((col (her-contrast-large 0.2 0.9)))
                    `(:background ,col :distant-foreground ,(her-bg-for col))))))
     ;;      `(secondary-selection ((t (:background ,theme-bg-less1))))


     

     ;; ;;; custom faces
     ;;      `(linum ((t (:foreground "#505050" :background ,theme-bg-less1 :weight normal))))
     ;;      `(tooltip ((t (:foreground ,theme-bg :background ,theme-fg))))
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
     (vertical-border ((t (:foreground ,(her-bg 0.9)))))
     (fringe ((t (:inverse-video nil :background ,(her-bg)))))
										;(lv-separator ((t(:background ,(her-red)))))
     ,(let* ((bg (her-primary))
			 (fg (her-bg-for bg)))
		`(header-line ((t (:inherit variable-pitch
									:foreground ,fg :background ,bg
									:box (:line-width 5 :color ,bg))))))
     (highlight ((t (:inverse-video nil ;:background ,(her-contrast 0.75 1.0)
										;:distant-foreground ,(her-bg-for (her-color her-secondary))
                                    ))))
     (success ((t (:foreground ,(her-success)))))
     (warning ((t (:foreground ,(her-warning)))))
     (error ((t (:foreground ,(her-error)))))
     ;;(table-cell ((t (:foreground ,(her-success)))))

     ,(let* ((bg (her-yellow 0.5 0.9))
			 (fg (her-bg-for bg)))
        `(tool-tip ((t (:foreground ,fg :background ,bg)))))

     

     ,(let* ((bg (her-bg 0.1))
			 (fg (her-bg-for bg)))
		`(hl-line ((t (:background ,bg :distant-foreground ,fg )))))


     ;;      `(trailing-whitespace ((t (:underline t :foreground ,theme-red))))
     ,(let ((col (her-alternative-large 0.5 0.9)))
        `(show-paren-match ((t (:background ,col :distant-foreground ,(her-bg-for col))))))
     (show-paren-mismatch ((t (:inherit error :inverse-video t))))

     ;;next-error
     

     ;;button
     ;;buffer-menu-buffer

     ;;      ;; faces used by isearch
     ,@(cl-labels ((color (&rest rest) (apply 'her-success rest)))
		 `((isearch ((t (:inverse-video t :foreground ,(color) :background ,(her-bg)))))
		   (lazy-highlight ((t (:inherit isearch :foreground ,(color 0.1 1.0) :background ,(her-fg)))))))
     (match ((t (:inverse-video t :foreground ,(her-neutral 0.2)))))
     (isearch-fail ((t (,@her-error-face :inverse-video t))))

     ;;phisearch
     (phi-search-selection-face ((t (:inherit isearch))))
     (phi-search-match-face ((t (:inherit lazy-highlight))))
     (phi-search-failpart-face ((t (:inherit isearch-fail))))
     (phi-replace-preview-face ((t (:box t :foreground ,(her-success)))))


     ;;basic text
     ;;nobreak-space
     (escape-glyph ((t (:inherit font-lock-constant-face))))
     (glyphless-char ((t (:inherit font-lock-constant-face :box t))))

     (link ((t (:underline t :foreground ,(her-blue)))))
     (link-visited ((t (:inherit link :foreground ,(her-blue 0.24)))))

	 (her-theme-hierarchy-1-face ((t (:foreground ,(her-fg)))))
	 (her-theme-hierarchy-2-face ((t (:foreground ,(her-color 3)))))
	 (her-theme-hierarchy-3-face ((t (:foreground ,(her-color 1)))))
	 (her-theme-hierarchy-4-face ((t (:foreground ,(her-color 2)))))
	 (her-theme-hierarchy-5-face ((t (:foreground ,(her-color 4)))))
	 (her-theme-hierarchy-6-face ((t (:foreground ,(her-color 5)))))
	 (her-theme-hierarchy-7-face ((t (:foreground ,(her-color 6)))))
	 (her-theme-hierarchy-8-face ((t (:foreground ,(her-color 7)))))
	 (her-theme-hierarchy-9-face ((t (:foreground ,(her-color 8)))))

	 ;; :box (:line-width 5 :color ,(her-bg))										  
     ,@(let* ((outline-face `(:weight bold :box (:line-width 5 :color ,(her-bg))))
              (subheading-size (her-font-size 15))
			  ;;(shades (color-gradient her-fg her-bg 15))
			  )
		 `((outline-1 ((t (,@outline-face :inherit her-theme-hierarchy-1-face))))
		   (outline-2 ((t (,@outline-face :inherit her-theme-hierarchy-2-face))))
		   (outline-3 ((t (,@outline-face :inherit her-theme-hierarchy-3-face))))
		   (outline-4 ((t (,@outline-face :inherit her-theme-hierarchy-4-face))))
		   (outline-5 ((t (,@outline-face :inherit her-theme-hierarchy-5-face))))
		   (outline-6 ((t (,@outline-face :inherit her-theme-hierarchy-6-face))))
		   (outline-7 ((t (,@outline-face :inherit her-theme-hierarchy-7-face))))
		   (outline-8 ((t (,@outline-face :inherit her-theme-hierarchy-8-face))))))

	 (rainbow-delimiters-base-face ((t (:foreground nil :weight bold))))
	 (rainbow-delimiters-depth-1-face ((t (:foreground nil :inherit (rainbow-delimiters-base-face her-theme-hierarchy-1-face)))))
	 (rainbow-delimiters-depth-2-face ((t (:foreground nil :inherit (rainbow-delimiters-base-face her-theme-hierarchy-2-face)))))
	 (rainbow-delimiters-depth-3-face ((t (:foreground nil :inherit (rainbow-delimiters-base-face her-theme-hierarchy-3-face)))))
	 (rainbow-delimiters-depth-4-face ((t (:foreground nil :inherit (rainbow-delimiters-base-face her-theme-hierarchy-4-face)))))
	 (rainbow-delimiters-depth-5-face ((t (:foreground nil :inherit (rainbow-delimiters-base-face her-theme-hierarchy-5-face)))))
	 (rainbow-delimiters-depth-6-face ((t (:foreground nil :inherit (rainbow-delimiters-base-face her-theme-hierarchy-6-face)))))
	 (rainbow-delimiters-depth-7-face ((t (:foreground nil :inherit (rainbow-delimiters-base-face her-theme-hierarchy-7-face)))))
	 (rainbow-delimiters-depth-8-face ((t (:foreground nil :inherit (rainbow-delimiters-base-face her-theme-hierarchy-8-face)))))
	 (rainbow-delimiters-depth-9-face ((t (:foreground nil :inherit (rainbow-delimiters-base-face her-theme-hierarchy-9-face)))))
	 (rainbow-delimiters-unmatched-face ((t (:inherit (rainbow-delimiters-base-face show-paren-mismatch)))))

      ;;; font lock
     (font-lock-comment-face ((t (;,@theme-text-like-face
								  :foreground ,(her-red) :weight normal  :slant italic
											  ))))
     (font-lock-comment-delimiter-face ((t (:foreground ,(her-fg -.5)))))
     (font-lock-constant-face ((t (:foreground ,(her-cyan 0.25)))))
     (font-lock-doc-face ((t (:inherit font-lock-comment-face :foreground ,(her-pink 0.75)))))
     (font-lock-doc-string-face ((t (:inherit font-lock-doc-face :foreground ,(her-pink 1.0)))))
     (font-lock-function-name-face ((t (:foreground ,(her-blue 0.75) :weight bold))))
     (font-lock-builtin-face ((t (:foreground ,(her-pink 0.75)))))
     (font-lock-keyword-face ((t (:foreground ,(her-pink 1.0)))))
     (font-lock-string-face ((t (:foreground ,(her-red 0.25)))))
     (font-lock-type-face ((t (:foreground ,(her-green 0.75 0.25)))))
     (font-lock-variable-name-face ((t (:foreground ,(her-yellow 0.75)))))
     (font-lock-warning-face ((t (;,@theme-warning-face
								  ,(her-yellow)))))
     (font-lock-preprocessor-face ((t (:foreground ,(her-cyan 0.75 0.25)))))
     (font-lock-negation-char-face ((t ())))

	 ;;calendar
	 (diary ((t (:inherit font-lock-builtin-face))))
	 ;; TODO calendar-today  
	 ;;calendar-weekday-header
	 ;;calendar-weekend-header

	 ;;TODO company

	 ;;TODO diff
	 ;;diff-added          
	 ;;diff-changed                
	 ;;diff-context                
	 ;;diff-file-header         
	 ;;diff-function                
	 ;;diff-header                
	 ;;diff-hunk-header         
	 ;;diff-index                
	 ;;diff-indicator-added          
	 ;;diff-indicator-changed        
	 ;;diff-indicator-removed        
	 ;;diff-nonexistent                
	 ;;diff-refine-added          
	 ;;diff-refine-changed        
	 ;;diff-refine-removed        
	 ;;diff-removed        


	 ;;TODO flycheck

	 (fixed-pitch-serif ((t (:family "Monospace Serif" :inherit fixed-pitch))))

     (hl-todo ((t (:inherit fixed-pitch :foreground ,(her-error) :inverse-video t))))

     ;;org-mode

     (org-hide ((t (:foreground ,(her-bg) :inherit fixed-pitch))))
	 (org-special-keyword ((t (:inherit (fixed-pitch font-lock-comment-delimiter-face) :foreground ,(her-fg 0.65)))))
     (org-table ((t (:inherit fixed-pitch :foreground ,(her-contrast-text)))))
     (org-tag ((t (:inherit org-special-keyword :height ,(her-font-size 10)))))
     (org-date ((t (:inherit org-drawer))))
     (org-checkbox ((t (:inherit (fixed-pitch)))))
     (org-agenda-date ((t (:inherit (calendar-weekday-header) :inverse-video t ))))
     (org-agenda-date-weekend ((t (:inherit (calendar-weekend-header org-agenda-date) :inverse-video t))))
	 (org-agenda-calendar-event ((t (:inherit variable-pitch :weight bold :slant italic))))
	 (org-upcoming-deadline ((t (:inherit (org-warning variable-pitch) ))))
	 (org-scheduled ((t (:inherit fixed-pitch :foreground ,(her-fg)))))
	 ;;for habits
	 (org-scheduled-today ((t (:inherit (org-scheduled)))))
	 (org-scheduled-previously ((t (:inherit (org-scheduled org-warning)))))

	 (org-habit-face ((t (:inherit (fixed-pitch) :inverse-video t))))
	 (org-habit-ready-face ((t (:inherit (org-habit-face) :background ,(her-success 1.0 0.3) :foreground ,(her-success nil 0.75) :box t))))
	 (org-habit-ready-future-face ((t (:inherit (org-habit-face)
												:foreground ,(her-neutral nil 0.75)))))
	 (org-habit-overdue-face ((t (:inherit (org-habit-face org-warning)
										   :foreground ,(her-error nil 0.75)
										   :background ,(her-error nil 0.5)
										   ))))
	 (org-habit-overdue-future-face ((t (:inherit (org-habit-face) :foreground ,(her-warning nil 0.75)))))
	 (org-habit-clear-face ((t (:inherit (org-habit-face) :foreground ,(her-success nil 0.75)))))
	 (org-habit-clear-future-face ((t (:inherit (org-habit-face) :foreground ,(her-success nil 0.75)))))
	 (org-habit-alert-face ((t (:inherit (org-habit-face org-warning) :foreground ,(her-warning nil 0.75)))))
	 (org-habit-alert-future-face ((t (:inherit (org-habit-alert-face org-warning)))))
	 (org-block ((t (:inherit fixed-pitch))))

     (highlight-symbol-face ((t (:background ,(her-cyan -0.6 0.8)))))

	 (lsp-face-highlight-read ((t (:inherit highlight-symbol-face))))
	 (lsp-face-highlight-textual ((t (:inherit font-lock-string-face :slant italic))))
	 (lsp-face-highlight-write ((t (:inherit highlight-symbol-face :slant italic))))

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

;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode 1))
;; End:

;;; her-theme.el ends here
