;;; proact-mode.el --- major mode for editing ACT files -*- coding: utf-8; lexical-binding: t; -*-

;; Authors: Rafael Campos Nunes <rcamposnunes@outlook.com>

(defvar act-keywords nil "act keywords")
(setq act-keywords '("export" "import"))

(defvar act-types nil "act types")
(setq act-types '("bool" "e1of" "e2of" "e3of" "globals"))

(defvar act-functions nil "act functions")
(setq act-functions '("defproc" "prs"))

(defvar act-fontlock nil "list for font-lock defaults")
(setq act-fontlock
	  (let (act-comments-regex
			act-highlights
			act-keywords-regex
			act-types-regex
			act-functions-regex)

		;; a regex for each category of word within the language
		(setq act-keywords-regex (regexp-opt act-keywords 'words))
		(setq act-types-regex (regexp-opt act-types 'words))
		(setq act-functions-regex (regexp-opt act-functions 'words))
		(setq act-comments-regex nil)
		(setq act-highlights nil)
		;; (setq act-comments-regex '("//.+"))
		;; (setq act-highlights '("<[[:digit:]]+>\\|defproc\s\\w+("))

		(list (cons act-highlights 'font-lock-constant-face)
			  (cons act-keywords-regex 'font-lock-keyword-face)
			  (cons act-functions-regex 'font-lock-function-name-face)
			  (cons act-types-regex 'font-lock-type-face)
			  (cons act-comments-regex 'font-lock-comment-face))))


;;;###autoload
(define-derived-mode act-mode prog-mode "act"
  "Major mode for editing act files"

  (setq font-lock-defaults '((act-fontlock))))

(add-to-list 'auto-mode-alist '("\\.act\\'" . act-mode))

(provide 'act-mode)

;;; proact-mode.el ends here
