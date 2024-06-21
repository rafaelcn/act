;;; proact-mode.el --- major mode for editing ACT files -*- coding: utf-8; lexical-binding: t; -*-

;; Authors: Rafael Campos Nunes <rcamposnunes@outlook.com>

(defvar act-keywords '("export" "import") "act keywords")
(defvar act-types '("bool" "e1of" "e2of" "e3of" "globals") "act types")
(defvar act-functions '("defproc" "prs") "act functions")

(defvar act-fontlock nil "list for font-lock defaults")

(setq act-fontlock
	  (let ((act-comments-regex "//.*")
			(act-highlights "<[[:digit:]]+>")
			(act-keywords-regex (regexp-opt act-keywords 'words))
			(act-types-regex (regexp-opt act-types 'words))
			(act-functions-regex (regexp-opt act-functions 'words)))

		;; a regex for each category of word within the language

		(list (cons act-keywords-regex 'font-lock-keyword-face)
			  (cons act-functions-regex 'font-lock-function-name-face)
			  (cons act-types-regex 'font-lock-type-face)
			  (cons act-comments-regex 'font-lock-comment-face)
			  (cons act-highlights 'font-lock-constant-face))))


;;;###autoload
(define-derived-mode act-mode prog-mode "act"
  "Major mode for editing act files"

  (setq-local font-lock-defaults '((act-fontlock))))

(add-to-list 'auto-mode-alist '("\\.act\\'" . act-mode))

(provide 'act-mode)

;;; proact-mode.el ends here
