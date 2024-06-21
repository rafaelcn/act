;;; proact-mode.el --- Major mode for the ACT programming language -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;; Copyright 2024 The proact-mode Authors. All rights reserved.
;; Use of this source code is governed by a MIT license that
;; can be found in the LICENSE file.

;; Authors: Rafael Campos Nunes <rcamposnunes@outlook.com>
;; Package-Version: 1.0
;; Package-Requires: ((emacs "26.1"))
;; URL: https://github.com/rafaelcn/proact

;;
;; This file is not part of GNU Emacs.
;;

;;; Code:

(defvar proact-keywords '("export" "import") "List of keywords in act.")
(defvar proact-types '("bool" "e1of" "e2of" "e3of" "globals") "List of types in act.")
(defvar proact-functions '("defproc" "prs") "List of functions in act.")

(defvar proact-fontlock nil "List for font-lock defaults.")

(setq proact-fontlock
	  (let ((proact-comments-regex "//.*")
			(proact-highlights "<[[:digit:]]+>")
			(proact-keywords-regex (regexp-opt proact-keywords 'words))
			(proact-types-regex (regexp-opt proact-types 'words))
			(proact-functions-regex (regexp-opt proact-functions 'words)))

		;; a regex for each category of word within the language

		(list (cons proact-keywords-regex 'font-lock-keyword-face)
			  (cons proact-functions-regex 'font-lock-function-name-face)
			  (cons proact-types-regex 'font-lock-type-face)
			  (cons proact-comments-regex 'font-lock-comment-face)
			  (cons proact-highlights 'font-lock-constant-face))))


;;;###autoload
(define-derived-mode proact-mode prog-mode "proact"
  "Major mode for the act programming language."

  (setq-local font-lock-defaults '((act-fontlock))))

(add-to-list 'auto-mode-alist '("\\.act\\'" . act-mode))

(provide 'proact-mode)

;;; proact-mode.el ends here
