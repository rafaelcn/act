;;; proact-mode.el --- Major mode for the ACT programming language -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;; SPDX-License-Identifier: MIT

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

(defvar proact-fontlock
  ;; a regex for each category of word within the language
  (list (cons (regexp-opt proact-keywords 'words) 'font-lock-keyword-face)          ;; keywords regex
		(cons (regexp-opt proact-functions 'words) 'font-lock-function-name-face)   ;; functions regex
		(cons (regexp-opt proact-types 'words) 'font-lock-type-face)                ;; types regex
		(cons "//.*" 'font-lock-comment-face)                                       ;; comments regex
		(cons  "<[[:digit:]]+>" 'font-lock-constant-face))                          ;; highlights regex
  "List for font-lock defaults.")

;;;###autoload
(define-derived-mode proact-mode prog-mode "proact"
  "Major mode for the act programming language."
  
  (setq-local font-lock-defaults '((proact-fontlock))))

(add-to-list 'auto-mode-alist '("\\.act\\'" . act-mode))

(provide 'proact-mode)

;;; proact-mode.el ends here
