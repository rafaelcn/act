;;; act-mode.el --- Major mode for the ACT programming language -*- coding: utf-8; lexical-binding: t; -*-

;; Authors: Rafael Campos Nunes <rcamposnunes@outlook.com>
;; Package-Version: 1.0
;; Package-Requires: ((emacs "26.1"))
;; URL: https://github.com/rafaelcn/act
;; SPDX-License-Identifier: MIT

;;
;; This file is not part of GNU Emacs.
;;

;;; Commentary:

;; This mode currently only supports syntax highlighting.

;;; Code:

(defvar act-keywords
  '("export" "import")
  "List of keywords in act.")

(defvar act-types
  '("preal" "pint" "bool" "int" "e1of" "e2of" "e3of" "c1of" "globals" "globals_np")
  "List of types in act.")

(defvar act-functions
  '("defproc" "deftype" "defchan" "prs")
  "List of functions in act.")

(defvar act-fontlock
  ;; a regex for each category of word within the language
  (list
   (cons "//.*" 'font-lock-comment-face)                                       ;; comments regex
   (cons (regexp-opt act-keywords 'words) 'font-lock-keyword-face)          ;; keywords regex
   (cons (regexp-opt act-functions 'words) 'font-lock-function-name-face)   ;; functions regex
   (cons (regexp-opt act-types 'words) 'font-lock-type-face)                ;; types regex
   (cons "<[[:digit:]]+>" 'font-lock-constant-face))                           ;; highlights regex
  "List for font-lock defaults.")

;;;###autoload
(define-derived-mode act-mode prog-mode "act"
  "Major mode for the act programming language."
  
  (setq-local font-lock-defaults '((act-fontlock))))

(add-to-list 'auto-mode-alist '("\\.act\\'" . act-mode))

(provide 'act-mode)

;;; act-mode.el ends here
