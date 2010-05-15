;;; -*- Mode: Lisp; Package: Python; encoding: sjis -*-
;;;
;;; This files is not part of xyzzy.
;;;
;;; = python-mode.l
;;; 
;;; == Install
;;; 
;;; (1) Put python-mode.l on /site-lisp
;;; (2) At .xyzzy
;;;     
;;;      (require "python-mode")
;;;      (push '("\\.py$" . python-mode) *auto-mode-alist*)
;;; 
;;; == Usage
;;; 
;;; : C-Tab
;;;   Unindent 1 level current line.
;;; : C-c x
;;;   Execute editing file by Python.
;;;   When with prefix argument, input script arguments interactively.
;;; 
;;; 
;;; == License
;;;
;;; Distributes under following terms (modified BSD license):
;;;
;;;
;;; Copyright (c) 2009, arikui <arikui.ruby@gmail.com>
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:
;;;
;;;  * Redistributions of source code must retain the above copyright notice,
;;;    this list of conditions and the following disclaimer.
;;;  * Redistributions in binary form must reproduce the above copyright notice,
;;;    this list of conditions and the following disclaimer in the documentation
;;;    and/or other materials provided with the distribution.
;;;  * Neither the name of the arikui-python-mode nor the names of its contributors
;;;    may be used to endorse or promote products derived from this software
;;;    without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;; POSSIBILITY OF SUCH DAMAGE.
;;;


(provide "python-mode")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "python") (defpackage "python")))

(in-package "python")
(use-package "editor")
(use-package "python" "user")

(export '(python-mode
	  *python-mode-hook*
	  *python-mode-map*
	  *python-mode-syntax-table*
	  *python-mode-abbrev-table*
	  *python-prog*
	  python-run
	  python-run-with-edited))


;;; general purpose functions
(defun curry (fn &rest args)
  (lambda (&rest rest-args)
    (apply fn (append args rest-args))))

(defun tabulate (fn m &optional (n 1) (s 1))
  (labels ((f (n acc)
	     (if (> n m) (nreverse acc)
	       (f (+ n s) (cons (funcall fn n) acc)))))
    (f n nil)))


;;; public special variables
(defvar *python-mode-hook* nil)
(defvar *python-mode-map* nil)
(defvar *python-mode-syntax-table* nil)
(defvar *python-mode-abbrev-table* nil)
(defvar *python-prog* "python")


;;; private special variables
(defvar *keywords* nil)
(defvar *keyword-file* "Python")
(defvar *runner-history* nil)
(setf (get '*runner-history* 'ed::minibuffer-history-variable) '*runner-history*)

(defconstant *block-beg*
  "\\(class\\|def\\|if\\|elif\\|else\\|for\\|while\\|try\\|except\\|finally\\|with\\)\\>")

(defconstant *summary-target*
  "^[ \t]*\\(class\\|def\\)\\>.*$")


;;; initialize and memonize
(unless *python-mode-map*
  (let ((m (make-sparse-keymap)))
    (setf *python-mode-map* m)
    (define-key m #\TAB 'python-indent-line)
    (define-key m #\F20 'python-unindent-line)
    (define-key m #\RET 'python-newline-and-indent)
    (define-key m '(#\C-c #\x) 'python-run-with-edited)))

(set-extended-key-translate-table exkey-C-tab #\F20)


(unless *python-mode-syntax-table*
  (let ((s (make-syntax-table)))
    (setf *python-mode-syntax-table* s)
    (mapc (curry #'set-syntax-punctuation s)
	  (remove-if #'alpha-char-p (tabulate #'code-char #x7e #x21)))
    (set-syntax-string        s #\")
    (set-syntax-string        s #\')
    (set-syntax-match         s #\( #\))
    (set-syntax-match         s #\{ #\})
    (set-syntax-match         s #\[ #\])
    (set-syntax-escape        s #\\)
    (set-syntax-start-comment s #\#)
    (set-syntax-end-comment   s #\LFD nil t)
    (set-syntax-word          s #\_)))

(unless *python-mode-abbrev-table*
  (define-abbrev-table '*python-mode-abbrev-table*))


;;; entry point
(defun python-mode ()
  (interactive)
  (kill-all-local-variables)
  (setf mode-name "Python" buffer-mode 'python-mode)
  (use-keymap *python-mode-map*)
  (use-syntax-table *python-mode-syntax-table*)
  (setf *local-abbrev-table* *python-mode-abbrev-table*)
  (unless *keywords*
    (setf *keywords* (load-keyword-file *keyword-file*)))
  (when *keywords*
    (make-local-variable 'keyword-hash-table)
    (setq keyword-hash-table *keywords*))
  (make-local-variable 'mode-specific-indent-command)
  (setf mode-specific-indent-command 'python-indent-line)
  (make-local-variable 'ed::build-summary-function)
  (setf ed::build-summary-function 'python-build-summary)
  (run-hooks '*python-mode-hook*))


(defun calc-current-line-indent ()
  (save-excursion
    (goto-bol)
    (let ((bol (point)))
      (skip-chars-forward " \t")
      (- (point) bol))))

(defun calc-block-indent ()
  (save-excursion
    (backward-line)
    (skip-chars-forward " \t")
    (+ (calc-current-line-indent)
       (if (looking-at *block-beg*) 4 0))))

(defun py-indent-to (n)
  (goto-bol)
  (skip-chars-forward " \t")
  (delete-trailing-spaces)
  (insert " " n))

(defun python-indent-line ()
  (interactive)
  (let ((indent (calc-block-indent))
	(cur (calc-current-line-indent)))
    (unless (= indent cur) (py-indent-to indent))))

(defun python-unindent-line ()
  (interactive)
  (let (indent
	(cur (calc-current-line-indent)))
    (setf indent (- cur 4))
    (when (minusp indent) (setf indent 0))
    (unless (= indent cur) (py-indent-to indent))))

(defun python-newline-and-indent ()
  (interactive)
  (insert "\n")
  (python-indent-line))

(defun python-build-summary ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (labels ((f (acc)
	       (if (re-search-forward *summary-target* t)
		   (progn
		     (push (list (current-line-number) (match-string 0)) acc)
		     (goto-char (match-end 0))
		     (f acc))
		 (nreverse acc))))
      (f nil))))


;;; runner
(defun python-run (args)
  (interactive "sARGS: ")
  (pipe-command (concat *python-prog* " " args)))

(defun python-run-with-edited (&optional prearg)
  (interactive "P")
  (let ((edited (or (get-buffer-file-name (selected-buffer))
		    (error 'simple-error "ファイル名がありません")))
	(args '()))
    (when prearg
      (push (read-string "ARGS: " :history '*runner-history*) args)
      (push " " args))
    (push edited args)
    (python-run (apply 'concat args))))

