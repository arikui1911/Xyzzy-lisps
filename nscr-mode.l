;;; -*- Mode: Lisp; Package: NSCRIPTER -*-
;;;
;;; This files is not part of xyzzy.
;;;

;;; (add-hook '*find-file-hooks* 'nscr-mode-auto-boot-on-find-file-if-find-nscr-executable)


(provide "nscr-mode")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "nscripter") (defpackage "nscripter")))

(in-package "nscripter")
(use-package "editor")
(use-package "nscripter" "user")

(export '(nscr-mode
	  *nscr-mode-hook*
	  *nscr-mode-map*
	  *nscr-mode-syntax-table*
	  *nscr-mode-abbrev-table*
	  nscr-mode-auto-boot-on-find-file-if-find-nscr-executable
	  *nscr-prog*
	  nscr-run
	  ))

;;; public special variables
(defvar *nscr-mode-hook*         nil         "Hook for nscr-mode")
(defvar *nscr-mode-map*          nil         "Keymap for nscr-mode")
(defvar *nscr-mode-syntax-table* nil         "Syntax table for nscr-mode")
(defvar *nscr-mode-abbrev-table* nil         "Abbrev table for nscr-mode")
(defvar *nscr-prog*              "nscr.exe"  "NScripter executable basename")


(unless *nscr-mode-map*
  (let ((m (make-sparse-keymap)))
    (setf *nscr-mode-map* m)))

(unless *nscr-mode-syntax-table*
  (let ((s (make-syntax-table)))
    (set-syntax-string s #\")
    (set-syntax-escape s #\\)
    (set-syntax-start-comment s #\;)
    (set-syntax-end-comment s #\LFD nil t)
    (set-syntax-word s #\_)
    (setf *nscr-mode-syntax-table* s)))

;;; private special variables
(defvar *keywords*            nil)
(defvar *keyword-file*        "Nscr")
(defvar *regexp-keyword-list* nil)

(defun ready-keywords ()
  (unless *keywords*
    (setf *keywords* (load-keyword-file *keyword-file*)))
  (when *keywords*
    (make-local-variable 'keyword-hash-table)
    (setf keyword-hash-table *keywords*)))

(unless *regexp-keyword-list*
  (setf
   *regexp-keyword-list*
   (compile-regexp-keyword-list
    '(;; label
      ("^[ \t]*\\*[_a-zA-Z][_a-zA-Z0-9]*" nil (:keyword :tag :underline))
      ;; marks
      ("[@\\/]" nil 0)
      ;; ruby
      ("([^)/]+/[^)]+)" nil 9)
      ))))


(defun nscr-mode ()
  "Major mode for editing NScripter scripts."
  (interactive)
  (kill-all-local-variables)
  (setf mode-name "Nscr" buffer-mode 'nscr-mode)
  (use-keymap *nscr-mode-map*)
  (use-syntax-table *nscr-mode-syntax-table*)
  (setf *local-abbrev-table* *nscr-mode-abbrev-table*)
  (ready-keywords)
  (make-local-variable 'regexp-keyword-list)
  (setf regexp-keyword-list *regexp-keyword-list*)
  ;;
  (run-hooks *nscr-mode-hook*))

(defun nscr-mode-auto-boot-on-find-file-if-find-nscr-executable ()
  "Suppose to be added to *find-file-hooks*; run nscr-mode if
found NScripter (*nscr-prog*) from the same directory a editing file is and
a extension of editing file was `txt'."
  (when (let ((fname (get-buffer-file-name)))
	  (and (file-exist-p (concat (append-trail-slash (directory-namestring fname))
				     *nscr-prog*))
	       (string= "txt" (string-downcase (pathname-type fname)))))
    (nscr-mode)))


(defun nscr-run ()
  "Run NScripter (*nscr-prog*) which is in the same directory a editing file is."
  (interactive)
  (call-process *nscr-prog*
		:exec-directory (directory-namestring (get-buffer-file-name))))


