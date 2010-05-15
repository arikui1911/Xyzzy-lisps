;; -*- Mode: Lisp; Package: NSCRIPTER -*-
;;
;; This files is not part of xyzzy.
;;
;; = nscr-mode
;;
;; == Install
;;
;; (1) Put nscr-mode.l on /site-lisp
;; (2) Put Nscr on /etc 
;; (3) At ~/.xyzzy
;;      (require "nscr-mode")
;;      (add-hook '*find-file-hooks* 'nscr-mode-auto-boot-on-find-file-if-find-nscr-executable)
;;
;; == License
;;
;; Distributes under following terms (modified BSD license):
;;
;;
;; Copyright (c) 2010, arikui <arikui.ruby@gmail.com>
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;;  * Redistributions of source code must retain the above copyright notice,
;;    this list of conditions and the following disclaimer.
;;  * Redistributions in binary form must reproduce the above copyright notice,
;;    this list of conditions and the following disclaimer in the documentation
;;    and/or other materials provided with the distribution.
;;  * Neither the name of the nscr-mode nor the names of its contributors
;;    may be used to endorse or promote products derived from this software
;;    without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

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

;; public special variables
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

;; private special variables
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


