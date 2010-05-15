;; -*- Mode: Lisp; Package: RD; encoding: sjis -*-
;; 
;; This files is not part of xyzzy.
;;
;;
;; = rd-mode.l
;; 
;; == Install
;; 
;; (1) Put rd-mode.l on /site-lisp
;; (2) At .xyzzy
;;     
;;      (require "rd-mode")
;;      (push '("\\.rd$" . rd-mode) *auto-mode-alist*)
;; 
;; 
;; == Features
;; 
;;  * Syntax color hilighting
;;  * Input helper commands
;;  * Preview document which compiled to HTML (depend on browserex)
;; 
;; 
;; == License
;;
;; Distributes under following terms (modified BSD license):
;;
;;
;; Copyright (c) 2009, arikui <arikui.ruby@gmail.com>
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
;;  * Neither the name of the arikui-rd-mode nor the names of its contributors
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
;;

(provide "rd")
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "rd") (defpackage "rd")))
(in-package "rd")
(use-package "editor")
(use-package "rd" "user")

(export '(rd-mode
          *rd-mode-hook*
          *rd-mode-map*
          rd-insert-newline
          rd-indent-line
          rd-inline-em
          rd-inline-code
          rd-inline-var
          rd-inline-kbd
          rd-inline-link
          rd-inline-index
          rd-inline-note
          rd-inline-verb
          rd-inline-input-em
          rd-inline-input-code
          rd-inline-input-var
          rd-inline-input-kbd
          rd-inline-input-link
          rd-inline-input-index
          rd-inline-input-note
          rd-inline-input-verb
          rd-inline-link-wizard
          rd-inline-reference-wizard
          rd-insert-toc
          *rd-preview-style*
          rd-browser-preview
          rd-close-preview
          ))


;; public special variables
(defvar *rd-mode-hook* nil "rd-mode起動時のフック")
(defvar *rd-mode-map* nil "rd-mode用キーマップ")
(defvar *regexp-keyword-list* nil "rd-mode構文ハイライト")


;; initialize and memonize
(unless *rd-mode-map*
  (setf *rd-mode-map* (make-sparse-keymap))
  (define-key *rd-mode-map* #\TAB 'rd-indent-line)
  (define-key *rd-mode-map* #\RET 'rd-insert-newline))

(unless *regexp-keyword-list*
  (setf
   *regexp-keyword-list*
   (compile-regexp-keyword-list
    '(("^=begin\\b"      nil (:keyword 6))
      ("^=end\\b"        nil (:keyword 6))
      ("^#"              t   (:keyword :comment :line))
      ("^<<<"            t   (:keyword 9 :line :underline))
      ("^=\\{1,4\\}"     t   (:keyword 0 :line))
      ("^\\+\\{1,2\\}"   t   (:keyword 0 :line))
      ("^[ \t]*\\*"      t   (:keyword 1))
      ("^[ \t]*([0-9]+)" t   (:keyword 1))
      ("^[ \t]*---"      t   (:keyword 1 :line))
      ("^[ \t]*:"        t   (:keyword 1 :line))
      ("((\\*.*?\\*))"   t   :tag)
      ("(({.*?}))"       t   :tag)
      ("((|.*?|))"       t   :tag)
      ("((%.*?%))"       t   :tag)
      ("((:.*?:))"       t   :tag)
      ("((<.*?>))"       t   :tag)
      ("((-.*?-))"       t   :tag)
      ("(('.*?'))"       t   :tag)))))


(defun rd-mode ()
  "Major mode to edit RD format document"
  (interactive)
  (kill-all-local-variables)
  (setf buffer-mode 'rd-mode
        mode-name   "RD")
  (use-keymap *rd-mode-map*)
  (make-local-variable 'regexp-keyword-list) (setf regexp-keyword-list *regexp-keyword-list*)
  (make-local-variable 'paragraph-separate)  (setf paragraph-separate  "=+\\|\\++\\|[ \t\n\^L]*$")
  (make-local-variable 'paragraph-start)     (setf paragraph-start     "=+\\|\\++\\|[ \t\n\^L]")
  (make-local-variable 'ed::build-summary-function)
  (setf ed::build-summary-function 'build-summary-labels)
  (run-hooks '*rd-mode-hook*))


(defun rd-insert-newline ()
  (interactive)
  (insert "\n")
  (do-indent-based-on-previous-line))

(defun do-indent-based-on-previous-line ()
  (multiple-value-bind (type baseline) (lex-previous-line)
    (case type
      ((:headline :include) nil)
      ((:itemlist :enumlist)
       (multiple-value-bind (type baseline) (lex-previous-line t) (indent-to baseline)))
      (:commentline
       (save-excursion
         (backward-line)
         (looking-at "[ \t#]+")
         (skip-chars-forward " \t#")
         (setf baseline (current-column)))
       (insert (match-string 0))
       (indent-to baseline))
      (t (indent-to baseline)))))

(defun rd-indent-line ()
  (interactive)
  (unless (member (lex-current-line) '(:headline :include))
    (multiple-value-bind (type baseline) (lex-previous-line)
      (save-excursion
        (goto-bol)
        (delete-region (point) (progn (skip-chars-forward " \t") (point)))
        (indent-to baseline))
      (if (and (bolp) baseline)
          (skip-chars-forward " \t")))))

(defun lex-current-line (&optional ignore-list)
  (save-excursion
    (goto-bol)
    (cond ((looking-for "=") (values :headline 0))
          ((looking-for "+") (values :headline 0))
          ((looking-for "<<<") (values :include 0))
          ((looking-for "#") (values :commentline 0))
          (t (let ((type nil))
               (skip-chars-forward " \t")
               (cond ((looking-at "\\*") (setf type :itemlist))
                     ((looking-at "([0-9]+)") (setf type :enumlist))
                     ((looking-at ":") (setf type :desclist))
                     ((looking-at "---") (setf type :methodlist)))
               (if type (progn
                          (unless ignore-list
                            (forward-char (length (match-string 0))))
                          (skip-chars-forward " \t"))
                 (setf type (if (looking-at "[^ \t\n]") :textline :whiteline)))
               (values type (current-column)))))))


(defun lex-previous-line (&optional ignore-list)
  (save-excursion
    (backward-line)
    (lex-current-line ignore-list)))

(defun insert-inline (opener closer prompt)
  (cond ((pre-selection-p) (inline-region opener closer (selection-mark) (selection-point)))
        ((mark t)          (inline-region opener closer (mark) (point)))))

(defun inline-region (opener closer from to)
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char (point-min)) (insert opener)
      (goto-char (point-max)) (insert closer))))

(defun input-inline (opener closer prompt &optional str)
  (let ((str (or str (read-string prompt))))
    (insert opener str closer)))

(defmacro def-inline-command (insert-func input-func opener closer prompt)
  `(progn
     (defun ,insert-func ()
       (interactive)
       (or (insert-inline ,opener ,closer ,prompt) (,input-func)))
     (defun ,input-func (&optional str)
       (interactive)
       (input-inline ,opener ,closer ,prompt str))))

;; rd-inline-xxx は セレクションやリージョンを Inline 要素 xxx として囲い、
;; rd-inline-input-xxx はミニバッファに内容を入力して、要素をバッファに挿入します。
(def-inline-command rd-inline-em    rd-inline-input-em    "((*" "*))" "Em: ")
(def-inline-command rd-inline-code  rd-inline-input-code  "(({" "}))" "Code: ")
(def-inline-command rd-inline-var   rd-inline-input-var   "((|" "|))" "Var: ")
(def-inline-command rd-inline-kbd   rd-inline-input-kbd   "((%" "%))" "Kbd: ")
(def-inline-command rd-inline-link  rd-inline-input-link  "((<" ">))" "Link: ")
(def-inline-command rd-inline-index rd-inline-input-index "((:" ":))" "Index: ")
(def-inline-command rd-inline-note  rd-inline-input-note  "((-" "-))" "Note: ")
(def-inline-command rd-inline-verb  rd-inline-input-verb  "(('" "'))" "Verb: ")

;; Link の 表示文字列にエスケープが必要な場合、ダブルクォーテーションでエスケープします。
(defun inline-link-caption-escape (org)
  (if (string-match "[|/]" org) (format nil "\"~A\"" org) org))

;; URL と、必要ならリンクを張る表示文字列をミニバッファに入力し、
;; Link を バッファに挿入します。
;;
;; 本来、表示文字列に "|" や "/" を用いるにはエスケープが必要ですが、
;; この関数は必要なら勝手にエスケープしてくれます。
;;
(defun rd-inline-link-wizard (url caption)
  (interactive "sURL: \nsCaption: ")
  (rd-inline-input-link (if (zerop (length caption))
                            (format nil "URL:~A" url)
                          (format nil "~A|URL:~A" (inline-link-caption-escape caption) url))))

(defvar *reference-history* nil)
(setf (get '*reference-history* 'ed::minibuffer-history-variable) '*reference-history*)

;; ミニバッファに RD の Label を入力し、 Reference をバッファに挿入します。
;;
;; Label 名には補完が効きます。また、履歴が効かない代わりに、バッファ内の Label が
;; 履歴の要領で選択できます。
;;
(defun rd-inline-reference-wizard ()
  (interactive)
  (let ((rd-labels (list-labels)))
    (setf *reference-history* rd-labels)
    (rd-inline-input-link
     (inline-link-caption-escape
      (completing-read "Label: " rd-labels
                       :history '*reference-history*)))))

;; RD の Label になりうるもの (Headline, DescList term, MethodList term) を集めます。
(defun list-labels ()
  (mapcar #'second
          (collect-matched-lines
           "^\\(=\\{1,4\\}\\|\\+\\{1,2\\}\\|[ \t]*:\\|[ \t]*---\\)[ \t]*\\(.*?\\)$" 2)))

;; list-function 用。 Headline だけを捜します。
(defun build-summary-labels ()
  (collect-matched-lines "^\\(=\\{1,4\\}\\|\\+\\{1,2\\}\\)[ \t]*\\(.*?\\)$" 2))

;; 正規表現を用いて行情報を集めます。
;;
;; 行情報: (list 行番号 (match-string (first regnums)) (match-string (second regnums)) ... )
;;
(defun collect-matched-lines (re &rest regnums)
  (save-excursion
    (goto-char (point-min))
    (let ((lst ()))
      (labels ((f (acc)
                 (if (re-search-forward re t)
                     (progn
                       (push (cons (current-line-number) (mapcar #'match-string regnums)) acc)
                       (goto-char (match-end 0))
                       (f acc))
                   acc)))
        (nreverse (f lst))))))


;; Table of Contents として使える ItemList を挿入します。
;; 内容は Headline の Label への Reference 集です。
;;
;; 集められる Headline は レベルが 4 以上のものですが、この下限は前置引数で変更できます。
;;
(defun rd-insert-toc (&optional (arg 4))
  (interactive "p")
  (let ((current-level 0))
    (dolist (lst (collect-matched-lines "^\\(?:\\(=\\{1,4\\}\\)\\|\\(\\+\\{1,2\\}\\)\\)[ \t]*\\(.*?\\)$" 1 2 3))
      (let ((level (+ (length (or (second lst) (third lst))) (if (third lst) 4 0))))
        (when (>= arg level)
          (when (> level current-level)
            (dotimes (n (- level current-level 1))
              (insert "  " n)
              (insert "* -\n")))
          (insert "  " (1- level))
          (insert "* ")
          (rd-inline-input-link (inline-link-caption-escape (fourth lst)))
          (insert "\n")
          (setf current-level level))))))



(defvar *browserex-missing*
  (let ((feature "browserex") (missing nil))
    (handler-case (require feature)
      (simple-error (c)
        (if (string= feature (simple-error-format-arguments c))
          (setf missing t)
          (error c))))
    missing))

(defun need-browserex ()
  (when *browserex-missing*
    (error 'simple-error
           :format-string "browserex がロードされていません")))


(defvar *rd-preview-style* nil "ブラウザプレビューに適用するCSS")

;; 編集中の RD の変換結果を、
;; browserex を利用して、ブラウザと RDtool でプレビューする
(defun rd-browser-preview ()
  (interactive)
  (need-browserex)
  (save-window-excursion
    (force-selected-buffer-visit-file
     src
     (let ((dest (get-buffer-create "*RD-Preview-SRC*")))
       (setup-temp-buffer dest)
       (execute-shell-command
	(if *rd-preview-style*
	    (format nil "rd2 -r rd/rd2html-lib --with-css=file://~A" (truename *rd-preview-style*))
	  "rd2")
	(get-buffer-file-name src) dest)
       (set-buffer dest)
       (browserex::navigate-current-buffer)
       (kill-buffer dest)))))


(defmacro force-selected-buffer-visit-file (buf &rest body)
  `(if (file-visited-p)
       (let ((,buf (selected-buffer))) ,@body)
     (let (,buf
	   (content (buffer-substring (point-min) (point-max))))
       (find-file (make-temp-file-name))
       (trap-errors
	 (setup-temp-buffer (selected-buffer))
	 (insert content)
	 (save-buffer)
	 (setf ,buf (selected-buffer))
	 ,@body)
       (delete-file (get-buffer-file-name ,buf))
       (delete-buffer ,buf))))


(defun rd-close-preview ()
  (interactive)
  (need-browserex)
  (browserex::close-browser))

