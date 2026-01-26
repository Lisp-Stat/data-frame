;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: DATA-FRAME -*-
;;; Copyright (c) 2021-2022, 2026 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL
(in-package #:data-frame)

(defparameter *print-data-frame-precision* 2
  "Number of digits after the decimal point when printing numeric values.")

(defun print-data-frame-formatter (x)
  "Data frame formatter with type-specific alignment.  Strings are left-aligned, numbers are right-aligned.
Returns: (values formatted-string alignment)."
  (let ((precision (if (boundp 'nu:*print-matrix-precision*)
                       nu:*print-matrix-precision*
                       *print-data-frame-precision*)))
    (typecase x
      (string (values x :left))
      (integer
       (if (plusp precision)
           ;; Format as float if in a column with real numbers
           (values (format nil "~,vf" precision (float x 1.0d0)) :right)
           (values (format nil "~d" x) :right)))
      (real (values (format nil "~,vf" precision x) :right))
      (complex (values (format nil "~,vf+~,vfi"
                               precision (realpart x)
                               precision (imagpart x))
                       :right))
      (t (values (princ-to-string x) :left)))))

;;; TODO:SN:20260124 Simplify this now that we have print-matrix as a base
(defun show-data-frames (&key (head nil) (stream *standard-output*))
  "Print all data frames in the current environment in reverse order of creation, i.e. most recently created first.
If HEAD is not NIL, print the first six rows, similar to the (head) function"
  (let ((*print-pretty* nil))
    (if head
	(loop for df-sym in *data-frames* do
	  ;; (loop for df in (sort (copy-list *data-frames*) #'string<=) do ;alphabetical order
	  (let ((df (symbol-value df-sym)))
	    (let* ((*print-lines* 6)
		   (*print-pretty* t))
	      (format stream "~2&~A" (symbol-name df-sym))
	      (print df stream))))
	(pprint-logical-block (stream nil)
	  (pprint-logical-block (stream nil)
	    (pprint-indent :block 2 stream)
	    (loop for df-sym in *data-frames*
		  do (progn
		       (format stream "~@:_~A:~@:_" (symbol-name df-sym))
		       (fresh-line stream)
		       (pprint-logical-block (stream nil :per-line-prefix "  ")
			 (format stream "~A" (symbol-value df-sym)))
		       (fresh-line stream)
		       (terpri stream))))))))

;;; Not pretty printing per-se, but related

(defmethod head ((df data-frame) &optional (n 6))
  "Return the first N rows of DF; N defaults to 6"
  (let ((*print-pretty* t))
    (if (< (aops:nrow df) 6) (setf n (aops:nrow df)))
    (pprint (select df (select:range 0 n) t))))

(defmethod tail ((df data-frame) &optional (n 6))
  "Return the last N rows of DF; N defaults to 6"
  (let ((*print-pretty* t))
    (if (< (aops:nrow df) 6) (setf n (aops:nrow df)))
    (pprint (select df (select:range (- n) nil) t))))

(defun short-string (str)
  "Return up to the first newline
This is useful when docstrings are multi-line.  By convention, the first line is the title."
  (subseq str
	  0
	  (position #\newline str)))

(defmethod print-object ((ordered-keys ordered-keys) stream)
  (print-unreadable-object (ordered-keys stream :type t)
    (format stream "~{~a~^, ~}" (coerce (keys-vector ordered-keys) 'list))))

(defmethod print-object ((data-vector data-vector) stream)
  (let ((alist (as-alist data-vector)))
      (print-unreadable-object (data-vector stream :type t)
        (format stream "of ~d variables" (length alist)))))

(defmethod print-object ((df data-frame) stream)
  "Print DATA-FRAME dimensions and type.
After defining this method it is permanently associated with data-frame objects"
  (if (not cl:*print-pretty*)
      (print-unreadable-object (df stream :type t)
	(let ((description (and (slot-boundp df 'name)
				(documentation (find-symbol (name df)) 'variable))))
	  (format stream
		  "(~d observations of ~d variables)"
		  (aops:nrow df)
		  (aops:ncol df))
	  (when description
	    (format stream "~&~A" (short-string description)))))
      (nu:print-matrix (aops:as-array df) *standard-output*
		       :column-labels (keys df)
		       :row-labels t
		       :indent ";; "
		       :formatter #'print-data-frame-formatter)))

(defmethod describe-object ((df data-frame) stream)
  (let ((name (when (slot-boundp df 'name) (name df))))
    (format stream "~A~%" name)
    (format stream "  A data-frame with ~D observations of ~D variables~2%" (aops:nrow df) (aops:ncol df))
    (show-properties df)))


;;; KLUDGE ALERT
;;; We need this so that (describe 'data-frame) will return documentation and the meta-data.
;;; This violates the spec.  It's not easy at all to get good
;;; behaviour from describe.  See code and comments in describe.lisp.
#+allegro (setf excl:*enable-package-locked-errors* nil)
#+lispworks (setf lw:*handle-warn-on-redefinition* :warn)
(defmethod describe-object :after ((s symbol) stream)
  (unless (boundp s) (return-from describe-object))
  (unless (eq #+sbcl (SB-CLTL2:variable-information s)
	      #+ccl  (ccl:variable-information s)
	      #+allegro (system:variable-information s)
              #+lispworks (hcl:variable-information s)
	      :symbol-macro)
    (let ((*print-pretty* nil)
	  (df (symbol-value s))
	  (name (symbol-name s)))

      (pprint-logical-block (stream nil)
	(pprint-logical-block (stream nil)
          (pprint-indent :block 2 stream)
	  (when-let ((pkg (find-package name)))
	    (format stream "~@:_Variables: ~@:_")
	    (pprint-logical-block (stream nil :per-line-prefix "  ")
	      (show-properties df))))))))
#+allegro (setf excl:*enable-package-locked-errors* t)


;;;
;;; Markdown
;;;
(defun print-markdown (data-frame &key (stream *standard-output*) (column-labels nil))
  "Print DATA-FRAME as a markdown-compatible table to STREAM.
COLUMN-LABELS is an optional vector of column labels. If NIL, uses the data frame's keys."
  (let* ((keys (if column-labels column-labels (df:keys data-frame)))
         (ncol (length keys))
         (matrix (aops:as-array data-frame)))

    ;; Print header row
    (princ "|" stream)
    (dotimes (col ncol)
      (format stream " ~A |" (princ-to-string (aref keys col))))
    (terpri stream)

    ;; Print separator row (auto-align: strings left, numbers right)
    (princ "|" stream)
    (dotimes (col ncol)
      (let* ((column (aref (df:columns data-frame) col))
             (first-elem (when (plusp (aops:nrow data-frame)) (aref column 0)))
             (align (typecase first-elem
                      ((or number integer real) " ---: |")
                      (t " :--- |"))))
        (princ align stream)))
    (terpri stream)

    ;; Print data rows using print-matrix with markdown padding
    (nu:print-matrix matrix stream
                  :formatter #'print-data-frame-formatter
                  :padding " | "
                  :indent "| "
                  :aligned? t))
  (values))

;;;
;;; Pretty printer system configuration
;;;

;; After setting the dispatch tables below and *print-pretty* nil, we
;; get the following behaviour:
;; (print df)  => one line summary of number of rows and columns
;; (pprint df) => print full table, subject to *print-lines* and *print-length*
;; print-object is equal to 'print'
;; Unfortunately, it also means that any CL function that sets
;; *print-pretty* to T, like 'describe' will print the entire
;; data-frame.

;; we can't use this and still have nice 'describe' for data frames
#+nil
(set-pprint-dispatch 'df:data-frame
		     #'(lambda (s df) (print-data df s)))

(set-pprint-dispatch '(array * 2)
		     #'(lambda (s array) (nu:print-matrix array s)))

;;; These only work when *print-pretty* is T
#+nil
(set-pprint-dispatch 'float
                     #'(lambda (s obj)
                         (format s "~,4F" obj)))
#+nil
(set-pprint-dispatch 'double-float
                     #'(lambda (s obj)
                         (format s "~,4F" obj)))

;;;
;;; Reference
;;;

;;; Use this to duplicate the behaviour in the documentation
#+nil
(defmethod print-object ((df data-frame) stream)
  "Print the first six rows of DATA-FRAME"
  (let* ((*print-lines* 6)
	 (*print-pretty* t))
    (df:pprint-data df stream nil)))




