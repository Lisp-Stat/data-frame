;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: DATA-FRAME -*-
;;; Copyright (c) 2021 by Symbolics Pte. Ltd. All rights reserved.
(cl:in-package :data-frame)

;;; Pretty print data-frames and 2D arrays

(defparameter *column-summary-minimum-length* 10
  "Columns are only summarised when longer than this, otherwise they are returned as is.")

;;; Recall the following definitions:
;;; *print-length* - controls how many elements at a given level are printed (rows)
;;; *print-lines*  - controls how many output lines are printed (columns)

;;;
;;; Utility functions
;;;

(defun printer-status ()
  "Print values of all the printer variables"
  (format t ";;           *print-array* = ~a~%" *print-array*)
  (format t ";;            *print-base* = ~a~%" *print-base*)
  (format t ";;            *print-case* = ~a~%" *print-case*)
  (format t ";;          *print-circle* = ~a~%" *print-circle*)
  (format t ";;          *print-escape* = ~a~%" *print-escape*)
  (format t ";;          *print-gensym* = ~a~%" *print-gensym*)
  (format t ";;          *print-length* = ~a~%" *print-length*)
  (format t ";;           *print-level* = ~a~%" *print-level*)
  (format t ";;           *print-lines* = ~a~%" *print-lines*)
  (format t ";;     *print-miser-width* = ~a~%" *print-miser-width*)
  (format t ";; *print-pprint-dispatch* = ~a~%" *print-pprint-dispatch*)
  (format t ";;          *print-pretty* = ~a~%" *print-pretty*)
  (format t ";;           *print-radix* = ~a~%" *print-radix*)
  (format t ";;        *print-readably* = ~a~%" *print-readably*)
  (format t ";;    *print-right-margin* = ~a~%" *print-right-margin*))


;;; Note: cl:type-of is under-constrained and returns implementation
;;; specific results, so we use our own version
;;; See: https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node53.html
(defun get-type (x)
  "Return the most specific type symbol for x"
  (typecase x
    (bit     'bit)
    (float   'float)
    (integer 'integer)
    (ratio   'ratio)
    (complex 'complex)
    ;; (rational 'cl:rational)		; SBCL doesn't recognise this return type
    ;; (real    'real)			; SBCL doesn't recognise this return type

    (simple-string 'simple-string)
    (string 'string)

    (list          'list)
    (symbol        'symbol)
    (bit-vector    'bit-vector)
    (simple-vector 'simple-vector)
    (simple-array  'simple-array)
    (vector        'vector)
    (array         'array)
    (sequence      'sequence)

    (function 'function)
    (package  'package)))



;;;
;;; Printing
;;;

;;; To properly print float values we need to use the ~F directive to
;;; control the number of digits so that the decimal points will line
;;; up.

;;; There are three things we need to know to print a data frame:
;;; - width of the column
;;; - type of the column
;;; - maximum number of digits in any value in the column

;;; The three utility functions below give us this information.

(defun max-width (sequence)
  "Return the largest printed string size of the elements of SEQUENCE"
  (apply #'max (map 'list #'(lambda (x)
			      (length
			       (typecase x
				 (float (format nil "~F" x))
				 (t (format nil "~A" x)))))
		    sequence)))

(defun column-type (sequence)
  "Return a format string for the most general type found in sequence
Use this for sequences of type T to determine how to format the column."
  (when (bit-vector-p sequence) (return-from column-type "D"))
  (let ((type-list (map 'list #'(lambda (x) (get-type x)) sequence )))
    (cond ((member 'float   type-list) "F" )
	  ((member 'integer type-list) "D" )
	  ((member 'bit     type-list) "D" )
	  (t "A" ))))

(defun max-decimal (sequence)
  "Return the maximum number of digits to the right of the decimal point in the numbers of SEQUENCE"
  (apply #'max (map 'list
		    #'(lambda (x)
			(typecase x
			  (float (let* ((str (format nil "~F" x))
					(pos (position #\. str)))
				   (length (subseq str (1+ pos)))))
			  (t 0)))
		    sequence)))

;;;
;;; Formatters
;;;

;;; These methods return formatting strings for arrays and data-frames.

;;; For an array
(defmethod default-column-formats ((array simple-array))
  "Return a list of formatting strings for ARRAY
The method returns a set of default formatting strings using heuristics."
  (let ((col-widths (aops:margin #'max-width   array 0))
	(col-types  (aops:margin #'column-type array 0))
	(col-digits (aops:margin #'max-decimal array 0)))
    (map 'list #'(lambda (type width digits)
		   (alexandria:switch (type :test #'string=)
		     ("F" (format nil "~~~A,~AF" width digits))
		     ("D" (format nil "~~~AD"    width))
		     ("A" (format nil "~~~AA"    width))))
	 col-types col-widths col-digits)))

(defmethod df-data-formats ((df data-frame))
  "Return a list of formatting strings for data columns in a data-frame
ARRAY is the data portion of the data-frame (type #2A), KEYS are the data-frame variables (type SYMBOL). The difference with default-column-formats is that df-data-formats returns the greater of column or variable name width."
  (let* ((array      (aops:as-array df))
	 (col-widths (aops:margin #'max-width   array 0))
	 (col-types  (aops:margin #'column-type array 0))
	 (col-digits (aops:margin #'max-decimal array 0))
	 (variables  (map 'list #'symbol-name (keys df))) ; get variable names
	 (variables-widths (map 'list #'length variables))   ; get their widths
	 (widths (map 'list #'(lambda (x y) (max x y)) variables-widths col-widths))) ; take the max
    (map 'list #'(lambda (type width digits)
		   (alexandria:switch (type :test #'string=)
		     ("F" (format nil "~~~A,~AF" width digits))
		     ("D" (format nil "~~~AD"    width))
		     ("A" (format nil "~~~AA"    width))))
	 col-types widths col-digits)))

(defmethod df-variable-formats ((df data-frame))
  "Returns a list of formatting strings for the variable names in a data-frame
This is similar to df-data-formats except that we must use non-default values for the column headers. Strings are left-aligned by default, here we want them to match the justification of the data column."
  ;; Basically repeat df-data-formats with different formatting strings
  (let* ((array      (aops:as-array df))
	 (col-widths (aops:margin #'max-width   array 0))
	 (col-types  (aops:margin #'column-type array 0))
	 (variables  (map 'list #'symbol-name (keys df)))  ; get variable names
	 (variables-widths (map 'list #'length variables)) ; get their widths
	 (widths (map 'list #'(lambda (x y) (max x y)) variables-widths col-widths))) ; take the max
    (map 'list #'(lambda (type width)
		   (alexandria:switch (type :test #'string=)
		     ("F" (format nil "~~~A@A" width))
		     ("D" (format nil "~~~A@A" width))
		     ("A" (format nil "~~~AA"  width))))

	             ;; These are helpful in debugging
		     ;; ("F" (format nil "~~~A,,,'-@A" width))
		     ;; ("D" (format nil "~~~A,,,'-@A" width))
		     ;; ("A" (format nil "~~~A,,,'-A"  width))))
	 col-types widths)))


;;;
;;; Pretty printers
;;;

(defun pprint-data-frame (df &optional (stream *standard-output*))
  (let* ((df-array  (aops:as-array df))
	 (df-lists  (2d-array-to-list df-array))
	 (variables (map 'list #'symbol-name (keys df)))
	 (data-fmt  (df-data-formats df))
	 (var-fmt   (df-variable-formats df))
	 (f 0))

    (if *print-pretty*
	(progn
	  ;; Print column headers
	  (pprint-logical-block (stream variables :per-line-prefix ";; ")
	    (loop (pprint-exit-if-list-exhausted)
		  (format stream (nth f var-fmt) (pprint-pop))
		  (incf f)
		  (write-char #\Space stream)))

	  (write-char #\Newline stream)
	  (setf f 0)

	  ;; Print data
	  (pprint-logical-block (stream df-lists)
	    (loop (pprint-exit-if-list-exhausted)
		  (let ((row (pprint-pop)))
		    (pprint-logical-block (stream row :per-line-prefix ";; ")
		      (loop (pprint-exit-if-list-exhausted)
			    (format stream (nth f data-fmt) (pprint-pop))
			    (incf f)
			    (write-char #\Space stream))))
		  (pprint-newline :mandatory stream)
		  (setf f 0))))

	;; not *print-pretty*
	(pprint-logical-block (stream df-lists)
	  (print-unreadable-object (df stream :type t)
            (format stream "(~d observations of ~d variables)"
		    (aops:nrow df)
		    (if (string= "" (first variables)) ; Remove row-name from count
			(1- (aops:ncol df))
			(aops:ncol df))))))))

(defun pprint-array (array &optional (stream *standard-output*))
  "Print an array to *standard-output* in a tabular format.  Print to STREAM otherwise."
    (let* ((df-lists (2d-array-to-list array))
	   (data-fmt (default-column-formats array))
	   (f 0))

      ;; (write-char #\Newline stream)	; this is neccessary for some reason
      (if *print-pretty*

	  (pprint-logical-block (stream df-lists)
	    (loop (pprint-exit-if-list-exhausted)
		  (let ((row (pprint-pop)))
		    (pprint-logical-block (stream row :per-line-prefix ";; ")
		      (loop (pprint-exit-if-list-exhausted)
			    (format stream (nth f data-fmt) (pprint-pop))
			    (incf f)
			    (write-char #\Space stream))))
		    (pprint-newline :mandatory stream)
		    (setf f 0)))

	  (format stream "~A" array))))



;;; Not pretty printing per-se, but related

(defmethod head ((df data-frame) &optional (n 6))
  "Return the first N rows of DF; N defaults to 6"
  (pprint-data-frame (select df (select:range 1 n) t)))

(defmethod tail ((df data-frame) &optional (n 6))
  "Return the last N rows of DF; N defaults to 6"
  (pprint-data-frame (select df (select:range (- n) nil) t)))

(defmethod column-names ((df data-frame))
  "Return a list column names in DF, as strings"
   (map 'list #'symbol-name (keys df)))

(defmethod row-names ((df data-frame))
  "Return a list row names in DF, as strings"
  (coerce (select df t :||) 'list))


;;;
;;; Pretty printer system configuration
;;;

;; Do not delete, used in a few places when printing to REPL
(defmethod print-object ((ordered-keys ordered-keys) stream)
  (print-unreadable-object (ordered-keys stream :type t)
    (format stream "~{~a~^, ~}" (coerce (keys-vector ordered-keys) 'list))))

;; data-vectors should probably be deprecated
(defmethod print-object ((data-vector data-vector) stream)
  (let ((alist (as-alist data-vector)))
    (pprint-logical-block (stream alist)
      (print-unreadable-object (data-vector stream :type t)
        (format stream "(~d)" (length alist))
        (loop (pprint-exit-if-list-exhausted)
              (let+ (((key . column) (pprint-pop)))
                (format stream "~_ ~W ~W" key column))
              (pprint-exit-if-list-exhausted)
              (princ "," stream))))))

(defmethod print-object ((df data-frame) stream)
  "Print DATA-FRAME dimensions and type
After defining this method it is permanently associated with data-frame objects"
  (let* ((df-array  (aops:as-array df))
	 (df-lists  (2d-array-to-list df-array))
	 (variables (map 'list #'symbol-name (keys df))))

	(pprint-logical-block (stream df-lists)
	  (print-unreadable-object (df stream :type t)
            (format stream "(~d observations of ~d variables)"
		    (aops:nrow df)
		    (if (string= "" (first variables)) ; Remove row-name from count
			(1- (aops:ncol df))
			(aops:ncol df)))))))


;;; Printer dispatch tables

;; After setting this, with *print-pretty* nil, we get the following behaviour:
;; (print df)  => one line summary of number of rows and columns
;; (pprint df) => print full table, subject to *print-lines* and *print-length*
;; print-object is equal to 'print'
(set-pprint-dispatch 'df:data-frame
		     #'(lambda (s df) (pprint-data-frame df s)))

(set-pprint-dispatch '(array * 2)
		     #'(lambda (s array) (pprint-array array s)))


;;;
;;; Reference
;;;

;;; Use this to duplicate the behaviour in the documentation
#+nil
(defmethod print-object ((df data-frame) stream)
  "Print the first six rows of DATA-FRAME"
  (let* ((*print-lines* 6)
	 (*print-pretty* t))
    (df:pprint-data-frame df stream)))
