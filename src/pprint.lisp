;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: DATA-FRAME -*-
;;; Copyright (c) 2021 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:data-frame)

#+genera (eval-when (eval load compile) (xp::install))


;;; Pretty print data-frames and 2D arrays

;;; This is an extension of the original work by Tamas that used the
;;; Common Lisp pretty printing system to format data-frames for
;;; printing. As an experiment, I have to conclude that using the
;;; pretty printer this way is a bad idea. It was intended to format
;;; lisp code, not more complicated output. Future work should be done
;;; in formatted-output.lisp and these functions are considered
;;; deprecated.

;;; It is not easy to line up columns dynamically when printing them.
;;; Common lisp does have good control over justification and digits
;;; but, unlike, say, markdown, it's up to the programmer to
;;; explicitly specify the paddings.  If you knew a prori the widths,
;;; this would be easy, but if you want to compute them at run-time
;;; things get a bit ugly.

;;; There are two patterns here for this:
;;; 1. create a new structure that contains the strings in their printed format (pprint-data-frame)
;;; 2. compute the formatting and apply each format to the original value as you loop through the structure (pprint-array)

;;; Neither is particularly efficient. Option 2 is better than option
;;; 1 in that regard, at the expense of some ugliness in the code and
;;; keeping track of formatting strings for each column.


;;; The following global variables control aspects of printing:
;; *print-length* - controls how many elements at a given level are printed (rows)
;; *print-lines*  - controls how many output lines are printed (columns)
(defparameter *max-digits* 4)		;max digits after decimal
(defparameter *row-numbers-p* t)	;print row numbers


;;;
;;; Utility functions
;;;

;; Consider exporting this if it turns out to be generally useful
(defun reverse-df (df)
  "Return DF with columns in reverse order"
  (make-df (reverse (keys df)) (reverse (columns df))))

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


(defun 2d-array-to-list (array)
  "Convert an array to a list of lists" 		; make flet?
  (loop for i below (array-dimension array 0)
        collect (loop for j below (array-dimension array 1)
                      collect (aref array i j))))

;;;
;;; Computing column formats
;;;

;;; To properly print float values we need to use the ~F directive to
;;; control the number of digits so that the decimal points will line
;;; up.

;;; There are three things we need to know to print a data frame:
;;; - width of the column
;;; - type of the column
;;; - maximum number of digits in any value in the column

;;; The three utility functions below give us this information.

(defun max-width (sequence &optional (max-width nil))
  "Return the largest printed string size of the elements of SEQUENCE, equal to or less than MAX-WIDTH"
  (let ((actual-width (apply #'max (map 'list #'(lambda (x)
					 (length
					  (typecase x
					    (float (format nil "~F" x))
					    (t (format nil "~A" x)))))
			       sequence))))
    (if max-width
	(min actual-width max-width)
	actual-width)))

(defun column-type-format (sequence)
  "Return a format string for the most specific type found in sequence
Use this for sequences of type T to determine how to format the column."
  (when (bit-vector-p sequence) (return-from column-type-format "D"))
  ;; (let ((x (column-type sequence)))
  (case (column-type sequence)
    (single-float "F")
    (double-float "F")
    (integer "D")
    (bit "D")
    (symbol "S")
    (t "A")))

(defun max-decimal (sequence &optional (max-digits nil))
  "Return the maximum number of digits to the right of the decimal point in the numbers of SEQUENCE, equal to or less than MAX-DIGITS"
  (let ((actual-digits (apply #'max
			      (map 'list
				   #'(lambda (x)
				       (typecase x
					 (float (let* ((str (format nil "~F" x))
						       (pos (position #\. str)))
						  (length (subseq str (1+ pos)))))
					 (t 0)))
				   sequence))))
    (if max-digits
	(min actual-digits max-digits)
	actual-digits)))

;;;
;;; Formatters
;;;

(defmethod default-column-formats (#-genera (array simple-array)
				   #+genera (array 'simple-array))
  "Return a list of formatting strings for ARRAY
The method returns a set of default formatting strings using heuristics."
  (let ((col-widths (aops:margin #'max-width   array 0))
	(col-types  (aops:margin #'column-type-format array 0))
	(col-digits (aops:margin #'max-decimal array 0)))
    (map 'list #'(lambda (type width digits)
		   (alexandria:switch (type :test #'string=)
		     ("F" (format nil "~~~A,~AF" width digits))
		     ("D" (format nil "~~~AD"    width))
		     ("A" (format nil "~~~AA"    width))))
	 col-types col-widths col-digits)))



;;;
;;; Pretty printers
;;;

(defun pprint-data-frame (data-frame
			  &optional
			    (stream *standard-output*)
			    (row-numbers-p *row-numbers-p*)
			    (max-digits *max-digits*))
  "Print DATA-FRAME to STREAM using the pretty printer"
  (check-type data-frame data)
  (let* ((col-names '())
	 (df (copy data-frame :key #'copy-array))
	 (*print-pretty* t))
    (when row-numbers-p
      (setf df (reverse-df
		(add-columns (reverse-df df)
			     '||
			     (aops:linspace 0 (1- (aops:nrow df)) (aops:nrow df))))))
    (flet ((format-column (c)
	     (let* ((width (max (max-width (column df c))
				(length (symbol-name c))))
		    (type (column-type-format (column df c)))
		    (digits (min (max-decimal (column df c))
				 max-digits))
		    (data-fmt (alexandria:switch (type :test #'string=)
				("F" (format nil "~~~A,~AF" width digits))
				("D" (format nil "~~~AD"    width))
				("A" (format nil "~~~AA"    width))
				("S" (format nil "~~~A@A"   width))))
		    (var-fmt (alexandria:switch (type :test #'string=) ; Why does SBCL complain about this, and only here?
			       ("F" (format nil "~~~A@A" width))
			       ("D" (format nil "~~~A@A" width))
			       ("A" (format nil "~~~AA"  width))
			       ("S" (format nil "~~~A@A"   width)))))

	       (replace-column! df c #'(lambda (cell)
					 (if (eq cell :na) ;should take same justification as column
					     (format nil (format nil "~~~A<~~A~~>" width) cell)
					     (format nil data-fmt cell))))
	       (setf col-names (cons (format nil var-fmt (symbol-name c)) col-names)))))
      (map nil #'format-column (keys df)))

    (write-char #\Newline stream)
    (pprint-logical-block (stream (reverse col-names) :per-line-prefix ";;")
      (loop (pprint-exit-if-list-exhausted)
	    (write-char #\Space stream)
	    (write-string (pprint-pop) stream)))
    (write-char #\Newline stream)
    (pprint-logical-block (stream (2d-array-to-list (aops:as-array df)))
      (loop (pprint-exit-if-list-exhausted)
	    (let ((row (pprint-pop)))
	      (pprint-logical-block (stream row :per-line-prefix ";; ")
		(loop (pprint-exit-if-list-exhausted)
		      (write-string (pprint-pop) stream)
		      (write-char #\Space stream))))
	    (pprint-newline :mandatory stream)))))

;;; TODO: refactor this using the pattern in pprint-data-frame,
;;; incorporating the code in default-column-formats and adding an
;;; optional max-digits parameter
(defun pprint-array (arr &optional (stream *standard-output*) (row-numbers-p *row-numbers-p*))
  "Print an array to STREAM, defaulting to *standard-output*, in a tabular format.  If ROW-NUMBERS-P, print row numbers."
  (let* ((array (cond (row-numbers-p (aops:stack-cols (aops:linspace 0 (1- (aops:nrow arr)) (aops:nrow arr)) arr))
		      (t arr)))
	 (df-lists (2d-array-to-list array))
	 (data-fmt (default-column-formats array))
	 (f 0))
      (pprint-logical-block (stream df-lists)
	(loop (pprint-exit-if-list-exhausted)
	      (let ((row (pprint-pop)))
		(pprint-logical-block (stream row :per-line-prefix ";;")
		  (loop (pprint-exit-if-list-exhausted)
			(write-char #\Space stream)
			(format stream (nth f data-fmt) (pprint-pop))
			(incf f))))
	      (setf f 0)
	      (write-char #\Newline stream)))))


;;; Not pretty printing per-se, but related

(defmethod head ((df data-frame) &optional (n 6))
  "Return the first N rows of DF; N defaults to 6"
  (let ((*print-pretty* t))
    (if (< (aops:nrow df) 6) (setf n (aops:nrow df)))
    (pprint-data-frame (select df (select:range 0 n) t))))

(defmethod tail ((df data-frame) &optional (n 6))
  "Return the last N rows of DF; N defaults to 6"
  (let ((*print-pretty* t))
    (if (< (aops:nrow df) 6) (setf n (aops:nrow df)))
    (pprint-data-frame (select df (select:range (- n) nil) t))))

;;;
;;; Pretty printer system configuration
;;;

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
    (df:pprint-data-frame df stream nil)))

