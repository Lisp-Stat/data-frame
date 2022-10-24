;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: DATA-FRAME -*-
;;; Copyright (c) 2021 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:data-frame)

;;; Formatted output of data frame structures without using the pretty printing subsystem

;;; These functions do not use the pretty printer facility, and can be
;;; used anywhere, i.e. Genera and other CL implementations. They are
;;; all prefaced by DF- to distinguish them from presentation functions.

;;; This example looks like it would map onto data frames easily:
;;; https://stackoverflow.com/questions/26894147/pretty-print-values-in-fixed-width-fields

;;; Table printing
;;; Taken from: https://github.com/vindarel/print-licenses/blob/master/print-licenses.lisp
(defun aesthetic-string (thing)
  "Return the string used to represent `thing` when printing aesthetically."
  (format nil "~A" thing))

(defun weave (&rest lists)
  "Return a list whose elements alternate between each of the lists
`lists`. Weaving stops when any of the lists has been exhausted."
  (apply #'mapcan #'list lists))

;;; TODO: Make print-table take a vector-of-vectors instead of a list-of-lists
;;; This way we could simply pass in (rows df) for processing
(defun print-table (rows &optional (stream *standard-output*))
  "Print ROWS as a nicely-formatted table.
  Each row should have the same number of colums.
  Columns will be justified properly to fit the longest item in each one.
  Example:
    (print-table '((1 :red something)
                   (2 :green more)))
    =>
    1 | RED   | SOMETHING
    2 | GREEN | MORE
  "
  (when rows
    (let ((column-sizes (reduce (curry #'mapcar #'max)
				(map 'list
				     (curry #'mapcar (compose #'length #'aesthetic-string))
				     rows))))
      (loop for row in rows do
	(format stream "~{~vA~^ | ~}~%" (weave column-sizes row)))))
  (values))

;;; Data structure printing

(defun df-print (df)
  "Print DF to *standard-output* in table format"
  (when df
    (let ((rows (loop for row across (rows df)
		      collect (coerce row 'list))))
      (print-table (push (column-names df) rows)))))


;;; TODO Summarise a data frame
;;; See https://github.com/Lisp-Stat/data-frame/issues/4



;;;
;;; Markdown
;;;

(defun print-markdown (df &key (stream *standard-output*) (row-numbers nil))
  "Print data frame DF, in markdown format, to STREAM
If ROW-NUMBERS is true, also print row numbers as the first column"
  (let* ((array      (aops:as-array df))
	 (col-types  (aops:margin #'column-type-format array 0))
	 (*print-pprint-dispatch* (copy-pprint-dispatch))
	 (*print-pretty* t))

    ;; For notebook printing, we only need four digits of accuracy
    (set-pprint-dispatch 'float  (lambda (s f) (format s "~4f" f)))

    ;; Print column names
    (if row-numbers (format stream "| "))
    (map nil #'(lambda (x)
		 (format stream "| ~A " x))
	 (keys df))
    (write-char #\| stream)
    (write-char #\Newline stream)

    ;; Print alignment
    (if row-numbers (format stream "| ---: "))
    (map nil #'(lambda (x)
		 (alexandria:switch (x :test #'string=)
		   ("F" (format stream "| ---: "))
		   ("D" (format stream "| ---: "))
		   ("A" (format stream "| :--- "))))
	 col-types)
    (write-char #\| stream)
    (write-char #\Newline stream)

    ;; Print data
    (aops:each-index i
      (if row-numbers (format stream "| ~A " i))
      (aops:each-index j
	(format stream "| ~A " (aref array i j)))
      (format stream " |~%"))
    (values)))

