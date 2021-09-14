;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: DATA-FRAME -*-
;;; Copyright (c) 2021 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:data-frame)

;;; Formatted output of data frame structures without using the pretty printing subsystem

;;; These functions do not use the pretty printer facility, and can be
;;; used anywhere, i.e. Genera and other CL implementations. They are
;;; all prefaced by DF- to distinguish them from presentation functions.

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
  "Print `rows` as a nicely-formatted table.
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

