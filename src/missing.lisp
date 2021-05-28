;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: DATA-FRAME -*-
;;; Copyright (c) 2021 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:data-frame)

;;; Handle missing data

;;; We're using :na as a sentinel value for missing data because we
;;; want to be able to have boolean data columns.

;;; TODO: consider a solution that includes true, false, NaN, etc.  JSON
;;; libraries have varied ways of importing this data.

(defgeneric missingp (data)
  (:method (data)
    nil)
  ;; (:method ((data null))		;nil sentinel for missing value
  ;;   t)
  (:method ((data (eql :na)))
    t)
  (:method ((data string))
    nil)
  (:method ((data sequence))
    (map 'vector #'missingp data))
  (:method ((data array))
    (nu:map-array data #'missingp))
  (:method ((data data-frame))
    (map-columns data #'missingp)))

(defmethod drop-missing ((df data-frame) &optional (predicate #'missingp))
  "Remove all rows from DF that are missing values according to PREDICATE"
  (select df
	  (bit-not (reduce #'bit-ior (map 'vector
					  #'(lambda (x)
					      (mask-rows df x predicate))
					  (keys df))))
	  t))

(defun drop-na (df)
  "Remove all rows from DF that are missing values. Convenience R-like function."
  (drop-missing df))

(defmethod replace-missing ((df data-frame) map-alist)
  "Replace missing values with the values specified
The alist consists of a column name in the CAR and the replacement value in the CDR
Example: (replace-missing mtcarsm '((mtcarsm:mpg . foo)))"
  (loop for (column . value) in map-alist
	do (setf df (replace-column df column (substitute value :na (column df column))))
	finally (return df)))
