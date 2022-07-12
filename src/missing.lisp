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
    (declare (ignore data))
    nil)
  ;; (:method ((data null))		;nil sentinel for missing value
  ;;   t)
  (:method ((data (eql :na)))
    t)
  (:method ((data (eql :missing)))
    t)
  (:method ((data string))
    nil)
  (:method ((data sequence))
    (map 'vector #'missingp data))
  (:method ((data array))
    (nu:map-array data #'missingp))
  (:method ((data data-frame))
    (map-columns data #'missingp))
  (:documentation "Return a vector indicating the position of any missing value indicators.  They currently are :na and :missing"))

(defmethod drop-missing ((df data-frame) &optional (predicate #'missingp))
  "Remove all rows from DF that are missing values according to PREDICATE"
  (select df
	  (bit-not (reduce #'bit-ior (map 'vector
					  #'(lambda (x)
					      (mask-rows df x predicate))
					  (keys df))))
	  t))

(defmethod drop-missing ((var vector) &optional (predicate #'missingp))
  "Remove all values from VAR that are missing according to PREDICATE.
Returns values:
   1. the vector with missing values removed
   2. the number of elements removed"
  (let ((len (length var))
	(without-missing (remove-if predicate var)))
    (values without-missing (- len (length without-missing)))))

(defun drop-na (df)
  "Remove all rows from DF that are missing values.  Convenience R-like function."
  (drop-missing df))

(defmethod replace-missing ((df data-frame) map-alist)
  "Replace missing values with the values specified
The alist consists of a column name in the CAR and the replacement value in the CDR
Example: (replace-missing mtcarsm '((mpg . foo)))"
  (loop for (column . value) in map-alist
	do (setf df (replace-column df column (substitute value :na (column df column))))
	   (setf df (replace-column df column (substitute value :missing (column df column))))
	finally (return df)))

(defun ignore-missing (function &key (warn-user nil) (provide-restart nil))
  "Wrap FUNCTION in a closure that removes missing values and applys FUNCTION in case any of the arguments are :MISSING, :NA or NIL to arguments.  Intended for functions accepting vectors."
  (lambda (&rest arguments)
    ;; (check-type arguments array "a vector")
    (let ((missing? (notevery #'null (apply #'missingp arguments))))
      (cond ((not missing?) (apply function arguments))
	    ((and missing?
		  provide-restart) (handler-bind ((missing-data
						    #'(lambda (c)
							(invoke-debugger c))))
				     (restart-case (signal 'missing-data :name "Input argument")
				       (remove-missing ()
					 :report (lambda (s)
						   (format s "Compute with missing data removed"))
					 (let+ (((&values clean-vec num) (apply #'drop-missing arguments)))
					   (when warn-user
					     (warn (format nil "Removed ~D (~D%) missing values"
							   num
							   (round (* 100 (float (/ (length (car arguments)))))))))
					   (funcall function clean-vec))))))
	    (missing? (let+ (((&values clean-vec num) (apply #'drop-missing arguments)))
			(when warn-user
			  (warn (format nil "Removed ~D (~D%) missing values"
					num
					(round (* 100 (float (/ (length (car arguments)))))))))
			(funcall function clean-vec)))))))

