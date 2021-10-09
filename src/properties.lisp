;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: DATA-FRAME -*-
;;; Copyright (c) 2021 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:data-frame)

;;; TODO Check if the type property is already set and, if different
;;; from the calculated type, offer the user a choice of whether to
;;; proceed.
(defun heuristicate-types (df)
  "Coerce each element of the column vectors to the most specific type in the column
Often when reading in a data set, the types will be inconsistent in a variable. For example one observation might be 5.1, and another 5. Whilst mathmatically equivalent, we want our variable vectors to have identical types. The COLUMN-TYPE function returns the most specific numeric type in the column, then coerce all the vector elements to this type"
  (map nil #'(lambda (key)
	       (let* ((data     (column df key))
		      (col-type (column-type data)))

		 ;; Assign type property to symbol-macro.
		 (eval `(alexandria+:defprop ,(find-symbol (symbol-name key) (find-package (name df))  )
			    ,col-type :type))))
       (keys df)))

;; This is here for the case of using '$' for a separator. Easier to
;; switch to package format than rewriting summary functions
(defun sym-mac (df var)
  "Return the symbol macro for VAR in the DATA-FRAME DF"
  (find-symbol (symbol-name var) (find-package (name df))))

#+nil
(defun var-name (var)
  "Return the name of the variable without the symbol-macro prefix
Example: (var-name mtcars$mpg) returns 'mpg'"
  (subseq var (1+ (search "$" var))))

(defun set-properties (df property prop-values)
  "Set the PROPERTY of each variable in DF to a value. The value is specified in the plist PROP-VALUES.
Example:
  To give the variables in the mtcars dataset a unit, use:
  (set-properties mtcars :unit '(:mpg m/g
			         :cyl :NA
			         :disp in³
			         :hp hp
			         :drat :NA
			         :wt lb
			         :qsec s
			         :vs :NA
			         :am :NA
			         :gear :NA
			         :carb :NA))"
  (loop for (key value) on prop-values by #'cddr
	for sym = (find-symbol (symbol-name key) (find-package (name df)))
	when sym do (eval `(alexandria+:defprop ,sym ,value ,property))))

(defun show-properties (df)
  "Show the standard properties of the variables of the data frame DF
Standard properties are 'label', 'type' and 'unit'"
  (let* ((rows (loop for key across (keys df)
		    collect (list (symbol-name key)
				  (get (sym-mac df key) :type)
				  (get (sym-mac df key) :unit)
				  (get (sym-mac df key) :label)))))
    (push '("--------" "----" "----" "-----------") rows)
    (push '("Variable" "Type" "Unit" "Label") rows)
    (print-table rows)))

