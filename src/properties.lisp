;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: DATA-FRAME -*-
;;; Copyright (c) 2021-2022 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:data-frame)

(defun heuristicate-types (df)
  "Coerce each element of the column vectors to the most specific type in the column
Often when reading in a data set, the types will be inconsistent in a variable.  For example one observation might be 5.1, and another 5.  Whilst mathmatically equivalent, we want our variable vectors to have identical types.  The COLUMN-TYPE function returns the most specific numeric type in the column, then coerces all the vector elements to this type"
  (check-type df data-frame)
  (assert (slot-boundp df 'name) () "name is not bound in the data-frame")
  (let ((name (name df)))
    (map nil #'(lambda (key)
		 (let* ((data     (column df key))
			(col-type (column-type data))
			(sym (find-symbol (symbol-name key) (find-package name))))
		   (setf (get sym :type) col-type)))
	 (keys df))))

(defun set-properties (df property prop-values)
  "Set the PROPERTY of each variable in DF to a value.  The value is specified in the plist PROP-VALUES.
Example:
  To give the variables in the mtcars dataset a unit, use:
  (set-properties mtcars :unit '(:mpg  m/g
			         :cyl  :NA
			         :disp in³
			         :hp   hp
			         :drat :NA
			         :wt   lb
			         :qsec s
			         :vs   :NA
			         :am   :NA
			         :gear :NA
			         :carb :NA))"
  (check-type df data-frame)
  (assert (slot-boundp df 'name) () "name is not bound in the data-frame")
  (let ((name (name df)))
    (loop for (key value) on prop-values by #'cddr
	  for sym = (find-symbol (symbol-name key) (find-package name))
	  do (assert (member property '(:type :label :unit))
		     (property)
		     "A property must be one of: :type, :label or :unit")
	  do (if (eq property :type)
		 (check-type value df:data-type "a valid data variable type"))
	  when sym do (setf (get sym property) value))))


;;; User convenience functions

(defun get-property (variable property)
  "Return the PROPERTY of data VARIABLE"
  (assert (member property '(:type :label :unit))
	  (property)
	  "Property ~A is not one of: :type, :label or :unit"
	  property)
  (get variable property))

(defun set-property (symbol value property)
  "Set the PROPERTY of SYMBOL to VALUE"
  (assert (member property '(:type :label :unit))
	  (property)
	  "Property ~A is not one of: :type, :label or :unit"
	  property)
  (if (eq property :type)
      (check-type value df:data-type "a valid data variable type"))
  (setf (get symbol property) value))


;; Not exported
(defun show-properties (df)
  "Show the standard properties of the variables of the data frame DF
Standard properties are 'label', 'type' and 'unit'"
  (let* ((rows (loop for key across (keys df)
		    collect (list (symbol-name key)
				  (get key :type)
				  (get key :unit)
				  (get key :label)))))
    (push '("--------" "----" "----" "-----------") rows)
    (push '("Variable" "Type" "Unit" "Label") rows)
    (print-table rows)))


