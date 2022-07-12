;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: DATA-FRAME -*-
;;; Copyright (c) 2020-2022 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:data-frame)

;;; Summary control variables
(defparameter *summary-minimum-length* 10
  "Columns are only summarised when longer than this, otherwise they are returned as is.")

(defparameter *quantile-threshold* 20
  "If the number of unique reals exceeds this threshold, they will be summarized with quantiles, otherwise print frequency table")

(defparameter *distinct-threshold* 10
  "If an integer variable has <= discrete values, consider it a factor")

(defparameter *distinct-maximum* 20
  "If a string/factor variable has > *distinct-maximum* values, exclude it")



(defstruct variable-summary%
  "Base class for summarizing variables.  Summary functions take SYMBOLs, rather than values, because the symbol property lists naming the variables have meta-data, e.g. type, label, that we want to print.  Not exported."
  (length  0 :type array-index :read-only t)
  (missing 0 :type fixnum      :read-only t)
  (name   "" :type string      :read-only t)
  (desc   "" :type string      :read-only t))

(defstruct (bit-variable-summary (:include variable-summary%)
			       (:print-function
				(lambda (summary stream depth)
				  (declare (ignore depth))
				  (let+ (((&structure-r/o bit-variable-summary- length count name desc) summary))
				    (format stream "~%~%~A (~A)~%" name desc)
				    (princ "ones: " stream)
				    (print-count-and-percentage stream count length)))))
   "Summary of a bit vector."
  (count 0 :type array-index :read-only t))

(defstruct (real-variable-summary (:include variable-summary%)
			      (:print-function
			       (lambda (summary stream depth)
				 (declare (ignore depth))
				 (let+ (((&structure-r/o real-variable-summary- name desc length missing min q25 q50 mean q75 max) summary)
					(*print-pprint-dispatch* (copy-pprint-dispatch)))
				   (set-pprint-dispatch 'float  (lambda (s f) (format s "~,2f" f)))
				   (format stream
					   "~%~%~A (~A)~& n: ~W~& ~:_missing: ~W~& ~:_min=~:W~& ~:_q25=~:W~& ~:_q50=~:W~& ~:_mean=~:W~& ~:_q75=~:W~& ~:_max=~:W"
					   name desc length missing min (ensure-not-ratio q25) (ensure-not-ratio q50) (ensure-not-ratio mean)
					   (ensure-not-ratio q75) max)))))
  "Summary of a real elements (using quantiles)."
  ;; (count 0 :type array-index :read-only t)
  (min 0 :type real :read-only t)
  (q25 0 :type real :read-only t)
  (q50 0 :type real :read-only t)
  (mean 0 :type real :read-only t)
  (q75 0 :type real :read-only t)
  (max 0 :type real :read-only t))

(defstruct (factor-variable-summary (:include variable-summary%)
			    (:print-function
			     (lambda (summary stream depth)
			       (declare (ignore depth))
			       (let+ (((&structure-r/o factor-variable-summary- name desc length element-count-alist) summary))
				 (format stream "~%~%~A (~A)~%" name desc)
				 (loop for (element . count) in element-count-alist do
				   (print-count-and-percentage stream count length)
				   (format stream " x ~W, " element))))))
  "Summary for factor variables"
  (element-count-alist nil :type list :read-only t))

(defstruct (generic-variable-summary (:include variable-summary%)
				     (:print-function
				      (lambda (summary stream depth)
					(declare (ignore depth))
					(let+ (((&structure-r/o generic-variable-summary- length name quantiles element-count-alist) summary))
					  (unless (string= name "")
					    (format stream "~%~%~:W: " name))
					  (when quantiles
					    (let+ (((&structure-r/o real-variable-summary- length min q25 q50 q75 max) quantiles))
					      (format stream
						      "~:W reals, ~:_min=~:W, ~:_q25=~:W, ~:_q50=~:W, ~:_q75=~:W, ~:_max=~:W"
						      length min (ensure-not-ratio q25) (ensure-not-ratio q50)
						      (ensure-not-ratio q75) max)))
					  (when element-count-alist
					    (loop for (element . count) in element-count-alist do
					      (print-count-and-percentage stream count length)
					      (format stream " x ~:W, " element)))))))
  "Summary for generic variables, i.e. those with mixed types."
  (quantiles nil :type (or null real-variable-summary) :read-only t)
  (element-count-alist nil :type list :read-only t))





(defgeneric column-length (column)
  (:documentation "Return the length of column.")
  (:method ((column vector))
    (length column)))

(defun print-count-and-percentage (stream count length)
  "Print COUNT as is and also as a rounded percentage"
  (format stream "~D (~D%)" count (round (/ count length) 1/100)))

(defun ensure-not-ratio (real)
  "When REAL is a RATIO, convert it to a float, otherwise return as is.  Used for printing."
  (if (typep real 'ratio)
      (float real 1.0)
      real))

;;; TODO figure out where distinct and monotonic should reside. Probably num-utils.
(defun distinct (column)
  "Returns the number of distinct elements in COLUMN, a symbol naming a variable.
Useful for formatting columns for human output"
  (let+ ((data (eval column))
	 (table (aprog1 (nu:make-sparse-counter :test #'equal)
                  (map nil (curry #'nu:add it) data)))
         (alist (as-alist table)))
    (length alist)))

(defun monotonicp (column)
  "Returns t if all elements of COLUMN, a SYMBOL, are increasing monotonically
Useful for detecting row numbers in imported data"
  (let ((data (eval column)))
    (if (not (every #'numberp data))
	nil
    (loop for x across data
	  for i = (1+ x) then (1+ i)
	  always (= (1+ x) i)))))



(defun summarize-real-variable (column)
  "Return a summary for a float variable"
  (let+ ((data (eval column))
	 (table (aprog1 (nu:make-sparse-counter :test #'equal)
                  (map nil (curry #'nu:add it) data)))
         (alist (as-alist table))
         ((&flet real? (item) (realp (car item))))
         (reals-alist (remove-if (complement #'real?) alist))
	 (#(min q25 q50 q75 max)
	   (nu:weighted-quantiles
            (mapcar #'car reals-alist)
            (mapcar #'cdr reals-alist)
            #(0 1/4 1/2 3/4 1)))
	 (mean-ignore-missing (ignore-missing #'mean :warn-user t)))

    (make-real-variable-summary
     :name (symbol-name column)
     :desc (if (get column :label)
	       (get column :label)
	       "")
     :length (length data)
     :missing (length (which data :predicate #'missingp))
     :min min :q25 q25 :q50 q50 :mean (funcall mean-ignore-missing data) :q75 q75 :max max)))

(defun summarize-factor-variable (column)
  "Return an alist of factor/count pairs"
  (let+ ((data (eval column))
	 (table (aprog1 (nu:make-sparse-counter :test #'equal)
                  (map nil (curry #'nu:add it) data)))
         (alist (as-alist table)))

    (make-factor-variable-summary
     ;; :name (df::var-name (symbol-name column)) ; for df$variable style
     :name (symbol-name column)
     :desc (if (get column :label) (get column :label) "")
     :length (length data)
     :missing (length (which data :predicate #'missingp))
     :element-count-alist (stable-sort alist #'>= :key #'cdr))))

(defun summarize-generic-variable (column &optional name)
  "Return an object that summarizes COLUMN of a DATA-FRAME.  Primarily intended for printing, not analysis, returned values should print nicely.  This function can be used on any type of column, even one with mixed types"
  (let+ ((data (eval column))
	 ;; (data column)
	 (length (length data))
         (table (aprog1 (nu:make-sparse-counter :test #'equal)
                  (map nil (curry #'nu:add it) data)))
         (alist (as-alist table))
         ((&flet real? (item) (realp (car item))))
         (reals-alist (remove-if (complement #'real?) alist))
         (quantiles (when (< *quantile-threshold*
                             (length reals-alist))
                      (let+ ((#(min q25 q50 q75 max)
                               (nu:weighted-quantiles
                                (mapcar #'car reals-alist)
                                (mapcar #'cdr reals-alist)
                                #(0 1/4 1/2 3/4 1))))
                        (make-real-variable-summary
                         :length (reduce #'+ reals-alist :key #'cdr)
                         :min min :q25 q25 :q50 q50 :q75 q75 :max max))))
         (alist (stable-sort (if quantiles
                                 (remove-if #'real? alist)
                                 (copy-list alist))
                             #'>= :key #'cdr)))
    (make-generic-variable-summary :length length
				   :name (if name
					     name
					     "")
                                   :quantiles quantiles
                                   :element-count-alist alist)))



(defun summarize-column (column &optional name)
  "Return a summary struct for COLUMN"
  (let ((data (eval column))
	(label (get column :label)))
    (case (get column :type)

      ;; Implementation types
      (:double-float (summarize-real-variable column))
      (:single-float (summarize-real-variable column))
      (:integer      (summarize-real-variable column))
      (:string       (summarize-factor-variable column)) ;we really should remove this at some point.
      (:catagorical  (summarize-factor-variable column))
      (:bit (make-bit-variable-summary
	     :name (if name
		       name
		       (symbol-name column))
	     :desc (if label
		       label
		       "")
	     :length (length data)
	     :count (count 1 data)))

      ;; Statistical types, note keyword in case key
      (:factor (summarize-factor-variable column))
      (t (summarize-generic-variable column)))))

(defun get-summaries (df)
  "Return a list of summaries of the variables in DF"
  (loop for key across (keys df)
	collect (summarize-column key)))

#+nil
(defmacro summary (df &optional (stream *standard-output*))
  `(summarize-dataframe ,df ,stream))

;; TODO add :remove-missing parameter so we can summarize in the early stages of data exploration.
;; Perhaps, if set, have it use summarize-generic-variable
(defun summary (df &optional (stream *standard-output*))
  "Print a summary of DF to STREAM, using heuristics for better formatting"
  (let* ((name (when (slot-boundp df 'name) (name df)))
	 (pkg (find-package name)))
    (if pkg
      (loop for key across (keys df)
	    for column = (find-symbol (string-upcase (symbol-name key)) pkg)
	    for data   = (column df key)
	    for length = (length data)

	    unless (or (= (length data)
			  (distinct data))	;exclude row names
		       (monotonicp data))	;exclude row numbers
	      collect (case (get column :type)	;special cases
			(:double-float (if (< (distinct data) *quantile-threshold*) ;summarise as a factor
					  (summarize-factor-variable column)
					  (summarize-real-variable column)))
			(:single-float (if (< (distinct data) *quantile-threshold*) ;summarise as a factor
					  (summarize-factor-variable column)
					  (summarize-real-variable column)))
			(:integer (if (< (distinct data) *distinct-threshold*) ;summarise as a factor
				     (summarize-factor-variable column)
				     (summarize-real-variable column)))
			(t (summarize-column column))))

      (loop for key across (keys df)	;no data-frame environment, use generic summary functions
	    for data = (column df key)
	    unless (or (= (length data)
			  (distinct data))	;exclude row names
		       (monotonicp data)	;exclude row numbers
		       (and (< *distinct-maximum* ;exclude row names with a few repeats
			       (distinct data))
			    (equal :string (column-type (column df key)))))
	      do (format stream "~%~A: ~A" key (summarize-generic-variable data))))))

