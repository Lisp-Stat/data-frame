;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: DATA-FRAME -*-
;;; Copyright (c) 2020-2021 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:data-frame)

(defparameter *column-summary-minimum-length* 10
  "Columns are only summarised when longer than this, otherwise they are returned as is.")

(defparameter *column-summary-quantiles-threshold* 10
  "If the number of reals exceeds this threshold, they will be summarized with quantiles.")

(defgeneric column-length (column)
  (:documentation "Return the length of column.")
  (:method ((column vector))
    (length column)))

(defstruct vector-summary%
  "Base class for summarizing vectors.  Not exported."
  (length 0 :type array-index :read-only t))

(defun print-count-and-percentage (stream count length)
  "Print COUNT as is and also as a rounded percentage of "
  (format stream "~D (~D%)" count (round (/ count length) 1/100)))

(defstruct (bit-vector-summary (:include vector-summary%))
  "Summary of a bit vector."
  (count 0 :type array-index :read-only t))

(defmethod print-object ((summary bit-vector-summary) stream)
  (let+ (((&structure-r/o bit-vector-summary- length count) summary))
    (princ "bits, ones: " stream)
    (print-count-and-percentage stream count length)))

(defstruct quantiles-summary
  "Summary of a real elements (using quantiles)."
  (count 0 :type array-index :read-only t)
  (min 0 :type real :read-only t)
  (q25 0 :type real :read-only t)
  (q50 0 :type real :read-only t)
  (q75 0 :type real :read-only t)
  (max 0 :type real :read-only t))

(defstruct (generic-vector-summary (:include vector-summary%))
  "Summary for generic vectors."
  (quantiles nil :type (or null quantiles-summary) :read-only t)
  (element-count-alist nil :type list :read-only t))

(defun ensure-not-ratio (real)
  "When REAL is a RATIO, convert it to a float, otherwise return as is.  Used for printing."
  (if (typep real 'ratio)
      (float real 1.0)
      real))

(defgeneric column-summary (column)
  (:documentation "Return an object that summarizes COLUMN of a DATA-FRAME.  Primarily intended for printing, not analysis, returned values should print nicely.")
  (:method ((column bit-vector))
    (make-bit-vector-summary :length (length column) :count (count 1 column)))
  (:method ((column vector))
    (let+ ((length (length column))
           (table (aprog1 (nu:make-sparse-counter :test #'equal)
                    (map nil (curry #'nu:add it) column)))
           (alist (as-alist table))
           ((&flet real? (item) (realp (car item))))
           (reals-alist (remove-if (complement #'real?) alist))
           (quantiles (when (< *column-summary-quantiles-threshold*
                               (length reals-alist))
                        (let+ ((#(min q25 q50 q75 max)
                                 (nu:weighted-quantiles
                                  (mapcar #'car reals-alist)
                                  (mapcar #'cdr reals-alist)
                                  #(0 1/4 1/2 3/4 1))))
                          (make-quantiles-summary
                           :count (reduce #'+ reals-alist :key #'cdr)
                           :min min :q25 q25 :q50 q50 :q75 q75 :max max))))
           (alist (stable-sort (if quantiles
                                   (remove-if #'real? alist)
                                   (copy-list alist))
                               #'>= :key #'cdr)))
      (make-generic-vector-summary :length length
                                   :quantiles quantiles
                                   :element-count-alist alist))))

(defmethod print-object ((summary generic-vector-summary) stream)
  (let+ (((&structure-r/o generic-vector-summary- length quantiles element-count-alist) summary))
    (when quantiles
      (let+ (((&structure-r/o quantiles-summary- count min q25 q50 q75 max)
              quantiles))
        (format stream
                "~W reals, ~:_min=~W, ~:_q25=~W, ~:_q50=~W, ~:_q75=~W, ~:_max=~W"
                count min (ensure-not-ratio q25) (ensure-not-ratio q50)
                (ensure-not-ratio q75) max)))
    ;; (when (and quantiles element-count-alist)
    (when element-count-alist
      (loop for (element . count) in element-count-alist do
        (print-count-and-percentage stream count length)
        (format stream " x ~W, " element)))))

;;; Deprecated, based on Tama's version
;;; TODO: write a better summary system.
;;; See https://github.com/Lisp-Stat/data-frame/issues/4
(defmethod summary ((df data-frame) &optional (stream *standard-output*))
  "Deprecated. Print simple stastical summary of data frame"
  (let* ((summarize? (<= *column-summary-minimum-length* (aops:nrow df)))
	 (alist (as-alist df)))
    (fresh-line stream)
    (print-unreadable-object (df stream :type t)
      (format stream "(~d x ~d)" (length alist) (aops:nrow df))
      (fresh-line stream)
      (loop for (key . column) in alist do
	(unless (string= (symbol-name key) "ROW-NAME")
          (format stream "~A: ~A~%" (symbol-name key) (if summarize?
							  (column-summary column)
							  column)))))))
