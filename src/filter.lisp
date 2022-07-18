;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: DATA-FRAME -*-
;;; Copyright (c) 2022 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:data-frame)

;;; Apply predicates to the rows of a data frame

(defun key-list (data form)
  "Return a list of keys used in REST, a form"
  (loop for key in (coerce (keys data) 'list)
	for variables = (flatten form)
	when (member key variables)
	  collect key into columns
	finally (return columns)))

(defun filter-rows (data body)
  "Filter DATA by a predicate given in BODY

Example
   (data :mtcars) ; load a data set
   (head mtcars)  ; view first 6 rows

;;   MODEL              MPG CYL DISP  HP DRAT    WT  QSEC VS AM GEAR CARB
;; 0 Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
;; 1 Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
;; 2 Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
;; 3 Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
;; 4 Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
;; 5 Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1

   (filter-rows mtcars '(< mpg 17))
   #<DATA-FRAME (11 observations of 12 variables)>

   (head *) ; view first 6 rows of filtered data frame

;;   MODEL                MPG CYL  DISP  HP DRAT    WT  QSEC VS AM GEAR CARB
;; 0 Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4
;; 1 Merc 450SE          16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3
;; 2 Merc 450SLC         15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3
;; 3 Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4
;; 4 Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4
;; 5 Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4"
   (let* ((variables (key-list data body))
	  (predicate (eval `(lambda ,variables ,body))))
     (select data
	     (mask-rows data variables predicate)
	     t)))
