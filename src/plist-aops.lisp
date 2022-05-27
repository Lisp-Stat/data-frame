;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: DATA-FRAME -*-
;;; Copyright (c) 2020-2021 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:data-frame)

;;; Plists are convenient data structures for ad-hoc data
;;; analysis.  These array operations definitions for plists make them
;;; easier to work with as arrays.  A plist used as a data frame has
;;; the following format:

;;; '(:a #(1 2 3 4)
;;;   :b #(foo bar baz quux))

;;; Like a data frame, all columns must be of equal length, and the
;;; CAR of VALUE must point to a VECTOR.

(defmacro ensure-plist (pl)
  `(progn
     (assert (plistp ,pl :allow-symbol-keys t) () "Argument is not a PLIST")
     (assert (notany #'null (map 'list #'vectorp (plist-values ,pl))) () "All values of PLIST must be vectors")
     (assert (apply #'= (map 'list #'length (plist-values ,pl))) () "All values of PLIST must be of equal length")))

(defmethod aops:as-array ((plist cons))
  "Return the data values of PLIST as an array. The second VALUE is the keys.
This method assumes that the plist is of the form (:col-name #(... ))"
  (ensure-plist plist)
  (values
   (nu:transpose (aops:combine (coerce (plist-values plist) 'vector)))
   (plist-keys plist)))

(defmethod aops:nrow ((plist cons))
  (ensure-plist plist)
  (length (cadr plist)))

(defmethod aops:ncol ((plist cons))
  (ensure-plist plist)
  (length (plist-keys plist)))

(defmethod aops:dims ((plist cons))
  (ensure-plist plist)
  (list (aops:nrow plist) (aops:ncol plist)))
