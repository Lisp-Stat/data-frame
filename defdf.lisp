;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: DATA-FRAME -*-
;;; Copyright (c) 2021 by Symbolics Pte. Ltd. All rights reserved.
(cl:in-package #:data-frame)

;;; These definitions are in a separate file because additional
;;; functionality is expected to be added in future.

;; Here be dragons. Thou art forewarned.

(defmacro define-data-frame (df body &optional doc)
  (when (and doc (not (stringp doc))) (error "Data frame documentation is not a string"))
  `(let* ((df-str (string ',df))
	  (*package* (if (find-package df-str)    ;exists?
			 (find-package df-str)    ;yes, return it
			 (make-package df-str)))) ;no, make it
     (defparameter ,df ,body ,doc)
     (eval '(define-column-names ,df))
     (eval ',df))) ; this is to return something to the user

(defun define-column-names (df)
  "Create and export a symbol macro for each column name in DF
After running this function, you can refer to a column by its name."
  (maphash #'(lambda (key index)
	       (eval `(cl:define-symbol-macro ,key (cl:aref (columns ,df) ,index))))
	   (ordered-keys-table (slot-value df 'ordered-keys))))

(defun make-data-package (pkg-name)
  "Create a package and import and change *PACKAGE*"
  (let ((package (string-upcase pkg-name)))
  (make-package package :use '("COMMON-LISP" "LISP-STAT"))
  (eval `(in-package ,package))))
