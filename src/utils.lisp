;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: DATA-FRAME -*-
;;; Copyright (c) 2021-2022 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:data-frame)

;;;
;;; Insert & remove items from vectors and arrays -- TODO move to array-operations
;;;

(defun delete-nth (sequence n)
  "Return SEQUENCE with the Nth item removed.
Note: DELETE-IF makes no guarantee of being destructive, so you cannot rely on this side-effect.  You must SETF the original sequence to the values returned from this function, or use the modify-macro DELETE-NTH*"
  (check-type sequence sequence)
  (delete-if (constantly t) sequence :start n :count 1))

(define-modify-macro delete-nth* (n)
  delete-nth
  "Destructively modifies N, a SEQUENCE by removing the Nth item.
Example:
    LS-USER> (defparameter *v* #(a b c d))
    *V*
    LS-USER> (delete-nth* *v* 1)
    #(A C D)
    LS-USER> *v*
    #(A C D)")



;;;
;;; Augment the type system
;;;

;;; cl:type-of is under-constrained and returns implementation
;;; specific results, so we use our own version
;;; See: https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node53.html
(defun get-type (x)
  "Return the most specific type symbol for x"
  (typecase x
    (bit     'bit)
    (single-float 'single-float)
    (double-float 'double-float)
    ;; (fixnum  'fixnum)
    (integer 'integer)
    (ratio   'ratio)
    (complex 'complex)
    ;; (rational 'cl:rational)		;SBCL doesn't recognise this return type
    ;; (real    'real)			;SBCL doesn't recognise this return type

    (simple-string 'simple-string)
    (string 'string)

    (list          'list)
    (symbol        'symbol)
    (bit-vector    'bit-vector)
    (simple-vector 'simple-vector)
    (simple-array  'simple-array)
    (vector        'vector)
    (array         'array)
    (sequence      'sequence)

    (function 'function)
    (package  'package)))

(defun types-in-column (seq)
  "Return a list of the types found in SEQ"
  (let (types)
    (loop for i across seq
	  for type = (get-type i)
	  when (not (member type types))
	       do (push type types)
	  finally (return types))))

(defun column-type (col)
  "Return the most specific type found in COL"
  (let ((type-list (types-in-column col)))
    (cond ((member 'single-float type-list) :single-float)
	  ((member 'double-float type-list) :double-float)
	  ((member 'fixnum  type-list) :fixnum)
	  ((member 'integer type-list) :integer)
	  ((member 'string  type-list) :string)
	  ((member 'bit     type-list) :bit)
	  ((member 'symbol  type-list) :symbol)
	  (t 'string ))))

