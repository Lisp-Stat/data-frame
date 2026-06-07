;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: DATA-FRAME -*-
;;; Copyright (c) 2021-2022, 2026 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:data-frame)

;;;
;;; Augment the type system
;;;

;;; cl:type-of is under-constrained and returns implementation
;;; specific results, so we use our own version
;;; See: https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node53.html


(defun timestamp-string-p (string)
  "Return T if STRING can be parsed as a timestamp by LOCAL-TIME."
  (and (stringp string)
       (when (local-time:parse-timestring string :fail-on-error nil)
		 t)))

(deftype timestamp-string ()
  '(satisfies timestamp-string-p))

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

	(timestamp-string 'temporal)
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
	  ((member 'temporal  type-list) :temporal)
	  ((member 'bit     type-list) :bit)
	  ((member 'symbol  type-list) :symbol)
	  (t 'string ))))

