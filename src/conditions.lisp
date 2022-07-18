;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: DATA-FRAME -*-
;;; Copyright (c) 2021-2022 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:data-frame)

(define-condition duplicate-key (error)
  ((key :initarg :key))
  (:documentation "Duplicate key.")
  (:report (lambda (condition stream)
             (format stream "Duplicate key ~A." (slot-value condition 'key))))
  (:documentation "An operation attempted to use a key that already exists in ORDERED-KEYS"))

(define-condition key-not-found (error)
  ((key :initarg :key)
   (keys :initarg :keys))
  (:documentation "Key not found.")
  (:report (lambda (condition stream)
             (format stream "Key ~A not found, valid keys are ~A."
                     (slot-value condition 'key)
                     (slot-value condition 'keys))))
  (:documentation "An operation was attempted on a non-existant key."))

(define-condition missing-data (error)
  ((name :initarg :name)
   (data :initarg :data))
  (:documentation "A variable has missing data, e.g. :na, nil")
  (:report (lambda (condition stream)
             (format stream "~A contains missing data"
                     (slot-value condition 'name)))))

(define-condition large-data (warning)
  ((data-size :initarg :data-size
	      :reader   data-size))
  (:report (lambda (condition stream)
	     (format stream
		     "You are attempting to embed a large number of data points (~D); the recommended maximum is ~D."
		     (data-size condition) *large-data*)))
  (:documentation "A operation was requested on a data set large enough to potentially cause problems."))

(define-condition data-frame-exists (error)
  ((data-frame :initarg :data-frame
	       :reader   data-frame))
  (:report (lambda (condition stream)
	     (format stream
		     "You are attempting to redefine ~A and *ask-on-redefine is ~A"
		     (slot-value condition 'name)
		     *ask-on-redefine*)))
  (:documentation "An attempt to redefine an existing data frame.  Triggered if either the symbol is bound or the package exists."))

;;; Validation functions for duologue:prompt, which is used to get user input within restarts
(defun df-exists-p (s)
  (if (or (find-package s)
	  (member (find-symbol (string-upcase s)) df::*data-frames*))
      nil
      t))

(defun invalid-df-name (s)
  "A user prompt, using DUOLOGUE, to select a valid data frame name."
  (duologue:say "~A names an existing data frame.  Please choose another name" s))



