;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: DATA-FRAME -*-
;;; Copyright (c) 2021-2022 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:data-frame)

;;; Functions for defining data frames in the statistical environment.

(defvar *data-frames* nil
  "Global list of all data frames")

(defvar *ask-on-redefine* t
  "If non-nil, the system will ask the user for confirmation before redefining a data frame")


(defmacro defdf (name data &optional (documentation nil documentation-p))
  "Define a data-frame and package by the same name.
Also defines symbol-macros for variable access, e.g. mtcars:mpg"
  `(progn
     (when (and ,documentation
		(not (stringp ,documentation)))
       (error "Data frame documentation is not a STRING"))
     (when (and (boundp ',name)
		(not (typep (symbol-value ',name) 'data-frame)))
       (error "Data is not of type DATA-FRAME"))

     (declaim (special ,name))
     (unless (and *ask-on-redefine*
		  (boundp ',name)
		  (not (y-or-n-p "Variable has a value. Redefine?")))
       ,(when documentation-p
	  `(setf (documentation ',name 'variable) ',documentation))

       (setf (symbol-value ',name) ,data)
       (pushnew ',name *data-frames*)
       (defdf-env ',name nil))))


(defun defdf-env (data-frame old-keys)
  "Create a package with the same name as DATA-FRAME.  Within it, create a symbol-macro for each column that will return the columns value.
Can also be used to remove and update the environment as the DATA-FRAME changes in destructive operations"
  (let* ((df      (symbol-value data-frame))
	 (df-name (symbol-name  data-frame))
	 (pkg     (find-package df-name))
	 (rem-keys (set-difference old-keys (coerce (keys df) 'list))) ;the keys that were replaced in an operation
	 (add-keys (set-difference (coerce (keys df) 'list) old-keys)));the keys that were added in an operation

    (check-type data-frame symbol     "a symbol")
    (check-type df         data-frame "a data frame")

    (unlessf pkg (make-package df-name :use '()))
    (unless (slot-boundp df 'name)
      (setf (name df) df-name))
    (when add-keys
      (maphash #'(lambda (key index)
		   (declare (ignore index))
		   (when (member key add-keys)
		     (let ((col (intern (symbol-name key) pkg)))
		       (export col pkg)
		       ;; (eval `(cl:define-symbol-macro ,col (aref (columns ,df) ,index)))))) ;remove me
		       (eval `(cl:define-symbol-macro ,col (column ,df ',key)))))) ;there appears no other way than 'eval'
	       (ordered-keys-table (slot-value df 'ordered-keys))))

    ;; rename-column! is a special case that requires us to copy over the symbol plist
    (when rem-keys
      (mapcar #'(lambda (key)
		  (let ((old-key (find-symbol (string key) pkg)))
		    (when (= 1 (length rem-keys) (length add-keys)) ;rename! special case
		      (setf (symbol-plist (find-symbol (string (first add-keys)) pkg))
     			    (symbol-plist old-key)))
		    (unintern old-key pkg)))
	      rem-keys))
    df))

(defun undef (&rest params)
  "Remove one or more data frames from the environment
PARAMS: a list of DATA-FRAMEs

Essentially reverses what DEFDF does.  Returns the data frames that were removed.  Don't use this if you have a data frame bound via DEFPARAMETER.
Examples:
    (undef mtcars vlcars)"
  (dolist (df params)
    (check-type df data-frame "a data-frame")
    (assert (slot-boundp df 'name) () "name is not bound in the data-frame")

    (let* ((pkg (find-package (name df))) ;package for symbol-macros
	   (df-sym (find-symbol (name df))))
      (assert (member df-sym *data-frames*)
	      ()
	      "~A is not known in the environment.  It may have been defined without defdf" (name df))

      ;; Remove the symbol macros
      (loop for var being the symbols in pkg
	    do (unintern var))
      (delete-package pkg)

      ;; Remove the data frame
      (setf *data-frames* (delete df-sym *data-frames*))
      (makunbound df-sym)))
  params)


;;; In order to show data frame consistently with different settings
;;; for print-object, we need to control printing here.
;;; TODO move to data-frame.lisp
(defun show-data-frames (&key (head nil) (stream *standard-output*))
  "Print all data frames in the current environment in reverse order of creation, i.e. most recently created first.
If HEAD is not NIL, print the first six rows, similar to the (head) function"
  (let ((*print-pretty* nil))
    (if head
	(loop for df-sym in *data-frames* do
	  ;; (loop for df in (sort (copy-list *data-frames*) #'string<=) do ;alphabetical order
	  (let ((df (symbol-value df-sym)))
	    (let* ((*print-lines* 6)
		   (*print-pretty* t))
	      (format stream "~2&~A" (symbol-name df-sym))
	      (df:print-data df stream nil))))
	(pprint-logical-block (stream nil)
	  (pprint-logical-block (stream nil)
	    (pprint-indent :block 2 stream)
	    (loop for df-sym in *data-frames*
		  do (progn
		       (format stream "~@:_~A:~@:_" (symbol-name df-sym))
		       (fresh-line stream)
		       (pprint-logical-block (stream nil :per-line-prefix "  ")
			 (format stream "~A" (symbol-value df-sym)))
		       (fresh-line stream)
		       (terpri stream))))))))


;; Unexported. For debugging
(defun show-symbols (pkg)
  "Print all symbols in PKG
Example: (show-symbols 'mtcars)"
  (do-symbols (s (find-package (symbol-name pkg))) (print s)))




#| This works, but we're not using at the moment
(defun redefinitionp (df)
     ;;; Give the user some restarts in the event the data frame exists
    (handler-bind ((data-frame-exists
		     #'(lambda (c)
			 (invoke-debugger c))))
      (cond ((and (or pkg
		      (member df df::*data-frames*))
		  *ask-on-redefine*)
	     (restart-case (signal 'data-frame-exists :data-frame df)
	       (redefine ()
		 :report (lambda (s)
			   (format s "Redefine ~A, losing all package symbols" df-name))
		 (undef df))
	       (new-name (n)
		 :report "Use a different name"
		 :interactive (lambda ()
				(list
				 (duologue:prompt "Enter a new name for the data frame: "
						  :type 'string :validator #'df-exists-p :if-invalid #'invalid-df-name)))
		 ;; (setf df n)
		 (setf df-name n))
	       (dont-ask ()
		 :report (lambda (s)
			   (format s "Redefine ~A and don't ask about data frame redefinitions again for this session" df-name))
		 :interactive (lambda ()
				(list
				 (duologue:prompt "Enter a new name for the data frame: "
						  :type 'string :validator #'df-exists-p :if-invalid #'invalid-df-name)))
		 (undef df)
		 (setf df:*ask-on-redefine* nil))))
	    ((and (or (find-package df-name)
		      (boundp df))
		  (not *ask-on-redefine*))
	     (warn "Redefining ~A" df-name)
	     (undef df)))))
|#
