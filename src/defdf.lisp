;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: DATA-FRAME -*-
;;; Copyright (c) 2021-2022,2026 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:data-frame)

;;; Functions for defining data frames in the statistical environment.

(defvar *data-frames* nil
  "Global list of all data frames, not exported")

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
       (pushnew ',name *data-frames* :test #'eq)
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

    (alexandria+:unlessf pkg (make-package df-name :use '()))
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

(defun undef (&rest all-args &key (packages nil packages-supplied-p))
  "Remove one or more data frames from the environment.

Each designator in ALL-ARGS (before any keyword) may be:
  - a symbol   — used directly
  - a string   — looked up case-insensitively via FIND-DATA-FRAME
  - a data-frame object — legacy; the NAME slot is used (defdf only)

:PACKAGES — package list passed to FIND-DATA-FRAME when a string
  designator is used.  Defaults to *default-df-search-packages* plus
  *package*.  Has no effect when symbols are passed directly.

Works for frames defined via DEFDF (removes the symbol-macro package)
and for frames bound via DEFPARAMETER or SETF (just unbinds the symbol).

Examples:
  (undef 'mtcars)
  (undef 'mtcars 'cars)
  (undef \"mtcars\")                          ; string look-up
  (undef \"mtcars\" :packages '(:my-pkg))    ; explicit search scope"
  ;; Strip keyword arguments out of the &rest list so we only iterate
  ;; the actual designators.
  (let ((designators (loop for arg in all-args
                           until (keywordp arg)
                           collect arg))
        (pkg-list (if packages-supplied-p
                      packages
                      (default-search-packages))))
    (dolist (designator designators)
      (let ((sym
              (etypecase designator
                ;; Preferred: a quoted symbol
                (symbol designator)
                ;; Programmatic / server path: a name string
                (string
                 (or (find-data-frame designator pkg-list)
                     (error "No data frame named ~S found in ~S"
                            designator pkg-list)))
                ;; Backward compat: the data-frame value itself
                (data-frame
                 (assert (slot-boundp designator 'name) ()
                   "Cannot undef a data frame with no NAME slot; ~
                    pass the symbol or string instead.")
                 (find-symbol (name designator))))))
        (check-type sym symbol)
        (unless (boundp sym)
          (error "~A is not bound" sym))
        (unless (typep (symbol-value sym) 'data-frame)
          (error "~A is not bound to a data frame" sym))

        ;; Tear down the defdf environment if one exists
        (let ((pkg (find-package (symbol-name sym))))
          (when pkg
            (do-symbols (var pkg) (unintern var pkg))
            (delete-package pkg)))

        ;; Remove from the *data-frames* registry
        (setf *data-frames* (delete sym *data-frames*))

        ;; Unbind
        (makunbound sym)))
    designators))


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
