;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: DATA-FRAME -*-
;;; Copyright (c) 2021 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:data-frame)

;;; These definitions are in a separate file because additional
;;; functionality is expected to be added in future.

(defvar *data-frames* nil
  "Global list of all data frames")

(defvar *ask-on-redefine* nil
  "If set, the system will ask the user for confirmation before redefining a data frame")

(defmacro defdf (df body &optional documentation)
  (when (and documentation (not (stringp documentation))) (error "Data frame documentation is not a string"))
  `(let* ((df-str (string ',df))
	  (*package* (if (find-package df-str)    ;exists?
			 (error "~S package exists and cannot use existing package for data frame name" df-str)    ;yes, raise error
			 (make-package df-str :use '())))) ;no, make it
     (unless (and *ask-on-redefine*
		  (boundp ',df)
		  (not (y-or-n-p "Data frame has a value. Redefine?")))
       ;; (format t "in defdf, package is: ~A" *package*)
       (defparameter ,df ,body ,documentation)
       (eval '(setf (name ,df) (symbol-name ',df)))
       (eval '(define-column-names ',df *package*))
       (when ,documentation
	  (eval '(setf (doc-string ,df) ,documentation)))
       (pushnew ',df *data-frames*)
       ',df)))

(defun define-column-names (df package)
  "Create a symbol macro for each column name in DF
After running this function, you can refer to a column by its name. This is useful if the column names of a data frame have changed.
Example: (define-column-names mtcars)"
  ;; (format t "package is ~A " package)
  (maphash #'(lambda (key index)
	       (let ((col (intern (symbol-name key) package)))
		 (export col)
		 (eval `(cl:define-symbol-macro ,col (aref (columns ,df) ,index)))))
	   (ordered-keys-table (slot-value (symbol-value df) 'ordered-keys))))

;; TODO
;; 1. Undefine the symbol macros associated with the data frame
;; 2. Delete the package associated with the data frame
;; See https://stackoverflow.com/questions/69296485/undefining-a-symbol-macro
(defun undef (df)
  "If DF is the symbol of a defined data-frame it is unbound and
removed from the list of data-frames. If DF is a list of data-frame
names each is unbound and removed. Returns DF.
Example: (undef 'mtcars)"
  (dolist (s (if (listp df) df (list df)))
    (when (member s *data-frames*)
      (loop for v being the symbols in (find-package (string-upcase (symbol-name df))) do
	(unintern v))
      (delete-package (find-package (string-upcase (symbol-name df))))
      (setf *data-frames* (delete s *data-frames*))
      (makunbound s)))
  df)

;;; In order to show data frame consistently with different settings
;;; for print-object, we need to control printing here.
(defun show-data-frames (&key (head nil) (stream *standard-output*))
  "Print all data frames in the current environment in reverse order of creation, i.e. most recently created first.
if HEAD is not NIL, print the first six rows, similar to the (head) function"
    (loop for df-sym in *data-frames* do
      ;; "Print all data frames in the current environment in alphabetical order"
      ;; (loop for df in (sort (copy-list *data-frames*) #'string<=) do
      (let ((df (symbol-value df-sym)))
	(if head
	    (let* ((*print-lines* 6)
		   (*print-pretty* t))
	      (when (slot-boundp df 'name) (format stream "~A" (name df)))
	      (df:pprint-data-frame df stream nil))

	  ;; Not head
	    (print-unreadable-object (df stream :type t)
	      (when (slot-boundp df 'name) (format stream "~A " (name df)))
	      (format stream "(~d observations of ~d variables)"
		      (aops:nrow df)
		      (aops:ncol df))
	      (when (slot-boundp df 'doc-string)
		(fresh-line stream)
		(format stream "~A" (doc-string df))))))
      (fresh-line stream)
      (terpri stream)))

;;; TODO Make this a function: (replace-key! (df 'new 'old) ...
(defmacro replace-key! (df new old)
  "Replace a key in DF, updating data package symbols
Example: (replace-key! mtcars row-name x1)"
  `(let* ((*package* (find-package (string-upcase (string ',df))))
	  (sym (intern (string ',new)))
	  (old-key (find-symbol (string ',old))))

     (export sym)
     (substitute-key! ,df sym old-key)
     (unintern old-key)
     (unintern (find-symbol (concatenate 'string (symbol-name ',df) "$" (symbol-name ',old))))
     (funcall #'define-column-names ',df *package*)))

;; Unexported. For debugging
(defun show-symbols (pkg)
  "Print all symbols in PKG
Example: (show-symbols 'mtcars)"
  (do-symbols (s (find-package (symbol-name pkg))) (print s)))

