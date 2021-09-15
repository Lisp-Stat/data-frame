;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: DATA-FRAME -*-
;;; Copyright (c) 2021 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:data-frame)

;;; These definitions are in a separate file because additional
;;; functionality is expected to be added in future.

(defmacro define-data-frame (df body &optional doc)
;  `(declare (ignorable ',df))
  (when (and doc (not (stringp doc))) (error "Data frame documentation is not a string"))
  `(let* ((df-str (string ',df))
	  (*package* (if (find-package df-str)    ;exists?
			 (find-package df-str)    ;yes, return it
			 (make-package df-str :use '())))) ;no, make it
     (defparameter ,df ,body ,doc)
     (eval '(define-column-names ,df *package*))
     #+sbcl (eval '(setf (doc-string ,df) ,doc)) ;If we don't use eval, SBCL complains about undefined variable
     #-sbcl (setf (doc-string ,df) ,doc)	 ;but no one else does
     ',df))

(defun define-column-names (df package)
  "Create a symbol macro for each column name in DF
After running this function, you can refer to a column by its name. This is useful if the column names of a data frame have changed.
Example: (define-column-names mtcars)"
  (maphash #'(lambda (key index)
	       (let ((col (intern (symbol-name key) package)))
		 (export col)
		 (eval `(cl:define-symbol-macro ,col (cl:aref (columns ,df) ,index)))))
	   (ordered-keys-table (slot-value df 'ordered-keys))))

(defun show-symbols (pkg)
  "Print all symbols in PKG
Example: (show-symbols 'mtcars)"
  (do-symbols (s (find-package (symbol-name pkg))) (print s)))

(defmacro replace-key! (df new old)
  "Replace a key in DF, updating data package symbols
Example: (replace-key! mtcars row-name x1)"
  `(let* ((*package* (find-package (string-upcase (string ',df))))
	  (sym (intern (string ',new)))
	  (old-key (find-symbol (string ',old))))
     (export sym)
     (substitute-key! ,df sym old-key)
     (unintern old-key)
     (funcall #'define-column-names ,df *package*)))

