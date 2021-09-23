;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: DATA-FRAME -*-
;;; Copyright (c) 2021 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:data-frame)

;;; These definitions are in a separate file because additional
;;; functionality is expected to be added in future.

(defvar *data-frames* nil
  "Global list of all data frames")
(defvar *ask-on-redefine* nil
  "If set, the system will ask the user for confirmation before redefining a data frame")

(defmacro define-data-frame (df body &optional documentation)
  (when (and documentation (not (stringp documentation))) (error "Data frame documentation is not a string"))
  `(let* ((df-str (string ',df))
	  (*package* (if (find-package df-str)    ;exists?
			 (find-package df-str)    ;yes, return it
			 (make-package df-str :use '())))) ;no, make it
     (unless (and *ask-on-redefine*
		  (boundp ',df)
		  (not (y-or-n-p "Data frame has a value. Redefine?")))

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
      (setf *data-frames* (delete s *data-frames*))
      (makunbound s)))
  df)

(defun show-data-frames (&optional (stream *standard-output*))
  "Print all data frames in the current environment in reverse order of creation, i.e. most recently created first."
  (loop for df in *data-frames* do
  ;; "Print all data frames in the current environment in alphabetical order"
  ;; (loop for df in (sort (copy-list *data-frames*) #'string<=) do
    (print-object (symbol-value df) stream)
    (fresh-line stream)
    (terpri stream)))

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

;; Unexported. For debugging
(defun show-symbols (pkg)
  "Print all symbols in PKG
Example: (show-symbols 'mtcars)"
  (do-symbols (s (find-package (symbol-name pkg))) (print s)))
