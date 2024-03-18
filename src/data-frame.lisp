;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: DATA-FRAME -*-
;;; Copyright (c) 2021-2023 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL
(in-package #:data-frame)

;;; Note: tpapp never mentions the difference between a data-vector
;;; and a data-frame. As near as I can tell, a data-frame must have
;;; the same number of rows for each variable.

(defparameter *large-data* most-positive-fixnum ;4611686018427387903
  "An indication that the data set is large for a particular use case.
This should be bound by a user to the maximum number of data points they consider to be 'normal'. The function can then signal a large-data warning if it is exceeded.

E.g. (let ((df:*large-data* 50000))
       (handler-bind ((large-data ...
          (some-data-operation ; this will signal if the data is too large
            (restart-bind ...")

(deftype data-type ()
  "A statistical type for a data variable.  All data columns must be one of these types if they are to be intepreted properly by Lisp-Stat"
  '(member :string :double-float :single-float :categorical :temporal :integer :bit))


;;; Ordered keys provide a mapping from column keys (symbols) to nonnegative
;;; integers.  They are used internally and the corresponding interface is
;;; NOT EXPORTED.

(defstruct (ordered-keys (:copier nil))
  "Representation of ordered keys
Ordered keys provide a mapping from column keys (symbols) to nonnegative
integers.  They are used internally and the corresponding interface is
NOT EXPORTED.

TABLE maps keys to indexes, starting from zero."
  (table (make-hash-table :test #'eq) :type hash-table :read-only t))

(defun keys-count (ordered-keys)
  "Number of keys."
  (hash-table-count (ordered-keys-table ordered-keys)))

(defun keys-vector (ordered-keys)
  "Vector of all keys."
  (map 'vector #'car
       (sort (hash-table-alist (ordered-keys-table ordered-keys))
             #'<=
             :key #'cdr)))

(defun key-index (ordered-keys key)
  "Return the index for KEY."
  (let+ (((&values index present?) (gethash key (ordered-keys-table ordered-keys))))
    (unless present?
      (error 'key-not-found :key key :keys (keys-vector ordered-keys)))
    index))

(defun add-key! (ordered-keys key)
  "Modify ORDERED-KEYS by adding KEY."
  (check-type key symbol)
  (let+ (((&structure ordered-keys- table) ordered-keys)
         ((&values &ign present?) (gethash key table)))
    (when present?
      (error 'duplicate-key :key key))
    (setf (gethash key table) (hash-table-count table)))
  ordered-keys)

(defun ordered-keys (keys)
  "Create an ORDERED-KEYS object from KEYS (a sequence)."
  (aprog1 (make-ordered-keys)
    (map nil (curry #'add-key! it) keys)))

(defun copy-ordered-keys (ordered-keys)
  "Return a copy of ORDERED-KEYS"
  (let+ (((&structure ordered-keys- table) ordered-keys))
    (make-ordered-keys :table (copy-hash-table table))))

(defun add-keys (ordered-keys &rest keys)
  "Add KEYS to ORDERED-KEYS"
  (aprog1 (copy-ordered-keys ordered-keys)
    (mapc (curry #'add-key! it) keys)))

(defun remove-key! (ordered-keys key)
  "Modify ORDERED-KEYS by removing KEY."
  (check-type key symbol)
  (let+ (((&structure ordered-keys- table) ordered-keys)
         ((&values &ign present?) (gethash key table))
	 (kv))
    (unless present?
      (error 'key-not-found :key key))

    (remhash key table)
    (setf kv (remove key (keys-vector ordered-keys)))
    (loop for key across kv
	  for i = 0 then (1+ i)
	  do (setf (gethash key table) i)))
  ordered-keys)

;;;
;;; Implementation of SELECT for ORDERED-KEYS
;;;
(defmethod axis-dimension ((axis ordered-keys))
  (hash-table-count (ordered-keys-table axis)))

(defmethod canonical-representation ((axis ordered-keys) (slice symbol))
  (if (select-reserved-symbol? slice)
      (call-next-method)
      (key-index axis slice)))

(defmethod select ((ordered-keys ordered-keys) &rest selections)
  (let+ (((slice) selections))
    (ordered-keys
     (select (keys-vector ordered-keys)
            (canonical-representation ordered-keys slice)))))


;;; generic implementation -- the class is not exported, only the functionality

(defclass data ()
  ((name				;same as the symbol-name
    :initarg nil
    :type string
    :accessor name
    :documentation "The name of the data frame.  MUST be the same as the symbol whose value cell points to this data frame.  This slot essentially allows us to go 'backwards' and get the symbol that names the data frame.")
   (ordered-keys
    :initarg :ordered-keys
    :type ordered-keys)
   (columns
    :initarg :columns
    :type vector)
   #+nil
   (doc-string			;I'd like this to be 'documentation', but that conflicts with the CL version
    :initarg :doc-string
    :initform ""
    :type string
    :accessor doc-string))
  (:documentation "This class is used for implementing both data-vector and data-frame, and represents an ordered collection of key-column pairs.  Columns are not assumed to have any specific attributes.  This class is not exported."))

(defmethod aops:element-type ((data data)) ;should this be doing something?
  t)

(defun make-data (class keys columns)
  "Create a DATA object from KEYS and COLUMNS.  FOR INTERNAL USE.  Always creates a copy of COLUMNS in order to ensure that it is an adjustable array with a fill pointer.  KEYS are converted to ORDERED-KEYS if necessary."
  (let ((n-columns (length columns))
        (ordered-keys (atypecase keys
                        (ordered-keys it)
                        (t (ordered-keys it)))))
    (assert (= n-columns (keys-count ordered-keys)))
    (assert (subtypep class 'data))
    (make-instance class
                   :ordered-keys ordered-keys
                   :columns (make-array n-columns
                                        :adjustable t
                                        :fill-pointer n-columns
                                        :initial-contents columns))))

(defgeneric check-column-compatibility (data column)
  (:documentation "Check if COLUMN is compatible with DATA.")
  (:method ((data data) column) ;no-op. Was Tamas going to implement it later?
    (declare (ignore column))))

(defun ensure-arguments-alist (rest)
  "Recognizes the following and converts them to an alist:

  plist
  alist
  (plist)
  (alist)
  (data-frame)"
  (let+ (((&flet error% (&optional (list rest))
            (error "Could not interpret ~A as a plist or alist." list)))
         ((&flet ensure-alist (list)
            (typecase (car list)
              (cons rest)
              (symbol (plist-alist rest))
              (t (error% list))))))
    (if (cdr rest)
        (ensure-alist rest)
        (let ((first (car rest)))
          (typecase first
            (data (as-alist first))
            (cons (if (consp (cdr first))
                      (ensure-alist first)
                      rest))            ; first element of an alist
            (t (error%)))))))


;;; These two functions, alist-data & plist-data can be used to create
;;; DATA-VECTOR classes, that is a class works just like DATA-FRAME,
;;; but permits unequal length variables.  To date, I haven't found
;;; much (any) need for a DATA-VECTOR and haven't done any
;;; improvements to that class.
(defun alist-data (class alist)
  "Create an object of CLASS (subclass of DATA) from ALIST which contains key-column pairs."
  (assert alist () "Can't create an empty data frame.")
  (make-data class (mapcar #'car alist) (mapcar #'cdr alist)))

(defun plist-data (class plist)
  "Create an object of CLASS (subclass of DATA) from PLIST which contains keys and columns, interleaved."
  (alist-data class (plist-alist plist)))

(defun keys (data)
  "Return a vector of keys."
  (check-type data data)
  (copy-seq (keys-vector (slot-value data 'ordered-keys))))

(defmethod as-alist ((data data))
  "Key-column pairs as an alist."
  (map 'list #'cons (keys data) (columns data)))

(defun copy (data &key (key #'identity))
  "Copy data frame or vector.  Keys are copied (and thus can be modified), columns or elements are copied using KEY, making the default give a shallow copy."
  (check-type data data)
  (let+ (((&slots-r/o ordered-keys columns) data)
	 (new-data (make-data (class-of data)
			      (copy-ordered-keys ordered-keys)
			      (map 'vector key columns))))
    new-data))

(defun column (data key)
  "Return column corresponding to key."
  (check-type data data)
  (let+ (((&slots-r/o ordered-keys columns) data))
    (aref columns (key-index ordered-keys key))))

(defun (setf column) (column data key)
  "Set column corresponding to key."
  (check-column-compatibility data column)
  (let+ (((&slots-r/o ordered-keys columns) data))
    (setf (aref columns (key-index ordered-keys key)) column)))

(defun columns (data &optional (slice t))
  "Return the columns of DATA as a vector, or a selection if given (keys are resolved)."
  (check-type data data)
  (let+ (((&slots-r/o ordered-keys columns) data))
    (select columns (canonical-representation ordered-keys slice))))

(defun column-names (df)
  "Return a list of column names in DF, as strings"
   (map 'list #'symbol-name (keys df)))

(defun rows (data)
  "Return the rows of DATA as a vector"
  (loop for index below (aops:nrow data)
	collecting (columns (select:select data index t)) into rows
	finally (return (coerce rows 'vector ))))

(defun map-columns (data function &optional (result-class (class-of data)))
  "Map columns of DATA-FRAME or DATA-VECTOR using FUNCTION.  The result is a new DATA-FRAME with the same keys."
  (make-data result-class (keys data) (map 'vector function (columns data))))


(defun df-env-p (df)
  "Returns T if there is environment set-up for the data frame, or NIL if there isn't one."
  (if (and (slot-boundp df 'name)
	   (find-package (string-upcase (slot-value df 'name))))
      t
      nil))


(defun add-column! (data key column &optional update-env)
  "Modify DATA (a data-frame or data-vector) by adding COLUMN with KEY.  Return DATA."
  (check-column-compatibility data column)
  (let+ (((&slots ordered-keys columns) data))
    (add-key! ordered-keys key)
    (vector-push-extend column columns))
  (when (and (df-env-p data)
	     update-env)
    (defdf-env (find-symbol (string-upcase (name data))) nil))
  data)

(defun add-columns! (data &rest keys-and-columns)
  "Modify DATA (a data-frame or data-vector) by adding columns with keys.
If a data-frame environment exists, add columns to it as well."
  (mapc (lambda+ ((key . column))
          (add-column! data key column t))
        (ensure-arguments-alist keys-and-columns))
  data)

(defun add-columns (data &rest keys-and-columns)
  "Return a new data-frame or data-vector with keys and columns added.  Does not modify DATA."
  (aprog1 (copy data)
    (apply #'add-columns! it keys-and-columns)))


(defun remove-columns (data keys)
  "Return a new data-frame or data-vector with keys and columns removed.  Does not modify DATA.
ARGS: DATA data frame
      KEYS list of keys (variables) to be removed"
  (select data t (reverse (set-difference (coerce (keys data) 'list) keys))))

(defun remove-column! (data key)
  "Modify DATA (a data-frame or data-vector) by removing COLUMN with KEY.  Return DATA."
  (check-type key symbol)
  (let+ (((&slots ordered-keys columns) data)
	 (index (key-index ordered-keys key)))
    (remove-key! ordered-keys key)
    (delete-nth* columns index))
  (when (df-env-p data)
    (defdf-env (find-symbol (string-upcase (name data))) '(key)))
  data)

;;; TODO document me!
(defun remove-columns! (data &rest keys)
  "Modify DATA (a data-frame or data-vector) by removing columns with keys.
If a data-frame environment exists, add columns to it as well."
  (mapc (lambda (key)
          (remove-column! data key))
	(car keys))
  data)



;;; TODO take a plist for new & old and process it so we can rename
;;; multiple variables in one function call.  See ensure-arguments-alist.
(defmethod rename-column! (data new old) ;generic so will work on data-frame subclasses
  "Substitute NEW, a SYMBOL, for OLD in DF

Useful when reading data files that have an empty or generated column name.

Example: (rename-column! cars 'name :||) will replace an empty symbol with 'name"
  (let+ ((old-keys (coerce (keys data) 'list))
         (present? (member old old-keys)))
    (unless present?
      (error 'key-not-found :key old))
    (setf (slot-value data 'ordered-keys) (ordered-keys (substitute new old (keys data))))
    (when (df-env-p data)
      (defdf-env (find-symbol (string-upcase (name data))) old-keys))
    data))



(defmacro define-data-subclass (class abbreviation)
  (check-type class symbol)
  (check-type abbreviation symbol)
  (let+ (((&flet fname (prefix)
            (symbolicate prefix '#:- abbreviation)))
         (alist-fn (fname '#:alist))
         (plist-fn (fname '#:plist)))
    `(progn
       (defclass ,class (data)
         ())
       (defun ,(fname '#:make) (keys columns)
         (make-data ',class keys columns))
       (defun ,alist-fn (alist)
         (alist-data ',class alist))
       (defun ,plist-fn (plist)
         (plist-data ',class plist))
       (defun ,abbreviation (&rest plist-or-alist)
         (if (alistp plist-or-alist)
             (,alist-fn plist-or-alist)
             (,plist-fn plist-or-alist))))))

(define-data-subclass data-vector dv)

(defmethod aops:dims ((data-vector data-vector))
  (list (length (columns data-vector))))

(defmethod aops:as-array ((data-vector data-vector))
  (columns data-vector))

(defmethod select ((data-vector data-vector) &rest slices)
  (let+ (((column-slice) slices)
         ((&slots-r/o ordered-keys columns) data-vector)
         (column-slice (canonical-representation ordered-keys column-slice)))
    (if (singleton-representation? column-slice)
        (aref columns column-slice)
        (make-dv (select ordered-keys column-slice)
                 (select columns column-slice)))))

(define-data-subclass data-frame df)

(defmethod initialize-instance :after ((data-frame data-frame) &rest initargs)
  (declare (ignore initargs))
  (let+ (((first . rest) (coerce (columns data-frame) 'list))
         (length (column-length first)))
    (assert (every (lambda (column)
                     (= length (column-length column)))
                   rest)
            () "Columns don't have the same length.")))

(defmethod aops:nrow ((data-frame data-frame))
  (column-length (aref (columns data-frame) 0)))

(defmethod aops:ncol ((data-frame data-frame))
  (length (columns data-frame)))

(defmethod aops:dims ((data-frame data-frame))
  (list (aops:nrow data-frame) (aops:ncol data-frame)))

(defmethod aops:as-array ((data-frame data-frame))
  ;; Return contents of DATA-FRAME as a matrix.
  (nu:transpose (aops:combine (columns data-frame))))

(defmethod check-column-compatibility ((data data-frame) column)
  (assert (= (column-length column) (aops:nrow data))))

(defun matrix-df (keys matrix)
  "Convert a matrix to a data-frame with the given keys."
  (let+ ((columns (aops:split (nu:transpose matrix) 1)))
    (assert (length= columns keys))
    (alist-df (map 'list #'cons keys columns))))


;;; implementation of SELECT for DATA-FRAME

(defmethod select ((data-frame data-frame) &rest slices)
  (let+ (((row-slice &optional (column-slice t)) slices)
         ((&slots-r/o ordered-keys columns) data-frame)
         (row-slice (canonical-representation (aops:nrow data-frame) row-slice))
         (column-slice (canonical-representation ordered-keys column-slice))
         (columns (select columns column-slice))
         ((&flet slice-column (column)
            (select column row-slice))))
    (if (singleton-representation? column-slice)
        (slice-column columns)
        (let ((keys (select ordered-keys column-slice))
              (columns (map 'vector #'slice-column columns)))
          (if (singleton-representation? row-slice)
              (make-dv keys columns)
              (make-df keys columns))))))

;;; TODO: (setfs election)


;;; mapping rows and adding columns

(defun map-rows (data-frame keys function &key (element-type t))
  "Map rows using FUNCTION, on the columns corresponding to KEYS.  Return the result with the given ELEMENT-TYPE."
  (let ((columns (map 'list (curry #'column data-frame) (ensure-list keys)))
        (nrow (aops:nrow data-frame)))
    (aprog1 (make-array nrow :element-type element-type)
      (dotimes (index nrow)
        (setf (aref it index)
              (apply function
                     (mapcar (lambda (column)
                               (ref column index))
                             columns)))))))

(defun do-rows (data-frame keys function)
  "Traverse rows from first to last, calling FUNCTION on the columns corresponding to KEYS.  Return no values."
  (let ((columns (map 'list (curry #'column data-frame) (ensure-list keys)))
        (nrow (aops:nrow data-frame)))
    (dotimes (index nrow (values))
      (apply function
             (mapcar (lambda (column)
                       (ref column index))
                     columns)))))

(defun map-df (data-frame keys function result-keys)
  "Map DATA-FRAME to another one by rows.  Function is called on the columns corresponding to KEYS, and should return a sequence with the same length as RESULT-KEYS, which give the keys of the resulting data frame.  RESULT-KETS should be either symbols, or of the format (symbol &optional (element-type t))."
  (let* ((columns (map 'list (curry #'column data-frame) keys))
         (nrow (aops:nrow data-frame))
         (result-keys-and-element-types
           (mapcar (lambda (key-and-element-type)
                     (let+ (((key &optional (element-type t))
                             (ensure-list key-and-element-type)))
                       (cons key element-type)))
                   result-keys))
         (result-columns (map 'vector
                              (lambda (key-and-element-type)
                                (make-array nrow
                                            :element-type (cdr key-and-element-type)))
                              result-keys-and-element-types)))
    (dotimes (index nrow)
      (let ((result-row (apply function
                               (mapcar (lambda (column)
                                         (ref column index))
                                       columns))))
        (assert (length= result-row result-columns))
        (map nil (lambda (result-column result-element)
                   (setf (aref result-column index) result-element))
             result-columns result-row)))
    (make-df (mapcar #'car result-keys-and-element-types) result-columns)))

(defun mask-rows (data-frame keys predicate)
  "Return a bit-vector containing the result of calling PREDICATE on rows of the columns corresponding to KEYS (0 for NIL, 1 otherwise)."
  (map-rows data-frame keys (compose (lambda (flag) ;translate nil/non-nil to 0 or 1
                                       (if flag 1 0))
                                     predicate)
            :element-type 'bit))

(defun count-rows (data-frame keys predicate)
  "Count the number of rows for which PREDICATE called on the columns corresponding to KEYS returns non-NIL."
  (let ((columns (map 'list (curry #'column data-frame) (ensure-list keys))))
    (loop for index below (aops:nrow data-frame)
          count (apply predicate
                       (mapcar (lambda (column)
                                 (ref column index))
                               columns)))))

(defun replace-column! (data key function-or-column &key (element-type t))
  "Modify column KEY of data-frame DATA by replacing it either with the given column, or applying the function to the current values (ELEMENT-TYPE is used.)"
  (let+ (((&slots ordered-keys columns) data)
         (index (key-index ordered-keys key)))
    (setf (aref columns index)
          (if (functionp function-or-column)
              (map-rows data key function-or-column :element-type element-type)
              (prog1 function-or-column
                (check-column-compatibility data function-or-column)))))
  data)

(defun replace-column (data key function-or-column &key (element-type t))
  "Create a new data frame with new column KEY from data-frame DATA by replacing it either with the given column, or applying the function to the current values (ELEMENT-TYPE is used.)"
  (replace-column! (copy data) key function-or-column :element-type element-type))

;; We give this a df- prefix to avoid symbol clash with the CL
;; function (which, sadly, is not generic).  After adding
;; df:delete-duplicates, shadow both in the package declaration.
(defun df-remove-duplicates (data)
  "Return a modified copy of DATA from which any element (row, if a DATA-FRAME) that matches another element has been removed"
  (etypecase data
    (alexandria:proper-sequence (cl:remove-duplicates data)) ; Eventually shadow the CL version
    (df:data-frame (let* ((new-rows (cl:remove-duplicates (rows data) :test #'equalp))
			  (new-array (make-array (list (length new-rows)
						       (length (svref new-rows 0)))
						 :initial-contents new-rows)))
			  (matrix-df (keys data) new-array)))))


;; TODO
#+nil
(defun delete-duplicates (data)
  "Like REMOVE-DUPLICATES, but may modify DATA"
...)

(defmethod print-object ((ordered-keys ordered-keys) stream)
  (print-unreadable-object (ordered-keys stream :type t)
    (format stream "~{~a~^, ~}" (coerce (keys-vector ordered-keys) 'list))))

(defmethod print-object ((data-vector data-vector) stream)
  (let ((alist (as-alist data-vector)))
      (print-unreadable-object (data-vector stream :type t)
        (format stream "of ~d variables" (length alist)))))

(defmethod print-object ((df data-frame) stream)
  "Print DATA-FRAME dimensions and type
After defining this method it is permanently associated with data-frame objects"
  (print-unreadable-object (df stream :type t)
    (let ((description (and (slot-boundp df 'name)
			    (documentation (find-symbol (name df)) 'variable))))
    (format stream
	    "(~d observations of ~d variables)"
	    (aops:nrow df)
	    (aops:ncol df))
    (when description
      (format stream "~&~A" (short-string description))))))

(defmethod describe-object ((df data-frame) stream)
  (let ((name (when (slot-boundp df 'name) (name df))))
    (format stream "~A~%" name)
    (format stream "  A data-frame with ~D observations of ~D variables~2%" (aops:nrow df) (aops:ncol df))
    (when name
      (let ((rows (loop for key across (keys df)
			for sym = (find-symbol (string-upcase (symbol-name key)) (find-package name))
			collect (list (symbol-name key)
				      (get sym :type)
				      (get sym :unit)
				      (get sym :label)))))
      (push '("--------" "----" "----" "-----------") rows)
      (push '("Variable" "Type" "Unit" "Label") rows)
      (print-table rows)))))


;;; KLUDGE ALERT
;;; This violates the spec.  It's not easy at all to get good
;;; behaviour from describe.  See code and comments in describe.lisp.
#+allegro (setf excl:*enable-package-locked-errors* nil)
(defmethod describe-object :after ((s symbol) stream)
  (unless (boundp s) (return-from describe-object))
  (unless (eq #+sbcl (SB-CLTL2:variable-information s)
	      #+ccl  (ccl:variable-information s)
	      #+allegro (system:variable-information s)
	      :symbol-macro)
    (let ((*print-pretty* t)
	  (df (symbol-value s))
	  (name (symbol-name s)))

      (pprint-logical-block (stream nil)
	(pprint-logical-block (stream nil)
          (pprint-indent :block 2 stream)
	  (when-let ((pkg (find-package name)))
	    (format stream "~@:_Variables: ~@:_")
	    (pprint-logical-block (stream nil :per-line-prefix "  ")
	      (let ((rows (loop for key across (keys df)
				for sym = (find-symbol (string-upcase (symbol-name key)) (find-package name))
				collect (list (symbol-name key)
					      (get sym :type)
					      (get sym :unit)
					      (get sym :label)))))
		(push '("--------" "----" "----" "-----------") rows)
		(push '("Variable" "Type" "Unit" "Label") rows)
		(print-table rows stream)))))))))
#+allegro (setf excl:*enable-package-locked-errors* t)

(defmethod sample ((df data-frame) n &key
				       with-replacement
				       skip-unselected)
  "Return N rows of DF taken at random.

If WITH-REPLACEMENT is true, return a random sample with
replacement (a \"draw\").

If WITH-REPLACEMENT is false, return a random sample without
replacement (a \"deal\").

If SKIP-UNSELECTED is non-NIL, do not return the elements of DF that we not part of the selection.  Non-NIL by default, as the typical use case is to split a data set into training and test data sets."
  (declare (data-frame df))
  (let+ (((&dims nrow &ign) df))
    (cond ((> n nrow) (error "Requested number of rows N is greater than rows in the data frame."))
	  ((= n 0) nil)
          ((= n 1) (select df (random nrow) t)) ;return unselected row?  This seems an edge case.
	  ((= n nrow) df)
	  (t (let+ ((indices (linspace 0 (1- nrow) nrow))
		    (selected (sample indices n :with-replacement with-replacement))
		    (not-selected (set-difference* indices selected)))
	       (if skip-unselected
		   (select df selected t)
		   (values (select df selected t)
			   (select df not-selected t))))))))
