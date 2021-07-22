;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;; Copyright (c) 2020 by Symbolics Pte. Ltd. All rights reserved.
#+nil
(uiop:define-package #:data-frame.column
  (:use #:cl
        #:alexandria
        #:anaphora
        #:let-plus)
  (:export
   #:column-length
   #:column-summary)
  (:import-from #:nu
                #:as-alist))

(uiop:define-package #:data-frame
  (:nicknames #:df #:dframe)
  (:use
   #:cl
   #:alexandria
   #:anaphora
   #:let-plus
   ;; #:data-frame.column
   #:select
   #:select-dev)
  (:import-from #:nu #:as-alist)
  (:export

   ;; error messages for ordered keys
   #:duplicate-key
   #:key-not-found

   ;; generic - both data-vector and data-frame
   #:columns
   #:map-columns
   #:column
   #:column-type
   #:keys
   #:copy
   #:add-columns
   #:add-column!
   #:add-columns!

   ;; data-vector
   #:data-vector
   #:make-dv
   #:alist-dv
   #:plist-dv
   #:dv

   ;; data-frame
   #:data-frame
   #:make-df
   #:alist-df
   #:plist-df
   #:df
   #:matrix-df
   #:rows
   #:define-data-frame
   #:define-column-names
   #:make-data-package

   ;; transformations for data-frames
   #:map-rows
   #:do-rows
   #:mask-rows
   #:count-rows
   #:map-df
   #:replace-column!
   #:replace-column
   #:remove-columns
   #:substitute-key!
   #:replace-key
   #:missingp
   #:drop-missing
   #:replace-missing
   #:df-remove-duplicates

   ;; Pretty printing
   #:pprint-data-frame
   #:pprint-markdown
   #:pprint-array
   #:head
   #:tail
   #:column-names
   #:row-names

   ;; Summary methods
   #:summary
   #:column-summary
   #:*column-summary-minimum-length*))

