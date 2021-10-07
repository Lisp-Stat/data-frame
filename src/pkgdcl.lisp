;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;; Copyright (c) 2020-2021 by Symbolics Pte. Ltd. All rights reserved.

(uiop:define-package #:data-frame
  (:nicknames #:df)
  (:use
    #:cl
    #:alexandria
    #:alexandria+
    #:anaphora
    #:let-plus
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
    #:column-names
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
    #:doc-string
    #:make-df
    #:alist-df
    #:plist-df
    #:df
    #:matrix-df
    #:rows
    #:defdf
    #:undef
    #:define-column-names
    #:show-data-frames

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
    #:replace-key!
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

    ;; Data properties
    #:heuristicate-types
    #:set-properties

    ;; Formatted output
    #:df-print
    #:df-summary
    #:name
    #:doc-string

   ;; Summary methods
   #:summary
   #:column-summary
   #:*column-summary-minimum-length*))


