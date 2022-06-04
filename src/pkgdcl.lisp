;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;; Copyright (c) 2020-2022 by Symbolics Pte. Ltd. All rights reserved.

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

    ;; errors & conditions
    #:*large-data*			;maximum data size for a particular use case
    #:large-data
    #:duplicate-key
    #:key-not-found

    ;; generic - both data-vector and data-frame
    #:data-type
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
    #:summary				;summarize a data frame
    #:summarize-column			;summarize a variable
    #:get-summaries			;return a list of variable summaries
    #:bit-variable-summary
    #:real-variable-summary
    #:factor-variable-summary
    #:generic-variable-summary
    #:*summary-minimum-length*		;columns are only summarised when longer than this
    #:*quantile-threshold*		;if the number of unique reals exceeds this threshold, they will be summarized with quantiles
    #:*distinct-threshold*		;if an integer variable has <= discrete values, consider it a factor
    #:*distinct-maximum*))              ;If a string/factor variable has > *distinct-maximum* values, exclude it


