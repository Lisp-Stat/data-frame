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
    #:doc-string			;same as CL:documentation, but for data-frames
    #:source				;return the source of the data
    #:make-df
    #:alist-df
    #:plist-df
    #:df
    #:matrix-df
    #:rows
    #:defdf
    #:undef
    #:defdf-env				;define package/symbol macros for environment
    #:show-data-frames
    #:*ask-on-redefine*			;if non-nil, ask user if a data frame will be overwritten

    ;; transformations for data-frames
    #:map-rows
    #:do-rows
    #:mask-rows
    #:count-rows
    #:filter-rows
    #:map-df
    #:replace-column!
    #:replace-column
    #:remove-column!
    #:remove-columns
    #:rename-column!
    #:replace-key!
    #:df-remove-duplicates

    ;; missing values
    #:missingp
    #:drop-missing
    #:replace-missing
    #:ignore-missing

    ;; Pretty printing
    #:print-data
    #:print-markdown
    #:print-array
    #:head
    #:tail
    #:short-string			;shorten a long doc-string by returning up to the first newline

    ;; Data properties
    #:heuristicate-types
    #:set-properties
    #:get-property
    #:set-property

    ;; Formatted output
    #:df-print
    #:df-summary
    #:name
    #:doc-string

    ;; Sequence utilities -- these should be in array-operations
    #:delete-nth			;delete the nth item from a sequence
    #:delete-nth*			;modify macro for delete-nth

    ;; Subsets of data
    #:filter-rows

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
    #:*distinct-threshold*		;if an integer variable has <= discrete values, consider it a categorical variable
    #:*distinct-maximum*))              ;if a string/factor variable has > *distinct-maximum* values, exclude it


