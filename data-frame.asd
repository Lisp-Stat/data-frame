;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;; Copyright (c) 2020 by Symbolics Pte. Ltd. All rights reserved.

(asdf:defsystem #:data-frame
  :version      (:read-file-form "version.sexp")
  :description "Data frames for Common Lisp"
  :long-description "An experimental data manipulation package, conceptually similar to R's data.frame, but with a lisp-oriented API."
  :maintainer  "Steve Nunez <steve@symbolics.tech>"
  :author      "Tamas Papp <tkpapp@gmail.com>"
  :licence     "MS-PL"
  :depends-on (#:alexandria
               #:anaphora
               #:array-operations
               #:cl-num-utils
               #:cl-slice
               #:let-plus)
  :serial t
  :components ((:file "pkgdcl")
	       (:file "data-frame-column")
	       (:file "data-frame")))

(asdf:defsystem #:data-frame/tests
  :version "0"
  :description "Unit tests for DATA-FRAME."
  :maintainer  "Steve Nunez <steve@symbolics.tech>"
  :author      "Tamas Papp <tkpapp@gmail.com>"
  :licence     "MS-PL"
  :depends-on (#:data-frame
               #:clunit)
  :serial t
  :components ((:file "data-frame-tests")))
