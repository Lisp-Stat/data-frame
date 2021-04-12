;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;; Copyright (c) 2021-2020 by Symbolics Pte. Ltd. All rights reserved.

(asdf:defsystem #:data-frame
  :version      (:read-file-form "version.sexp")
  :description "Data frames for Common Lisp"
  :long-description "A data manipulation package, conceptually similar to R's data.frame, but with a lisp-oriented API."
  :maintainer  "Steve Nunez <steve@symbolics.tech>"
  :author      "Tamas Papp <tkpapp@gmail.com>"
  :licence     :MS-PL
  :source-control (:git "https://github.com/Lisp-Stat/data-frame.git")
  :bug-tracker "https://github.com/Lisp-Stat/data-frame/issues"
  :depends-on (#:alexandria
               #:anaphora
               #:array-operations
               #:num-utils
               #:select
               #:let-plus)
  :serial t
  :components ((:file "pkgdcl")
	       (:file "data-frame-column")
	       (:file "data-frame"))
  :in-order-to ((test-op (test-op "data-frame/tests"))))

(asdf:defsystem #:data-frame/tests
  :version "0"
  :description "Unit tests for DATA-FRAME."
  :maintainer  "Steve Nunez <steve@symbolics.tech>"
  :author      "Tamas Papp <tkpapp@gmail.com>"
  :licence     :MS-PL
  :depends-on (#:data-frame
               #:clunit)
  :serial t
  :components ((:file "data-frame-tests"))
  :perform (test-op (o s)
		    (uiop:symbol-call :clunit :run-suite
				      (uiop:find-symbol* :data-frame
							 :data-frame-tests))))
