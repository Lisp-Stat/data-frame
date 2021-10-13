;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: ASDF -*-
;;; Copyright (c) 2020-2021 by Symbolics Pte. Ltd. All rights reserved.

(defsystem "data-frame"
  :version      (:read-file-form "version.sexp")
  :description "Data frames for Common Lisp"
  :long-description "A data manipulation package, conceptually similar to R's data.frame"
  :author      "Steve Nunez <steve@symbolics.tech>"
  :licence     :MS-PL
  :source-control (:git "https://github.com/Lisp-Stat/data-frame.git")
  :bug-tracker "https://github.com/Lisp-Stat/data-frame/issues"
  :depends-on ("alexandria"
	       "alexandria+"
               "anaphora"
               "array-operations"
               "num-utils"
               "select"
               "let-plus")
  :serial t
  :pathname "src/"
  :components ((:file "pkgdcl")
	       (:file "utils")
	       (:file "data-frame")
	       (:file "pprint")
	       (:file "formatted-output")
	       (:file "summary")
	       (:file "defdf")
	       (:file "properties")
	       (:file "missing"))
  :in-order-to ((test-op (test-op "data-frame/tests"))))

(defsystem "data-frame/tests"
  :version "0"
  :description "Unit tests for DATA-FRAME."
  :author      "Steve Nunez <steve@symbolics.tech>"
  :licence     :MS-PL
  :depends-on ("data-frame"
               "clunit2")
  :serial t
  :pathname "tests/"
  :components ((:file "data-frame-tests"))
  :perform (test-op (o s)
		    (symbol-call :clunit :run-suite
				 (find-symbol* :data-frame
					       :data-frame-tests))))
