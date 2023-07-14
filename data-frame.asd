;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: ASDF -*-
;;; Copyright (c) 2020-2023 by Symbolics Pte. Ltd. All rights reserved.

(defsystem "data-frame"
  :version     "1.2.1"
  :licence     :MS-PL
  :author      "Steve Nunez <steve@symbolics.tech>"
  :long-name   "Data frames for Common Lisp"
  :description "A data manipulation library for statistical computing"
  :long-description  #.(uiop:read-file-string
			(uiop:subpathname *load-pathname* "description.text"))
  :homepage    "https://lisp-stat.dev/docs/manuals/data-frame"
  :source-control (:git "https://github.com/Lisp-Stat/data-frame.git")
  :bug-tracker "https://github.com/Lisp-Stat/data-frame/issues"

  :depends-on ("alexandria"
	       "alexandria+"
               "anaphora"
               "array-operations"
               "num-utils"
               "select"
               "statistics"
               "let-plus"
               "duologue"
	       "sb-cltl2")
  :serial t
  :pathname "src/"
  :components ((:file "pkgdcl")
	       (:file "utils")
	       (:file "data-frame")
	       (:file "pprint")
	       (:file "formatted-output")
	       (:file "summary")
	       (:file "defdf")
	       (:file "conditions")
	       (:file "properties")
	       (:file "missing")
	       (:file "filter")
	       (:file "plist-aops"))
  :in-order-to ((test-op (test-op "data-frame/tests"))))

(defsystem "data-frame/tests"
  :version "1.0.0"
  :description "Unit tests for DATA-FRAME."
  :author      "Steve Nunez <steve@symbolics.tech>"
  :licence     :MS-PL
  :depends-on ("data-frame"
               "clunit2")
  :serial t
  :pathname "tests/"
  :components ((:file "data-frame-tests"))
  :perform (test-op (o s)
		    (let ((*print-pretty* t)) ;work around clunit issue #9
		      (symbol-call :clunit :run-suite
				   (find-symbol* :data-frame
						 :data-frame-tests)
				   :use-debugger nil))))
