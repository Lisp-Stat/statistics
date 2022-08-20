;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: ASDF -*-
;;; Copyright (c) 2022 by Symbolics Pte. Ltd. All rights reserved.

(defsystem :statistics
  :name "Statistics functions"
  :version     "1.0.0"
  :license     :MIT
  :author      "Larry Hunter <Larry.Hunter@CUAnschutz.edu>"
  :maintainer  "Steve Nunez <steve@symbolics.tech>"
  :long-name   "Statistical routines"
  :description "A set of statistical functions"
  :long-description  #.(uiop:read-file-string
			(uiop:subpathname *load-pathname* "description.text"))
  :homepage    "https://lisp-stat.dev/"
  :source-control (:git "https://github.com/Lisp-Stat/statistics.git")
  :bug-tracker "https://github.com/Lisp-Stat/statistics/issues"
  :depends-on ()
  :in-order-to ((test-op (test-op lisp-stat/tests)))
  :components (;(:file "pkgdcls")
	       (:static-file #:LICENSE)
	       (:file "cl-statistics")))

#+nil
(defsystem :lisp-stat/tests
  :version "1.0.0"
  :description "Unit tests for Lisp-Stat"
  :author "Steve Nunez <steve@symbolics.tech>"
  :license :MS-PL
  :depends-on ("lisp-stat" "parachute")
  :serial t
  :pathname "tests/"
  :components ((:file "tstpkg")
	       (:file "statistics"))
  :perform (test-op (o s)
  		    (symbol-call :ls-tests :run-tests)))
