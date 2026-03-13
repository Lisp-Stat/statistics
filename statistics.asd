;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: ASDF -*-
;;; Copyright (c) 2022, 2024-2026 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL

(defsystem "statistics"
  :name "Statistics functions"
  :version     "1.3.5"
  :license     :MS-PL
  :author      "Steve Nunez <steve@symbolics.tech>"
  :long-name   "Consolidated Common Lisp statistical functions"
  :description "A consolidated system of statistical functions"
  :long-description  #.(uiop:read-file-string
			(uiop:subpathname *load-pathname* "description.text"))
  :homepage    "https://lisp-stat.dev/"
  :source-control (:git "https://github.com/Lisp-Stat/statistics.git")
  :bug-tracker "https://github.com/Lisp-Stat/statistics/issues"
  :depends-on (#:anaphora
               #:alexandria
               #:distributions
               #:let-plus
	       #:num-utils
	       #:org.tfeb.conduit-packages)
  :in-order-to ((test-op (test-op statistics/tests)))
  :components ((:static-file #:LICENSE)
	       (:file "online")
	       (:file "statistics")
	       (:file "pkgdcl")))

(defsystem "statistics/tests"
  :version "1.0.0"
  :description "Unit tests for Lisp-Stat's statistics system"
  :author "Steve Nunez <steve@symbolics.tech>"
  :license :MS-PL
  :depends-on ("statistics" "num-utils" "clunit2")
  :serial t
  :pathname "tests/"
  :components ((:file "tstpkg")
               (:file "main")
	       (:file "statistics"))
  :perform (test-op (o s)
		    (let ((*print-pretty* t)) ;work around clunit issue #9
		      (symbol-call :clunit :run-suite
				   (find-symbol* :tests
						 :statistics-tests)
					   :use-debugger nil))))

