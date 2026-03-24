;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: ASDF -*-
;;; Copyright (c) 2022, 2024-2026 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL

(defsystem "statistics/streaming"
  :name        "Online (streaming) statistics"
  :version     "1.4.0"
  :license     :MS-PL
  :author      "Steve Nunez <steve@symbolics.tech>"
  :description "Single-pass, numerically stable accumulators for streaming data."
  :long-description  #.(uiop:read-file-string
                        (uiop:subpathname *load-pathname* "streaming-description.text"))
  :homepage    "https://lisp-stat.dev/"
  :source-control (:git "https://github.com/Lisp-Stat/statistics.git")
  :bug-tracker    "https://github.com/Lisp-Stat/statistics/issues"
  :depends-on (#:alexandria
               #:anaphora
               #:let-plus
               #:num-utils
               #:stat-generics)
  :pathname "src/"
  :components ((:file "streaming"))
  :in-order-to ((test-op (test-op "statistics/tests"))))

(defsystem "statistics/batch"
  :name        "Batch statistics"
  :version     "1.4.0"
  :license     :MS-PL
  :author      "Steve Nunez <steve@symbolics.tech>"
  :description "Descriptive statistics for in-memory sequences and arrays."
  :long-description  #.(uiop:read-file-string
                        (uiop:subpathname *load-pathname* "batch-description.text"))
  :homepage    "https://lisp-stat.dev/"
  :source-control (:git "https://github.com/Lisp-Stat/statistics.git")
  :bug-tracker    "https://github.com/Lisp-Stat/statistics/issues"
  :depends-on (#:alexandria
               #:let-plus
               #:num-utils
               #:stat-generics)
  :pathname "src/"
  :components ((:file "batch"))
  :in-order-to ((test-op (test-op "statistics/tests"))))

(defsystem "statistics"
  :name        "Statistics functions"
  :version     "1.4.0"
  :license     :MS-PL
  :author      "Steve Nunez <steve@symbolics.tech>"
  :long-name   "Common Lisp statistical functions for streaming data"
  :description "A consolidated system of statistical functions for streaming data"
  :long-description  #.(uiop:read-file-string
                        (uiop:subpathname *load-pathname* "description.text"))
  :homepage    "https://lisp-stat.dev/"
  :source-control (:git "https://github.com/Lisp-Stat/statistics.git")
  :bug-tracker    "https://github.com/Lisp-Stat/statistics/issues"
  :depends-on ("statistics/streaming"
               "statistics/batch")
  :in-order-to ((test-op (test-op "statistics/tests"))))

(defsystem "statistics/tests"
  :version     "1.1.0"
  :description "Unit tests for the statistics system"
  :author      "Steve Nunez <steve@symbolics.tech>"
  :license     :MS-PL
  :depends-on  ("statistics/streaming"
                "statistics/batch"
		"stat-generics"
                "num-utils"
                "clunit2")
  :serial t
  :pathname "tests/"
  :components ((:file "tstpkg")
               (:file "main")
               (:file "streaming")
               (:file "batch"))
  :perform (test-op (o s)
             (uiop:symbol-call :statistics-tests :run-tests)))
