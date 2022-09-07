;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;; Copyright (c) 2022 by Symbolics Pte. Ltd. All rights reserved.

(uiop:define-package #:statistics-tests
    (:use #:cl
	  #:alexandria
	  #:nu.statistics
	  #:ls.statistics
	  #:num-utils
	  #:let-plus
	  #:clunit)
  (:shadowing-import-from #:nu.statistics #:mean #:variance #:median #:quantile))

