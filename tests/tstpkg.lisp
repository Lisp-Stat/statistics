;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;; Copyright (c) 2022,2026 by Symbolics Pte. Ltd. All rights reserved.

(uiop:define-package #:statistics-tests
  (:use #:cl
        #:clunit
	#:let-plus
        #:online-statistics
        #:num-utils.arithmetic
	#:num-utils.num=)
  (:import-from #:alexandria #:iota #:curry #:length=)
  (:shadowing-import-from #:stat-generics #:quantile #:quantiles)
  ;; (:shadow #:mean
  ;;          #:variance
  ;;          #:median)
  ;; (:import-from #:batch-statistics
  ;;               #:interquartile-range
  ;;               #:fivenum
  ;;               #:scale
  ;;               #:sd)
  )
