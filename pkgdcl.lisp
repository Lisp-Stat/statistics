;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: ORG.TFEB.CLC-USER -*-
;;; Copyright (c) 2022 by Symbolics Pte. Ltd. All rights reserved.
(in-package :org.tfeb.clc-user)

;;; Top level package

;;; Versioned using the mechanism suggested in defpackage+
;;; https://github.com/rpav/defpackage-plus#versioning

(defpackage :statistics-1
  (:use)
  (:extends/excluding :lh.statistics
		      #:sd
		      #:standard-deviation
		      #:mean
		      #:variance
		      #:median
		      #:square)
  (:extends/excluding :nu.statistics
		      #:mean
		      #:variance)
  #+ignore
  (:extends/including :cl-mathstats
		      #:anova-one-way-variables
		      #:anova-two-way-variables)
  (:extends :ls.statistics))

