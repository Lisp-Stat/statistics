;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: ORG.TFEB.CLC-USER -*-
;;; Copyright (c) 2022,2023 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL
(in-package :org.tfeb.clc-user)

;;; Top level package

;;; Versioned using the mechanism suggested in defpackage+
;;; https://github.com/rpav/defpackage-plus#versioning

(defpackage :statistics-1
  (:use #:let-plus)
  (:extends :ls.statistics)
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
		      #:anova-two-way-variables))


