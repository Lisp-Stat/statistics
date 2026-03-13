;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: ORG.TFEB.CLC-USER -*-
;;; Copyright (c) 2022,2023,2026 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL
(in-package :org.tfeb.clc-user)

;;; Top level package

(defpackage :statistics
  (:use #:let-plus)
  (:extends :ls.statistics)
  (:extends/excluding #:online-stats
		      #:mean
		      #:variance)



