;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;; Copyright (c) 2022,2023,2026 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL
(in-package :org.tfeb.clc-user)

(uiop:define-package "statistics"
  (:use #:cl #:let-plus)
  (:export #:mean
	   #:variance))
