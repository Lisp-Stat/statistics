;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: STATISTICS-TESTS -*-
;;; Copyright (c) 2022 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:statistics-tests)

#+genera (setf *print-array* t)

(defsuite tests ())

(defun run (&optional interactive?)
  "Run all tests in the test suite."
  (run-suite 'tests :use-debugger interactive?))


