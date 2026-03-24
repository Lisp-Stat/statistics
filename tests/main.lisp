;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: STATISTICS-TESTS -*-
;;; Copyright (c) 2022 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:statistics-tests)

#+genera (setf *print-array* t)

;;; Root suite
(defsuite tests ())

;;; Test runner
(defun run-tests (&optional (report-progress t))
  "Run all statistical test suites. Returns the clunit-report object."
  (let ((*print-pretty* t)
        (clunit:*test-output-stream* *standard-output*))
    (unwind-protect
         (run-suite 'tests :report-progress report-progress))))

