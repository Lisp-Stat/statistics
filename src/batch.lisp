;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;; Copyright (c) 2022, 2024-2026 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL

;;; Design rule: this file owns all STAT-GENERICS methods specialised on CL
;;; collection types (SEQUENCE, VECTOR, LIST, ARRAY).  Methods on accumulator
;;; types (central-sample-moments, sorted-reals, etc.) belong in
;;; statistics/streaming.
;;;
;;; Specialising on SEQUENCE rather than VECTOR means lists are handled
;;; without a separate method.  AS-VECTOR coerces internally where random
;;; access or LOOP ACROSS is required.

(uiop:define-package #:batch-statistics
  (:use #:cl #:let-plus #:alexandria+)
  (:import-from #:stat-generics
                #:weight-sum
                #:weight-values
                #:correction-denominator)
  (:export #:interquartile-range
           #:fivenum
           #:scale
           #:sd
           #:weighted-mean
           #:weighted-variance))

(in-package #:batch-statistics)


;;; Internal helpers

(defun as-vector (sequence)
  "Return SEQUENCE as a vector, coercing only if necessary."
  (if (vectorp sequence)
      sequence
      (coerce sequence 'vector)))

(defun sorted-vector (sequence)
  "Return a freshly sorted copy of SEQUENCE as a vector."
  (sort (copy-seq (as-vector sequence)) #'<))


;;; Weighted statistics helpers
;;; These operate on a vector and a WEIGHTS object (from stat-generics).

(defun weighted-mean (vec w)
  "Compute Σ(wᵢ·xᵢ) / Σwᵢ."
  (let ((wv (weight-values w))
        (sv (as-vector vec)))
    (assert (= (length sv) (length wv)) ()
            "Data length ~A ≠ weight length ~A." (length sv) (length wv))
    (/ (loop for xi across sv
             for wi across wv
             sum (* wi xi))
       (weight-sum w))))

(defun weighted-variance (vec w &key (corrected t) mean)
  "Compute weighted variance, dispatching the correction denominator onto W's type."
  (let* ((sv (as-vector vec))
         (n  (length sv))
         (wv (weight-values w))
         (m  (or mean (weighted-mean sv w)))
         (ss (loop for xi across sv
                   for wi across wv
                   sum (* wi (expt (- xi m) 2)))))
    (if corrected
        (/ ss (correction-denominator w n))
        (/ ss (weight-sum w)))))


;;; Quantile helpers

(defun empirical-quantile (sorted-vec q)
  "Empirical quantile of SORTED-VEC at probability Q.  Uses a 0.5 correction.
SORTED-VEC must already be sorted in ascending order."
  (let* ((n (length sorted-vec))
         (c (/ 1/2 n)))
    (cond
      ((or (< q 0) (< 1 q)) (error "Quantile ~A is not in [0,1]." q))
      ((<= q c) (aref sorted-vec 0))
      ((<= (- 1 c) q) (aref sorted-vec (1- n)))
      (t (let* ((r    (- (* q n) 1/2))
                (int  (floor r))
                (frac (- r int))
                (left (aref sorted-vec int)))
           (if (zerop frac)
               left
               (+ (* (- 1 frac) left)
                  (* frac (aref sorted-vec (1+ int))))))))))

(defun batch-quantiles (sequence qs)
  "Compute quantiles QS of SEQUENCE.  Sorts once and maps."
  (let ((sv (sorted-vector sequence)))
    (map 'vector (lambda (q) (empirical-quantile sv q)) qs)))


;;; stat-generics methods on SEQUENCE
;;; These are the authoritative implementations for fully-realised data.

(defmethod stat-generics:mean ((seq sequence) &key weights &allow-other-keys)
  "Arithmetic mean of SEQ.  When WEIGHTS is a WEIGHTS object, returns the
weighted mean."
  (if weights
      (weighted-mean seq weights)
      (/ (reduce #'+ seq) (length seq))))

(defmethod stat-generics:variance ((seq sequence)
                                   &key weights (corrected t) mean
                                   &allow-other-keys)
  "Variance of SEQ.

With CORRECTED T (default) returns the sample variance normalised by n-1 (or
the appropriate weighted correction denominator).  With CORRECTED NIL returns
the population variance normalised by n (or Σw).

When WEIGHTS is supplied it must be a WEIGHTS object of matching length."
  (let* ((vec (as-vector seq))
         (n   (length vec))
         (m   (or mean (stat-generics:mean vec :weights weights))))
    (if weights
        (weighted-variance vec weights :corrected corrected :mean m)
        (let ((ss (loop for xi across vec sum (expt (- xi m) 2))))
          (if corrected
              (/ ss (1- n))
              (/ ss n))))))

(defmethod stat-generics:standard-deviation ((seq sequence)
                                             &key weights (corrected t) mean
                                             &allow-other-keys)
  "Standard deviation of SEQ.  Same keyword semantics as VARIANCE."
  (sqrt (stat-generics:variance seq
                                :weights   weights
                                :corrected corrected
                                :mean      mean)))

(defmethod stat-generics:quantiles ((seq sequence) qs
                                    &key weights &allow-other-keys)
  "Quantiles QS of SEQ.  WEIGHTS is not yet supported for batch quantiles."
  (when weights
    (error "Weighted quantiles on plain sequences are not yet implemented ~
            in batch-statistics.  Use a SORTED-REALS accumulator from ~
            statistics/online instead."))
  (batch-quantiles seq qs))

(defmethod stat-generics:quantile ((seq sequence) q
                                   &key weights &allow-other-keys)
  "Quantile Q of SEQ."
  (when weights
    (error "Weighted quantiles on plain sequences are not yet implemented ~
            in batch-statistics.  Use a SORTED-REALS accumulator from ~
            statistics/online instead."))
  (empirical-quantile (sorted-vector seq) q))


;;; Descriptive functions
;;; Plain functions (not generic) for summaries without a natural generic home.

(defun sd (object &rest keys &key &allow-other-keys)
  "Alias for STAT-GENERICS:STANDARD-DEVIATION."
  (apply #'stat-generics:standard-deviation object keys))

(defun interquartile-range (x)
  "Interquartile range of X: Q75 - Q25."
  (let ((qs (batch-quantiles x #(0.25 0.75))))
    (- (aref qs 1) (aref qs 0))))


(defun fivenum (x &key tukey)
  "Five-number summary of X: #(min q25 median q75 max).

With TUKEY non-nil uses Tukey's hinges (lower/upper halves split at the
median) instead of the default 0.5-correction empirical quantiles."
  (if tukey
      (let* ((sv    (sorted-vector x))
             (n     (length sv))
             (md    (empirical-quantile sv 1/2))
             (left  (subseq sv 0 (floor (/ (1+ n) 2))))
             (right (subseq sv (floor (/ n 2)) n)))
        (vector (aref sv 0)
                (empirical-quantile left  1/2)
                md
                (empirical-quantile right 1/2)
                (aref sv (1- n))))
      (batch-quantiles x #(0 0.25 0.5 0.75 1))))

(defun scale (x &key (center (stat-generics:mean x))
                     (scale  (sd x)))
  "Standardise X: return (x - center) / scale.  Modelled on R's scale().

CENTER defaults to the mean of X; SCALE defaults to the standard deviation.
Pass NIL to suppress centering or scaling independently.  Both NIL returns X."
  (when (and (not center) (not scale))
    (return-from scale x))
  (let ((c (or center 0))
        (s (or scale  1)))
    (map 'vector (lambda (xi) (/ (- xi c) s)) x)))


;;; stat-generics mode/modes methods on SEQUENCE

(defmethod stat-generics:mode ((sequence sequence) &key &allow-other-keys)
  "Most frequent element of SEQUENCE.  Returns the first mode in order of
appearance when ties exist."
  (let ((table        (make-hash-table :test #'equal))
        (best-element nil)
        (best-count   0))
    (map nil (lambda (x)
               (let ((count (incf (gethash x table 0))))
                 (when (> count best-count)
                   (setf best-count   count
                         best-element x))))
         sequence)
    best-element))

(defmethod stat-generics:modes ((sequence sequence) &key &allow-other-keys)
  "All most-frequent elements of SEQUENCE, in order of first appearance."
  (let ((table      (make-hash-table :test #'equal))
        (order      '())
        (best-count 0))
    (map nil (lambda (x)
               (unless (gethash x table)
                 (push x order))
               (let ((count (incf (gethash x table 0))))
                 (when (> count best-count)
                   (setf best-count count))))
         sequence)
    (let ((result '()))
      (dolist (x (reverse order))
        (when (= (gethash x table) best-count)
          (push x result)))
      (nreverse result))))


;;; Helpful shortcuts
(defalias iqr interquartile-range)

