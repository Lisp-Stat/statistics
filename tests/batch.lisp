;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: STATISTICS-TESTS -*-
;;; Copyright (c) 2026 by Symbolics Pte. Ltd. All rights reserved.

(in-package #:statistics-tests)

(defsuite batch (tests))


;;; ---------------------------------------------------------------------------
;;; Mean
;;; ---------------------------------------------------------------------------

(deftest test-batch-mean (batch)
  ;; Simple integer sequences
  (assert-equalp 2   (stat-generics:mean #(0 1 2 3 4)))
  (assert-equalp 4   (stat-generics:mean #(0 1 2 3 4 5 6 7 8)))
  ;; Single element
  (assert-equalp 7   (stat-generics:mean #(7)))
  ;; Known float result
  (assert-equality #'num= 2.5d0 (stat-generics:mean #(1d0 2d0 3d0 4d0))))

(deftest test-batch-weighted-mean (batch)
  ;; Equal weights should give same result as unweighted
  (assert-equality #'num=
      (stat-generics:mean #(1d0 2d0 3d0))
      (stat-generics:mean #(1d0 2d0 3d0)
                          :weights (make-instance 'stat-generics:frequency-weights
                                                  :values #(1 1 1))))
  ;; Verify by hand: (1*1 + 2*2 + 3*3) / (1+2+3) = 14/6
  (assert-equality #'num= 14/6
      (stat-generics:mean #(1d0 2d0 3d0)
                          :weights (make-instance 'stat-generics:frequency-weights
                                                  :values #(1 2 3)))))


;;; ---------------------------------------------------------------------------
;;; Variance
;;; ---------------------------------------------------------------------------

(deftest test-batch-variance (batch)
  ;; Population variance (corrected nil): known results for iota sequences
  (assert-equality #'num= 2      (stat-generics:variance #(0 1 2 3 4) :corrected nil))
  ;; Sample variance (corrected t, default): (n/(n-1)) * population variance
  (assert-equality #'num= 15/2   (stat-generics:variance (coerce (iota 9)  'vector)))
  (assert-equality #'num= 35     (stat-generics:variance (coerce (iota 20) 'vector)))
  ;; Two identical elements — population variance is 0
  (assert-equality #'num= 0      (stat-generics:variance #(5 5) :corrected nil)))

(deftest test-batch-variance-consistency (batch)
  ;; Variance should equal square of standard-deviation
  (let* ((v   #(1d0 2d0 3d0 4d0 5d0))
         (var (stat-generics:variance v))
         (sd  (stat-generics:standard-deviation v)))
    (assert-equality #'num= var (* sd sd))))

(deftest test-batch-weighted-variance (batch)
  ;; Equal frequency weights should give same result as unweighted (corrected)
  (let* ((v  #(1d0 2d0 3d0 4d0 5d0))
         (fw (make-instance 'stat-generics:frequency-weights :values #(1 1 1 1 1))))
    (assert-equality #'num=
        (stat-generics:variance v)
        (stat-generics:variance v :weights fw)))
  ;; Hand-computed: data #(1 3), fweights #(1 3)
  ;; weighted mean = (1*1 + 3*3)/(1+3) = 10/4 = 5/2
  ;; ss = 1*(1-5/2)^2 + 3*(3-5/2)^2 = 1*(9/4) + 3*(1/4) = 12/4 = 3
  ;; correction-denominator (fweights, n=2) = sum-w - 1 = 4 - 1 = 3
  ;; weighted variance = 3/3 = 1
  (assert-equality #'num= 1
      (stat-generics:variance #(1d0 3d0)
                              :weights (make-instance 'stat-generics:frequency-weights
                                                      :values #(1 3)))))


;;; ---------------------------------------------------------------------------
;;; Standard deviation
;;; ---------------------------------------------------------------------------

(deftest test-batch-sd (batch)
  ;; sd is sqrt of variance
  (assert-equality #'num=
      (sqrt (stat-generics:variance #(2d0 4d0 4d0 4d0 5d0 5d0 7d0 9d0)))
      (stat-generics:standard-deviation #(2d0 4d0 4d0 4d0 5d0 5d0 7d0 9d0)))
  ;; Classic textbook example: population sd of #(2 4 4 4 5 5 7 9) = 2
  ;; (mean=5, SS=32, population variance=32/8=4, sd=2)
  ;; Must pass :corrected nil — the default is the sample (corrected) sd.
  (assert-equality #'num= 2d0
      (stat-generics:standard-deviation #(2d0 4d0 4d0 4d0 5d0 5d0 7d0 9d0)
                                        :corrected nil)))


;;; ---------------------------------------------------------------------------
;;; Quantile and quantiles
;;; ---------------------------------------------------------------------------

(deftest test-batch-quantile (batch)
  ;; min and max
  (assert-equality #'num= 0d0 (stat-generics:quantile #(0d0 1d0) 0))
  (assert-equality #'num= 1d0 (stat-generics:quantile #(0d0 1d0) 1))
  ;; median of two elements — 0.5-correction interpolates to 0.5
  (assert-equality #'num= 0.5d0 (stat-generics:quantile #(0d0 1d0) 1/2))
  ;; should match online-stats for same input and quantile probabilities
  (let* ((sample   #(0.0d0 1.0d0))
         (qs       (numseq 0 1 :length 11 :type 'double-float))
         (expected #(0.0 0.0 0.0 0.1 0.3 0.5 0.7 0.9 1.0 1.0 1.0)))
    (assert-equality #'num= expected
        (stat-generics:quantiles sample qs))))

(deftest test-batch-quantile-probabilities (batch)
  ;; round-trip: quantiles at empirical probabilities should recover the sorted sample
  (let* ((n      10)
         (sample (sort (aops:generate (lambda () (random (* n 2))) n) #'<)))
    (assert-equalp sample
        (stat-generics:quantiles sample
                                 (empirical-quantile-probabilities (length sample))))))


;;; ---------------------------------------------------------------------------
;;; Interquartile range
;;; These functions live in batch-statistics, not stat-generics.
;;; ---------------------------------------------------------------------------

(deftest test-interquartile-range (batch)
  ;; IQR of numseq 0..1 length 11, using the 0.5-correction empirical-quantile:
  ;; q25 = 0.225, q75 = 0.775, IQR = 0.55
  (let ((sample (numseq 0 1 :length 11 :type 'double-float)))
    (assert-equality #'num= 0.55d0
        (batch-statistics:interquartile-range sample)))
  ;; IQR of a constant sequence is 0
  (assert-equality #'num= 0
      (batch-statistics:interquartile-range #(5 5 5 5 5))))


;;; ---------------------------------------------------------------------------
;;; Fivenum
;;; ---------------------------------------------------------------------------

(deftest test-fivenum (batch)
  ;; numseq 0..1 length 11 = #(0.0 0.1 ... 1.0)
  ;; 0.5-correction quantiles: q25=0.225, q50=0.5, q75=0.775
  (let* ((sample  (numseq 0 1 :length 11 :type 'double-float))
         (summary (batch-statistics:fivenum sample)))
    ;; min
    (assert-equality #'num= 0d0    (aref summary 0))
    ;; q25 — interpolated per 0.5-correction algorithm
    (assert-equality #'num= 0.225d0 (aref summary 1))
    ;; median
    (assert-equality #'num= 0.5d0  (aref summary 2))
    ;; q75 — interpolated per 0.5-correction algorithm
    (assert-equality #'num= 0.775d0 (aref summary 3))
    ;; max
    (assert-equality #'num= 1d0    (aref summary 4))))

(deftest test-fivenum-tukey (batch)
  (let* ((sample  #(1d0 2d0 3d0 4d0 5d0))
         (summary (batch-statistics:fivenum sample :tukey t)))
    (assert-equality #'num= 1d0 (aref summary 0))  ; min
    (assert-equality #'num= 2d0 (aref summary 1))  ; lower hinge
    (assert-equality #'num= 3d0 (aref summary 2))  ; median
    (assert-equality #'num= 4d0 (aref summary 3))  ; upper hinge
    (assert-equality #'num= 5d0 (aref summary 4)))) ; max


;;; ---------------------------------------------------------------------------
;;; Scale
;;; ---------------------------------------------------------------------------

(deftest test-scale (batch)
  ;; Default: center and scale — result has mean~0 and sd~1
  (let* ((v      #(1d0 2d0 3d0 4d0 5d0))
         (scaled (batch-statistics:scale v)))
    (assert-equality #'num= 0d0 (stat-generics:mean scaled))
    (assert-equality #'num= 1d0 (stat-generics:standard-deviation scaled)))
  ;; With scale= explicit divisor, center=nil — only scaling applied
  (let* ((v      #(2d0 4d0 6d0))
         (scaled (batch-statistics:scale v :center nil :scale 2d0)))
    (assert-equality #'num= #(1d0 2d0 3d0) scaled))
  ;; With scale=nil — only centering, sd unchanged
  (let* ((v      #(1d0 2d0 3d0))
         (scaled (batch-statistics:scale v :scale nil)))
    (assert-equality #'num= 0d0 (stat-generics:mean scaled)))
  ;; Both nil — identity, returns original vector
  (let ((v #(1d0 2d0 3d0)))
    (assert-equalp v (batch-statistics:scale v :center nil :scale nil))))


;;; ---------------------------------------------------------------------------
;;; Mode and modes
;;; ---------------------------------------------------------------------------

(deftest test-mode (batch)
  ;; Clear single mode
  (assert-equalp 3 (stat-generics:mode #(1 2 3 3 3 4)))
  ;; Tie — first in order of appearance wins (documented contract)
  (assert-equalp 1 (stat-generics:mode #(1 2 1 2)))
  ;; Single element
  (assert-equalp 7 (stat-generics:mode #(7)))
  ;; Works on lists too (sequence method)
  (assert-equalp 'a (stat-generics:mode '(a b a c a b))))

(deftest test-modes (batch)
  ;; Single mode
  (assert-equalp '(3) (stat-generics:modes #(1 2 3 3 3)))
  ;; Bimodal — both returned in order of first appearance
  (assert-equalp '(1 2) (stat-generics:modes #(1 2 1 2)))
  ;; All distinct — all are modes
  (assert-equalp '(1 2 3) (stat-generics:modes #(1 2 3)))
  ;; Works on lists
  (assert-equalp '(a b) (stat-generics:modes '(a b a b c))))
