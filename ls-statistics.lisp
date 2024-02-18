;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;; Copyright (c) 2022,2024 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL

(uiop:define-package #:ls.statistics
  (:use #:cl #:let-plus)
  (:import-from #:nu.statistics
		#:quantiles
		#:ensure-sorted-reals
		#:sorted-reals-elements
		#:sd)
  (:import-from #:alexandria #:random-elt #:shuffle #:if-let)
  (:export #:interquartile-range
	   #:fivenum
	   #:mean
	   #:scale
	   #:variance))
	   ;; #:quantile))

(in-package #:ls.statistics)

(defun interquartile-range (x)
  "Returns the interquartile range of the elements of X."
  (reduce #'- (quantiles x '(0.75 0.25))))

(defun fivenum (x &key (tukey nil))
  "By default, returns the five number summary (min, 1st quartile, median, 3rd quartile, max) of the elements X.
   If the keyword :tukey is set to a non-nil value, Tukey's fivenum summary is computed instead."
  (if tukey
      (let+ ((#(mn md mx) (quantiles x '(0 0.5 1)))
	     (sorted (sorted-reals-elements (ensure-sorted-reals x)))
	     (l (length sorted))
	     (left (subseq sorted 0 (floor (/ (1+ l) 2))))
	     (right (subseq sorted (floor (/ l 2)) l))
	     (hinge-left (nu.statistics:median left))
	     (hinge-right (nu.statistics:median right)))
        (vector mn hinge-left md hinge-right mx))
      (quantiles x '(0 0.25 0.5 0.75 1))))


(defun mean (object &key weights)
  "Return the mean of OBJECT. OBJECT must be either a sequence of numbers, a sequence of BOOLEAN or a DISTRIBUTION object.

A sequence of BOOLEAN is converted to a BIT-VECTOR and the mean of it returned.  This gives you the ratio of TRUE/FALSE values in the sequence (which is most often interpreted as a probability).

For samples (numeric-vectors), normalized by the weight-1 (and thus unbiased if certain assumptions hold, eg weights that count frequencies."
  ;; If optimisation is needed, See: https://hal.archives-ouvertes.fr/hal-01770939/
  (typecase object
    (distributions::r-univariate   (distributions:mean object))
    (nu:simple-fixnum-vector       (nu.statistics:mean object :weights weights))
    (nu:simple-double-float-vector (nu.statistics:mean object :weights weights))
    (nu:simple-single-float-vector (nu.statistics:mean object :weights weights))
    (nu:simple-boolean-vector      (nu.statistics:mean (nu:as-bit-vector object)))
    (t (alexandria:mean object))))	;prefer alexandria because we want to remove redundant functions from LH stats

(defun variance (object &key weights (biased? nil))
  "Variance of OBJECT.  For samples, normalized by the weight-1 (and thus unbiased if certain assumptions hold, e.g. weights that count frequencies).

Note that alexandria's default for variance will return biased variance.  We change that here for consistency.  If you want a biased variance use alexandria:variance directly."
  (typecase object
    (distributions::r-univariate   (distributions:variance object))
    (nu:simple-fixnum-vector       (nu.statistics:variance object :weights weights))
    (nu:simple-double-float-vector (nu.statistics:variance object :weights weights))
    (nu:simple-single-float-vector (nu.statistics:variance object :weights weights))
    (nu:simple-boolean-vector      (nu.statistics:variance (nu:as-bit-vector object)))
    (t (alexandria:variance object :biased biased?))))	;no bias to be consistent with nu.statistics

;; Should quantile be empirical-quantile or quantile?  Probably nu.statistics should be updated to use distributions.  If so, what about mean and variance?


;; TODO: test for scale = T and then use root-mean-square
;; Note: the dispatch on distributions has not been tested
(defun scale (x &key (center (mean x)) (scale (sd x)))
  "Return (x - x̄) / s where X̄ is the mean and S is the standard deviation

Modeled on the R scale function.
See https://stat.ethz.ch/R-manual/R-devel/library/base/html/scale.html
"
  (typecase x
    (distributions::r-rayleigh (distributions:scale x))
    (distributions::r-t (distributions:scale x))
    (t (when (and (not center) (not scale))
	 (return-from scale x))
     (if (not center)
	 (setf center 0))
     (if (not scale)
	 (setf scale 1))
     (values (nu:e/ (nu:e- x center)
		    scale)
	     center scale))))

