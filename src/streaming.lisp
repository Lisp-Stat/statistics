;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;; Copyright (c) 2022, 2026 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL

;;; Design rule: every STAT-GENERICS method defined in this file must be
;;; specialised on a type that this file itself defines (CENTRAL-SAMPLE-MOMENTS,
;;; SORTED-REALS, SPARSE-COUNTER).  Methods on CL collection types (SEQUENCE,
;;; VECTOR, LIST, ARRAY) belong in statistics/batch.

(uiop:define-package #:streaming-statistics
  (:use #:cl
        #:anaphora
        #:num-utils.arithmetic
        #:num-utils.num=
        #:num-utils.utilities
        #:let-plus)
  (:import-from #:alexandria
                #:curry
                #:length=
                #:copy-sequence
                #:hash-table-alist
                #:hash-table-keys
                #:parse-body           ; used by define-central-sample-moment
                #:lerp                 ; used by empirical-quantile
                #:non-negative-real-p) ; used by sparse-counter add method

  ;; Import the stat-generics generic functions that this package adds
  ;; accumulator-type methods to.  Importing (rather than qualifying) means
  ;; (defmethod mean ...) in this file extends stat-generics:mean, not a
  ;; separate streaming-statistics-local generic.  These symbols are re-exported
  ;; so that (streaming-statistics:mean acc) and (stat-generics:mean acc) are
  ;; identical calls.
  (:import-from #:stat-generics
                #:mean
                #:variance
                #:standard-deviation
                #:quantile
                #:quantiles)
  (:export
   ;; Accumulator protocol
   #:tally
   #:add
   #:pool
   #:empty-accumulator
   #:not-enough-elements-in-accumulator
   #:information-not-collected-in-accumulator
   ;; Central moments accumulator
   #:central-sample-moments
   #:central-sample-moments-degree
   #:*central-sample-moments-default-degree*
   ;; stat-generics functions re-exported for convenience
   #:mean
   #:variance
   #:standard-deviation
   ;; Higher-order moments (online-statistics-local generics, no batch equivalent)
   #:central-m2
   #:central-m3
   #:central-m4
   #:skewness
   #:kurtosis
   ;; Sorted-reals accumulator
   #:sorted-reals
   #:sorted-reals-elements
   #:empirical-quantile
   #:empirical-quantile-probabilities
   ;; stat-generics quantile functions re-exported
   #:quantile
   #:quantiles
   #:ensure-sorted-reals
   #:ensure-sorted-vector
   #:weighted-quantiles
   ;; Sparse counters
   #:make-sparse-counter
   #:sparse-counter
   #:sparse-counter-count
   #:sparse-counter-table
   #:tabulate
   #:cross-tabulate))

(in-package #:streaming-statistics)

;;; -------------------------------------------------------------------------
;;; Generic accumulator interface
;;; tally, add, pool are online-statistics-local generics: they have no
;;; meaning in the batch world and are not part of stat-generics.
;;; -------------------------------------------------------------------------

(defgeneric tally (accumulator)
  (:documentation "The total weight of elements in ACCUMULATOR."))

(defgeneric add (accumulator object &key)
  (:documentation "Add OBJECT to ACCUMULATOR.  Return OBJECT.  NIL is ignored unless a specialised method decides otherwise.  Keywords may carry additional information such as :weight.")
  (:method (accumulator (object null) &key)
    object))

(defgeneric pool2 (accumulator1 accumulator2)
  (:documentation "Pool two accumulators.  When types differ the result is downgraded to the information available in both accumulators."))

(defun pool (&rest accumulators)
  "Pool ACCUMULATORS."
  (reduce #'pool2 accumulators))

(define-condition empty-accumulator (error) ())
(define-condition not-enough-elements-in-accumulator (error) ())
(define-condition information-not-collected-in-accumulator (error) ())

(defstruct tally-mixin
  "Mixin that holds a total weight W.  Not exported."
  (w 0 :type (real 0)))

(defmethod tally ((accumulator tally-mixin))
  (tally-mixin-w accumulator))


;;; -------------------------------------------------------------------------
;;; Central sample moments
;;; One-pass, numerically stable computation following Bennett et al. (2009)
;;; and Pébay (2008).  M_2...M_4 in those papers correspond to s2...s4 here.
;;; -------------------------------------------------------------------------

(defstruct (central-sample-moments (:include tally-mixin))
  "Central sample moments calculated on-line/single-pass.

   M   weighted mean
   S2  weighted sum of squared deviations from the mean, NIL if not collected
   S3  weighted sum of cubed deviations from the mean, NIL if not collected
   S4  weighted sum of 4th-power deviations from the mean, NIL if not collected"
  (m  0d0 :type real)
  (s2 0d0 :type (or (real 0) null))
  (s3 0d0 :type (or real null))
  (s4 0d0 :type (or (real 0) null)))

(define-structure-num= central-sample-moments w m s2 s3 s4)

(defmethod add ((moments central-sample-moments) (y real) &key (weight 1))
  (assert (<= 0 weight) () "Algorithm is only stable with nonnegative weights.")
  (when (plusp weight)
    (let+ ((y (coerce y 'double-float))
           ((&structure central-sample-moments- (wa w) m s2 s3 s4) moments)
           (d (- y m))
           (w (+ wa weight))
           (d/w (/ d w))
           (d-weighted (* d/w weight)))
      (incf m d-weighted)
      (when s2
        (let ((s2-increment (* d-weighted wa d)))
          (when s3
            (let* ((x1 (* 3 d-weighted s2))
                   (x2 (* d/w s2-increment))
                   (s3-increment (- (* x2 (- wa weight)) x1)))
              (when s4
                (incf s4 (+ (* -4 s3 d-weighted)
                            (* 2 x1 d-weighted)
                            (* x2 d/w (+ (expt weight 2) (* wa (- wa weight)))))))
              (incf s3 s3-increment)))
          (incf s2 s2-increment)))
      (setf wa w)))
  y)

(defmethod pool2 ((moments-a central-sample-moments)
                  (moments-b central-sample-moments))
  (let+ (((&structure-r/o central-sample-moments- (wa w) (ma m) (s2a s2)
                          (s3a s3) (s4a s4)) moments-a)
         ((&structure-r/o central-sample-moments- (wb w) (mb m) (s2b s2)
                          (s3b s3) (s4b s4)) moments-b)
         (w   (+ wa wb))
         (wab (* wa wb))
         (d   (- mb ma))
         (d^2 (expt d 2))
         (pa  (coerce (/ wa w) 'double-float))
         (pb  (coerce (/ wb w) 'double-float))
         (m   (+ ma (* pb d)))
         (s2  (when (and s2a s2b)
                (+ s2a s2b (* d^2 (/ wab w)))))
         (s3  (when (and s2 s3a s3b)
                (+ s3a s3b
                   (* (expt d 3) (/ (* wab (- wa wb)) (expt w 2)))
                   (* 3 (- (* pa s2b) (* pb s2a)) d))))
         (s4  (when (and s3 s4a s4b)
                (+ s4a s4b
                   (* (expt d 4)
                      (/ (* wab (- (+ (expt wa 2) (expt wb 2)) wab))
                         (expt w 3)))
                   (* 6 d^2
                      (+ (* (expt pa 2) s2b)
                         (* (expt pb 2) s2a)))
                   (* 4 d (- (* pa s3b) (* pb s3a)))))))
    (make-central-sample-moments :w w :m m :s2 s2 :s3 s3 :s4 s4)))

(defun central-sample-moments-degree (central-sample-moments)
  "Return the degree of CENTRAL-SAMPLE-MOMENTS."
  (let+ (((&structure-r/o central-sample-moments- s2 s3 s4)
          central-sample-moments))
    (cond (s4 4) (s3 3) (s2 2) (t 1))))

(defparameter *central-sample-moments-default-degree* 4
  "Default degree for (weighted) central sample moments.")

(defgeneric central-sample-moments (object &key degree weights)
  (:documentation "Return a CENTRAL-SAMPLE-MOMENTS object accumulating central sample moments of OBJECT up to DEGREE.  When WEIGHTS is given it must be a sequence of matching length.")
  (:method ((object null)
            &key (degree *central-sample-moments-default-degree*) weights)
    (check-type degree (integer 1 4))
    (assert (not weights))
    (make-central-sample-moments :s2 (when (<= 2 degree) 0d0)
                                 :s3 (when (<= 3 degree) 0d0)
                                 :s4 (when (<= 4 degree) 0d0)))
  (:method ((moments central-sample-moments)
            &key (degree (central-sample-moments-degree moments) degree?)
                 weights)
    (assert (or (not degree?)
                (<= degree (central-sample-moments-degree moments)))
            () 'information-not-collected-in-accumulator)
    (assert (not weights))
    moments)
  (:method ((sequence sequence)
            &key (degree *central-sample-moments-default-degree*) weights)
    (if weights
        (aprog1 (central-sample-moments nil :degree degree)
          (assert (length= sequence weights))
          (map nil (lambda (e w) (add it e :weight w)) sequence weights))
        (aprog1 (central-sample-moments nil :degree degree)
          (map nil (curry #'add it) sequence)))))


;;; -------------------------------------------------------------------------
;;; define-central-sample-moment macro
;;;
;;; Generates a single method specialised on CENTRAL-SAMPLE-MOMENTS.
;;; The t-fallback that previously auto-generated a sequence→CSM conversion
;;; path has been removed: batch owns all sequence specialisations.
;;; Higher-order moments (central-m2 etc.) that have no batch equivalent
;;; define their own t-fallback explicitly below.
;;; -------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-central-sample-moment (function (variable degree) &body body)
    "Define a method for FUNCTION specialised on CENTRAL-SAMPLE-MOMENTS.

FUNCTION must be an already-existing generic (either from stat-generics or
a local defgeneric).  DEGREE (1-4) documents which moments are required.
Re-weighting a pre-built accumulator is an error."
    (check-type degree (integer 1 4))
    (let+ (((&values remaining-forms declarations docstring)
            (parse-body body :documentation t))
           (body (append declarations remaining-forms)))
      `(defmethod ,function ((,variable central-sample-moments)
                             &key weights &allow-other-keys)
         ,@(when docstring (list docstring))
         (assert (not weights) () "You can't re-weight an accumulator.")
         ,@body))))


;;; -------------------------------------------------------------------------
;;; stat-generics methods on CENTRAL-SAMPLE-MOMENTS
;;; mean, variance, standard-deviation — symbols imported from stat-generics.
;;; -------------------------------------------------------------------------

(define-central-sample-moment mean (object 1)
  "Mean of the accumulated elements."
  (central-sample-moments-m object))

(define-central-sample-moment variance (object 2)
  "Sample variance (normalised by weight-1) of the accumulated elements."
  (let+ (((&structure-r/o central-sample-moments- w s2) object))
    (assert s2 () 'information-not-collected-in-accumulator)
    (assert (plusp w) () 'empty-accumulator)
    (assert (< 1 w) () 'not-enough-elements-in-accumulator)
    (/ s2 (1- w))))

(defmethod standard-deviation ((object central-sample-moments)
                               &key weights &allow-other-keys)
  "Standard deviation of the accumulated elements."
  (assert (not weights) ()
          "Cannot re-weight a central-sample-moments accumulator.")
  (sqrt (variance object)))


;;; -------------------------------------------------------------------------
;;; Online-statistics-local higher-order moment generics
;;; These have no stat-generics equivalent and no batch implementation.
;;; The t-fallback converts any sequence to a CSM accumulator on demand,
;;; which is appropriate because there is no other owner for these functions.
;;; -------------------------------------------------------------------------

(defgeneric central-m2 (object &key weights &allow-other-keys)
  (:documentation "Second central moment (population, normalised by total weight).")
  (:method ((object t) &key weights &allow-other-keys)
    (central-m2 (central-sample-moments object :degree 2 :weights weights))))

(defgeneric central-m3 (object &key weights &allow-other-keys)
  (:documentation "Third central moment.")
  (:method ((object t) &key weights &allow-other-keys)
    (central-m3 (central-sample-moments object :degree 3 :weights weights))))

(defgeneric central-m4 (object &key weights &allow-other-keys)
  (:documentation "Fourth central moment.")
  (:method ((object t) &key weights &allow-other-keys)
    (central-m4 (central-sample-moments object :degree 4 :weights weights))))

(defgeneric skewness (object &key weights &allow-other-keys)
  (:documentation "Skewness.")
  (:method ((object t) &key weights &allow-other-keys)
    (skewness (central-sample-moments object :degree 3 :weights weights))))

(defgeneric kurtosis (object &key weights &allow-other-keys)
  (:documentation "Kurtosis.")
  (:method ((object t) &key weights &allow-other-keys)
    (kurtosis (central-sample-moments object :degree 4 :weights weights))))

(define-central-sample-moment central-m2 (object 2)
  "Second central moment, normalised by total weight (not unbiased; see VARIANCE)."
  (let+ (((&structure-r/o central-sample-moments- w s2) object))
    (assert s2 () 'information-not-collected-in-accumulator)
    (assert (plusp w) () 'empty-accumulator)
    (/ s2 w)))

(define-central-sample-moment central-m3 (object 3)
  "Third central moment."
  (let+ (((&structure-r/o central-sample-moments- w s3) object))
    (assert s3 () 'information-not-collected-in-accumulator)
    (assert (plusp w) () 'empty-accumulator)
    (/ s3 w)))

(define-central-sample-moment central-m4 (object 4)
  "Fourth central moment."
  (let+ (((&structure-r/o central-sample-moments- w s4) object))
    (assert s4 () 'information-not-collected-in-accumulator)
    (assert (plusp w) () 'empty-accumulator)
    (/ s4 w)))

(define-central-sample-moment skewness (object 3)
  "Skewness."
  (/ (central-m3 object)
     (expt (central-m2 object) 3/2)))

(define-central-sample-moment kurtosis (object 4)
  "Kurtosis."
  (/ (central-m4 object)
     (expt (central-m2 object) 2)))


;;; -------------------------------------------------------------------------
;;; SORTED-REALS accumulator
;;; An accumulator that lazily sorts its elements for quantile computation.
;;; stat-generics:quantile and stat-generics:quantiles are specialised here
;;; on SORTED-REALS, which is a type this file defines — within the design rule.
;;; -------------------------------------------------------------------------

(defstruct sorted-reals
  "Accumulator that collects real numbers and sorts them on demand."
  (ordered-elements   #() :type vector)
  (unordered-elements nil :type list))

(define-structure-let+ (sorted-reals) ordered-elements unordered-elements)

(defmethod add ((accumulator sorted-reals) object &key)
  (push object (sorted-reals-unordered-elements accumulator)))

(defun sorted-reals-elements (sorted-reals)
  "Return the elements of SORTED-REALS as a sorted vector, merging any
pending unordered elements first."
  (let+ (((&sorted-reals ordered-elements unordered-elements) sorted-reals))
    (when unordered-elements
      (setf ordered-elements (sort (concatenate 'vector ordered-elements
                                                unordered-elements) #'<)
            unordered-elements nil))
    ordered-elements))

(defmethod print-object ((acc sorted-reals) stream)
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (acc stream :type t)
        (let+ (((&accessors-r/o (elements sorted-reals-elements)) acc))
          (if (plusp (length elements))
              (format stream "min: ~A, q25: ~A, q50: ~A, q75: ~A, max: ~A"
                      (aref elements 0)
                      (quantile acc 0.25)
                      (quantile acc 0.5)
                      (quantile acc 0.75)
                      (aref elements (1- (length elements))))
              (format stream "no elements"))))))

(defun sort-reals (sequence)
  "Return a SORTED-REALS built from SEQUENCE."
  (make-sorted-reals :ordered-elements (sort (copy-sequence 'vector sequence) #'<)
                     :unordered-elements nil))

(defgeneric ensure-sorted-reals (object)
  (:documentation "Return the contents of OBJECT as a SORTED-REALS.")
  (:method ((sorted-reals sorted-reals)) sorted-reals)
  (:method ((array array))   (sort-reals (aops:flatten array)))
  (:method ((list list))     (sort-reals list)))

(defun ensure-sorted-vector (object)
  "Return the elements of OBJECT as a sorted vector."
  (sorted-reals-elements (ensure-sorted-reals object)))

(defun empirical-quantile (sorted-vector q)
  "Empirical quantile of SORTED-VECTOR at probability Q.  Uses a 0.5 correction.
SORTED-VECTOR must be sorted in ascending order (not checked)."
  (let+ ((n (length sorted-vector))
         (c (/ 1/2 n)))
    (cond
      ((or (< q 0) (< 1 q)) (error "Quantile ~A is not in [0,1]." q))
      ((<= q c) (aref sorted-vector 0))
      ((<= (- 1 c) q) (aref sorted-vector (1- n)))
      (t (let+ ((r (- (* q n) 1/2))
                ((&values int frac) (floor r))
                (left (aref sorted-vector int)))
           (if (zerop frac)
               left
               (lerp frac left (aref sorted-vector (1+ int)))))))))

(defun empirical-quantile-probabilities (n)
  "Probabilities corresponding to empirical quantiles of a vector of length N.
That is, (quantiles sample (empirical-quantile-probabilities (length sample)))
recovers SAMPLE for any sorted vector."
  (numseq (/ (* 2 n)) nil :length n :by (/ n) :type 'rational))

;;; stat-generics methods on SORTED-REALS — within design rule.

(defmethod quantiles ((accumulator sorted-reals) qs
                      &key weights &allow-other-keys)
  (if weights
      (weighted-quantiles (sorted-reals-elements accumulator) weights qs)
      (map 'vector
           (curry #'empirical-quantile (sorted-reals-elements accumulator))
           qs)))

(defmethod quantile ((accumulator sorted-reals) q
                     &key weights &allow-other-keys)
  (if weights
      (aref (weighted-quantiles (sorted-reals-elements accumulator)
                                weights (vector q)) 0)
      (aref (quantiles accumulator (vector q)) 0)))


;;; -------------------------------------------------------------------------
;;; Weighted quantiles
;;; A standalone function for use with the sorted-reals methods above.
;;; Exported for users who construct their own sorted value/weight pairs.
;;; -------------------------------------------------------------------------

(defun weighted-quantile-p-table (weights)
  "Return probability-bracket table for weighted quantile calculations.
Uses a 0.5 correction."
  (aprog1 (aops:make-array-like weights)
    (loop with sum = (sum weights)
          with cumulative-sum = 0
          for index from 0
          for w across weights
          do (setf (aref it index) (/ (+ cumulative-sum (/ w 2)) sum))
             (incf cumulative-sum w))))

(defun weighted-empirical-quantile (sorted-reals p-table q)
  "Weighted empirical quantile using a pre-built probability table."
  (let+ ((p-index (binary-search p-table q)))
    (cond
      ((or (< q 0) (< 1 q)) (error "Quantile ~A is not in [0,1]." q))
      ((not p-index) (aref sorted-reals 0))
      ((eq p-index t) (aref sorted-reals (1- (length sorted-reals))))
      (t (let+ ((p-left (aref p-table p-index))
                (left   (aref sorted-reals p-index)))
           (if (= p-left q)
               left
               (lerp (/ (- q p-left)
                        (- (aref p-table (1+ p-index)) p-left))
                     left
                     (aref sorted-reals (1+ p-index)))))))))

(defun weighted-quantiles (values weights qs)
  "Compute quantiles QS of weighted observations VALUES/WEIGHTS.
Uses a 0.5 correction."
  (let* ((pairs          (sort (map 'vector #'cons values weights) #'<= :key #'car))
         (sorted-vals    (map 'vector #'car pairs))
         (sorted-weights (map 'vector #'cdr pairs))
         (p-table        (weighted-quantile-p-table sorted-weights)))
    (map 'vector (lambda (q) (weighted-empirical-quantile sorted-vals p-table q)) qs)))


;;; -------------------------------------------------------------------------
;;; Sparse counters
;;; -------------------------------------------------------------------------

(defstruct (sparse-counter (:constructor make-sparse-counter%))
  (table nil :type hash-table :read-only t))

(defun make-sparse-counter (&key (test #'equal))
  "Create a sparse counter using TEST for element equality."
  (make-sparse-counter% :table (make-hash-table :test test)))

(defmethod add ((accumulator sparse-counter) object &key (weight 1))
  "Increment the count of OBJECT in ACCUMULATOR by WEIGHT."
  (assert (non-negative-real-p weight) () "Weight must be nonnegative.")
  (incf (gethash object (sparse-counter-table accumulator) 0) weight)
  object)

(defmethod tally ((accumulator sparse-counter))
  "Total weight accumulated."
  (let ((sum 0))
    (maphash (lambda (object count)
               (declare (ignore object))
               (incf sum count))
             (sparse-counter-table accumulator))
    sum))

(defmethod as-alist ((object sparse-counter))
  "Return (OBJECT . COUNT) pairs as an alist."
  (hash-table-alist (sparse-counter-table object)))

(defun sparse-counter-count (sparse-counter object)
  "Return the count for OBJECT in SPARSE-COUNTER."
  (gethash object (sparse-counter-table sparse-counter) 0))

(defmethod print-object ((sparse-counter sparse-counter) stream)
  (let+ (((&structure-r/o sparse-counter- table) sparse-counter)
         (varieties (hash-table-count table))
         (alist (sort (as-alist sparse-counter) #'>= :key #'cdr))
         (tally (reduce #'+ alist :key #'cdr))
         ((&values print-length truncated?)
          (num-utils.print-matrix:print-length-truncate varieties)))
    (print-unreadable-object (sparse-counter stream :type t)
      (format stream "tally: ~D, varieties: ~D" tally varieties)
      (loop repeat print-length
            for (object . count) in alist
            do (format stream "~&  ~a  ~d  (~,1f%)" object count
                       (round* (* 100 (/ count tally)) 1/10)))
      (when truncated?
        (format stream "  ~&...")))))

(defun tabulate (sequence &key (test #'equalp))
  "Tabulate SEQUENCE, returning a SPARSE-COUNTER."
  (aprog1 (make-sparse-counter :test test)
    (map nil (curry #'add it) sequence)))

(defun cross-tabulate (sequence1 sequence2 &key (test #'equalp))
  "Cross-tabulate two sequences of equal length, returning a SPARSE-COUNTER
whose keys are (element-from-1 . element-from-2) cons cells."
  (assert (length= sequence1 sequence2))
  (aprog1 (make-sparse-counter :test test)
    (map nil (lambda (s1 s2) (add it (cons s1 s2))) sequence1 sequence2)))


;;; References
;;;
;;; Bennett, J. et al. (2009). Numerically stable, single-pass, parallel
;;; statistics algorithms. IEEE Cluster Computing and Workshops, pp. 1-8.
;;;
;;; West, D.H.D. (1979). Updating mean and variance estimates: An improved
;;; method. Communications of the ACM, 22(9), pp. 532-535.
