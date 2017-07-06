;; This library is licensed under the GNU LGPL v3.0.
;; Copyright Andrew Young, 2017

;;;;; Sorting Algorithms ;;;;;

(defpackage :vaelen-sort
  (:use :common-lisp)
  (:export :radix-sort
           :merge-sort))

(in-package :vaelen-sort)

;; Sorts 64bit integers using MSD radix sort
(defun radix-sort (lst &optional (predicate #'<=) (pos 63))
  (cond
    ((< pos 0) lst)
    ((not lst) lst)
    ((not (rest lst)) lst)
    (t
     (let ((b0 '())
           (b1 '()))
       (dolist (i lst)
               (cond
                 ((= 0 (ldb (byte 1 pos) i)) (setf b0 (cons i b0)))
                 (t (setf b1 (cons i b1)))))
       (append (radix-sort b0 predicate (- pos 1))
               (radix-sort b1 predicate (- pos 1)))))))

;; Sorts values uses merge sort
(defun merge-sort (lst &optional (predicate #'<=))
  (cond
    ((not lst) lst)
    ((not (rest lst)) lst)
    (t
     (let ((mid (ceiling (length lst) 2)))
       (merge
        'list
        (merge-sort (subseq lst 0 mid) predicate)
        (merge-sort (subseq lst mid nil) predicate)
        predicate)))))
