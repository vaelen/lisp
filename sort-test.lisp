;; This library is licensed under the GNU LGPL v3.0.
;; Copyright Andrew Young, 2017

;;;;; Test Functions ;;;;;

(defpackage :vaelen-sort-test
  (:use :common-lisp
        :vaelen-sort)
  (:export :test-all-sorts
           :test-sort
           :generate-integers
           :list-is-sorted?
           :test-radix-sort
           :test-merge-sort))

(in-package :vaelen-sort-test)

;; Generate some random 64bit integers
(defun generate-integers (n)
  (let ((lst '()))
    (dotimes (i n)
             (setf lst (cons (random #xffffffff) lst)))
    lst))

;; Test that a list is sorted
(defun list-is-sorted? (lst)
  (let ((last-value 0)
        (list-size 0))
    (dolist (value lst)
            (setf list-size (+ list-size 1))
            (cond
              ((< value last-value)
               (format t "Failed: ~a < ~a~%" value last-value)
               (setf last-value nil)
               (return))
              (t (setf last-value value))))
    (cond
      ((null last-value) nil)
      (t (format t "Success: ~a Items~%" list-size)
         t))))

;; Test a given sort function
(defun test-sort (&key (sort-function #'merge-sort)
                       ((:list lst) '() list-supplied-p)
                       (size 0 size-supplied-p)
                       (predicate #'<=))
  (cond
    ((and list-supplied-p (not size-supplied-p))
     (setf size (length lst)))
    ((and size-supplied-p (not list-supplied-p))
     (setf lst (generate-integers size))))
  (format t "Sort Algorithm: ~s, List Size: ~s~%" sort-function (length lst))
  (time (setf lst (funcall sort-function lst predicate)))
  (list-is-sorted? lst))

;; Test the radix-sort function
(defun test-radix-sort (size)
  (test-sort :sort-function #'radix-sort :size size))

  ;; Test the merge-sort function
(defun test-merge-sort (size)
  (test-sort :sort-function #'merge-sort :size size))

(defun test-all-sorts (size)
  (let ((lst (generate-integers size)))
    (test-sort :sort-function #'sort :list lst)
    (test-sort :sort-function #'stable-sort :list lst)
    (test-sort :sort-function #'radix-sort :list lst)
    (test-sort :sort-function #'merge-sort :list lst)
    ))
