(load "~/quicklisp/setup.lisp")
(asdf:load-system 'cl-heap)

(defpackage :huffman
  (:use :common-lisp)
  (:export :encode
           :test)
  (:import-from :cl-heap
                :fibonacci-heap
                :add-to-heap
                :pop-heap
                :heap-size
                :peep-at-heap))

(in-package :huffman)

(defparameter *chunk-size* 4096)

(defun count-characters (char-list &optional counts)
  (cond ((not counts)
         (count-characters char-list (make-hash-table :test #'equal)))
        ((not (listp char-list))
         (count-characters (coerce char-list 'list) counts))
        ((car char-list)
         (let* ((char (car char-list))
                (count (gethash char counts)))
           (if (not count)
               (setf (gethash char counts) 1)
               (incf (gethash char counts)))
           (count-characters (cdr char-list) counts)))
        (t counts)))

(defun print-hash-table (h)
  (mapc #'(lambda (x) (princ x))
        (maphash #'(lambda (key val)
                     (format t "~A ~A~%" key val)) h)))

(defun counts-to-queue (counts)
  (let ((queue (make-instance 'fibonacci-heap :key #'first)))
    (maphash #'(lambda (key value) (add-to-heap queue (list value key))) counts)
    queue))

(defun build-tree (queue)
  (if (> (heap-size queue) 1)
      (let ((left (pop-heap queue))
            (right (pop-heap queue)))
        (add-to-heap queue (list (+ (first left) (first right)) left right))
        (build-tree queue))
      (pop-heap queue)))

(defun find-in-tree (tree char &optional prefix)
  (let ((left (second tree))
        (right (third tree)))
    (if (not (listp left))
        (if (equal char left)
            (reverse prefix)
            'nil)
        (or (find-in-tree left char (cons 0 prefix))
            (find-in-tree right char (cons 1 prefix))))))

(defmethod encode ((buffer sequence))
  (let* ((buf (coerce buffer 'list))
         (counts (count-characters buf))
         (queue (counts-to-queue counts))
         (tree (build-tree queue)))
    (print-hash-table counts)
    (pprint queue)
    (pprint tree)
    (pprint (find-in-tree tree '#\p))
    tree))

(defmethod encode ((input stream))
   (let ((buffer (make-array *chunk-size* :fill-pointer t)))
     (loop
        (setf (fill-pointer buffer) (read-sequence buffer input))
        (when (zerop (fill-pointer buffer)) (return))
        (encode buffer))))

(defun test ()
  (with-open-file (input "huffman.lisp" :direction :input)
    (encode input)))


