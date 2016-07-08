(defpackage :huffman
  (:use :common-lisp)
  (:export :encode
           :encode-chunk
           :test
           :bit-vector-to-integer)
  (:import-from :cl-heap
                :fibonacci-heap
                :add-to-heap
                :pop-heap
                :heap-size
                :peep-at-heap))

(in-package :huffman)

;;(defparameter *chunk-size* (expt 2 12)) ; 4k
(defparameter *chunk-size* (expt 2 16)) ; 64k
(defparameter *bit-buffer-size* (expt 2 12)) ; 4k

(defstruct huffman-tree (tree))

(defun count-characters (char-list &optional counts)
  (cond ((not counts)
         (count-characters char-list (make-hash-table :test #'equal)))
        ((not (listp char-list))
         (count-characters (coerce char-list 'list) counts))
        ((first char-list)
         (let* ((char (first char-list))
                (count (gethash char counts)))
           (if (not count)
               (setf (gethash char counts) 1)
               (incf (gethash char counts)))
           (count-characters (rest char-list) counts)))
        (t counts)))

(defun print-hash-table (h)
  (fresh-line)
  (maphash #'(lambda (key value)
               (prin1 key)
               (princ " : ")
               (prin1 value)
               (princ '#\newline))
           h))

(defun counts-to-queue (counts)
  (let ((queue (make-instance 'fibonacci-heap :key #'first)))
    (maphash #'(lambda (key value) (add-to-heap queue (list value key))) counts)
    queue))

(defun build-tree (queue)
  (if (> (heap-size queue) 1)
      (let ((left (pop-heap queue))
            (right (pop-heap queue)))
        (add-to-heap queue (list (+ (first left) (first right)) (rest left) (rest right)))
        (build-tree queue))
      (rest (pop-heap queue))))

(defun find-in-tree (tree char &optional prefix)
  (let ((left (first tree))
        (right (second tree)))
    (if (not (listp left))
        (if (equal char left)
            (reverse prefix)
            'nil)
        (or (find-in-tree left char (cons 0 prefix))
            (find-in-tree right char (cons 1 prefix))))))

(defun find-all-in-tree (tree &optional prefix)
  (let ((left (first tree))
        (right (second tree)))
    (if (not (listp left))
        (list (list left (reverse prefix)))
        (concatenate 'list (find-all-in-tree left (cons 0 prefix))
                     (find-all-in-tree right (cons 1 prefix))))))

(defun build-char-map-from-tree (tree)
  (tree-to-hash
   (find-all-in-tree tree)
   (make-hash-table :test #'equal)))

(defun tree-to-hash (tree hash)
  (cond ((not tree) hash)
        (t (let ((entry (first tree)))
             (setf (gethash (first entry) hash)
                 (coerce (second entry) 'bit-vector))
             (tree-to-hash (rest tree) hash)))))

(defun bit-vector-to-integer (bits)
  (cond ((not bits) 'nil)
        ((> (length bits) 8)
         (concatenate 'list
                      (bit-vector-to-integer (subseq bits 0 8))
                      (bit-vector-to-integer (subseq bits 8))))
        ((< (length bits) 8)
         (let ((x (make-array 8 :element-type 'bit)))
           (setf (subseq x 0 (length bits)) bits)
           (bit-vector-to-integer x)))
        (t
         (list (reduce #'(lambda (a b) (+ (ash a 1) b)) bits)))))

(defun vector-push-all (collection vector)
  (loop for x in (coerce collection 'list)
     do (vector-push x vector)))

(defun write-chunk-header (tree bytes)
  ;; TODO: Write out a binary version of the tree
  (declare (ignore tree bytes))
)

(defun encode-chunk (buffer)
  (let* ((tree (build-tree (counts-to-queue
                            (count-characters
                             (coerce buffer 'list)))))
         (char-map (build-char-map-from-tree tree))
         (bit-buffer (make-array *bit-buffer-size*
                                 :element-type 'bit
                                 :fill-pointer 0))
         (byte-buffer))
    (princ '#\#)
    (write-chunk-header tree byte-buffer)
    (loop for char in (coerce buffer 'list) do
         (let ((bits (gethash char char-map))
               (space-left (- (array-total-size bit-buffer)
                              (length bit-buffer))))
           (cond ((< space-left (length bits)) ; This add will cause a buffer overflow
                  (vector-push-all (subseq bits 0 space-left) bit-buffer) ; Fill buffer
                  (setf byte-buffer (concatenate 'list byte-buffer
                               (bit-vector-to-integer bit-buffer))) ; Convert buffer to bytes
                  (setf (fill-pointer bit-buffer) 0) ; Reset buffer
                  (vector-push-all (subseq bits space-left) bit-buffer)) ; Add remaining bits
                 (t
                  (vector-push-all bits bit-buffer))))
       finally (setf byte-buffer (concatenate 'list
                                              byte-buffer
                                              (bit-vector-to-integer bit-buffer))))
    byte-buffer))

(defmethod encode ((input stream) (output stream))
  (let ((buffer (make-array *chunk-size* :fill-pointer t)))
     (loop
        (setf (fill-pointer buffer) (read-sequence buffer input))
        (when (zerop (fill-pointer buffer)) (return))
        (write-sequence (encode-chunk buffer) output))))

(defmethod encode ((input pathname) (output pathname))
  (with-open-file (input-stream input
                         :direction :input
                         :element-type '(unsigned-byte 8))
    (with-open-file (output-stream "out.bin"
                            :direction :output
                            :if-exists :supersede
                            :element-type '(unsigned-byte 8))
      (encode input-stream output-stream)
      (fresh-line))))

(defmethod encode ((input string) (output string))
  (encode (pathname input) (pathname output)))

(defun test ()
  (encode "in.bin" "out.bin"))
