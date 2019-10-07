(defpackage :huffman
  (:use :common-lisp)
  (:export :encode
           :*block-size*
           :print-mapping
           :test))

(in-package :huffman)

(defparameter *block-size* 1024)

(defparameter *end-of-block* 256)

(defstruct node (score 0) value left right)

(defun node-lt (first second)
  (< (node-score first) (node-score second)))

(defun count-chars (char-list &optional (counts (make-hash-table)))
  "Counts characters using a hash table"
  (setf (gethash *end-of-block* counts) 1)
  (loop for c in char-list do (incf (gethash c counts 0)))
  counts)

(defun build-tree (text)
  "Build a Huffman Tree from the given input"
  (let ((counts-hash (count-chars (coerce text 'list)))
        (counts '()))
    (flet ((add (key value) (push (make-node :score value :value key) counts)))
      (maphash #'add counts-hash)
      (sort counts #'node-lt))
    (loop while (> (length counts) 1)
       do (let ((left (pop counts))
                (right (pop counts)))
            (push (make-node :score (+ (node-score left) (node-score right))
                             :left left
                             :right right)
                  counts)
            (setf counts (sort counts #'node-lt)))
       finally (return (first counts)))))

(defun tree->huffman-mapping (node &optional (value '()) (hash (make-hash-table)))
  "Convert a Huffman Tree to a character -> bit sequence mapping using the simple method."
  (when node
    (if (node-value node)
        (setf (gethash (node-value node) hash) value))
    (tree->huffman-mapping (node-left node) (append value '(0)) hash)
    (tree->huffman-mapping (node-right node) (append value '(1)) hash))
  hash)

(defun tree->code-length-mapping (node &optional (depth 0) (hash (make-hash-table)))
  "Convert a Huffman Tree to a mapping of character -> code length"
  (when node
    (if (node-value node)
        (setf (gethash (node-value node) hash) depth))
    (tree->code-length-mapping (node-left node) (+ depth 1) hash)
    (tree->code-length-mapping (node-right node) (+ depth 1) hash))
  hash)

(defun encode-bits (text encoding)
  (apply #'append 
         (loop for c in (coerce text 'list)
            collect (gethash c encoding))))

(defun bits->byte-list (bits)
  (append
   (loop while (> (length bits) 8)
      collect (let ((this-byte (subseq bits 0 8)))
                (setf bits (subseq bits 8))
                this-byte))
   (list bits)))

(defun byte-list->bytes (byte-list)
  (loop for b in byte-list collect
       (let ((n 0)
             (bit 7))
         (loop for value in b
            do (progn
                 (setf (ldb (byte 1 bit) n) value)
                 (decf bit))
            finally (return n)))))

(defun byte->bits (n)
  (if (characterp n)
      (byte->bits (char-code n))
      (loop for i from 7 downto 0 collect (ldb (byte 1 i) n))))

(defun header->bits (node)
  (if (node-p node)
      (append '(1)
              (if (node-value node)
                  (append `(1) (byte->bits (node-value node)))
                  `(0))
              (header->bits (node-left node))
              (header->bits (node-right node)))
      '(0)))

(defun encode-block (block output-stream)
  (let ((tree (build-tree block))
        (mapping (tree->huffman-mapping tree)))
    (write-sequence
     (byte-list->bytes
      (bits->byte-list
       (append
        (header->bits tree)
        (encode-bits block mapping)
        (gethash *end-of-block* mapping))))
     output-stream)))

(defun encode (input-stream output-stream &key (block-size *block-size*))
  (cond
    ;; If the input is a path, open the file and recurse
    ((pathnamep input-stream)
     (with-open-file (in input-stream
                         :direction :input
                         :element-type '(unsigned-byte 8))
       (encode in output-stream :block-size block-size)))
    ;; If the output is a path, open the file and recurse
    ((pathnamep output-stream)
     (with-open-file (out output-stream
                          :direction :output
                          :if-exists :supersede
                          :element-type '(unsigned-byte 8))
       (encode input-stream out :block-size block-size)))

    ;; Otherwise, create a temporary buffer and encode into it
    (t
     (let ((buffer (make-array block-size
                               :adjustable nil
                               :element-type '(unsigned-byte 8))))
       (loop for pos = (read-sequence buffer input-stream)
          while (plusp pos)
          do (encode-block (subseq buffer 0 pos) output-stream))))))

(defun print-mapping-helper (text &optional (block 0))
  (let* ((tree (build-tree text))
         (mapping (tree->huffman-mapping tree))
         (keys (loop for k being the hash-keys in mapping collect k)))
    (sort keys #'<)
    (format 't "~& Block ~a" block)
    (format 't "~&Mapping:")
    (loop for k in keys
       do (format 't "~&~t~a: ~a" k (gethash k mapping)))
    (format 't "~&")))

(defun print-mapping (input-stream &key (block-size *block-size*))
  (cond
    ;; If the input is a path, open the file and recurse
    ((pathnamep input-stream)
     (with-open-file (in input-stream
                         :direction :input
                         :element-type '(unsigned-byte 8))
       (print-mapping  in :block-size block-size)))

    ;; Otherwise, create a temporary buffer and print the huffman codes for each block
    (t
     (let ((buffer (make-array block-size
                               :adjustable nil
                               :element-type '(unsigned-byte 8)))
           (block 0))
       (loop for pos = (read-sequence buffer input-stream)
          while (plusp pos)
          do (print-mapping-helper (subseq buffer 0 pos)))))))

