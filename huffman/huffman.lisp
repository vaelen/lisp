;; Copyright 2019 Andrew C. Young

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files
;; (the "Software"), to deal in the Software without restriction,
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software,
;; and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(defpackage :huffman
  (:use :common-lisp)
  (:export :encode
           :*block-size*
           :print-info
           :test
           :test-tree))

(in-package :huffman)

(defparameter *block-size* 1024)

(defparameter *end-of-block* 256)

(defstruct node (score 0) value left right)

(defstruct encoder
  counts
  tree
  code-lengths
  mapping
  header-bits
  eob-bits
  block-number
  data-length
  compressed-size)

(defun node-lt (first second)
  (< (node-score first) (node-score second)))

(defun string->bytes (s)
  (loop
     for c in (coerce s 'list)
     collect (char-code c)))

(defun hash-keys (hash)
  "Returns the keys for the given hash-table"
  (loop for k being the hash-keys in hash collect k))

(defun sorted-hash-keys (hash pred)
  "Returns the keys for the given hash-table sorted according to pred"
  (stable-sort (hash-keys hash) pred))

(defun count-chars (char-list &optional (counts (make-hash-table)))
  "Counts characters using a hash table"
  (loop for c in char-list do (incf (gethash
                                     (if (characterp c)
                                         (char-code c)
                                         c) counts 0)))
  (setf (gethash *end-of-block* counts) 1)
  counts)

(defun counts->tree (counts)
  "Build a Huffman Tree from the given character counts"
  (let ((queue '()))
    (flet ((add (key value) (push (make-node :score value :value key) queue)))
      (maphash #'add counts)
      (setf queue (stable-sort queue #'node-lt)))
    (loop while (> (length queue) 1)
       do (let ((left (pop queue))
                (right (pop queue)))
            (if (and (= (node-score left) (node-score right))
                     (node-value left) (node-value right)
                     (> (node-value left) (node-value right)))
                (let ((temp left))
                  (setf left right)
                  (setf right temp)))
            (push (make-node :score (+ (node-score left) (node-score right))
                             :left left
                             :right right)
                  queue)
            (setf queue (stable-sort queue #'node-lt)))
       finally (return (first queue)))))

(defun tree->huffman-mapping (node &optional (value '()) (hash (make-hash-table)))
  "Convert a Huffman Tree to a character -> bit sequence mapping using the simple method."
  (when node
    (if (node-value node)
        (setf (gethash (node-value node) hash) value))
    (tree->huffman-mapping (node-left node) (append value '(0)) hash)
    (tree->huffman-mapping (node-right node) (append value '(1)) hash))
  hash)

(defun tree->code-length-mapping (node &optional (depth 0) (hash (make-hash-table)))
  "Convert a Huffman Tree to a mapping of code length -> character"
  (when node
    (if (node-value node)
        (setf (gethash depth hash) (cons (node-value node) (gethash depth hash '()))))
    (tree->code-length-mapping (node-left node) (+ depth 1) hash)
    (tree->code-length-mapping (node-right node) (+ depth 1) hash))
  hash)

(defun code-length-mapping->huffman-mapping (clm)
  "Coverts a code length mapping table (length -> list of bytes) into a huffman mapping"
  (let ((hash (make-hash-table))
        (keys (sorted-hash-keys clm #'<))
        (last 1)
        (b 0))
    (loop for k in keys
       do (progn
            (setf b (ash b (- k last)))
            (setf last k)
            (loop for v in (stable-sort (gethash k clm) #'<)
               do (progn
                    (setf (gethash v hash) (byte->bits b k))
                    (incf b)))))
    hash))
    

(defun calculate-compressed-size (encoder)
  (ceiling 
   (+ (length (encoder-header-bits encoder))
      (length (encoder-eob-bits encoder))
      (reduce '+ (loop for k being the hash-keys in (encoder-counts encoder)
                    collect (* (gethash k (encoder-counts encoder))
                               (length (gethash k (encoder-mapping encoder)))))))
   8))

(defun data->bits (text mapping)
  (apply #'append 
         (loop for c in (coerce text 'list)
            collect (gethash c mapping))))

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

(defun byte->bits (n &optional (bits 8))
  (if (characterp n)
      (byte->bits (char-code n))
      (loop for i from (1- bits) downto 0 collect (ldb (byte 1 i) n))))

;; TODO: Change this to match Deflate format
(defun header->bits (code-lengths)
  (let ((hash (make-hash-table))
        (bits '()))
    (loop for k being the hash-keys in code-lengths
       do (loop for v in (gethash k code-lengths)
             do (setf (gethash v hash) k)))
    (loop for c upto 256
       do (if (gethash c hash)
              (setf bits (append bits
                                 (byte->bits c)
                                 (byte->bits (gethash c hash) 5)))))
    bits))

(defun end-of-block->bits (mapping)
  (gethash *end-of-block* mapping))

(defun data->encoder (data &optional (block-number 1))
  (let* ((counts (count-chars (coerce data 'list)))
         (tree (counts->tree counts))
         (code-lengths (tree->code-length-mapping tree))
         (mapping (code-length-mapping->huffman-mapping code-lengths))
         (header-bits (header->bits code-lengths))
         (eob-bits (end-of-block->bits mapping))
         (encoder (make-encoder :counts counts
                                :tree tree
                                :code-lengths code-lengths
                                :mapping mapping
                                :header-bits header-bits
                                :eob-bits eob-bits
                                :block-number block-number
                                :data-length (length data))))
    (setf (encoder-compressed-size encoder)
          (calculate-compressed-size encoder))
    encoder))

(defun print-encoder-data (encoder)
  (format 't "~&Block #~a" (encoder-block-number encoder))
  (format 't "~&  Original Size: ~a bytes" (encoder-data-length encoder))
  (format 't "~&  Compressed Size: ~a bytes (~a%)"
          (encoder-compressed-size encoder)
          (* (/ (encoder-compressed-size encoder)
                (encoder-data-length encoder))
             100.0)))

(defun print-info-helper (encoder &optional verbose)
  (let ((keys (sorted-hash-keys (encoder-mapping encoder) #'<)))
    (print-encoder-data encoder)
    (cond (verbose (format 't "~&  Mapping:")
                   (loop for k in keys
                      do (let* ((v (gethash k (encoder-mapping encoder)))
                                (c (if (and (> k 31) (< k 127))
                                       (code-char k)
                                       k)))
                           (if (= k *end-of-block*) (setf c "EOB"))
                           (format 't "~&    ~a (~a): ~a" k c v)))))
    (format 't "~&")))

(defun print-info (input-stream &key (block-size *block-size*) verbose)
  (cond
    ;; If the input is a path, open the file and recurse
    ((pathnamep input-stream)
     (with-open-file (in input-stream
                         :direction :input
                         :element-type '(unsigned-byte 8))
       (print-info in :block-size block-size :verbose verbose)))

    ;; Otherwise, create a temporary buffer and print the huffman codes for each block
    (t
     (let ((buffer (make-array block-size
                               :adjustable nil
                               :element-type '(unsigned-byte 8)))
           (block-number 0))
       (loop for pos = (read-sequence buffer input-stream)
          while (plusp pos)
          do (print-info-helper
              (data->encoder (subseq buffer 0 pos) (incf block-number))
              verbose))))))

(defun encode-block (data output-stream &optional (block-number 1) verbose)
  (let ((encoder (data->encoder data block-number)))
    (if verbose (print-encoder-data encoder))
    (write-sequence
     (byte-list->bytes
      (bits->byte-list
       (append
        (encoder-header-bits encoder)
        (data->bits data (encoder-mapping encoder))
        (encoder-eob-bits encoder))))
     output-stream)))

(defun encode (input-stream output-stream &key (block-size *block-size*) (block-number 0) verbose)
  (cond
    ;; If the input is a path, open the file and recurse
    ((pathnamep input-stream)
     (with-open-file (in input-stream
                         :direction :input
                         :element-type '(unsigned-byte 8))
       (encode in output-stream :block-size block-size :verbose verbose)))
    ;; If the output is a path, open the file and recurse
    ((pathnamep output-stream)
     (with-open-file (out output-stream
                          :direction :output
                          :if-exists :supersede
                          :element-type '(unsigned-byte 8))
       (encode input-stream out :block-size block-size :verbose verbose)))

    ;; Otherwise, create a temporary buffer and encode into it
    (t
     (let ((buffer (make-array block-size
                               :adjustable nil
                               :element-type '(unsigned-byte 8))))
       (loop for pos = (read-sequence buffer input-stream)
          while (plusp pos)
          do (encode-block (subseq buffer 0 pos) output-stream (incf block-number) verbose))))))
