(defpackage :huffman
  (:use :common-lisp)
  (:export :encode
           :*block-size*
           :print-info
           :test))

(in-package :huffman)

(defparameter *block-size* 1024)

(defparameter *end-of-block* 256)

(defstruct node (score 0) value left right)

(defstruct encoder counts tree mapping header-bits eob-bits block-number data-length compressed-size)

(defun node-lt (first second)
  (< (node-score first) (node-score second)))

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
  "Convert a Huffman Tree to a mapping of character -> code length"
  (when node
    (if (node-value node)
        (setf (gethash (node-value node) hash) depth))
    (tree->code-length-mapping (node-left node) (+ depth 1) hash)
    (tree->code-length-mapping (node-right node) (+ depth 1) hash))
  hash)

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

(defun end-of-block->bits (mapping)
  (gethash *end-of-block* mapping))

(defun data->encoder (data &optional (block-number 1))
  (let* ((counts (count-chars (coerce data 'list)))
         (tree (counts->tree counts))
         (mapping (tree->huffman-mapping tree))
         (header-bits (header->bits tree))
         (eob-bits (end-of-block->bits mapping))
         (encoder (make-encoder :counts counts
                                :tree tree
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
  (format 't "~&  Compressed Size: ~a bytes" (encoder-compressed-size encoder))
  (format 't "~&  Compression Ratio: ~a%~&" (* (/ (encoder-compressed-size encoder)
                                                     (encoder-data-length encoder))
                                                  100.0)))

(defun print-info-helper (encoder &optional verbose)
  (let ((keys
        (loop
           for k being the hash-keys in (encoder-mapping encoder)
           collect k)))
    (setf keys (stable-sort keys #'<))
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

(defun encode (input-stream output-stream &key (block-size *block-size*) (block-number 1) verbose)
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
          do (encode-block (subseq buffer 0 pos) output-stream block-number verbose))))))
