(defconstant ipsum (concatenate 'string
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod "
  "tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim "
  "veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea "
  "commodo consequat. Duis aute irure dolor in reprehenderit in voluptate "
  "velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat "
  "cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id "
  "est laborum."))

(defconstant example
  (concatenate
   'string
   "aaaaa"
   "bbbbbbbbb"
   "cccccccccccc"
   "ddddddddddddd"
   "eeeeeeeeeeeeeeee"
   "fffffffffffffffffffffffffffffffffffffffffffff"))

(defparameter *buffer-size* 4096)

(defstruct node (score 0) value left right)

(defun node-lt (first second)
  (< (node-score first) (node-score second)))

(defun count-chars-h (char-list &optional (counts (make-hash-table)))
  (loop for c in char-list do (incf (gethash c counts 0)))
  counts)

(defun count-chars (text)
  (let ((counts-hash (count-chars-h (coerce text 'list)))
        (counts '()))
    (flet ((add (key value) (push (make-node :score value :value key) counts)))
      (maphash #'add counts-hash)
      (sort counts #'node-lt))))

(defun build-tree (counts)
  (loop while (> (length counts) 1)
     do (let ((left (pop counts))
              (right (pop counts)))
          (push (make-node :score (+ (node-score left) (node-score right))
                           :left left
                           :right right)
                counts)
          (setf counts (sort counts #'node-lt)))
     finally (return (first counts))))

(defun tree-to-hash (node &optional (value '(1)) (hash (make-hash-table)))
  (when node
    (if (node-value node)
        (setf (gethash (node-value node) hash) value))
    (tree-to-hash (node-left node) (append value '(0)) hash)
    (tree-to-hash (node-right node) (append value '(1)) hash))
  hash)

(defun encode-bits (text)
  (let ((encoding (tree-to-hash (build-tree (count-chars text)))))
    (apply #'append 
           (loop for c in (coerce text 'list)
              collect (gethash c encoding)))))

(defun bits-to-byte-list (bits)
  (append
   (loop while (> (length bits) 8)
      collect (let ((this-byte (subseq bits 0 8)))
                (setf bits (subseq bits 8))
                this-byte))
   (list bits)))

(defun byte-list-to-bytes (byte-list)
  (loop for b in byte-list collect
       (let ((n 0)
             (bit 7))
         (loop for value in b
            do (progn
                 (setf (ldb (byte 1 bit) n) value)
                 (decf bit))
            finally (return n)))))

(defun encode-chunk (chunk output-stream)
  (write-sequence
   (byte-list-to-bytes
    (bits-to-byte-list
     (encode-bits chunk)))
   output-stream))

(defun encode (input-stream output-stream)
  (let ((buffer (make-array *buffer-size*
                            :adjustable nil
                            :element-type '(unsigned-byte 8))))
    (loop for pos = (read-sequence buffer input-stream)
       while (plusp pos)
        do (encode-chunk (subseq buffer 0 pos) output-stream))))

(defun test ()
  (let* ((text example)
         (tree (build-tree (count-chars text)))
         (encoding (tree-to-hash tree))
         (byte-list (bits-to-byte-list (encode-bits text))))
    (format *standard-output* "~&Tree:")
    (pprint tree)
    (format 't "~&~%")
    (format 't "~&Mapping:")
    (loop for k being the hash-keys in encoding using (hash-value v)
       do (format 't "~&~t~a: ~a" k v))
    (format 't "~&")
    (pprint (encode-bits text))
    (format 't "~&")
    (pprint byte-list)
    (format 't "~&")
    (pprint (byte-list-to-bytes byte-list))
    (format 't "~&")))
