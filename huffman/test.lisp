(in-package :huffman)

(defparameter example1
  (concatenate
   'string
   "aaaaa"
   "bbbbbbbbb"
   "cccccccccccc"
   "ddddddddddddd"
   "eeeeeeeeeeeeeeee"
   "fffffffffffffffffffffffffffffffffffffffffffff"
   "ABCDEABCDECDEE"))

(defparameter example2
  (concatenate
   'string
   example1
   example1))

(defparameter example3
  (concatenate
   'string
   example1
   example1
   example1))

(defun test-tree-helper (s n)
  (format 't "~&~%~a~&" s)
  (let* ((data (string->bytes s))
         (encoder (data->encoder data n)))
    (print-info-helper encoder t)))

(defun test-tree ()
  (test-tree-helper example1 1)
  (test-tree-helper example2 2)
  (test-tree-helper example3 3)
  (format 't "~&~%Length of texts: ~a ~a ~a~&"
          (length example1)
          (length example2)
          (length example3)))

(defun test ()
  (time
   (let* ((data (string->bytes example1))
          (encoder (data->encoder data 1))
          (tree (encoder-tree encoder))
          (code-lengths (tree->code-length-mapping tree))
          (header (header->bits code-lengths))
          (bits (data->bits data (encoder-mapping encoder)))
          (eob (end-of-block->bits (encoder-mapping encoder)))
          (byte-list (bits->byte-list (append header bits eob)))
          (bytes (byte-list->bytes byte-list)))
     (print-info-helper encoder t)
     (format 't "~&~%Header:")
     (pprint header)
     (format 't "~&~%Data:")
     (pprint bits)
     (format 't "~&~%End of Block:")
     (pprint eob)
     (format 't "~&~%Byte List:")
     (pprint byte-list)
     (format 't "~&~%Bytes:")
     (pprint bytes)
     (format 't "~&~%"))))
