#!/usr/bin/cl -E main

(load "huffman.lisp")

(defun main (argv)
  (let ((input-stream *standard-input*)
        (output-stream *standard-output*))
    (encode input-stream output-stream)
    (quit)))
