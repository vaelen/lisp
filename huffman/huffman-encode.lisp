#!/usr/bin/cl --quicklisp --entry huffman-encode:main --system unix-opts --load huffman.lisp

;;(load "~/quicklisp/setup.lisp")
;;(ql:quickload "unix-opts")

(defpackage :huffman-encode
  (:use :common-lisp
        :huffman)
  
  (:export :main))

(in-package :huffman-encode)

(defun unknown-option (condition)
  (format t "warning: ~s option is unknown!~%" (opts:option condition))
  (invoke-restart 'opts:skip-option))

(defmacro when-option ((options opt) &body body)
  `(let ((it (getf ,options ,opt)))
     (when it
       ,@body)))

(defun main (argv)
  (let ((input-stream *standard-input*)
        (output-stream *standard-output*)
        (block-size huffman:*block-size*)
        (verbose nil))

    
    (opts:define-opts
        (:name :input
               :description "read input from file FILE (defaults to standard input)"
               :short #\i
               :long "input"
               :arg-parser #'identity
               :meta-var "FILE")
        (:name :output
               :description "redirect output to file FILE (defaults to standard output)"
               :short #\o
               :long "output"
               :arg-parser #'identity
               :meta-var "FILE")
        (:name :block-size
               :description (format nil "set block size to BYTES (defaults to ~a)" *block-size*)
               :short #\b
               :long "block-size"
               :arg-parser #'parse-integer
               :meta-var "BYTES")
        (:name :print
               :description "print information about each block (verbose output will print the mapping)"
               :short #\p
               :long "print")
        (:name :verbose
               :description "be more verbose"
               :short #\v
               :long "verbose")
        (:name :help
               :description "print this help text"
               :short #\h
               :long "help"))

    (multiple-value-bind (options free-args)
        (handler-case
            (handler-bind ((opts:unknown-option #'unknown-option))
              (opts:get-opts))
          (opts:missing-arg (condition)
            (format t "fatal: option ~s needs an argument!~%"
                    (opts:option condition)))
          (opts:arg-parser-failed (condition)
            (format t "fatal: cannot parse ~s as argument of ~s~%"
                    (opts:raw-arg condition)
                    (opts:option condition)))
          (opts:missing-required-option (con)
            (format t "fatal: ~a~%" con)
            (opts:exit 1)))

      (when-option (options :help)
                   (opts:describe
                    :prefix "Compresses data using huffman encoding."
                    :usage-of "huffman-encode"
                    :args     "[FREE-ARGS]")
                   (opts:exit 1))
      (when-option (options :block-size)
                   (setf block-size it))
      (when-option (options :input)
                   (setf input-stream (pathname (getf options :input))))
      (when-option (options :output)
                   (setf output-stream (pathname (getf options :output))))
      (when-option (options :verbose)
                   (setf verbose 't))
      (when-option (options :print)
                   (huffman:print-info input-stream :block-size block-size :verbose verbose)
                   (opts:exit 0)))

    (flet ((f ()
             (encode input-stream output-stream :block-size block-size :verbose verbose)))
      (if verbose (time (f)) (f)))
    (opts:exit 0)))

