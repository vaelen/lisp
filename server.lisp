(require "usocket")

(defparameter *remote-host* 'nil)
(defparameter *remote-port* 'nil)

(defparameter *log-stream* *standard-output*)
(defvar *server-thread* 'nil)

(defun log-message (message)
  (let ((current-time (get-universal-time)))
    (format *log-stream* "[~A] ~A~C" current-time message #\newline)
    (force-output *log-stream*)))

(defun user-repl (stream)
  (format stream "Welcome User.~C" #\newline)
  (force-output stream)
  (loop while (open-stream-p stream) do
       (format stream "> ")
       (force-output stream)
       (let ((line (read-line stream)))
         (format stream "Your command was: ~A~C" line #\newline)
         (force-output stream)
         )))

(defun user-tcp-handler (stream)
  (declare (type stream stream))
  (log-message (format 'nil "Port Opened: ~A ~A" *remote-host* *remote-port*))
  ;; TODO : Keep track of threads
  (user-repl stream)
  (log-message (format 'nil "Port Closed: ~A ~A" *remote-host* *remote-port*)))

(defun stop-server ()
  (cond (*server-thread*
         (log-message (format 'nil "Shutting Down Server"))
         (sb-thread:terminate-thread *server-thread*)
         (log-message (format 'nil "Server Shut Down")))
        (t (log-message (format 'nil "Server Is Not Running")))))
         

(defun start-server (host port)
  (log-message (format 'nil  "Starting Server ~A:~A" host port))
  (setq *server-thread*
       (usocket:socket-server host port #'user-tcp-handler '() :in-new-thread t :multi-threading t)))

 
