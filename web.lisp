(defpackage :ichiran/web
  (:use :cl :ichiran/all :hunchentoot)
  (:export :start-server :stop-server))

(in-package :ichiran/web)

(defvar *server* nil)
(defvar *default-port* 8080)
(defparameter *max-concurrent-requests* 50)
(defparameter *request-semaphore* (sb-thread:make-semaphore :count *max-concurrent-requests*))
(defparameter *server-ready* nil)
(defparameter *db-pool-max-size* 20)
(defparameter *db-pool-max-age* 300)
(defparameter *connection-timeout* 300) ; 5 minutes in seconds

(defclass ichiran-acceptor (easy-acceptor)
  ((cache :initform (make-hash-table :test 'equal) :accessor acceptor-cache)
   (connection-spec :initarg :connection-spec :accessor connection-spec)))

(defmethod initialize-instance :after ((acceptor ichiran-acceptor) &key)
  (setf (hunchentoot:acceptor-input-chunking-p acceptor) t))

(defun set-cors-headers ()
  (setf (header-out "Access-Control-Allow-Origin") "*")
  (setf (header-out "Access-Control-Allow-Methods") "GET, POST, OPTIONS")
  (setf (header-out "Access-Control-Allow-Headers") "Content-Type")
  (setf (header-out "Content-Type") "application/json; charset=utf-8"))

(defun json-response (data)
  (set-cors-headers)
  (jsown:to-json data))

(defun format-word-info (romaji info)
  (jsown:new-js
    ("romaji" romaji)
    ("words" (loop for (word . gloss) in info
                   collect (jsown:new-js
                            ("word" word)
                            ("gloss" gloss))))))

(defmethod jsown:to-json ((word-info ichiran/dict:word-info))
  (jsown:to-json (ichiran/dict:word-info-gloss-json word-info)))

(defmacro with-thread-connection (&body body)
  `(progn
     (sb-thread:wait-on-semaphore *request-semaphore*)
     (unwind-protect
         (let ((ichiran/conn:*connection* (connection-spec *acceptor*)))
           (handler-case
               (postmodern:with-connection ichiran/conn:*connection*
                 ;; Test the connection before use
                 (handler-case
                     (postmodern:query "SELECT 1")
                   (error (e)
                     (format t "~&Connection test failed, reconnecting: ~A~%" e)
                     (postmodern:clear-connection-pool)))
                 (handler-bind
                     ((cl-postgres:database-connection-error
                       (lambda (e)
                         (format t "~&Connection error, clearing pool: ~A~%" e)
                         (postmodern:clear-connection-pool)
                         ;; Try to reconnect once
                         (handler-case
                             (progn
                               (format t "~&Attempting to reconnect...~%")
                               (postmodern:with-connection ichiran/conn:*connection*
                                 ,@body))
                           (error (e2)
                             (format t "~&Reconnection failed: ~A~%" e2)
                             (signal e2))))))
                   ,@body))
             (error (e)
               (format t "~&Error in request: ~A~%" e)
               (signal e))))
       (sb-thread:signal-semaphore *request-semaphore*))))

(define-easy-handler (analyze :uri "/api/analyze") (text info full)
  (with-thread-connection
    (json-response 
      (when text
        (cond
          (full
           (let* ((limit-value 1)
                  (result (romanize* text :limit limit-value)))
             result))
          (info
           (multiple-value-bind (romaji info) 
               (romanize text :with-info t)
             (format-word-info romaji info)))
          (t
           (jsown:new-js
             ("romaji" (romanize text)))))))))

(define-easy-handler (word-info :uri "/api/word-info") (text reading)
  (json-response
    (when text
      (find-word-info-json text :reading reading))))

(defun test-db-connection ()
  "Test database connection and return T if successful"
  (handler-case
      (postmodern:with-connection (connection-spec *acceptor*)
        (postmodern:query "SELECT 1")
        t)
    (error (e)
      (format t "~&Health check detected DB issue: ~A~%" e)
      (postmodern:clear-connection-pool)
      nil)))

(defun health-check ()
  (setf (hunchentoot:content-type*) "application/json")
  (if *server-ready*
      (if (test-db-connection)
          "{\"status\": \"ok\"}"
          (progn
            (setf (hunchentoot:return-code*) 503)
            "{\"status\": \"database error\"}"))
      (progn
        (setf (hunchentoot:return-code*) 503)
        "{\"status\": \"initializing\"}")))

(defun start-server (&key (port *default-port*))
  (when *server*
    (stop-server))
  (setf *server-ready* nil)
  (ichiran/conn:load-settings :keep-connection t)
  (postmodern:clear-connection-pool)
  (setf postmodern:*max-pool-size* *db-pool-max-size*)
  (setf *server* 
        (make-instance 'ichiran-acceptor 
                      :port port
                      :address "0.0.0.0"
                      :connection-spec ichiran/conn:*connection*))
  (push (create-prefix-dispatcher "/health" 'health-check)
        *dispatch-table*)
  (start *server*)
  (handler-case
      (progn
        (postmodern:with-connection ichiran/conn:*connection*
          (postmodern:query "SELECT 1"))
        (setf *server-ready* t)
        (format t "~&Server started and ready on port ~A~%" port))
    (error (e)
      (format t "~&Server failed to initialize: ~A~%" e)
      (stop-server)
      (error e))))

(defun stop-server ()
  (when *server*
    (stop *server*))
  (setf *server* nil))
