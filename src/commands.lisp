;;; cl-disque - commands
;;; Copyright (c) 2015 - Cody Reichert <codyreichert@gmail.com>

(in-package #:cl-disque)

;;; Connection handling
(def-cmd INFO () :bulk
  "Generic server information / stats.")

(def-cmd HELLO () :multi
  "Returns hello format version, this node ID, all the nodes IDs, IP
  addresses, ports, and priority (lower is better, means node more
  available). Clients should use this as an handshake command when
  connecting with a Disque node.")

(def-cmd QLEN (queue) :integer
  "Returns the length of the queue.")

(def-cmd QPEEK (queue count) :multi
  "Return, without consuming from queue, count jobs. If count is
positive the specified number of jobs are returned from the oldest to
the newest (in the same best-effort FIFO order as GETJOB). If count is
negative the commands changes behavior and shows the count newest
jobs, from the newest from the oldest.")


;;; Jobs

(def-cmd GETJOB (queue &rest args &key nohang timeout count withcounters) :multi
  "Gets a job from the specific queue.")

(defmethod tell ((cmd (eql 'GETJOB)) &rest args)
  (ds-bind (queue &key nohang timeout count withcounters) args
    (apply #'tell (cl:append (list "GETJOB")
                             (if nohang `("NOHANG"))
                             (when timeout `("TIMEOUT" ,timeout))
                             (when count `("COUNT" ,count))
                             (when withcounters `("WITHCOUNTERS"))
                             (list "FROM") (list queue)))))


(def-cmd ADDJOB (queue job ms-timeout) :status
  "Adds a job to the specified queue.")

;;; end
