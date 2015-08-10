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

;; Queue Handling

(def-cmd QLEN (queue) :integer
  "Returns the length of the queue.")


(def-cmd QPEEK (queue count) :multi
  "Return, without consuming from queue, count jobs. If count is
positive the specified number of jobs are returned from the oldest to
the newest (in the same best-effort FIFO order as GETJOB). If count is
negative the commands changes behavior and shows the count newest
jobs, from the newest from the oldest.")


(def-cmd QSCAN (&rest args &key count busyloop minlen maxlen importrate) :multi
  "The command provides an interface to iterate all the
existing queues in the local node, providing a cursor in the
form of an integer that is passed to the next command
invocation. During the first call cursor must be 0, in the
next calls the cursor returned in the previous call is used
in the next. The iterator guarantees to return all the
elements but may return duplicated elements.")

(defmethod tell ((cmd (eql 'QSCAN)) &rest args)
  (ds-bind (&key count busyloop minlen maxlen importrate) args
    (apply #'tell (cl:append (list "QSCAN")
                             (when count `("COUNT" ,count))
                             (when busyloop `("BUSYLOOP"))
                             (when minlen `("MINLEN" ,minlen))
                             (when maxlen `("MAXLEN" ,maxlen))
                             (when importrate `("IMPORTRATE" ,importrate))))))


;;; Jobs handling

(def-cmd GETJOB (queues &rest args &key nohang timeout count withcounters) :multi
  "Gets a job from the specific queue.")

(defmethod tell ((cmd (eql 'GETJOB)) &rest args)
  (ds-bind (queues &key nohang timeout count withcounters) args
    (apply #'tell (cl:append (list "GETJOB")
                             (when nohang `("NOHANG"))
                             (when timeout `("TIMEOUT" ,timeout))
                             (when count `("COUNT" ,count))
                             (when withcounters `("WITHCOUNTERS"))
                             (list "FROM") (if (eql (type-of queues) 'cons)
                                               (loop for q in queues collecting q)
                                               (list queues))))))


(def-cmd ADDJOB (queue job timeout &rest args &key replicate delay retry ttl maxlen async) :status
  "Adds a job to the specified queue.")

(defmethod tell ((cmd (eql 'ADDJOB)) &rest args)
  (ds-bind (queue job timeout &key replicate delay retry ttl maxlen async) args
    (apply #'tell (cl:append (list "ADDJOB" queue job timeout)
                             (when replicate `("REPLICATE" ,replicate))
                             (when delay `("DELAY" ,delay))
                             (when retry `("RETRY" ,retry))
                             (when ttl `("TTL" ,ttl))
                             (when maxlen `("MAXLEN" ,maxlen))
                             (when async `("ASYNC"))))))

(def-cmd ACKJOB (job &rest jobs) :integer
  "Acknowledges the execution of one or more jobs via jobs IDs. The
node receiving the ACK will replicate it to multiple nodes and will
try to garbage collect both the job and the ACKs from the cluster so
that memory can be freed.")


(def-cmd FASTACK (job &rest jobs) :integer
  "Acknowledges the execution of one or more jobs via jobs IDs. The
node receiving the ACK will replicate it to multiple nodes and will
try to garbage collect both the job and the ACKs from the cluster so
that memory can be freed.")


(def-cmd WORKING (job) :integer
  "Claims to be still working with the specified job, and asks Disque to
postpone the next time it will deliver again the job. The next
delivery is postponed for the job retry time, however the command
works in a best effort way since there is no way to guarantee during
failures that another node in a different network partition is
performing a delivery of the same job.")


(def-cmd NACK (job &rest jobs) :integer
  "The NACK command tells Disque to put back the job in the queue
ASAP. It is very similar to ENQUEUE but it increments the job nacks
counter instead of the additional-deliveries counter. The command
should be used when the worker was not able to process a message and
wants the message to be put back into the queue in order to be
processed again.")


(def-cmd ENQUEUE (job &rest jobs) :integer
  "Queue jobs if not already queued.")


(def-cmd DEQUEUE (job &rest jobs) :integer
  "Remove jobs from the queue.")


(def-cmd DELJOB (job &rest jobs) :integer
  "Completely delete a job from a node. Note that this is similar to
FASTACK, but limited to a single node since no DELJOB cluster bus
message is sent to other nodes.")


(def-cmd SHOW (job) :multi
  "Describe the job.")


;; TODO: JSCAN

;;; end
