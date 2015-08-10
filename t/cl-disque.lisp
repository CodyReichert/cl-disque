;;; cl-disque testsuite package definition

(in-package :cl-user)
(defpackage cl-disque-test
  (:use :cl
        :cl-disque
        :prove))
(in-package :cl-disque-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-disque-test)' in your Lisp.

;; NOTE: This requires a Disque server to be running locally - it does not start one

(plan 20)

;; disque connection
(cl-disque:connect)
(ok (cl-disque:connected-p)
    "Can connect to disque server.")
(cl-disque:disconnect)
(ok (not (cl-disque:connected-p))
    "Can disconnect from disque server.")

(cl-disque:with-connection ()

  ;; Test `with-connection'
  (ok (cl-disque:connected-p)
      "Can persist disque connection.")


  ;;  Test `HELLO' response
  (destructuring-bind (_ node-id info)
      (disque:hello)
    (is 1 _)
    (is-type node-id 'simple-array
             "Node_ID is of type string")
    (is-type info 'cons)
    (is-type (first info) 'string
             "Node-ID is of type string, and there is only one.")
    (is (second info) ""
        "Host is empty (default)")
    (is (third info) "7711"
        "Port is 7711")
    (is (fourth info) "1"
        "Node Priority is 1"))


  ;; Test `INFO' response
  (let ((info (disque:info)))
    (is-type info 'string
             "Info is of type string."))


  ;; Test `QLEN'
  (is (disque:qlen "test-queue") 0 "QLEN is 0.")

  (disque:addjob "test-queue" "test-job" 0)
  (diag "   (addjob to test-queue)")

  (is (disque:qlen "test-queue") 1 "QLEN is 1.")

  (disque:getjob "test-queue")
  (diag "   (getjob from test-queue)")


  ;; Test `QPEEK'
  (disque:addjob "test-queue" "test-job" 0)
  (diag "   (addjob to test-queue)")
  (is (disque:qlen "test-queue") 1 "QLEN is 1 before QPEEK.")

  (destructuring-bind (queue job-id job-name)
      (first (disque:qpeek "test-queue" 1))
    (is queue "test-queue"
        "Job is from \"test-queue\".")
    (is-type job-id 'string
             "Job-ID is of type string.")
    (is job-name "test-job"
        "Job-Name is \"test-jobs\"."))
    
  (is (disque:qlen "test-queue") 1 "QLEN is 1 after QPEEK.")
  (disque:getjob "test-queue")
  (diag "   (getjob from test-queue)")


  ;; Test `QSCAN'
  (let ((qinfo (disque:qscan)))
    (is (length (cadr qinfo)) 1
        "One queue registered with Disque."))

  (let ((qinfo (disque:qscan :minlen 1)))
    (is (length (cadr qinfo)) 0
        "No queue's registered with Disque have jobs.")))


;;; TODO
;; Need tests for the following:
;; GETJOB, ADJOB, ACKJOB, FASTACK, WORKING, NACK, ENQUEUE 
;; DEQUEUE, DELJOB, SHOW


(finalize)


;;; end
