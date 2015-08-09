;; cl-disque

(in-package :cl-user)


(defpackage #:cl-disque
  (:use #:common-lisp #:rutil)
  (:shadow #:quit #:sort #:set #:get #:substr #:eval #:type #:append
           #:watch #:unwatch #:shutdown #:time)
  (:export #:dique-connection
           #:connect
           #:disconnect
           #:reconnect
           #:*connection*
           #:open-connection
           #:close-connection
           #:connected-p
           #:with-connection
           #:with-recursive-connection
           #:with-persistent-connection

           #:*echo-p*
           #:*echo-stream*

           #:*cmd-prefix*

           #:def-cmd
           #:def-expect-method
           #:expect
           #:tell

           #:disque-error
           #:disque-error-message
           #:disque-bad-reply
           #:disque-error-reply
           #:disque-connection-error

           #:with-pipelining))

(defpackage #:disque
  (:use #| nothing |# ))


;;; end
