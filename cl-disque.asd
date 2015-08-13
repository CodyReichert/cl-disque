#|
    cl-disque - A Disque client for Common Lisp
    Cody Reichert - Copyright (c) 2015
|#

(in-package :asdf)

(defsystem #:cl-disque
  :version "0.0.1"
  :author "Cody Reichert <codyreichert@gmail.com>"
  :maintainer "Cody Reichert <codyreichert@gmail.com>"
  :license "MIT"
  :description "A Disque client for Common Lisp"
  :depends-on (#:rutils #:cl-ppcre #:usocket #:flexi-streams #:babel)
  :serial t
  :components ((:module "src"
                       :components ((:file "package")
                                    (:file "float")
                                    (:file "connection")
                                    (:file "cl-disque")
                                    (:file "commands"))))
  :in-order-to ((test-op (test-op cl-disque-test))))

;;; end
