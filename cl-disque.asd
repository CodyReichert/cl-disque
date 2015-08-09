#|
    cl-disque - A Disque client for Common Lisp
    Cody Reichert - Copyright (c) 2015
|#

(in-package :asdf)

(defsystem #:cl-disque
  :version "0.0.1"
  :author "Cody Reichert <codyreichert@gmail.com>"
  :maintainer "Cody Reichert <codyreichert@gmail.com>"
  :licence "MIT"
  :description "A Disque client for Common Lisp"
  :depends-on (#:rutils #:cl-ppcre #:usocket #:flexi-streams #:babel)
  :serial t
  :components ((:module "src"
                       :components ((:file "package")
                                    (:file "float")
                                    (:file "connection")
                                    (:file "cl-disque")
                                    (:file "commands")))))


(defmethod perform ((o test-op)
                    (c (eql (find-system 'cl-disque))))
  (operate 'load-op '#:cl-disque)
  (operate 'test-op '#:cl-disque-test))

(defsystem #:cl-disque-test
  :version "0.0.1"
  :author "cody Reichert"
  :maintainer "Cody Reichert <codyreichert@gmail.com>"
  :licence "MIT"
  :description "CL-Disque test suite"
  :depends-on (#:cl-disque #:bordeaux-threads #:flexi-streams #:should-test)
  :components ((:module "t"
                       :components ((:file "cl-disque-test")))))

(defmethod perform ((o test-op)
                    (c (eql (find-system 'cl-disque-test))))
  (asdf:load-system '#:cl-disue-test)
  (funcall (read-from-string "cl-disque-test:run-tests")))

;;; end
