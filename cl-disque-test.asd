#|
  This file is a part of cl-push-worker project.
  Copyright (c) 2015 Cody Reichert (codyreichert@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-disque-test-asd
  (:use :cl :asdf))
(in-package :cl-disque-test-asd)

(defsystem cl-disque-test
  :author "Cody Reichert"
  :license "MIT"
  :depends-on (:cl-disque
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "cl-disque"))))
  :description "Test system for cl-disque"
  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
