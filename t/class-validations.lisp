(in-package :cl-user)
(defpackage mito-test.class.validations
  (:use #:cl
        #:prove
        #:mito.class.validations))
(in-package :mito-test.class.validations)

(plan nil)

(subtest "Ya boi"
  (is (foobar) 10))

(finalize)
