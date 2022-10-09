(in-package :cl-user)
(defpackage mito-test.validations
  (:use #:cl
        #:prove
        #:mito.validations))
(in-package :mito-test.validations)

(plan nil)

(defclass account ()
  ((id
     :initarg :id
     :accessor id)
   (name
    :initarg :name
    :accessor name)
   (balance
    :initarg :balance
    :accessor balance
    :initform 0)))

(defun account (id name balanec)
  (make-instance 'account
                 :id 1
                 :name "Steve"
                 :balance 0))

(subtest "VALIDATE-PRESENCE"
  (subtest "fails when the value is nil"
    (is (validate-presence (account 1 "Steve" nil) 'balance nil) nil))
  (subtest "passes when the value is present"
    (is (validate-presence (account 1 "Steve" 0) 'balance 0) t)))

(subtest "VALIDATE-LENGTH"
  (let ((test-account (account 1 "Steve" 10)))
    (subtest "errors when no specification is supplied"
      (is-error (validate-length test-account 'name "Steve")
                'simple-error))
    (subtest "errors when :is"
      (is-error (validate-length test-account 'name "Steve" :is 0 :min 0)
                'simple-error
                "and :min are supplied")
      (is-error (validate-length test-account 'name "Steve" :is 0 :max 0)
                'simple-error
                "and :max are supplied"))
    (subtest "checking the exact length"
      (is (validate-length test-account 'name "Steve" :is 5) t "passes when matching")
      (is (validate-length test-account 'name "Steve" :is 4) nil "fails when greater")
      (is (validate-length test-account 'name "Steve" :is 6) nil "fails when less"))
    (subtest "checking the minimum length"
      (is (validate-length test-account 'name "Steve" :min 4) t "passes when greater")
      (is (validate-length test-account 'name "Steve" :min 5) t "passes when equal")
      (is (validate-length test-account 'name "Steve" :min 6) nil "fails when less"))
    (subtest "checking the maximum length"
      (is (validate-length test-account 'name "Steve" :max 6) t "passes when less")
      (is (validate-length test-account 'name "Steve" :max 5) t "passes when equal")
      (is (validate-length test-account 'name "Steve" :max 4) nil "fails when greater"))
    (subtest "checking minimum & maximum length"
      (is (validate-length test-account 'name "Steve" :min 4 :max 6) t "passes when inside range")
      (is (validate-length test-account 'name "Steve" :min 2 :max 4) nil "fails when above range")
      (is (validate-length test-account 'name "Steve" :min 6 :max 8) nil "fails when below range"))))

(subtest "VALIDATE-INCLUSION"
  (let ((admin-account (account 1 "Steve" 10)))
    (is (validate-inclusion admin-account 'name "Steve" :in '())
        nil
        "fails against the empty list")
    (is (validate-inclusion admin-account 'name "Steve" :in '("Betsy" "Steve" "Susan"))
        t
        "passes when the value is in the list")
    (is (validate-inclusion admin-account 'name "Jimbo" :in '("Betsy" "Steve" "Susan"))
        nil
        "fails when the value is not in the list")
    (is (validate-inclusion admin-account 'name "Steve" :in #("Betsy" "Steve" "Susan"))
        t
        "passes when the value is in the vector")))

(finalize)
