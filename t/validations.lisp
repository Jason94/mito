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
    :initform 0)
   (old-balance
    :initarg :old-balance
    :accessor balance
    :initform 0)))

(defun account (&optional (id 1) (name "Steve") (balance 0) (old-balance 0))
  (make-instance 'account
                 :id id
                 :name name
                 :balance balance
                 :old-balance old-balance))

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

(subtest "VALIDATE-EXCLUSION"
  (let ((test-account (account 1 "Steve" 10)))
    (is (validate-exclusion test-account 'name "Steve" :from '())
        t
        "passes against the empty list")
    (is (validate-exclusion test-account 'name "Steve" :from '("Amanda" "Steve"))
        nil
        "fails when the value is in the list")
    (is (validate-exclusion test-account 'name "Steve" :from '("Betsy" "Amanda" "Susan"))
        t
        "passes when the value is not in the lit")
    (is (validate-exclusion test-account 'name "Steve" :from #("Betsy" "Amanda" "Susan"))
        t
        "passes when the value is not in the vector")))

(subtest "VALIDATE-COMPARISON"
  (let ((test-account (account 1 "Steve" 10 0)))
    (is-error (validate-comparison test-account 'balance 10 :with :less-than)
              'simple-error
              "errors without a comparison function")
    (is-error (validate-comparison test-account 'balance 10 :other 'old-balance)
              'simple-error
              "errors without an other slot")
    (subtest "using :greater-than"
      (is (validate-comparison test-account 'balance 10 :other 'old-balance
                                                        :with :greater-than)
          t
          "passes when true")
      (is (validate-comparison test-account 'old-balance 0 :other 'balance
                                                           :with :greater-than)
          nil
          "fails when false"))
    (is (validate-comparison test-account 'balance 10
                                          :other 'old-balance
                                          :with :greater-or-equal-to)
        t
        "using :greater-or-equal-to passes when true")
    (is (validate-comparison test-account 'balance 10
                                          :other 'old-balance
                                          :with :equal-to)
        nil
        "using :equal-to fails when false")
    (is (validate-comparison test-account 'balance 10
                                          :other 'old-balance
                                          :with :not-equal-to)
        t
        "using :not-equal-to passes when true")
    (is (validate-comparison test-account 'balance 10
                                          :other 'old-balance
                                          :with :less-than)
        nil
        "using :less-than fails when false")
    (is (validate-comparison test-account 'balance 10
                                          :other 'old-balance
                                          :with :less-or-equal-to)
        nil
        "using :less-or-equal-to fails when false")))


(finalize)
