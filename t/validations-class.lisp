(in-package :cl-user)
(defpackage mito-test.test-foo-validations-package
  (:use #:cl)
  (:export #:validate-not-empty))
(in-package :mito-test.test-foo-validations-package)

(defun validate-not-empty (obj slot value &key &allow-other-keys)
  (declare (ignore obj slot))
  (if (string= "" value)
    (values nil "must not be empty")
    t))

(in-package :cl-user)
(defpackage mito-test.class.validations
  (:use #:cl
        #:prove
        #:mito.class.validations
        #:mito-test.test-foo-validations-package))
(in-package :mito-test.class.validations)

(plan nil)

(defun validate-positive (obj slot value &key &allow-other-keys)
  (declare (ignore obj slot))
  (if (>= value 0)
    t
    (values nil "must be positive")))

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
    :initform 0))
  (:metaclass validated-class)
  (:validates
    (presence id)
    (positive balance)
    (not-empty name)))

(defun account (&optional (id 1) (name "Steve") (balance 0))
  (make-instance 'account
                 :id id
                 :name name
                 :balance balance))

(subtest "Validating"
  (subtest "looks for symbols in the defining package, mito.validations, and
            any functions imported into the defining package."
    (validp (account 1 "Steve" 10))
    (pass "validp found all validation functions"))

  (subtest "passes validations when all validations are satisfied"
    (ok (validp (account 1 "Steve" 10))))

  (subtest "fails validations when one validation is not satisfied"
    (multiple-value-bind (result fail-specs)
                         (validp (account 1 "Steve" -10))
      (is result nil "failed validation")
      (is (getf (first fail-specs) :slot)
          'balance
          "includes fail-spec slot")
      (is (getf (first fail-specs) :invalid-value)
          -10
          "includes invalid-value")
      (is-type (getf (first fail-specs) :message)
               'string
               "includes string error message"))))

(subtest "ENSURE"
  (subtest "returns T if the object is valid"
    (ok (ensure (account 1 "Steve" 10)))))

(finalize)
