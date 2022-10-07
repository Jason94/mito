(in-package :cl-user)
(defpackage mito-test.class.validations
  (:use #:cl
        #:prove
        #:mito.class.validations))
(in-package :mito-test.class.validations)

(plan nil)

(defun validate-positive (obj slot value &key &allow-other-keys)
  (declare (ignore obj slot))
  (if (>= value 0)
    t
    (values nil "must be positive")))

(defun validate-not-empty (obj slot value &key &allow-other-keys)
  (declare (ignore obj slot))
  (if (string= "" value)
    (values nil "must not be empty")
    t))

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

(subtest "Passes validations."
  (ok (validp (make-instance 'account
                             :id 1
                             :name "Steve"
                             :balance 10))))

(finalize)
