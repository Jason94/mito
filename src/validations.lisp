;;;; This file contains the standard validations. See mito.class.validations
;;;; for the validated-class mixin and the validation interface.

;;;; For documentation on the expected interface and contract of validation
;;;; functions, also see mito.class.validations.
(in-package :cl-user)
(defpackage mito.validations
  (:use #:cl)
  (:export #:validate-presence))
(in-package :mito.validations)

(defun validate-presence (obj slot value &key &allow-other-keys)
  (declare (ignore obj))
  (if value
    t
    (values nil "~a must be present" slot)))
