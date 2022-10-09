;;;; This file contains the standard validations. See mito.class.validations
;;;; for the validated-class mixin and the validation interface.

;;;; For documentation on the expected interface and contract of validation
;;;; functions, also see mito.class.validations.
(in-package :cl-user)
(defpackage mito.validations
  (:use #:cl)
  (:export #:validate-presence
           #:validate-length
           #:validate-inclusion
           #:validate-exclusion
           #:validate-comparison))
(in-package :mito.validations)

(defun validate-presence (obj slot value &key &allow-other-keys)
  (declare (ignore obj))
  (if value
    t
    (values nil "~a must be present" slot)))

(defun validate-length (obj slot value &key is min max &allow-other-keys)
  (declare (ignore obj slot))
  (when (not (or is min max))
    (error "Must validate is, or min and/or max."))
  (when (and is (or min max))
    (error "Cannot check exact length and min/max length."))
  (let ((len (length value)))
    (cond
      ((and is (/= len is))
       (values nil (format nil "String '~a' does not match length ~a" value len)))
      ((and min (< len min))
       (values nil (format nil "String '~a' shorter than minimum length ~a" value len)))
      ((and max (> len max))
       (values nil (format nil "String '~a' longer than maxmimum length ~a" value len)))
      (t t))))

(defun validate-inclusion (obj slot value &key in &allow-other-keys)
  (declare (ignore obj slot))
  (if (find value in :test #'equal)
    t
    (values nil (format nil "~a must be in the specified list" value))))

(defun validate-exclusion (obj slot value &key from &allow-other-keys)
  (declare (ignore obj slot))
  (if (find value from :test #'equal)
    (values nil (format nil "~a must be in the specified list" value))
    t))

(defun validate-comparison (obj slot value &key other with &allow-other-keys)
  (when (not (and other with))
    (error "Must specify a second slot and comparison function."))
  (multiple-value-bind (comparison-function error-message)
                       (case with
                         (:greater-than
                           (values #'> "be greater than"))
                         (:greater-or-equal-to
                           (values #'>= "be greater than or equal to"))
                         (:equal-to
                           (values #'equal "be equal to"))
                         (:not-equal-to
                           (values #'(lambda (x y) (not (equal x y)))
                                   "not be equal to"))
                         (:less-than
                           (values #'< "be less than"))
                         (:less-or-equal-to
                           (values #'< "be less than or equal to")))
    (when (not comparison-function)
      (error (format nil "Invalid comparison argument ~a given" with)))
    (if (funcall comparison-function value (slot-value obj other))
      t
      (values nil
              (format nil "~a [~a] must ~a ~a [~a]"
                      slot value error-message other (slot-value obj other))))))
