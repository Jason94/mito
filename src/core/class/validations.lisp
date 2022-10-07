;;;; This function defines the core machinery of the validation system:
;;;; the validated-class metaclass and generic functions to determine
;;;; if an instance of a validated-class is valid.

;;;; The validated-class metaclass is inherited by table-class to provide
;;;; validation functionality to any class representing a SQL table.

;;;; The validations themselves are defined in mito.validations.
;;;;   * A validation function should be named "validation-xxx". "xxx" will be
;;;;     used to reference the validation function in a class definition.
;;;;   * A validation is a function that takes (object slot-symbol slot-value).
;;;;   * They should return t if the object/slot-value validates.
;;;;   * They should return (values nil "explanation string") if the
;;;;     object/slot-value fails validation.
;;;;   * Validation functions can take &key parameters to customize their
;;;;     behavior.
;;;;   * All validation functions must declare &allow-other-keys to
;;;;     accomodate global validation parameters. Validation functions
;;;;     themselves are not responsible for implementing global validations
;;;;     parameters (such as :allow-nil), but are allowed to respond to them.
;;;; See mito.validations for examples.
(declaim (optimize (speed 0) (space 0) (debug 3)))

(in-package :cl-user)
(defpackage mito.class.validations
  (:use #:cl)
  (:export #:validated-class
           #:validp))
(in-package :mito.class.validations)

(defclass validated-class (standard-class)
  ((validations :initform nil
                :initarg :validates)
   (allow-invalid :initform nil
                  :initarg :allow-invalid)))

(defmethod c2mop:validate-superclass ((class validated-class) (super standard-class))
  t)

(defun symbol-to-validate-func (symbol class)
  "Get the function for a symbol with 'validate-' prepended."
  (symbol-function (find-symbol (concatenate 'string "VALIDATE-" (string symbol))
                                (symbol-package (class-name class)))))

(defun validp (validated-obj)
  "If valid returns:
         t

   If invalid returns (multiple values):
         (nil ((:slot slot  :invalid-value invalid-value  :message message)
               (:slot slot2 :invalid-value invalid-value2 :message message2)
                ...))"
  (let* ((validation-specs (slot-value (class-of validated-obj) 'validations))
         (failure-specs (mapcar
                          #'(lambda (validation-spec)
                              (destructuring-bind (function-symbol slot-symbol &rest validation-args)
                                                  validation-spec
                                (multiple-value-bind (result fail-message)
                                    (apply (symbol-to-validate-func function-symbol
                                                                    (class-of validated-obj))
                                           (nconc (list
                                                    validated-obj
                                                    slot-symbol
                                                    (slot-value validated-obj slot-symbol))
                                                  validation-args))
                                  (if result
                                    nil
                                    (list :slot slot-symbol
                                          :invalid-value (slot-value validated-obj slot-symbol)
                                          :message fail-message)))))
                          validation-specs)))
    (if (every #'not failure-specs)
      t
      (values nil (remove-if #'not failure-specs)))))
