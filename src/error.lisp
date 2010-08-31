(defclass ometa-error-type () ())
(defun throw-ometa-error () (throw 'ometa (make-instance 'ometa-error-type)))

; util
(defun ometa-error-p (x)
  (eq (class-name (class-of x)) 'ometa-error-type))

