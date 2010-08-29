(in-package :ometa)


;; error reporting

(defstruct error-data pos found message)

(defun error-reporting-cmp (e f)
  (> (error-data-pos e) (error-data-pos f)))

(defun make-error-reporting ()
  (list))

(defun add-error(reporting-list pos found msg)
  (sort (cons (make-error-data :pos pos :found found :message msg) reporting-list)
        #'error-reporting-cmp))

(defun make-message (rule &optional args)
  (if args
      (format nil "Failed matching: ~A [~A]" rule args)
      (format nil "Failed matching: ~A" rule)))


;;;;;;;;; 
  


(defclass ometa-base ()
  ((input        :accessor ometa-input     :initarg :input)
   (errors       :accessor ometa-reporting :initform (make-error-reporting))
   (rules        :accessor ometa-rules     :initform nil) ;;list of rules to report errors on
   (dbg-indent   :accessor dbg-indent      :initform "")))

(defmethod inc-dbg-indent ((o ometa-base))
  (setf (dbg-indent o) (concatenate 'string (dbg-indent o) " ")))

(defmethod dec-dbg-indent ((o ometa-base))
  (setf (dbg-indent o) (subseq (dbg-indent o) 0 (1- (array-total-size (dbg-indent o))))))

(defmethod ometa-add-error ((o ometa-base) pos msg)
  (setf (ometa-reporting o) 
        (add-error (ometa-reporting o) pos (stream-current-phrase (ometa-input o)) msg)))


;  (find rule (ometa-rules o)))
(defmethod ometa-report-on ((o ometa-base) rule)
  t)

(defmethod ometa-current-pos ((o ometa-base))
  (stream-index (ometa-input o)))

(defmethod ometa-stream-head ((o ometa-base))
  (stream-head (ometa-input o)))



;; core functions
;; unmemoized-version
;; (defmethod core-apply ((o ometa-base) rule)
;;   (let ((res (catch 'ometa (funcall rule o))))
;;     (if (ometa-errorp res)
;;         (throw 'ometa res)
;;         res)))

;; TODO: memoize failures
(defmethod core-apply ((o ometa-base) rule)
  (format t "~A [i] ~A~%" (dbg-indent o) rule)
  (inc-dbg-indent o)
  (let ((d-begin (ometa-current-pos o)))
    (let ((orig-input (ometa-input o)))
      (let ((memo (stream-memo-for (ometa-input o) rule)))
        (if memo
            (progn
              ;; (format t "m[~A] current: ~A, memo:[pos: ~A res: ~A] ] ~%" rule
              ;;         (ometa-current-pos o) (stream-index (stream-memo-next memo)) (stream-memo-result memo))
              (dec-dbg-indent o)
              (format t "~A [o] ~A [memo]~%" (dbg-indent o) rule)
              (setf (ometa-input o) (stream-memo-next memo))
              (stream-memo-result memo))
            (let ((res (catch 'ometa (funcall rule o))))
              (if (ometa-errorp res)
                  (progn 
                    (format t "~A [o] ~A [error]~%" (dbg-indent o) rule)
                    (dec-dbg-indent o)
                    (throw 'ometa res))
                  (progn
                    (stream-memoize orig-input rule (ometa-input o) res)
                    (format t "~A [o] ~A [match]~%" (dbg-indent o) rule)
                    (dec-dbg-indent o)))
              res))))))

;; debugging / reporting
 ;; (defmethod core-apply :around ((o ometa-base) rule)  
 ;;   (let ((debug-begin (stream-index (ometa-input o))))    
 ;;     (if (ometa-report-on o rule) (inc-dbg-indent o))
 ;;     (let ((res (call-next-method)))
 ;;       (if (ometa-errorp res)
 ;;           (progn
 ;;             (if (ometa-report-on o rule)
 ;;                 (ometa-add-error o (stream-index (ometa-input o)) (make-message rule)))
 ;;             (if (ometa-report-on o rule) (dec-dbg-indent o))
 ;;             (throw 'ometa res))
 ;;           (if (ometa-report-on o rule)
 ;;               (format t "~A-~A on '~A'~%" (dbg-indent o) rule  
 ;;                       (subseq (stream-input (ometa-input o)) debug-begin (stream-index (ometa-input o))))))
 ;;       (if (ometa-report-on o rule) (dec-dbg-indent o))
 ;;       res)))

(defmethod core-apply-with-args ((o ometa-base) rule &rest args)
  (dolist (arg (reverse args))
    (core-prepend-input o arg))
  (catch 'ometa (funcall rule o)))

; debuging / reporting
(defmethod core-apply-with-args :around ((o ometa-base) rule &rest args)
  (let ((debug-begin (stream-index (ometa-input o))))    
    (let ((res (call-next-method)))
      (if (ometa-errorp res)
          (progn
            (if (ometa-report-on o rule)
                (ometa-add-error o (stream-index (ometa-input o)) (make-message rule)))
            (throw 'ometa res)))
      res)))


;; (defmethod core-apply-with-args :around ((o ometa-base) rule &rest args)
;;   (format t "entered core-apply-with-args~%")
;;   (let ((res (call-next-method)))
;;     (format t "result of core-apply-with-args ~A~%" res)
;;     res))

(defmethod core-prepend-input ((o ometa-base) value)
  (setf (ometa-input o) 
        (make-ometa-stream-with value (ometa-input o))))

;; pred anything
(defmethod core-pred ((o ometa-base) x)
  (if x
      T
      (throw-ometa-error)))

;; not fun
;;  returns true if (fun) fails. otherwise, it fails
(defmethod core-not ((o ometa-base) fun)
  (let ((orig-input (ometa-input o)))
    (if (ometa-errorp (catch 'ometa (funcall fun)))
        (progn
          (setf (ometa-input o) orig-input)
          T)
        (throw-ometa-error))))

;; lookahead fun
;;   runs (fun), restores input when done, returns the result
(defmethod core-lookahead ((o ometa-base) fun)
  (let* ((orig-input (ometa-input o))
         (r (funcall fun)))
    (setf (ometa-input o) orig-input)
    r))

;; or (funs)
;;  return result of the first (fun) success or fails if no success.
(defmethod core-or ((o ometa-base) &rest funs)
  (let ((orig-input (ometa-input o)))
    (labels ((iterate-over (funs)
               (if (null funs)
                   (throw-ometa-error)
                   (progn
                     (setf (ometa-input o) orig-input)
                     (let ((res (catch 'ometa (funcall (car funs)))))
                       (if (ometa-errorp res)
                           (iterate-over (cdr funs))
                           res))))))
      (iterate-over funs))))

;; opt fun
;;  runs (fun), restore input state if it fails, returns result if any
(defmethod core-opt ((o ometa-base) fun)
  (let* ((orig-input (ometa-input o))
         (r (catch 'ometa (funcall fun))))
    (if (ometa-errorp r)         
        (progn
          (setf (ometa-input o) orig-input)
          nil)
        r)))

;; many fun
;;  while (fun) accumulate results. returns result list    
(defmethod core-many ((o ometa-base) fun &optional first)
  (let ((res (if first (list first) (list))))
    (loop
         (let* ((orig-input (ometa-input o))
                (r (catch 'ometa (funcall fun))))
           (if (ometa-errorp r)
               (progn
                 (setf (ometa-input o) orig-input)
                 (return res))
               (setf res (cons r res)))))
    (reverse res)))



(defmethod core-many1 ((o ometa-base) fun)
  (core-many o fun (funcall fun)))

;; TODO: applyWithArgs, supperApplyWithArgs, memoizeParam..Rules, 
;;       xor, disableXor, form, consumedBy*, interleave, currIdx


;; basic rules

(defmethod o-anything ((o ometa-base))
  (let* ((input (ometa-input o))
         (r (stream-head input)))
    (setf (ometa-input o) (stream-tail input))
    r))

(defmethod o-end ((o ometa-base))
  (core-not o (lambda () (core-apply o 'o-anything))))

(defmethod o-apply ((o ometa-base))
  (let ((r (core-apply o 'o-anything)))
    (core-apply o r)))

(defmethod o-exactly ((o ometa-base))
  (let ((wanted (core-apply o 'o-anything)))
    (if (eq wanted (core-apply o 'o-anything))
        wanted
        (throw-ometa-error))))

(defmethod o-true ((o ometa-base))
  (let ((r (core-apply o 'o-anything)))
    (core-pred o (eq r T))
    r))

(defmethod o-false ((o ometa-base))
  (let ((r (core-apply o 'o-anything)))
    (core-pred o (eq r NIL))
    r))

(defmethod o-number ((o ometa-base))
  (let ((r (core-apply o 'o-anything)))
    (core-pred o (numberp r))
    r))

(defmethod o-string ((o ometa-base))
  (let ((r (core-apply o 'o-anyhting)))
    (core-pred o (stringp r))
    r))

(defmethod o-char ((o ometa-base))
  (let ((r (core-apply o 'o-anything)))
    (core-pred o (characterp r))
    r))

(defmethod o-space ((o ometa-base))
  (let ((r (core-apply o 'o-char)))
    (core-pred o (<= (char-int r) 32))
    r))

(defmethod o-spaces ((o ometa-base))
  (core-many o (lambda () (core-apply o 'o-space))))

(defmethod o-digit ((o ometa-base))
  (let ((r (core-apply o 'o-char)))
    (core-pred o (and (char>= r #\0) (char<= r #\9)))
    r))

(defmethod o-lower ((o ometa-base))
  (let ((r (core-apply o 'o-char)))
    (core-pred o (and (char>= r #\a) (char<= r #\z)))
    r))

(defmethod o-upper ((o ometa-base))
  (let ((r (core-apply o 'o-char)))
    (core-pred o (and (char>= r #\A) (char<= r #\Z)))
    r))

(defmethod o-letter ((o ometa-base))
  (core-or o 
           (lambda () (core-apply o 'o-lower))
           (lambda () (core-apply o 'o-upper))))
           
(defmethod o-letter-or-digit ((o ometa-base))
  (core-or o
           (lambda () (core-apply o 'o-letter))
           (lambda () (core-apply o 'o-digit))))

(defmethod o-alpha-char ((o ometa-base))
  (core-or o
           (lambda () (core-apply o 'o-letter-or-digit))
           (lambda () (core-apply-with-args o 'o-exactly #\_))
           (lambda () (core-apply-with-args o 'o-exactly #\-))))

(defmethod o-first-and-rest ((o ometa-base))
  (let* ((first (core-apply o 'o-anything))
         (rest  (core-apply o 'o-anything)))
    (core-many o (lambda () (core-apply o rest)) (core-apply o first))))

(defmethod o-seq ((o ometa-base))
  (let ((xs (core-apply o 'o-anything)))
    (dolist (x xs)
      (core-apply-with-args o 'o-exactly x))
    xs))

;; matches a sequence followed by space*
(defmethod o-seq-s ((o ometa-base))
  (let ((res (core-apply o 'o-seq)))
    (core-apply o 'o-spaces)
    res))

(defmethod o-token ((o ometa-base))
  (coerce (core-apply-with-args o 'o-first-and-rest 'o-letter 'o-alpha-char) 'string))

;; (defmethod o-token-s ((o ometa-base))
;;   (let ((res (coerce 
;;               (core-apply-with-args o 
;;                                     'o-first-and-rest 
;;                                     'o-letter 
;;                                     'o-letter-or-digit) 'string)))
;;     (core-apply o 'o-spaces)
;;     res))

    

(defmethod o-identifier ((o ometa-base))
  (let ((ret (core-apply o 'o-token)))
    (core-apply o 'o-spaces)
    ret))



;;TODO: pos, empty, foreign, seq, end, notLast, match/All ...
