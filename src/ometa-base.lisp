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


  
(defmethod ometa-report-on ((o ometa-base) rule)
  (find rule (ometa-rules o)))

(defmethod ometa-current-pos ((o ometa-base))
  (stream-index (ometa-input o)))

(defmethod ometa-stream-head ((o ometa-base))
  (stream-head (ometa-input o)))


(defun ometa-match (data grammar rule)
  (let ((o (make-instance grammar :input (make-ometa-stream data))))
    (let ((res (catch 'ometa (core-apply o rule))))
      (if (ometa-error-p res)
          (cons 'error (ometa-reporting o))
          res))))



;; core functions
;; unmemoized-version
;; (defmethod core-apply ((o ometa-base) rule)
;;   (let ((res (catch 'ometa (funcall rule o))))
;;     (if (ometa-errorp res)
;;         (throw 'ometa res)
;;         res)))

;; (defun d1 (o rule) 
;;   (if (ometa-report-on o rule)
;;       (format t "Entrando ~A" rule)))

;; (defun d2 (o rule phrase) 
;;   (if (ometa-report-on o rule)
;;       (format t "(~A)~%" phrase)))

;; (defun d3 (o orig-head next-head rule)
;;   (if (ometa-report-on o rule)
;;       (progn 
;;         (format t " MEMOFOUND:")
;;         (format t "[~A, ~A, ~A] .. " orig-head next-head rule)
;;         (format t "Saindo ~A~%" rule))))

;; (defun d4 (o rule phrase)
;;   (if (ometa-report-on o rule)
;;       (format t "MATCHFAIL: ~A em '~A' ... saindo de ~A ~%" rule phrase rule)))

;; (defun d5 (o rule org-phrase curr-phrase)
;;   (if (ometa-report-on o rule)
;;       (format t "* sucesso! ~A em '~A' (proximo input '~A') ~%" rule org-phrase curr-phrase)))


;; apply -- (memoization of results and errors)
(defmethod core-apply ((o ometa-base) rule)
  ;; (d1 o rule)
  (let ((orig-input (ometa-input o)))
    ;; (d2 o rule (stream-current-phrase orig-input))
    (let ((memo (stream-memo-for orig-input rule)))
      (if memo
          (progn
            ;; (d3 o (catch 'ometa (stream-head orig-input)) (catch 'ometa (stream-head (stream-memo-next memo))) rule)
            (setf (ometa-input o) (stream-memo-next memo))
            (let ((mres (stream-memo-result memo)))
              (if (ometa-error-p mres) ;error memoized
                  (throw 'ometa mres)
                  mres)))
          (let ((res (catch 'ometa (funcall rule o))))
            (stream-memoize orig-input rule (ometa-input o) res)
            (if (ometa-error-p res)
                (progn
                  ;; (d4 o rule (stream-current-phrase orig-input))                  
                  (setf (ometa-input o) orig-input)
                  (throw 'ometa res))
                (progn
                  ;; (d5 o rule (stream-current-phrase orig-input) (stream-current-phrase (ometa-input o)))
                  ;(stream-memoize orig-input rule (ometa-input o) res)
                  res)))))))


;; (defmethod core-apply ((o ometa-base) rule)
;;   (let ((d-begin (ometa-current-pos o)))
;;     (let ((orig-input (ometa-input o)))
;;       (let ((memo (stream-memo-for (ometa-input o) rule)))
;;         (if memo
;;             (progn
;;               (setf (ometa-input o) (stream-memo-next memo))
;;               (stream-memo-result memo))
;;             (let ((res (catch 'ometa (funcall rule o))))
;;               (if (ometa-errorp res)
;;                   (progn 
;;                     (throw 'ometa res))
;;                   (progn
;;                     (stream-memoize orig-input rule (ometa-input o) res)
;;                     (dec-dbg-indent o)))
;;               res))))))

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
  (funcall rule o))

; debuging / reporting
;; (defmethod core-apply-with-args :around ((o ometa-base) rule &rest args)
;;   (let ((debug-begin (stream-index (ometa-input o))))    
;;     (let ((res (call-next-method)))
;;       (if (ometa-error-p res)
;;           (progn
;;             (if (ometa-report-on o rule)
;;                 (ometa-add-error o (stream-index (ometa-input o)) (make-message rule)))
;;             (throw 'ometa res)))
;;       res)))


;; (defmethod core-apply-with-args :around ((o ometa-base) rule &rest args)
;;   (format t "entered core-apply-with-args~%")
;;   (let ((res (call-next-method)))
;;     (format t "result of core-apply-with-args ~A~%" res)
;;     res))

(defmethod core-prepend-input ((o ometa-base) value)
  (setf (ometa-input o) 
        (make-ometa-stream-with value (ometa-input o))))

;; (defmethod core-prepend-input-list ((o ometa-base) lst)
;;   (dolist (value lst)  
;;     (setf (ometa-input o) 
;;           (make-ometa-stream-with value (ometa-input o)))))

;; pred anything
(defmethod core-pred ((o ometa-base) x)
  (if x
      T
      (throw-ometa-error)))

;; not fun
;;  returns true if (fun) fails. otherwise, it fails
(defmethod core-not ((o ometa-base) fun)
  (let ((orig-input (ometa-input o)))
    (if (ometa-error-p (catch 'ometa (funcall fun)))
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
                       (if (ometa-error-p res)
                           (iterate-over (cdr funs))
                           res))))))
      (iterate-over funs))))

;; opt fun
;;  runs (fun), restore input state if it fails, returns result if any
(defmethod core-opt ((o ometa-base) fun)
  (let* ((orig-input (ometa-input o))
         (r (catch 'ometa (funcall fun))))
    (if (ometa-error-p r)         
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
           (if (ometa-error-p r)
               (progn
                 (setf (ometa-input o) orig-input)
                 (return res))
               (setf res (cons r res)))))
    (reverse res)))


(defmethod core-many1 ((o ometa-base) fun)
  (core-many o fun (funcall fun)))

(defmethod core-form ((o ometa-base) fun)
  (let ((v (core-apply o 'anything)))
    (core-pred o (listp v))
    (let ((next-input (ometa-input o)))
      (setf (ometa-input o) (make-ometa-stream v))
      (let ((res (funcall fun)))
        (core-apply o 'end)
        (setf (ometa-input o) next-input)
        res))))
    


;; Missing: applyWithArgs, supperApplyWithArgs, memoizeParam..Rules, 
;;          xor, disableXor, consumedBy*, interleave, currIdx




;; basic rules

(defmethod anything ((o ometa-base))
  (let* ((input (ometa-input o))
         (r (stream-head input)))
    (setf (ometa-input o) (stream-tail input))
    r))

(defmethod end ((o ometa-base))
  (core-not o (lambda () (core-apply o 'anything))))

(defmethod ometa-apply ((o ometa-base))
  (let ((r (core-apply o 'anything)))
    (core-apply o r)))

(defmethod exactly ((o ometa-base))
  (let ((wanted (core-apply o 'anything)))
    (if (eq wanted (core-apply o 'anything))
        wanted
        (throw-ometa-error))))

(defmethod true ((o ometa-base))
  (let ((r (core-apply o 'anything)))
    (core-pred o (eq r T))
    r))

(defmethod false ((o ometa-base))
  (let ((r (core-apply o 'anything)))
    (core-pred o (eq r NIL))
    r))

(defmethod num ((o ometa-base))
  (let ((r (core-apply o 'anything)))
    (core-pred o (numberp r))
    r))

(defmethod str ((o ometa-base))
  (let ((r (core-apply o 'anything)))
    (core-pred o (stringp r))
    r))

(defmethod chr ((o ometa-base))
  (let ((r (core-apply o 'anything)))
    (core-pred o (characterp r))
    r))

(defmethod spacing ((o ometa-base))
  (let ((r (core-apply o 'chr)))
    (core-pred o (<= (char-int r) 32))
    r))

(defmethod spaces ((o ometa-base))
  (core-many o (lambda () (core-apply o 'spacing))))

(defmethod digit ((o ometa-base))
  (let ((r (core-apply o 'chr)))
    (core-pred o (and (char>= r #\0) (char<= r #\9)))
    r))

(defmethod lower ((o ometa-base))
  (let ((r (core-apply o 'chr)))
    (core-pred o (and (char>= r #\a) (char<= r #\z)))
    r))

(defmethod upper ((o ometa-base))
  (let ((r (core-apply o 'chr)))
    (core-pred o (and (char>= r #\A) (char<= r #\Z)))
    r))

(defmethod letter ((o ometa-base))
  (core-or o 
           (lambda () (core-apply o 'lower))
           (lambda () (core-apply o 'upper))))
           
(defmethod letter-or-digit ((o ometa-base))
  (core-or o
           (lambda () (core-apply o 'letter))
           (lambda () (core-apply o 'digit))))

(defmethod alpha-char ((o ometa-base))
  (core-or o
           (lambda () (core-apply o 'letter-or-digit))
           (lambda () (core-apply-with-args o 'exactly #\_))
           (lambda () (core-apply-with-args o 'exactly #\-))))

(defmethod first-and-rest ((o ometa-base))
  (let* ((first (core-apply o 'anything))
         (rest  (core-apply o 'anything)))
    (core-many o (lambda () (core-apply o rest)) (core-apply o first))))

(defmethod seq ((o ometa-base))
  (let ((xs (core-apply o 'anything)))
    (dolist (x xs)
      (core-apply-with-args o 'exactly x))
    xs))

;; matches a sequence followed by space*
(defmethod seq-s ((o ometa-base))
  (let ((res (core-apply o 'seq)))
    (core-apply o 'spaces)
    res))

(defmethod token ((o ometa-base))
  (coerce (core-apply-with-args o 'first-and-rest 'letter 'alpha-char) 'string))

   

(defmethod identifier ((o ometa-base))
  (let ((ret (core-apply o 'token)))
    (core-apply o 'spaces)
    (let ((min (string-upcase ret)))
      (intern min))))