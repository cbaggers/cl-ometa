;; Copyright (c) 2010 Thiago Silva <thiago@comum.org>

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

(defclass ometa-base ()
  ((input  :accessor ometa-input
           :initarg :input)))

  
(defmethod ometa-current-pos ((o ometa-base))
  (stream-index (ometa-input o)))

(defmethod ometa-stream-head ((o ometa-base))
  (stream-head (ometa-input o)))

(defmethod o-init ((o ometa-base)) nil)

(defun ometa-match (data grammar rule &rest r)
  (let ((bk-nil-print (pprint-dispatch nil)))
    (unwind-protect
         (progn
           (set-pprint-dispatch (type-of nil) #'nil-paren-print)
           (let* ((input (make-ometa-stream data))
                  (o (apply #'make-instance (cons grammar (append r (list :input input))))))
             (o-init o)
             (let ((res (catch 'ometa (core-apply o rule))))
               (if (ometa-error-p res)
                   (values 'error (stream-get-pretty-error input))
                   (values t res)))))
      (set-pprint-dispatch (type-of nil) bk-nil-print))))




(defmethod core-apply ((o ometa-base) rule)
  (let ((orig-input (ometa-input o)))
    (let ((memo (stream-memo-for orig-input rule)))
      (if memo
          (progn
            (setf (ometa-input o) (stream-memo-next memo))
            (let ((mres (stream-memo-result memo)))
              (if (ometa-error-p mres) ;error memoized
                  (throw 'ometa mres)
                  mres)))
          (let ((res (catch 'ometa (funcall rule o))))
            (stream-memoize orig-input rule (ometa-input o) res)
            (if (ometa-error-p res)
                (progn
                  (stream-add-error orig-input rule)
                  (setf (ometa-input o) orig-input)
                  (throw 'ometa res))
                res))))))


(defmethod core-apply-with-args ((o ometa-base) rule &rest args)
  (dolist (arg (reverse args))
    (core-prepend-input o arg))
  (core-apply o rule))

(defmethod core-prepend-input ((o ometa-base) value)
  (setf (ometa-input o) 
        (make-ometa-stream-with value (ometa-input o))))

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

;; many1 fun
;; same as many, but requires one success
(defmethod core-many1 ((o ometa-base) fun)
  (core-many o fun (funcall fun)))

(defmethod core-repeat ((o ometa-base) times fun)
  (let ((res (list)))
    (do ((i 0 (+ 1 i))) ((= times i)) 
      (let* ((orig-input (ometa-input o))
             (r (catch 'ometa (funcall fun))))
        (if (ometa-error-p r)
            (progn
              (setf (ometa-input o) orig-input)
              (throw-ometa-error))
            (setf res (cons r res)))))
    (reverse res)))


(defmethod core-form ((o ometa-base) fun)
  (let ((current-input (ometa-input o)))
    (let ((v (core-apply o 'anything)))
      (core-pred o (listp v))
      (let ((next-input (ometa-input o)))
        (setf (ometa-input o) (stream-branch current-input))
        (let ((res (funcall fun)))
          (core-apply o 'end)
          (setf (ometa-input o) next-input)
          res)))))
    


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

(defmethod str-number ((o ometa-base))
  (let ((ds (core-many1 o 
                        (lambda () 
                          (core-apply o 'digit)))))
    (parse-integer (concatenate 'string ds))))

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

(defmethod seq-s ((o ometa-base))
  (let ((res (core-apply o 'seq)))
    (core-apply o 'spaces)
    res))

(defmethod token ((o ometa-base))
  (coerce (core-apply-with-args o 'first-and-rest 'letter 'alpha-char) 'string))

(defmethod token-s ((o ometa-base))
  (let ((k (core-apply o 'token)))
    (core-apply o 'spaces)
    k))

(defmethod asymbol ((o ometa-base))
  (let ((s (core-apply o 'anything)))
    (core-pred o (symbolp s))
    s))

(defmethod identifier ((o ometa-base))
  (let ((ret (core-apply o 'token)))
    (core-apply o 'spaces)
    (let ((min (string-upcase ret)))
      (intern min))))

(defmethod str-eq ((o ometa-base))
  (let* ((wanted (core-apply o 'anything))
         (found  (core-apply o 'anything)))
    (core-pred o (and (stringp wanted) (stringp found)))
    (core-pred o (string= wanted found))
    found))
