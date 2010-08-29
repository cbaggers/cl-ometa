(defclass ometa-stream ()
  ((head   :accessor stream-head
           :initarg :head)   
   (input  :accessor stream-input
           :initarg :input)
   (idx    :accessor stream-index
           :initarg :idx
           :initform -1)
   (tail   :initarg :tail
           :initform nil)
   (memo   :initform (make-hash-table)
           :accessor stream-memo)))

(defclass ometa-stream-end (ometa-stream) ())
(defmethod stream-head  ((s ometa-stream-end)) (throw-ometa-error))
(defmethod stream-tail  ((s ometa-stream-end)) (throw-ometa-error))

(defclass stream-memo ()
  ((next   :accessor stream-memo-next
           :initarg :next)
   (result :accessor stream-memo-result
           :initarg :result)))

(defmethod stream-memo-for ((s ometa-stream) rule)
  (gethash rule (stream-memo s)))

(defmethod stream-memoize ((s ometa-stream) rule next res)
  (setf (gethash rule (stream-memo s)) (make-instance 'stream-memo :next next :result res)))

;public
;TODO: polimorph/generalize on seq (string, form, etc..). 'char' is specific of string
(defmethod make-ometa-stream (seq)
  (make-instance 'ometa-stream 
                 :input seq
                 :idx 0
                 :head (char seq 0)))

(defmethod make-ometa-stream-with (head (s ometa-stream))
  (make-instance 'ometa-stream
                 :head head
                 :input (stream-input s)
                 :idx (stream-index s)
                 :tail s))
                 

;private
(defun new-ometa-stream (str idx)
  (if (>= idx (array-total-size str))
      (make-instance 'ometa-stream-end :input str :idx (array-total-size str))
      (make-instance 'ometa-stream
                     :input str
                     :idx idx
                     :head (char str idx))))

(defmethod stream-tail ((s ometa-stream))
  (if (not (null (slot-value s 'tail)))
      (slot-value s 'tail)
      (setf (slot-value s 'tail)
            (new-ometa-stream (stream-input s)
                              (1+ (stream-index s))))))
      

(defmethod stream-length ((s ometa-stream))
  (array-total-size (stream-input s)))

(defmethod stream-current-phrase ((s ometa-stream))
  (let* ((begin (stream-index s))
         (len (stream-length s))
         (left (- len (stream-index s))))
    (concatenate 'string
                 "'"
                 (subseq (stream-input s) begin (if (< left 10) len (+ begin 10)))
                 "'")))


;; (defun test-slots (s)
;;   (list (slot-value s 'head)
;;         (slot-value s 'idx)
;;         (slot-value s 'input)
;;         (slot-value s 'tail)))
;; (setf a (make-ometa-stream "abc"))
;; (setf b (stream-tail a))
;; (setf c (stream-tail b))
;; (setf d (stream-tail c))

;; (test-slots a)
;; (test-slots b)
;; (test-slots c)
;; (test-slots d)
;; (funcall #'stream-tail a) 