(defclass ometa-stream ()
  ((head   :accessor stream-head
           :initarg :head)   
   (input  :accessor stream-input
           :initarg :input)
   (idx    :accessor stream-index
           :initarg :idx
           :initform -1)
   (line   :initarg :line
           :initform 1
           :accessor stream-current-line)
   (tail   :initarg :tail
           :initform nil)
   (memo   :initform (make-hash-table)
           :accessor stream-memo)
   (errors :initform (list)
           :accessor stream-errors)))

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


(defgeneric s-first (s))
(defgeneric s-size (s))

(defmethod s-first ((s string))
  (char s 0))

(defmethod s-first ((c cons))
  (car c))

(defmethod s-first ((n null))  n)

(defmethod s-size ((s string))
  (array-total-size s))

(defmethod s-size ((c cons))
  (length c))

(defmethod s-size ((n null)) 0)

(defmethod s-index ((s string) idx)
  (aref s idx))

(defmethod s-index ((c cons) idx)
  (nth idx c))

;; is there a need for s-index((n null))?

(defmethod make-ometa-stream (seq)
  (if (eq seq nil)
      (make-instance 'ometa-stream-end :input seq :idx 0)
      (make-instance 'ometa-stream 
                     :input seq
                     :idx 0
                     :head (s-first seq))))

(defmethod make-ometa-stream-with (head (s ometa-stream))
  (make-instance 'ometa-stream
                 :head head
                 :input (stream-input s)
                 :idx -1
                 :line -1
                 :tail s))
                 

;private
(defun new-ometa-stream (lst line idx)
  (if (>= idx (s-size lst))
      (make-instance 'ometa-stream-end :input lst :idx (s-size lst))
      (make-instance 'ometa-stream
                     :input lst
                     :idx idx
                     :line line
                     :head (s-index lst idx))))

(defmethod stream-tail ((s ometa-stream))
  (if (not (null (slot-value s 'tail)))
      (slot-value s 'tail)
      (let ((tail (new-ometa-stream (stream-input s)
                                    (stream-current-line s)
                                    (1+ (stream-index s)))))
        (if (newline-p (stream-head tail))
            (incf (slot-value tail 'line)))            
        (setf (slot-value s 'tail) tail))))


(defmethod stream-length ((s ometa-stream))
  (s-size (stream-input s)))

(defmethod stream-current-phrase ((s ometa-stream))
  (let* ((begin (stream-index s))
         (len (stream-length s))
         (left (- len (stream-index s)))
         (phrase (subseq (stream-input s) 
                         begin (if (< left 10) len (+ begin 10)))))
    (concatenate 'string "'" (substitute #\Space #\Newline phrase)  "'")))

(defmethod stream-farthest-error-element ((s ometa-stream))
  (labels ((lookup (cur)
             (let ((tail (slot-value cur 'tail)))
               (if tail
                   (let ((res (lookup tail)))
                     (if (stream-errors res) res cur))
                   cur))))
    (lookup s)))

;; must check 2-ahead. basic rule "anything
;; ends up creating a tail for the current element
(defmethod stream-ahead2-p ((s ometa-stream))
  (if (null (slot-value s 'tail))
      nil
      (null (slot-value (slot-value s 'tail) 'tail))))

;; lookup the tails for the first element with valid index
;; meaning, s might be an argument prepended. 
;; We want the original input
(defmethod stream-next-from-input ((s ometa-stream))
  (labels ((lookup (cur)
             (if (< (stream-index cur) 0)
                 (lookup (slot-value cur 'tail))
                 cur)))
    (lookup s)))

(defmethod stream-add-error ((s ometa-stream) rule)
  (let ((real-s (stream-next-from-input s)))
    (let ((hd (if (eq real-s s) nil (stream-head s))))
      (if (stream-ahead2-p real-s) ; we are the farthest scanned. 
                                   ; substitute current errors with the new one
          (setf (slot-value real-s 'errors) (cons rule hd))))))
          ;; this is not the farthest error. join the errors
          ;;(setf (slot-value real-s 'errors) (cons (cons rule hd) (slot-value real-s 'errors)))))))

(defmethod stream-get-pretty-error ((s ometa-stream))
  (let* ((last (stream-farthest-error-element s))
         (err (slot-value last 'errors)))
    (format nil "[~d] near ~a. Expected ~a with '~a'" 
            (stream-current-line last)
            (stream-current-phrase last)
            (car err) (cdr err))))
                 