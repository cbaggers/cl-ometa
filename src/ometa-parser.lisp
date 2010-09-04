(defclass ometa-parser (ometa-base)
          ((current-rule :initform nil :accessor ometa-current-rule)
           (local-variables :initform (make-hash-table) :accessor
            ometa-local-variables))) 
 (defmethod ometa-add-local-var ((o ometa-parser) var)
   (let* ((rule (ometa-current-rule o))
          (vars (gethash rule (ometa-local-variables o))))
     (unless (find var vars)
       (push var (gethash rule (ometa-local-variables o)))))) 
 (defmethod ometa-local-variables-list ((o ometa-parser))
   (let ((res nil))
     (maphash (lambda (k v) (setq res (cons (list k v) res)))
              (ometa-local-variables o))
     res)) 
 (defmethod spacing ((o ometa-parser))
   (core-or o
            (lambda ()
              (core-or o
                       (lambda ()
                         (progn
                          (core-apply-with-args o 'seq '(#\/ #\*))
                          (core-many o
                                     (lambda ()
                                       (core-or o
                                                (lambda ()
                                                  (progn
                                                   (core-not o
                                                             (lambda ()
                                                               (core-apply-with-args
                                                                o 'seq
                                                                '(#\* #\/))))
                                                   (core-apply o
                                                               'anything))))))
                          (core-apply-with-args o 'seq '(#\* #\/))))
                       (lambda () (progn (call-next-method o))))))) 
 (defmethod ometa ((o ometa-parser))
   (let ((r nil) (ic nil) (sl nil) (i nil) (name nil))
     (core-or o
              (lambda ()
                (core-or o
                         (lambda ()
                           (progn
                            (core-apply o 'spaces)
                            (progn
                             (core-apply-with-args o 'seq
                                                   '(#\o #\m #\e #\t #\a))
                             (core-apply o 'spaces))
                            (setq name (core-apply o 'identifier))
                            (setq i (core-apply o 'inheritance))
                            (progn
                             (core-apply-with-args o 'exactly #\{)
                             (core-apply o 'spaces))
                            (setq sl (core-apply o 'cl-slots))
                            (setq ic (core-apply o 'inline-code))
                            (setq r (core-apply o 'rules))
                            (progn
                             (core-apply-with-args o 'exactly #\})
                             (core-apply o 'spaces))
                            (core-apply o 'end)
                            `(grammar ,name ,i
                              (locals ,(ometa-local-variables-list o))
                              (slots ,sl) (inline ,ic) ,@r)))))))) 
 (defmethod inheritance ((o ometa-parser))
   (let ((i nil))
     (core-or o
              (lambda ()
                (core-or o
                         (lambda ()
                           (progn
                            (progn
                             (core-apply-with-args o 'seq '(#\< #\:))
                             (core-apply o 'spaces))
                            (setq i (core-apply o 'identifier))
                            `(parent ,i)))
                         (lambda () (progn '(parent ometa-base)))))))) 
 (defmethod cl-slots ((o ometa-parser))
   (let ((s nil))
     (core-or o
              (lambda ()
                (core-or o
                         (lambda ()
                           (progn
                            (progn
                             (core-apply-with-args o 'seq
                                                   '(#\_ #\s #\l #\o #\t #\s))
                             (core-apply o 'spaces))
                            (setq s
                                    (core-many o
                                               (lambda ()
                                                 (core-or o
                                                          (lambda ()
                                                            (progn
                                                             (core-not o
                                                                       (lambda
                                                                           ()
                                                                         (core-apply-with-args
                                                                          o
                                                                          'exactly
                                                                          #\;)))
                                                             (core-apply o
                                                                         'anything)))))))
                            (progn
                             (core-apply-with-args o 'exactly #\;)
                             (core-apply o 'spaces))
                            (concatenate 'string s)))
                         (lambda () 'nil)))))) 
 (defmethod inline-code ((o ometa-parser))
   (let ((c nil))
     (core-or o
              (lambda ()
                (core-or o
                         (lambda ()
                           (progn
                            (progn
                             (core-apply-with-args o 'seq
                                                   '(#\_ #\i #\n #\l #\i #\n
                                                     #\e))
                             (core-apply o 'spaces))
                            (core-apply-with-args o 'exactly #\[)
                            (setq c
                                    (core-many o
                                               (lambda ()
                                                 (core-or o
                                                          (lambda ()
                                                            (progn
                                                             (core-not o
                                                                       (lambda
                                                                           ()
                                                                         (core-apply-with-args
                                                                          o
                                                                          'exactly
                                                                          #\])))
                                                             (core-apply o
                                                                         'anything)))))))
                            (core-apply-with-args o 'exactly #\])
                            (progn
                             (core-apply-with-args o 'exactly #\;)
                             (core-apply o 'spaces))
                            (concatenate 'string c)))
                         (lambda () 'nil)))))) 
 (defmethod rules ((o ometa-parser))
   (core-or o
            (lambda ()
              (core-or o
                       (lambda ()
                         (progn
                          (core-many1 o (lambda () (core-apply o 'rule))))))))) 
 (defmethod rule ((o ometa-parser))
   (let ((p nil) (rname nil))
     (core-or o
              (lambda ()
                (core-or o
                         (lambda ()
                           (progn
                            (core-lookahead o
                                            (lambda ()
                                              (core-or o
                                                       (lambda ()
                                                         (progn
                                                          (setq rname
                                                                  (core-apply o
                                                                              'rule-name)))))))
                            (setq p
                                    (core-many1 o
                                                (lambda ()
                                                  (core-apply-with-args o
                                                                        'rule-part
                                                                        rname))))
                            `(rule ,rname (or ,@p))))))))) 
 (defmethod rule-part ((o ometa-parser))
   (let ((r nil) (rname nil) (rn nil))
     (core-or o
              (lambda ()
                (progn
                 (setq rn (core-apply o 'anything))
                 (core-or o
                          (lambda ()
                            (progn
                             (setq rname (core-apply o 'rule-name))
                             (core-pred o (eq rname rn))
                             (setq r (core-apply o 'rule-rest))
                             (progn
                              (core-apply-with-args o 'exactly #\;)
                              (core-apply o 'spaces))
                             r)))))))) 
 (defmethod rule-rest ((o ometa-parser))
   (let ((ac nil) (c nil) (args nil))
     (core-or o
              (lambda ()
                (core-or o
                         (lambda ()
                           (progn
                            (progn
                             (core-apply-with-args o 'exactly #\=)
                             (core-apply o 'spaces))
                            (core-apply o 'choices)))
                         (lambda () (progn (core-apply o 'action)))
                         (lambda ()
                           (progn
                            (setq args
                                    (core-many1 o
                                                (lambda ()
                                                  (core-apply o 'argument))))
                            (progn
                             (core-apply-with-args o 'exactly #\=)
                             (core-apply o 'spaces))
                            (setq c (core-apply o 'choices))
                            `(and ,@args ,c)))
                         (lambda ()
                           (progn
                            (setq args
                                    (core-many1 o
                                                (lambda ()
                                                  (core-apply o 'argument))))
                            (setq ac (core-apply o 'action))
                            `(and ,@args ,ac)))))))) 
 (defmethod rule-name ((o ometa-parser))
   (let ((rname nil))
     (core-or o
              (lambda ()
                (core-or o
                         (lambda ()
                           (progn
                            (setq rname (core-apply o 'identifier))
                            (progn
                             (setf (ometa-current-rule o) rname)
                             rname)))))))) 
 (defmethod argument ((o ometa-parser))
   (let ((b nil))
     (core-or o
              (lambda ()
                (core-or o (lambda () (progn (core-apply o 'bind-expression)))
                         (lambda ()
                           (progn
                            (setq b (core-apply o 'binding))
                            `(bind ,b (apply anything))))))))) 
 (defmethod choices ((o ometa-parser))
   (let ((xs nil) (c nil) (x nil))
     (core-or o
              (lambda ()
                (core-or o
                         (lambda ()
                           (progn
                            (setq x (core-apply o 'choice))
                            (setq xs
                                    (core-many o
                                               (lambda ()
                                                 (core-or o
                                                          (lambda ()
                                                            (progn
                                                             (progn
                                                              (core-apply-with-args
                                                               o 'exactly #\|)
                                                              (core-apply o
                                                                          'spaces))
                                                             (setq c
                                                                     (core-apply
                                                                      o
                                                                      'choice))
                                                             c))))))
                            `(or ,x ,@xs)))))))) 
 (defmethod choice ((o ometa-parser))
   (let ((ac nil) (x nil))
     (core-or o
              (lambda ()
                (core-or o
                         (lambda ()
                           (progn
                            (setq x
                                    (core-many o
                                               (lambda ()
                                                 (core-apply o
                                                             'top-expression))))
                            (setq ac (core-apply o 'action))
                            `(and ,@x ,ac)))
                         (lambda ()
                           (progn
                            (setq x
                                    (core-many o
                                               (lambda ()
                                                 (core-apply o
                                                             'top-expression))))
                            `(and ,@x)))))))) 
 (defmethod top-expression ((o ometa-parser))
   (core-or o
            (lambda ()
              (core-or o (lambda () (progn (core-apply o 'bind-expression)))
                       (lambda ()
                         (progn (core-apply o 'repeated-expression))))))) 
 (defmethod bind-expression ((o ometa-parser))
   (let ((b nil) (e nil))
     (core-or o
              (lambda ()
                (core-or o
                         (lambda ()
                           (progn
                            (setq e (core-apply o 'repeated-expression))
                            (setq b (core-apply o 'binding))
                            `(bind ,b ,e)))))))) 
 (defmethod repeated-expression ((o ometa-parser))
   (let ((e nil))
     (core-or o
              (lambda ()
                (core-or o
                         (lambda ()
                           (progn
                            (setq e (core-apply o 'term))
                            (progn
                             (core-apply-with-args o 'exactly #\*)
                             (core-apply o 'spaces))
                            `(many ,e)))
                         (lambda ()
                           (progn
                            (setq e (core-apply o 'term))
                            (progn
                             (core-apply-with-args o 'exactly #\+)
                             (core-apply o 'spaces))
                            `(many1 ,e)))
                         (lambda ()
                           (progn
                            (setq e (core-apply o 'term))
                            (progn
                             (core-apply-with-args o 'exactly #\?)
                             (core-apply o 'spaces))
                            `(optional ,e)))
                         (lambda () (progn (core-apply o 'term)))))))) 
 (defmethod term ((o ometa-parser))
   (let ((e nil))
     (core-or o
              (lambda ()
                (core-or o
                         (lambda ()
                           (progn
                            (core-apply-with-args o 'exactly #\~)
                            (setq e (core-apply o 'element))
                            `(not ,e)))
                         (lambda ()
                           (progn
                            (core-apply-with-args o 'exactly #\&)
                            (setq e (core-apply o 'element))
                            `(lookahead ,e)))
                         (lambda () (progn (core-apply o 'element)))))))) 
 (defmethod binding ((o ometa-parser))
   (let ((i nil))
     (core-or o
              (lambda ()
                (core-or o
                         (lambda ()
                           (progn
                            (core-apply-with-args o 'exactly #\:)
                            (setq i (core-apply o 'identifier))
                            (progn (ometa-add-local-var o i) i)))))))) 
 (defmethod element ((o ometa-parser))
   (let ((c nil) (s nil))
     (core-or o
              (lambda ()
                (core-or o (lambda () (progn (core-apply o 'prod-app)))
                         (lambda () (progn (core-apply o 'data-element)))
                         (lambda ()
                           (progn
                            (core-apply-with-args o 'exactly #\%)
                            (setq s (core-apply o 'host-lang-expr))
                            `(sem-predicate ,s)))
                         (lambda ()
                           (progn
                            (progn
                             (core-apply-with-args o 'exactly #\{)
                             (core-apply o 'spaces))
                            (setq c (core-apply o 'choices))
                            (progn
                             (core-apply-with-args o 'exactly #\})
                             (core-apply o 'spaces))
                            c))))))) 
 (defmethod data-element ((o ometa-parser))
   (core-or o
            (lambda ()
              (core-or o (lambda () (progn (core-apply o 'char-sequence)))
                       (lambda () (progn (core-apply o 'char-sequence-s)))
                       (lambda () (progn (core-apply o 'string-literal)))
                       (lambda () (progn (core-apply o 'asymbol)))
                       (lambda () (progn (core-apply o 's-expr)))
                       (lambda () (progn (core-apply o 'any-symb)))
                       (lambda () (progn (core-apply o 'end-symb))))))) 
 (defmethod action ((o ometa-parser))
   (let ((s nil))
     (core-or o
              (lambda ()
                (core-or o
                         (lambda ()
                           (progn
                            (progn
                             (core-apply-with-args o 'seq '(#\= #\>))
                             (core-apply o 'spaces))
                            (setq s (core-apply o 'host-lang-expr))
                            `(action ,s)))))))) 
 (defmethod host-lang-expr ((o ometa-parser))
   (let ((s nil) (e nil) (q nil))
     (core-or o
              (lambda ()
                (core-or o
                         (lambda ()
                           (progn
                            (setq q (core-apply o 'host-lang-quote))
                            (setq e (core-apply o 'host-lang-expand))
                            (setq s (core-apply o 'host-lang-s-expr))
                            (str-trim (concatenate 'string q e s))))))))) 
 (defmethod host-lang-quote ((o ometa-parser))
   (let ((q nil))
     (core-or o
              (lambda ()
                (core-or o
                         (lambda ()
                           (progn
                            (core-or o
                                     (lambda ()
                                       (progn
                                        (setq q
                                                (core-many1 o
                                                            (lambda ()
                                                              (core-apply-with-args
                                                               o 'exactly
                                                               #\`))))
                                        (coerce q 'string)))
                                     (lambda ()
                                       (progn
                                        (setq q
                                                (core-many1 o
                                                            (lambda ()
                                                              (core-apply-with-args
                                                               o 'exactly
                                                               #\'))))
                                        (coerce q 'string)))
                                     (lambda () (progn "")))))))))) 
 (defmethod host-lang-expand ((o ometa-parser))
   (let ((aa nil) (a nil) (qq nil) (q nil))
     (core-or o
              (lambda ()
                (core-or o
                         (lambda ()
                           (progn
                            (setq qq
                                    (core-or o
                                             (lambda ()
                                               (progn
                                                (setq q
                                                        (core-many1 o
                                                                    (lambda ()
                                                                      (core-apply-with-args
                                                                       o
                                                                       'exactly
                                                                       #\,))))
                                                (concatenate 'string q)))
                                             (lambda () (progn ""))))
                            (setq aa
                                    (core-or o
                                             (lambda ()
                                               (progn
                                                (setq a
                                                        (core-apply-with-args o
                                                                              'exactly
                                                                              #\@))
                                                (string a)))
                                             (lambda () (progn ""))))
                            (concatenate 'string qq aa)))))))) 
 (defmethod host-lang-s-expr ((o ometa-parser))
   (let ((x nil))
     (core-or o
              (lambda ()
                (core-or o (lambda () (progn (core-apply o 'host-lang-atom)))
                         (lambda ()
                           (progn
                            (progn
                             (core-apply-with-args o 'exactly #\()
                             (core-apply o 'spaces))
                            (setq x
                                    (core-many o
                                               (lambda ()
                                                 (core-or o
                                                          (lambda ()
                                                            (progn
                                                             (core-apply o
                                                                         'host-lang-atom)))
                                                          (lambda ()
                                                            (progn
                                                             (core-apply o
                                                                         'host-lang-expr)))))))
                            (progn
                             (core-apply-with-args o 'exactly #\))
                             (core-apply o 'spaces))
                            (if x
                                (concatenate 'string " ("
                                             (reduce
                                              (lambda (a b)
                                                (concatenate 'string a " " b))
                                              x)
                                             ") ")
                                "")))))))) 
 (defmethod host-lang-atom ((o ometa-parser))
   (let ((a nil) (l nil) (e nil) (q nil))
     (core-or o
              (lambda ()
                (core-or o
                         (lambda ()
                           (progn
                            (setq q (core-apply o 'host-lang-quote))
                            (setq e (core-apply o 'host-lang-expand))
                            (setq a
                                    (core-or o
                                             (lambda ()
                                               (progn
                                                (core-apply o 's-identifier)))
                                             (lambda ()
                                               (progn
                                                (setq l
                                                        (core-apply o
                                                                    'string-literal))
                                                (coerce `(,#\" ,@l #\")
                                                        'string)))))
                            (concatenate 'string q e a)))))))) 
 (defmethod s-identifier ((o ometa-parser))
   (let ((xs nil) (c nil))
     (core-or o
              (lambda ()
                (core-or o
                         (lambda ()
                           (progn
                            (setq xs
                                    (core-many1 o
                                                (lambda ()
                                                  (core-or o
                                                           (lambda ()
                                                             (progn
                                                              (core-not o
                                                                        (lambda
                                                                            ()
                                                                          (core-or
                                                                           o
                                                                           (lambda
                                                                               ()
                                                                             (progn
                                                                              (core-apply
                                                                               o
                                                                               'spacing)))
                                                                           (lambda
                                                                               ()
                                                                             (progn
                                                                              (core-apply-with-args
                                                                               o
                                                                               'exactly
                                                                               #\;)))
                                                                           (lambda
                                                                               ()
                                                                             (progn
                                                                              (core-apply-with-args
                                                                               o
                                                                               'exactly
                                                                               #\()))
                                                                           (lambda
                                                                               ()
                                                                             (progn
                                                                              (core-apply-with-args
                                                                               o
                                                                               'exactly
                                                                               #\))))
                                                                           (lambda
                                                                               ()
                                                                             (progn
                                                                              (core-apply-with-args
                                                                               o
                                                                               'exactly
                                                                               #\})))
                                                                           (lambda
                                                                               ()
                                                                             (progn
                                                                              (core-apply-with-args
                                                                               o
                                                                               'exactly
                                                                               #\{)))
                                                                           (lambda
                                                                               ()
                                                                             (progn
                                                                              (core-apply-with-args
                                                                               o
                                                                               'exactly
                                                                               #\|))))))
                                                              (setq c
                                                                      (core-apply
                                                                       o 'chr))
                                                              c))))))
                            (core-apply o 'spaces)
                            (coerce xs 'string)))))))) 
 (defmethod prod-app ((o ometa-parser))
   (let ((args nil) (p nil))
     (core-or o
              (lambda ()
                (core-or o
                         (lambda ()
                           (progn
                            (progn
                             (core-apply-with-args o 'exactly #\<)
                             (core-apply o 'spaces))
                            (setq p (core-apply o 'identifier))
                            (progn
                             (core-apply-with-args o 'exactly #\>)
                             (core-apply o 'spaces))
                            `(apply ,p)))
                         (lambda ()
                           (progn
                            (setq p (core-apply o 'identifier))
                            `(apply ,p)))
                         (lambda ()
                           (progn
                            (progn
                             (core-apply-with-args o 'exactly #\<)
                             (core-apply o 'spaces))
                            (setq p (core-apply o 'identifier))
                            (setq args (core-apply o 'prod-arg-list))
                            (progn
                             (core-apply-with-args o 'exactly #\>)
                             (core-apply o 'spaces))
                            `(apply-with-args ,p (arguments ,@args))))
                         (lambda ()
                           (progn
                            (progn
                             (core-apply-with-args o 'exactly #\^)
                             (core-apply o 'spaces))
                            `(apply-super ,(ometa-current-rule o))))
                         (lambda ()
                           (progn
                            (progn
                             (core-apply-with-args o 'seq '(#\< #\^))
                             (core-apply o 'spaces))
                            (setq args (core-apply o 'prod-arg-list))
                            (progn
                             (core-apply-with-args o 'exactly #\>)
                             (core-apply o 'spaces))
                            `(apply-super-with-args ,(ometa-current-rule o)
                              (arguments ,@args))))))))) 
 (defmethod prod-arg-list ((o ometa-parser))
   (let ((xs nil) (a nil) (x nil))
     (core-or o
              (lambda ()
                (core-or o
                         (lambda ()
                           (progn
                            (setq x (core-apply o 'prod-arg))
                            (setq xs
                                    (core-many o
                                               (lambda ()
                                                 (core-or o
                                                          (lambda ()
                                                            (progn
                                                             (progn
                                                              (core-apply-with-args
                                                               o 'exactly #\,)
                                                              (core-apply o
                                                                          'spaces))
                                                             (setq a
                                                                     (core-apply
                                                                      o
                                                                      'prod-arg))
                                                             a))))))
                            (cons x xs)))))))) 
 (defmethod prod-arg ((o ometa-parser))
   (core-or o
            (lambda ()
              (core-or o (lambda () (progn (core-apply o 'data-element)))
                       (lambda () (progn (core-apply o 'identifier))))))) 
 (defmethod char-sequence ((o ometa-parser))
   (let ((cs nil) (c nil))
     (core-or o
              (lambda ()
                (core-or o
                         (lambda ()
                           (progn
                            (core-apply-with-args o 'exactly #\')
                            (setq cs
                                    (core-many1 o
                                                (lambda ()
                                                  (core-or o
                                                           (lambda ()
                                                             (progn
                                                              (core-apply-with-args
                                                               o 'exactly #\\)
                                                              (core-apply-with-args
                                                               o 'exactly
                                                               #\')))
                                                           (lambda ()
                                                             (progn
                                                              (core-apply-with-args
                                                               o 'exactly #\\)
                                                              (core-apply-with-args
                                                               o 'exactly
                                                               #\\)))
                                                           (lambda ()
                                                             (progn
                                                              (core-not o
                                                                        (lambda
                                                                            ()
                                                                          (core-apply-with-args
                                                                           o
                                                                           'exactly
                                                                           #\')))
                                                              (setq c
                                                                      (core-apply
                                                                       o 'chr))
                                                              c))))))
                            (progn
                             (core-apply-with-args o 'exactly #\')
                             (core-apply o 'spaces))
                            `(seq ,(coerce cs 'string))))))))) 
 (defmethod char-sequence-s ((o ometa-parser))
   (let ((cs nil) (c nil))
     (core-or o
              (lambda ()
                (core-or o
                         (lambda ()
                           (progn
                            (core-apply-with-args o 'exactly #\")
                            (setq cs
                                    (core-many o
                                               (lambda ()
                                                 (core-or o
                                                          (lambda ()
                                                            (progn
                                                             (core-apply-with-args
                                                              o 'exactly #\\)
                                                             (core-apply-with-args
                                                              o 'exactly #\")))
                                                          (lambda ()
                                                            (progn
                                                             (core-apply-with-args
                                                              o 'exactly #\\)
                                                             (core-apply-with-args
                                                              o 'exactly #\\)))
                                                          (lambda ()
                                                            (progn
                                                             (core-not o
                                                                       (lambda
                                                                           ()
                                                                         (core-apply-with-args
                                                                          o
                                                                          'exactly
                                                                          #\")))
                                                             (setq c
                                                                     (core-apply
                                                                      o 'chr))
                                                             c))))))
                            (progn
                             (core-apply-with-args o 'exactly #\")
                             (core-apply o 'spaces))
                            `(and (seq ,(coerce cs 'string))
                                  (apply spaces))))))))) 
 (defmethod string-literal ((o ometa-parser))
   (let ((cs nil) (c nil))
     (core-or o
              (lambda ()
                (core-or o
                         (lambda ()
                           (progn
                            (core-apply-with-args o 'seq '(#\` #\`))
                            (setq cs
                                    (core-many o
                                               (lambda ()
                                                 (core-or o
                                                          (lambda ()
                                                            (progn
                                                             (core-not o
                                                                       (lambda
                                                                           ()
                                                                         (core-apply-with-args
                                                                          o
                                                                          'seq
                                                                          '(#\`
                                                                            #\`))))
                                                             (setq c
                                                                     (core-apply
                                                                      o 'char))
                                                             c))))))
                            (progn
                             (core-apply-with-args o 'seq '(#\' #\'))
                             (core-apply o 'spaces))
                            `(exactly ,(coerce cs 'string))))))))) 
 (defmethod asymbol ((o ometa-parser))
   (let ((s nil))
     (core-or o
              (lambda ()
                (core-or o
                         (lambda ()
                           (progn
                            (core-apply-with-args o 'exactly #\#)
                            (setq s (core-apply o 'identifier))
                            `(symbol ,s)))))))) 
 (defmethod s-expr ((o ometa-parser))
   (let ((s nil))
     (core-or o
              (lambda ()
                (core-or o
                         (lambda ()
                           (progn
                            (progn
                             (core-apply-with-args o 'exactly #\()
                             (core-apply o 'spaces))
                            (setq s (core-apply o 'choice))
                            (progn
                             (core-apply-with-args o 'exactly #\))
                             (core-apply o 'spaces))
                            `(form ,s)))))))) 
 (defmethod la-prefix ((o ometa-parser))
   (core-or o
            (lambda ()
              (core-or o
                       (lambda ()
                         (progn
                          (progn
                           (core-apply-with-args o 'exactly #\&)
                           (core-apply o 'spaces)))))))) 
 (defmethod not-prefix ((o ometa-parser))
   (core-or o
            (lambda ()
              (core-or o
                       (lambda ()
                         (progn
                          (progn
                           (core-apply-with-args o 'exactly #\~)
                           (core-apply o 'spaces)))))))) 
 (defmethod sem-prefix ((o ometa-parser))
   (core-or o
            (lambda ()
              (core-or o
                       (lambda ()
                         (progn
                          (progn
                           (core-apply-with-args o 'exactly #\%)
                           (core-apply o 'spaces)))))))) 
 (defmethod end-symb ((o ometa-parser))
   (core-or o
            (lambda ()
              (core-or o
                       (lambda ()
                         (progn
                          (progn
                           (core-apply-with-args o 'exactly #\$)
                           (core-apply o 'spaces))
                          '(apply end))))))) 
 (defmethod any-symb ((o ometa-parser))
   (core-or o
            (lambda ()
              (core-or o
                       (lambda ()
                         (progn
                          (progn
                           (core-apply-with-args o 'exactly #\_)
                           (core-apply o 'spaces))
                          '(apply anything))))))) 
 