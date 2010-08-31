(defclass ometa-parser (ometa-base) 
  ((current-rule    :initform nil 
                    :accessor ometa-current-rule)
   (local-variables :initform (make-hash-table)
                    :accessor ometa-local-variables)))

(defmethod ometa-add-local-var ((o ometa-parser) var)
  (let ((rule (ometa-current-rule o)))
    (push var (gethash rule (ometa-local-variables o)))))

(defmethod ometa-local-variables-list ((o ometa-parser))
  (let ((res nil))
    (maphash (lambda (k v) (setq res (cons (list k v) res))) (ometa-local-variables o))
    res))




;;   space = '/*' { ~'*/' _}* '*/'
;;         | ^
;;         ;
(defmethod spacing ((o ometa-parser))
  (core-or o
           (lambda () 
             (core-apply-with-args o 'seq '(#\/ #\*))
             (core-many o (lambda ()
                            (core-not o (lambda () (core-apply-with-args o 'seq '(#\* #\/))))
                            (core-apply o 'anything)))
             (core-apply-with-args o 'seq '(#\* #\/)))
           (lambda ()
             (call-next-method))))

;;   ometa = spaces "ometa" identifier:name inheritance:i "{" rules:r "}" $
;;            => `(grammar ,name ,i ,@r);
(defmethod ometa ((o ometa-parser))
  (core-apply o 'spaces)
  (core-apply-with-args o 'seq-s '(#\o #\m #\e #\t #\a))
  (let ((name (core-apply o 'identifier)))
    (let ((i (core-apply o 'inheritance)))
      (core-apply-with-args o 'seq-s '(#\{))
      (let ((r (core-apply o 'rules)))
        (core-apply-with-args o 'seq-s '(#\}))
        (core-apply o 'end)
        `(grammar ,name ,i (locals ,(ometa-local-variables-list o)) ,@r)))))

;;   inheritance = "<:" identifier:i => `(parent ,i)
;;               |                   => `(parent OMeta)
;;               ;
(defmethod inheritance ((o ometa-parser))
  (core-or o
   (lambda ()
     (core-apply-with-args o 'seq-s '(#\< #\:))
     (let ((i (core-apply o 'identifier)))
       `(parent ,i)))
   (lambda ()
     `(parent "OMeta"))))
    

;;   rules  = rule+;
(defmethod rules ((o ometa-parser))
  (core-many1 o
   (lambda () (core-apply o 'rule))))

;;  rule  = &{rule-name:rname} <rule-part rname>+:p => `(rule ,rname (or ,@p));
(defmethod rule ((o ometa-parser))
  (let ((rname (core-lookahead o (lambda () (core-apply o 'rule-name)))))
    (let ((p (core-many1 o (lambda () (core-apply-with-args o 'rule-part rname)))))
      `(rule ,rname, `(or ,@p)))))

;;  rule-part :rn = rule-name:rname %(equal rname rn) rule-rest:r ";" => r;
(defmethod rule-part ((o ometa-parser))
  (let ((rn (core-apply o 'anything)))
    (let ((rname (core-apply o 'rule-name)))
      (core-pred o (equal rname rn))
      (let ((r (core-apply o 'rule-rest)))
        (core-apply-with-args o 'seq-s '(#\;))
        r))))

;; rule-rest = "=" choices
;;           |  action
;;           |  argument+:args "=" choices:c => `(and ,@args ,c)
;;           |  argument+:args  action:ac      => `(and ,@args ,ac)
;;           ;
(defmethod rule-rest ((o ometa-parser))
  (core-or o 
           (lambda ()
             (core-apply-with-args o 'seq-s '(#\=))
             (core-apply o 'choices))
            (lambda ()
              (core-apply o 'action))
           (lambda ()
             (let ((args (core-many1 o (lambda () (core-apply o 'argument)))))
               (core-apply-with-args o 'seq-s '(#\=))
               (let ((c (core-apply o 'choices)))
                 `(and ,@args ,c))))
           (lambda ()
             (let ((args (core-many1 o (lambda () (core-apply o 'argument)))))
               (let ((ac (core-apply o 'action)))
                 `(and ,@args ,ac))))))



;; rule-name =  identifier:rname => (progn (setf (ometa-current-rule o) rname) rname)
;;           ;
(defmethod rule-name ((o ometa-parser))
  (let ((rname (core-apply o 'identifier)))
    (progn
      (setf (ometa-current-rule o) rname)
      rname)))

;; argument = bind-expression
;;          | binding:b       => `(bind ,b (apply anything))
;;          ;
(defmethod argument ((o ometa-parser))
  (core-or o 
           (lambda () 
             (core-apply o 'bind-expression))
           (lambda () 
             (let ((b (core-apply o 'binding)))
               `(bind ,b (apply anything))))))
  


;; choices = choice:x { "|" choice}*:xs => `(or ,x ,@xs);
(defmethod choices ((o ometa-parser))
  (let ((x (core-apply o 'choice)))
    (let ((xs (core-many o
                         (lambda () 
                           (core-apply-with-args o 'seq-s '(#\|))
                           (core-apply o 'choice)))))
      `(or ,x ,@xs))))


;; choice  = top-expression*:x action:ac => `(and ,@x ,ac)
;;         | top-expression*:x           => `(and ,@x)
;;         ;
(defmethod choice ((o ometa-parser))
  (core-or o
            (lambda ()
              (let ((x (core-many o
                                  (lambda ()
                                    (core-apply o 'top-expression)))))
                (let ((ac (core-apply o 'action)))
                  `(and ,@x ,ac))))
            (lambda ()
              (let ((z (core-many o
                                  (lambda ()
                                    (core-apply o 'top-expression)))))
                `(and ,@z)))))


;; top-expression  =   bind-expression
;;                 |   repeated-expression
;;                 ;
(defmethod top-expression ((o ometa-parser))
  (core-or o
           (lambda ()
             (core-apply o 'bind-expression))
           (lambda ()
             (core-apply o 'repeated-expression))))


;; bind-expression = repeated-expression:e binding:b => `(bind ,b ,e)
(defmethod bind-expression ((o ometa-parser))
  (let ((e (core-apply o 'repeated-expression)))
    (let ((b (core-apply o 'binding)))
      `(bind ,b, e))))

;; repeated-expression = term:t "*" => `(many ,t)
;;                     |    term:t "+" => `(many1 ,t)
;;                     |    term:t "?" => `(optional ,t)
;;                     |    term
;;                     ;
(defmethod repeated-expression ((o ometa-parser))
   (core-or o
            (lambda ()
              (let ((t1 (core-apply o 'term)))
                (core-apply-with-args o 'seq-s '(#\*))
                `(many ,t1)))
            (lambda ()
              (let ((t2 (core-apply o 'term)))
                (core-apply-with-args o 'seq-s '(#\+))
                `(many1 ,t2)))
            (lambda ()
              (let ((t3 (core-apply o 'term)))
                (core-apply-with-args o 'seq-s '(#\?))
                `(optional ,t3)))
            (lambda ()
              (core-apply o 'term))))

;; term  = '~'  element:e => `(not ,e)
;;        |  '&'  element:e => `(lookahead ,e)
;;        |  element
;;        ;
(defmethod term ((o ometa-parser))
  (core-or o
           (lambda ()
             (core-apply-with-args o 'exactly #\~)
              (let ((e (core-apply o 'element)))
                `(not ,e)))
           (lambda ()
             (core-apply-with-args o 'exactly #\&)
             (let ((e (core-apply o 'element)))
               `(lookahead ,e)))
           (lambda ()
             (core-apply o 'element))))
           
;; binding    = ':' identifier:i => (progn (ometa-add-local o i) i)
;;            ;
(defmethod binding ((o ometa-parser))
  (core-apply-with-args o 'exactly #\:)
  (let ((i (core-apply o 'identifier)))
    (progn
      (ometa-add-local-var o i)
      i)))

;; element    =   prod-app
;;             |  data-element
;;             |  '%' host-lang-expr:s => `(sem-predicate ,s)
;;             |  "{" choices:c "}" => c
;;             ;
(defmethod element ((o ometa-parser))
  (core-or o
           (lambda ()
             (core-apply o 'prod-app))
           (lambda ()
             (core-apply o 'data-element))
           (lambda ()
             (core-apply-with-args o 'exactly #\%)
             (let ((s (core-apply o 'host-lang-expr)))
               `(sem-predicate ,s)))
           (lambda ()
             (core-apply-with-args o 'seq-s '(#\{))
             (let ((c (core-apply o 'choices)))
               (core-apply-with-args o 'seq-s '(#\}))
               c))))
             


;; data-element =   char-sequence
;;               |  char-sequence-s
;;               |  string-literal
;;               |  symbol
;;               |  s-expr
;;               |  any-symb
;;               |  end-symb
;;               ; 
(defmethod data-element ((o ometa-parser))
  (core-or o
           (lambda ()
             (core-apply o 'char-sequence))
           (lambda ()
             (core-apply o 'char-sequence-s))
           (lambda ()
             (core-apply o 'string-literal))
           (lambda ()
             (core-apply o 'asymbol))
           (lambda ()
             (core-apply o 's-expr))
           (lambda ()
             (core-apply o 'any-symb))
           (lambda ()
             (core-apply o 'end-symb))))



;;action   = "=>" host-lang-expr:s => `(action ,s);
(defmethod action ((o ometa-parser))
  (core-apply-with-args o 'seq-s '(#\= #\>))
  (let ((s (core-apply o 'host-lang-expr)))
    `(action ,s)))


;; host-lang-expr  = host-lang-quote:q host-lang-expand:e host-lang-s-expr:s => (string-trim '(#\Space #\Tab #\Newline) (concatenate 'string q e s));
(defmethod host-lang-expr ((o ometa-parser))
  (let ((q (core-apply o 'host-lang-quote)))
    (let ((e (core-apply o 'host-lang-expand)))
      (let ((s (core-apply o 'host-lang-s-expr)))
        (string-trim '(#\Space #\Tab #\Newline) (concatenate 'string q e s))))))


;; host-lang-quote  = {'`'+:q => (coerce q 'string) | '\''+:q => (coerce q 'string) | => ""};
(defmethod host-lang-quote ((o ometa-parser))
  (core-or o
           (lambda ()
             (let ((q (core-many1 o 
                                  (lambda()
                                    (core-apply-with-args o 'exactly #\`)))))
               (coerce q 'string)))
           (lambda ()
             (let ((z (core-many1 o
                                  (lambda ()
                                    (core-apply-with-args o 'exactly #\')))))
               (coerce z 'string)))
           (lambda () "")))

;; host-lang-expand = {','+:q => (coerce q 'string) | => ""}:qq  {'@':a => (coerce a 'string) | => ""}:aa => (concatenate 'string qq aa);
(defmethod host-lang-expand ((o ometa-parser))
  (let ((qq (core-or o
                     (lambda ()
                       (let ((q (core-many1 o 
                                            (lambda ()
                                              (core-apply-with-args o 'exactly #\,)))))
                         (concatenate 'string q)))
                     (lambda () ""))))
    (let ((aa (core-or o
                       (lambda ()
                         (let ((a (core-apply-with-args o 'exactly #\@)))
                           (string a)))
                       (lambda () ""))))
      (concatenate 'string qq aa))))

    
;; host-lang-s-expr  = host-lang-atom
;;                   |  "(" { host-lang-atom | host-lang-expr }*:x ")"
;;                       =>  (concatenate 'string "(" x ")")
;;                   ;
(defmethod host-lang-s-expr ((o ometa-parser))
  (core-or o
           (lambda ()
             (core-apply o 'host-lang-atom))
           (lambda ()
             (core-apply-with-args o 'seq-s '(#\())
             (let ((x (core-many o
                                 (lambda ()
                                   (core-or o
                                            (lambda ()
                                              (core-apply o 'host-lang-atom))
                                            (lambda ()
                                              (core-apply o 'host-lang-expr)))))))
               (core-apply-with-args o 'seq-s '(#\)))
               (concatenate 'string "(" (reduce (lambda (a b) (concatenate 'string a " " b)) x) ")")))))


;; host-lang-atom    = host-lang-quote:q host-lang-expand:e 
;;                       { s-identifier | string-literal:l => (coerce `(#\" ,@l #\") 'string)}:a => (concatenate 'string q e a);
(defmethod host-lang-atom ((o ometa-parser))
  (let ((q (core-apply o 'host-lang-quote)))
    (let ((e (core-apply o 'host-lang-expand)))
      (let ((a (core-or o
                        (lambda ()
                          (core-apply o 's-identifier))
                        (lambda ()
                          (let ((l (core-apply o 'string-literal)))
                            (coerce `(#\" ,@l #\") 'string))))))
        (concatenate 'string q e a)))))

;; s-identifier = {~{space | ';' | '(' | ')' | '}' | '{' | '|' } char:c => c}+:xs spaces 
;;                => (coerce xs 'string);
(defmethod s-identifier ((o ometa-parser))
  (let ((xs 
         (core-many1 o 
                     (lambda ()
                       (core-not o 
                                 (lambda ()
                                   (core-or o
                                            (lambda ()
                                              (core-apply o 'spacing))
                                            (lambda ()
                                              (core-apply-with-args o 'exactly #\;))
                                            (lambda ()
                                              (core-apply-with-args o 'exactly #\())
                                            (lambda ()
                                              (core-apply-with-args o 'exactly #\)))
                                            (lambda ()
                                              (core-apply-with-args o 'exactly #\}))
                                            (lambda ()
                                              (core-apply-with-args o 'exactly #\{))
                                            (lambda ()
                                              (core-apply-with-args o 'exactly #\|)))))
                       (core-apply o 'chr)))))
    (core-apply o 'spaces)
    (coerce xs 'string)))
                          

(defmethod prod-app ((o ometa-parser))
  (core-or o

;; prod-app = "<" identifier:p ">" 
;;                => `(apply ,p)
;;           |  identifier:p
;;                => `(apply ,p)
;;           ;
           (lambda ()
             (core-apply-with-args o 'seq-s '(#\<))
             (let ((p (core-apply o 'identifier)))
               (core-apply-with-args o 'seq-s '(#\>))
               `(apply ,p)))
           (lambda ()
             (let ((p (core-apply o 'identifier)))
               `(apply ,p)))

;; prod-app = "<" identifier:p prod-arg-list:args ">" 
;;                => `(apply-with-args ,p (arguments ,@args))
;;           ;
           (lambda ()
             (core-apply-with-args o 'seq-s '(#\<))
             (let ((p (core-apply o 'identifier)))
               (let ((args (core-apply o 'prod-arg-list)))
                 (core-apply-with-args o 'seq-s '(#\>))
                 `(apply-with-args ,p (arguments ,@args)))))
;; prod-app = "^"
;;             => `(apply-super-with-args ,(ometa-current-rule o) (arguments))
;;           |  "<^" prod-arg-list:args ">"
;;             => `(apply-super-with-args ,(ometa-current-rule o) (arguments ,@args))
;;           ;
           (lambda ()
             (core-apply-with-args o 'seq-s '(#\^))
             `(apply-super ,(ometa-current-rule o)))
           (lambda ()
             (core-apply-with-args o 'seq-s '(#\< #\^))
             (let ((args (core-apply o 'prod-arg-list)))
               (core-apply-with-args o 'seq-s '(#\>))
               `(apply-super-with-args ,(ometa-current-rule o) (arguments ,@args))))))
 

;;  prod-arg-list  = prod-arg:x {"," prod-arg:a => a}*:xs => (cons x xs);
(defmethod prod-arg-list ((o ometa-parser))
  (let ((x (core-apply o 'prod-arg)))
    (let ((xs (core-many o
                         (lambda ()
                           (core-apply-with-args o 'seq-s '(#\,))
                           (core-apply o 'prod-arg)))))
      `(cons x xs))))


                   
;; prod-arg       = data-element | identifier
;;                 ;
(defmethod prod-arg ((o ometa-parser))
  (core-or o
           (lambda () (core-apply o 'data-element))
           (lambda () (core-apply o 'identifier))))

                   



;; char-sequence  = '\'' { '\\' '\'' | '\\' '\\' | ~'\'' char:c => c}+:cs "'" 
;;                     => `(seq ,(list->string cs));
(defmethod char-sequence ((o ometa-parser))
  (core-apply-with-args o 'exactly #\')
  (let ((cs (core-many1 o 
                        (lambda () 
                          (core-or o
                                   (lambda ()
                                     (core-apply-with-args o 'exactly #\\)
                                     (core-apply-with-args o 'exactly #\'))
                                   (lambda ()
                                     (core-apply-with-args o 'exactly #\\)
                                     (core-apply-with-args o 'exactly #\\))
                                   (lambda ()
                                     (core-not o
                                               (lambda () (core-apply-with-args o 'exactly #\')))
                                     (core-apply o 'chr)))))))
    (core-apply-with-args o 'seq-s '(#\'))
    `(seq ,(coerce cs 'string))))

;; char-sequence-s = '"' { '\\' '"' | '\\' '\\' | ~'"'  char:c => c}*:cs "\"" => `(and 
;;                                                                                    (seq ,(list->string cs))
;;                                                                                    (apply spaces));
(defmethod char-sequence-s ((o ometa-parser))
  (core-apply-with-args o 'exactly #\")
  (let ((cs (core-many o
                       (lambda ()
                         (core-or o
                                  (lambda ()
                                    (core-apply-with-args o 'exactly #\\)
                                    (core-apply-with-args o 'exactly #\"))
                                  (lambda ()
                                    (core-apply-with-args o 'exactly #\\)
                                    (core-apply-with-args o 'exactly #\\))
                                  (lambda ()
                                    (core-not o
                                              (lambda ()
                                                (core-apply-with-args o 'exactly #\")))
                                    (core-apply o 'chr)))))))
    (core-apply-with-args o 'seq-s '(#\"))
    `(apply-with-args seq-s (arguments ,(coerce cs 'string)))))

;;  string-literal = '``' {~'``' char:c => c}*:cs "''" => (exactly ,(coerce cs 'string));
(defmethod string-literal ((o ometa-parser))
  (core-apply-with-args o 'seq-s '(#\` #\`))
  (let ((cs (core-many o
                       (lambda ()
                         (core-not o
                                   (lambda ()
                                     (core-apply-with-args 'seq-s '(#\` #\`))))
                         (core-apply o 'chr)))))
    (core-apply-with-args o 'seq-s '(#\' #\'))
    `(exactly ,(coerce cs 'string))))


;; symbol         =  '#' identifier:t => `(symbol ,t);
(defmethod asymbol ((o ometa-parser))
  (core-apply-with-args o 'exactly #\#)
  (let ((t1 (core-apply o 'identifier)))
    `(symbol ,t1)))

;; s-expr         =  "(" choice:s ")" => `(form ,s);
(defmethod s-expr ((o ometa-parser))
  (core-apply-with-args o 'seq-s '(#\())
  (let ((s (core-apply o 'choice)))
    (core-apply-with-args o 'seq-s '(#\)))
    `(form ,s)))

;; la-prefix      = "&";
(defmethod la-prefix ((o ometa-parser))
  (core-apply-with-args o 'seq-s '(#\&)))

;; not-prefix     = "~";
(defmethod not-prefix ((o ometa-parser))
  (core-apply-with-args o 'seq-s '(#\~)))

;; sem-prefix     = "%";
(defmethod sem-prefix ((o ometa-parser))
  (core-apply-with-args o 'seq-s '(#\%)))


;; end-symb       = "$" => `(apply end);
(defmethod end-symb ((o ometa-parser))
  (core-apply-with-args o 'seq-s '(#\$))
  `(apply end))

;; any-symb       = "_" => `(apply anything);
(defmethod any-symb ((o ometa-parser))
  (core-apply-with-args o 'seq-s '(#\_))
  `(apply anything))
  


;; test1 = regra1 "K" => 1
;;       | regra1 "R" => 2
;;
;; regra1 = "xyz";

(defmethod test1 ((o ometa-parser))
  (core-or o
           (lambda ()
             (core-apply o 'regra1)
             (core-apply-with-args o 'exactly #\K)
             1)
           (lambda ()
             (core-apply o 'regra1)
             (core-apply-with-args o 'exactly #\R)
             2)))

(defmethod regra1 ((o ometa-parser))
  (core-apply-with-args o 'seq '(#\x #\y #\z)))

;; (defmethod regra2 ((o ometa-parser))
;;   (core-apply-with-args o 'seq '(#\y)))


(defun ometa-match (input rule)
  (let* ((o (make-instance 'ometa-parser :input (make-ometa-stream input))))
    (setf (ometa-rules o) '(ometa inheritance rules rule rule-part
                            rule-rest rule-name argument choices choice
                            top-expression bind-expression repeated-expression
                            term binding element data-element action
                            host-lang-expr host-lang-quote host-lang-expand
                            host-lang-s-expr host-lang-atom s-identifier
                            prod-app prod prod-arg-list prod-arg
                            char-sequence cha-sequence-s string-literal
                            asymbol s-expr la-prefix not-prefix sem-prefix
                            end-symb any-symb))
         (let ((res (catch 'ometa (core-apply o rule))))
           (if (ometa-error-p res)
               (cons 'error (ometa-reporting o))
               res))))

