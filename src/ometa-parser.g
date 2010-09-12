/*
 Copyright (c) 2010 Thiago Silva <thiago@comum.org>

 Permission is hereby granted, free of charge, to any person obtaining a copy
 of this software and associated documentation files (the "Software"), to deal
 in the Software without restriction, including without limitation the rights
 to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 copies of the Software, and to permit persons to whom the Software is
 furnished to do so, subject to the following conditions:

 The above copyright notice and this permission notice shall be included in
 all copies or substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 THE SOFTWARE.
*/

ometa ometa-parser {

    /* current-rule and local-variables slots */

  _slots ((current-rule    :initform nil 
           :accessor ometa-current-rule)
          (local-variables :initform (make-hash-table)
           :accessor ometa-local-variables));


  /* useful rules for controling var bindings and acessing grammar variables */

  _inline [(defmethod ometa-add-local-var ((o ometa-parser) var)
            (let* ((rule (ometa-current-rule o))
                   (vars (gethash rule (ometa-local-variables o))))
             (if (eq var 'o)
                (throw 'ometa "variable 'o' conflicts with internal 'o' var."))
             (unless (find var vars)
              (push var (gethash rule (ometa-local-variables o))))))
           
           (defmethod ometa-local-variables-list ((o ometa-parser))
            (let ((res nil))
             (maphash (lambda (k v) (setq res (cons (list k v) res))) (ometa-local-variables o))
             res))];

  spacing = '/*' { ~'*/' _ }* '*/'
          | ^
          ;

  ometa = spaces "ometa" identifier:name inheritance:i  "{"
              cl-slots:sl inline-code:ic 
              rules:r 
          "}" $
          => `(grammar ,name ,i (locals ,(ometa-local-variables-list o))
                                (slots ,sl)
                                (inline ,ic)  ,@r);


  inheritance = "<:" identifier:i => `(parent ,i)
              |                   => `(parent ometa-base)
              ;


  cl-slots    = "_slots" { ~';' _}*:s ";" => (concatenate 'string s)
              |
              ;

  inline-code = "_inline" '[' { ~']' _}*:c ']' ";" => (concatenate 'string c)
              |
              ;

  rules = rule+;

  rule = &{rule-name:rname} <rule-part rname>+:p => `(rule ,rname, `(or ,@p));

  rule-part :rn = rule-name:rname %(eq rname rn) rule-rest:r ";" => r;

  rule-rest = "=" choices
            |  action
            |  argument+:args "=" choices:c => `(and ,@args ,c)
            |  argument+:args  action:ac      => `(and ,@args ,ac)
            ;


  rule-name =  identifier:rname  => (progn (setf (ometa-current-rule o) rname) rname)
            ;


  argument = bind-expression
           | binding:b       => `(bind ,b (apply anything))
           ;

  choices  = choice:x { "|" choice:c => c}*:xs => `(or ,x ,@xs);

  choice   = top-expression*:x action:ac => `(and ,@x ,ac)
           | top-expression*:x           => `(and ,@x)
           ;

  top-expression =   bind-expression
                 |   repeated-expression
                 ;

  bind-expression = repeated-expression:e binding:b => `(bind ,b ,e)
                  ;

  repeated-expression = term:e "*" => `(many ,e)
                      | term:e "+" => `(many1 ,e)
                      | term:e "?" => `(optional ,e)
                      | term:e '[' str-number:rep ']' => `(repeat ,rep ,e)
                      | term
                      ;

  term  = '~'  element:e => `(not ,e)
        |  '&'  element:e => `(lookahead ,e)
        |  element
        ;

  binding = ':' identifier:i => (progn (ometa-add-local-var o i) i)
          ;

  element =  prod-app
          |  data-element
          |  '%' host-lang-expr:s => `(sem-predicate ,s)
          |  "{" choices:c "}" => c
          ;

  data-element =  char-sequence
               |  char-sequence-s
               |  string-object
               |  asymbol
               |  s-expr
               |  any-symb
               |  end-symb
               |  s-number
               ; 

  action     = "=>" host-lang-expr:s => `(action ,s);


  host-lang-expr  = host-lang-quote:q 
                    host-lang-expand:e 
                    host-lang-s-expr:s 
                    => (str-trim  (concatenate 'string q e s))
                   ;

  host-lang-quote   = {'`'+:q => (coerce q 'string)
                       | '\''+:q => (coerce q 'string)
                       | => ""};

  host-lang-expand  = {','+:q => (concatenate 'string q) 
                       | => ""}:qq  
                      {'@':a => (string a) 
                       | => ""}:aa => (concatenate 'string qq aa)
                    ;

  host-lang-s-expr  = host-lang-atom
                     |  "(" { host-lang-atom | host-lang-expr }*:x ")"
                           =>  (if x (concatenate 'string "(" (reduce (lambda (a b) (concatenate 'string a " " b)) x) ")")
                                     "()")
                     ;

  host-lang-atom    = host-lang-quote:q host-lang-expand:e 
                      {   s-identifier 
                        | string-object:l 
                          => (coerce `(#\" ,@l #\") 'string)
                       }:a  => (concatenate 'string q e a);

  s-identifier = {~{spacing | ';' | '(' | ')' | '}' | '{' | '|' } 
                 chr:c => c}+:xs spaces  => (coerce xs 'string);


  prod-app = "<" identifier:p ">" 
                 => `(apply ,p)
            |  identifier:p
                 => `(apply ,p)
            | "<" identifier:p prod-arg-list:args ">"
                => `(apply-with-args ,p (arguments ,@args))
            | "^"
                => `(apply-super ,(ometa-current-rule o))
            | "<^" prod-arg-list:args ">"
                => `(apply-super-with-args ,(ometa-current-rule o) (arguments ,@args))
            ;

  prod-arg-list = prod-arg:x {"," prod-arg:a => a}*:xs => (cons x xs);

  prod-arg = data-element | identifier;

  char-sequence  = '\'' { '\\' '\'' | '\\' '\\' | ~'\'' chr:c => c}+:cs "'" 
                     => `(seq ,(coerce cs 'string));

  char-sequence-s = '"' { '\\' '"' | '\\' '\\' | ~'"'  chr:c => c}*:cs "\"" => `(and 
                                                                                    (seq ,(coerce cs 'string))
                                                                                    (apply spaces));

  string-object = '``' {~'``' char:c => c}*:cs "''" => `(exactly ,(coerce cs 'string));

  asymbol     =  '#' identifier:s => `(symbol ,s);

  s-expr    =  "(" choice:s ")" => `(form ,s);

  la-prefix    = "&";

  not-prefix     = "~";
  sem-prefix     = "%";
  end-symb       = "$" => `(apply end);
  any-symb       = "_" => `(apply anything);

  s-number       = num:n => `(number ,n);
}
