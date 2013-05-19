(load "mk.scm")
(load "pmatch.scm")
(load "test-check.scm")

(define empty-env '())
(define ext-env
  (lambda (env x v)
    `((,x . ,v) . ,env)))
(define lookup
  (lambda (x env)
    (cond
      [(assq x env) => cdr]
      [else (error 'lookup "unbound variable")])))

(define make-closure
  (lambda (x body env)
    `(closure ,x ,body ,env)))
(define apply-proc
  (lambda (p a)
    (pmatch p
      [(closure ,x ,body ,env)
;;; might need a flag here to keep track of when I'm in eval-exp vs. eval-exp-scheme
       (eval-exp body (ext-env env x a))])))

(define eval-exp
  (lambda (exp env)
    (pmatch exp
      [,b (guard (boolean? b)) b]
      [,n (guard (number? n)) n]      
      [(sub1 ,e) (sub1 (eval-exp e env))]
      [(zero? ,e) (zero? (eval-exp e env))]
      [(* ,e1 ,e2) (* (eval-exp e1 env) (eval-exp e2 env))]
      [(if ,e1 ,e2 ,e3) (if (eval-exp e1 env) (eval-exp e2 env) (eval-exp e3 env))]
      [(run ,ne (,q) ,ge)
       (run (eval-exp ne env) (q^)
         (eval-exp-mk ge (ext-env env q q^)))]
      [,x (guard (symbol? x)) (lookup x env)]
      [(lambda (,x) ,body) (make-closure x body env)]
      [(,rator ,rand)
       (apply-proc (eval-exp rator env) (eval-exp rand env))])))

(define eval-exp-scheme
  (lambda (exp env)
    (pmatch exp
      [,b (guard (boolean? b)) b]
      [,n (guard (number? n)) n]
      [,x (guard (symbol? x)) (lookup x env)]
      [(sub1 ,e) (sub1 (eval-exp e env))])))

(define eval-exp-mk
  (lambda (exp env)
    (pmatch exp
      [(== ,e1 ,e2)
       (== (eval-exp-scheme e1 env) (eval-exp-scheme e2 env))]
      [(fresh (,x) ,ge1 ,ge2)
       (fresh (x^)
         (eval-exp-mk ge1 (ext-env env x x^))
         (eval-exp-mk ge2 (ext-env env x x^)))]
      [(conde [,ge1] [,ge2])
       (conde
         [(eval-exp-mk ge1 env)]
         [(eval-exp-mk ge2 env)])])))


;;; tests

(define fact-five
  '((lambda (f)
      ((f f) 5))
    (lambda (f)
      (lambda (n)
        (if (zero? n)
            1
            (* n ((f f) (sub1 n))))))))


(test "!"
  (eval-exp fact-five empty-env)
  120)

(test "==-1"
  (eval-exp '(run #f (z) (== 5 5)) empty-env)
  '(_.0))

(test "==-1"
  (eval-exp '(run #f (z) (== z 5)) empty-env)
  '(5))

(test "fresh-1"
  (eval-exp '(run #f (z) (fresh (w) (== w 5) (== z w))) empty-env)
  '(5))

(test "conde-1"
  (eval-exp '(run #f (z) (conde [(== z 5)] [(== z 6)])) empty-env)
  '(5 6))
