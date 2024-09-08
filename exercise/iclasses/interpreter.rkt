#lang racket

(require dcc019/util/env
         dcc019/util/memory
         dcc019/exercise/iclasses/ast)

(provide value-of-program)

; ;; Define an empty environment
; (define empty-env '())

; ;; Extend the environment by adding a new (var . value) pair to the list
; (define (extend-env var value env)
;   (cons (cons var value) env))

; ;; Apply the environment by searching for the variable's value
; (define (apply-env env var)
;   (let ([binding (assoc var env)])
;     (if binding
;         (cdr binding)  ; Return the value associated with the variable
;         (error "Variable not found" var))))

; (define init-env empty-env)


; value-of :: Exp -> ExpVal
(define (value-of exp Δ)
  (match exp
    [(ast:int v) v]
    [(ast:bool v) (if v 'true 'false)]
    [(ast:dif e1 e2) (- (value-of e1 Δ) (value-of e2 Δ))]
    [(ast:zero? e) (zero? (value-of e Δ))]
    [(ast:not e) (not (value-of e Δ))]
    [(ast:if e1 e2 e3) (if (value-of e1 Δ) (value-of e2 Δ) (value-of e3 Δ))]
    ; [(ast:var v) (apply-env Δ v)] ; esta implementação só funciona para variáveis imutáveis
    [(ast:var v) (deref (apply-env Δ v))]
    [(ast:let (ast:var x) e1 e2) (value-of e2 (extend-env x (newref (value-of e1 Δ)) Δ))]
    [(ast:send e (ast:var mth) args)
      (let* (
        [arguments (map (lambda (arg) (value-of arg Δ)) args)]
        [object (value-of e Δ)])
          (apply-method (find-method (object-class-name object) mth) object arguments))]
    [(ast:super (ast:var method-name) args)
      (let* ([arguments (map (lambda (arg) (value-of arg Δ)) args)]
        [obj (apply-env Δ '%self)]
        [super-class (apply-env Δ '%super)]
        [method (find-method super-class method-name)])
          (apply-method method obj arguments))]
    [(ast:self) (apply-env Δ '%self)]
    [(ast:new (ast:var c) args)
      (let* (
        [args (map (lambda (arg) (value-of arg Δ)) args)]
        [obj (new-object c)])
          (apply-method (find-method c "initialize") obj args) obj)]
    [e (raise-user-error "unimplemented-construction: " e)]
    ))

; result-of :: Stmt -> Env -> State -> State
(define (result-of stmt Δ)
  (match stmt
    [(ast:assign (ast:var x) e) (begin (setref! (apply-env Δ x) (value-of e Δ)) 42)]
    [(ast:print e) (display (value-of e Δ))]
    [(ast:return e) (value-of e Δ)]
    [(ast:block stmts) (for-each (lambda (stmt) (result-of stmt Δ)) stmts)]
    [(ast:if-stmt e s1 s2) (if (value-of e Δ) (result-of s1 Δ) (result-of s2 Δ))]
    [(ast:while e s) (let loop () (when (value-of e Δ) (result-of s Δ) (loop)))]
    [(ast:local-decl (ast:var x) s) (result-of s (extend-env x (newref 0) Δ))]
    [(ast:send e (ast:var mth) args)
      (let* (
        [arguments (map (lambda (arg) (value-of arg Δ)) args)]
        [object (value-of e Δ)])
          (apply-method (find-method (object-class-name object) mth) object arguments))]
    [(ast:super (ast:var method-name) args)
      (let* ([arguments (map (lambda (arg) (value-of arg Δ)) args)]
        [obj (apply-env Δ '%self)]
        [super-class (apply-env Δ '%super)]
        [method (find-method super-class method-name)])
          (apply-method method obj arguments))]
    [e (raise-user-error "unimplemented-construction: " e)]
    ))

(struct class (super-name field-names method-env) #:transparent)

(struct method (params body super-name field-names) #:transparent)

(struct object (class-name fields) #:transparent)

(define Γ '())

(define (add-to-class-env class-name class-representation)
  (set! Γ (cons (list class-name class-representation) Γ)))

(define (value-of-program prog)
  (empty-store)
  (match prog
    [(ast:prog decls stmt)
     (begin
        ; you must collect all the classes declared and building its respectively environment
        ; execute the prog expression in the correct environment
        (initialize-class-env decls)
        (result-of stmt init-env))]))

(define (initialize-class-env class-declarations)
  (set! Γ (list (list "object" (class #f '() '()))))
  (for-each initialize-class-declaration class-declarations))

(define (initialize-class-declaration class-declaration)
  (match class-declaration
    [(ast:decl class-var superclass-var field-names method-declarations)
     (let ([current-field-names (append-field-names
                     (class-field-names (lookup-class (ast:var-name superclass-var)))
                     field-names
                     (ast:var-name superclass-var))])
       (add-to-class-env (ast:var-name class-var)
                          (class (ast:var-name superclass-var) current-field-names
                                 (merge-method-envs
                                  (class-method-env (lookup-class (ast:var-name superclass-var)))
                                  (get-method-env-from-method-declarations method-declarations (ast:var-name superclass-var) current-field-names)))))]))

(define (append-field-names super-fields new-fields super-class-name)
  (cond
    [(null? super-fields) new-fields]
    [else (cons (if (member (car super-fields) new-fields)
                    (ast:var (string-append super-class-name (ast:var-name (car super-fields))))
                    (car super-fields))
                (append-field-names (cdr super-fields) new-fields super-class-name))]))

(define (lookup-class name)
  (let ([maybe-class-pair (assoc name Γ)])
    (if maybe-class-pair (cadr maybe-class-pair)
      (raise-user-error "unknown class: " name))))

(define (merge-method-envs super-method-env new-method-env)
  (append new-method-env super-method-env))

(define (get-method-env-from-method-declarations method-declarations superclass-name field-names)
  (map (lambda (method-declaration)
         (match method-declaration
           [(ast:method method-var params body)
            (list (ast:var-name method-var) (method params body superclass-name field-names))]))
       method-declarations))

(define (find-method class-name name)
  (let ([method-env (class-method-env (lookup-class class-name))])
    (let ([maybe-method-pair (assoc name method-env)])
      (if (pair? maybe-method-pair) (second maybe-method-pair)
          (raise-user-error "unknown method " name)))))

(define (apply-method method-representation self args)
  (match method-representation
    ((method params body super-name field-names)
      (result-of body
        (begin (display "") (extend-env-list params (map newref args)
          (extend-env-with-self-and-super
            self super-name
            (extend-env-list field-names (object-fields self) init-env))))))))

(define (extend-env-with-self-and-super self super env)
  (extend-env '%self self (extend-env '%super super env)))

(define (extend-env-list names values env)
  (cond
    [(or (empty? names) (empty? values)) env]
    [else (
      extend-env (ast:var-name (car names)) (car values) (extend-env-list (cdr names) (cdr values) env)
    )]))

(define (new-object class-name)
  (object class-name
    (map (lambda (field-name)
          (newref (list 'uninitialized-field field-name)))
        (class-field-names (lookup-class class-name)))))
