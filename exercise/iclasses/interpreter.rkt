#lang racket

(require dcc019/util/env
         dcc019/util/memory
         dcc019/exercise/iclasses/ast
         dcc019/exercise/iclasses/modules/Object
         dcc019/exercise/iclasses/modules/Class   
         dcc019/exercise/iclasses/utils/env-extensions)

(provide value-of-program)

; value-of :: Exp -> ExpVal
(define (value-of exp Δ)
  (match exp
    [(ast:int v) v]
    [(ast:bool v) (if v 'true 'false)]
    [(ast:dif e1 e2) (- (value-of e1 Δ) (value-of e2 Δ))]
    [(ast:zero? e) (zero? (value-of e Δ))]
    [(ast:not e) (not (value-of e Δ))]
    [(ast:if e1 e2 e3) (if (value-of e1 Δ) (value-of e2 Δ) (value-of e3 Δ))]
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
    [(ast:print e) (begin (display (value-of e Δ)) (display "\n"))]
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

(define (value-of-program prog)
  (empty-store)
  (match prog
    [(ast:prog decls stmt)
     (begin
        (initialize-class-env decls)
        (result-of stmt init-env))]))

(define (apply-method method-representation self args)
  (match method-representation
    ((method params body super-name field-names)
      (result-of body
        (begin (display "") (extend-env-list params (map newref args)
          (extend-env-with-self-and-super
            self super-name
            (extend-env-list field-names (object-fields self) init-env))))))))