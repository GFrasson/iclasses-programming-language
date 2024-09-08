#lang racket

(require
  dcc019/exercise/iclasses/ast)

(provide
  class
  class-field-names
  lookup-class
  initialize-class-env
  method
  find-method
  merge-method-envs
  get-method-env-from-method-declarations)

(struct class (super-name field-names method-env) #:transparent)

(struct method (params body super-name field-names) #:transparent)

; Class
(define Γ '())

(define (initialize-class-env class-declarations)
  (set! Γ (list (list "object" (class #f '() '()))))
  (for-each initialize-class-declaration class-declarations))

(define (initialize-class-declaration class-declaration)
  (match class-declaration
    [(ast:decl class-var superclass-var field-names method-declarations)
     (let ([current-field-names (
      append-field-names
        (class-field-names (lookup-class (ast:var-name superclass-var)))
        field-names
        (ast:var-name superclass-var))])
      (add-to-class-env (ast:var-name class-var)
        (class (ast:var-name superclass-var) current-field-names
          (merge-method-envs
          (class-method-env (lookup-class (ast:var-name superclass-var)))
          (get-method-env-from-method-declarations method-declarations (ast:var-name superclass-var) current-field-names)))))]))

(define (add-to-class-env class-name class-representation)
  (set! Γ (cons (list class-name class-representation) Γ)))

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

; Method
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


