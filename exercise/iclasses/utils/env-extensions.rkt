#lang racket

(require
  dcc019/util/env
  dcc019/exercise/iclasses/ast)

(provide extend-env-with-self-and-super extend-env-list)

(define (extend-env-with-self-and-super self super env)
  (extend-env '%self self (extend-env '%super super env)))

(define (extend-env-list names values env)
  (cond
    [(or (empty? names) (empty? values)) env]
    [else (
      extend-env (ast:var-name (car names)) (car values) (extend-env-list (cdr names) (cdr values) env)
    )]))
