#lang racket

; Nome: Gabriel Frasson Costa / Matrícula: 202035001
; Nome: Pedro do Couto Filgueiras / Matrícula: 201935015

(require 
  dcc019/exercise/iclasses/modules/Class
  dcc019/util/memory)

(provide
  object
  object-class-name
  object-fields
  new-object)

(struct object (class-name fields) #:transparent)

(define (new-object class-name)
  (object class-name
    (map (lambda (field-name)
          (newref (list 'uninitialized-field field-name)))
        (class-field-names (lookup-class class-name)))))
