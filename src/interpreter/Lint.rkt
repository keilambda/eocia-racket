#lang racket

(struct Int (value))
(struct Prim (op args))
(struct Program (info body))

(define rd          (Prim 'read '()))
(define (negate a)  (Prim '- (list a)))
(define (minus a b) (Prim '- (list a b)))
(define (plus a b)  (Prim '+ (list a b)))
