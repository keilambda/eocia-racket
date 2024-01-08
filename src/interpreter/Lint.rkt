#lang racket

(struct Int (value))
(struct Prim (op args))
(struct Program (info body))

(define rd          (Prim 'read '()))
(define (negate a)  (Prim '- (list a)))
(define (minus a b) (Prim '- (list a b)))
(define (plus a b)  (Prim '+ (list a b)))

(define (leaf? ast)
  (match ast
    [(Int _) #t]
    [(Prim 'read '()) #t]
    [_ #f]))

(define (exp? ast)
  (match ast
    [(Int _) #t]
    [(Prim 'read '()) #t]
    [(Prim _ args) (andmap exp? args)]
    [_ #f]))

(define (Lint? ast)
  (match ast
    [(Program '() e) (exp? e)]
    [_ #f]))
