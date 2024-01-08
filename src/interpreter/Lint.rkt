#lang racket

;;; AST
(struct Int (value))
(struct Prim (op args))
(struct Program (info body))

;;; Primitives
(define rd          (Prim 'read '()))
(define (negate a)  (Prim '- (list a)))
(define (minus a b) (Prim '- (list a b)))
(define (plus a b)  (Prim '+ (list a b)))

;;; Operations
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

;;; Interpreter
(define (interp_exp e)
  (match e
    [(Int n) n]
    [(Prim 'read '())
     (let ([r (read)])
       (cond [(fixnum? r) r]
             [else (error 'interp_exp "read expected an integer: ~v" r)]))]
    [(Prim '- (list a))   (- 0 (interp_exp a))]
    [(Prim '- (list a b)) (- (interp_exp a) (interp_exp b))]
    [(Prim '+ (list a b)) (+ (interp_exp a) (interp_exp b))]))

(define (interp_Lint p)
  (match p
    [(Program '() e) (interp_exp e)]))

;;; Partial evaluation
(define (pe_negate a)
  (match a
    [(Int n) (Int (- 0 n))]
    [_ (negate a)]))

(define (pe_minus a b)
  (match* (a b)
    [((Int na) (Int nb)) (Int (- na nb))]
    [(_ _) (minus a b)]))

(define (pe_plus a b)
  (match* (a b)
    [((Int na) (Int nb)) (Int (+ na nb))]
    [(_ _) (plus a b)]))

(define (pe_exp e)
  (match e
    [(Int n) (Int n)]
    [(Prim 'read '()) rd]
    [(Prim '- (list a))   (pe_negate (pe_exp a))]
    [(Prim '- (list a b)) (pe_minus (pe_exp a) (pe_exp b))]
    [(Prim '+ (list a b)) (pe_plus (pe_exp a) (pe_exp b))]))

(define (pe_Lint p)
  (match p
    [(Program '() e) (Program '() (pe_exp e))]))
