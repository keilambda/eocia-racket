#lang racket

(struct Int (value))
(struct Prim (op args))
(struct Program (info body))
