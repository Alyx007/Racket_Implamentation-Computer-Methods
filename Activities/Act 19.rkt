#lang racket
; Aleksandra Stupiec A00835071, 30/04/2024

; Atomo a lista
(define Lista '())
Lista
(define (atomo a)
  (cons a Lista)
  )
(atomo 10)
(atomo 2)
(atomo 3)

; no sabia si deberian guardarse todos los atomos a una o solo un atomo a la vez
(define (atom a lst)
  (if (null? lst)
      (list a)
      (cons (car lst) (atom a (cdr lst)))))

(set! Lista (atom 10 Lista))
(set! Lista (atom 2 Lista))
(set! Lista (atom 3 Lista))
Lista

; Lista a atomo
(define Plana '( 1 2 3 4 5 6 7 89 ))
(define Plana1 '( 98 76 54 32 11 ))
(newline)
(define (extract-elements elem)
    (cond
      ((null? elem) '())
      (else
       (begin
         (display (car elem))
         (newline)
         (extract-elements (cdr elem) )
         (newline)
         ))))
  (extract-elements Plana)
(extract-elements Plana1)

; Lista a lista
(define Desarreglada '(0 9 8 7 6 5 4 3 2 1))
Desarreglada
(newline)

(define (extract elem)
  (cond
    ((null? elem) '())
    (else (cons (car elem) (extract (cdr elem))))))

(define Arreglada-rev (reverse(extract Desarreglada)))
Arreglada-rev
(define Arreglada (extract Desarreglada))
Arreglada
