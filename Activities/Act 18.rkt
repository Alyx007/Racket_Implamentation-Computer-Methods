#lang racket

; CAR y CDR
(define cifras '(1 2 (3 4) (5 (6 7))))
cifras
(car cifras)
(car(cdr cifras))
(car (car (cdr (cdr cifras))))
(car (car (cdr (cdr cifras))))
(cdr (car (cdr (cdr cifras))))
(car (car (cdr (cdr (cdr cifras)))))
(car (car (cdr (car (cdr (cdr (cdr cifras)))))))
(car (cdr (car (cdr (car (cdr (cdr (cdr cifras))))))))

(define letras '(a (b c d) e (f g)))
letras
(car letras)
(car(car(cdr letras)))
(car(cdr(car(cdr letras))))
(cdr(cdr(car(cdr letras))))
(car(cdr(cdr letras)))
(car(car(cdr(cdr(cdr letras)))))
(cdr(car(cdr(cdr(cdr letras)))))

; Determina cuantos elementos tiene la lista
(define Lista '(1 2 3 4 5 6 7 8 9 9 0))
(display "Longitud de la lista: ")(length Lista)

; Funcion que suma todos los elementos de una lista plana
(define Plana '( 1 2 3 4 5 6 7 89 ))
(define (extract-elements elem)
  (define (sum-list L)
    (if (null? L)
        0 
        (+ (car L) (sum-list (cdr L)))))
  (define (extract-helper L sum)
    (cond
      ((null? L) '())
      (else
       (begin
         (display (car L))
         (newline)
         (display "Suma: ")
         (display (+ sum (car L)))
         (newline)
         (extract-helper (cdr L) (+ sum (car L)))
         (newline)
         )))) 
  (extract-helper elem 0))
(extract-elements Plana)

; Funcion que aumente todos los numeros de una lista plana en 1 (n+1)
; Ejemplo:   ( 1  12  30  4  7 )  pasa a ser ( 2  13  31  5  8 ) 
(define UnoMas '(1 12 30 4 7))
UnoMas
(define (mas lista)
  (cond
    ((null? lista) '())
    (else
     (begin
       (display (+ 1 (car lista)))
       (newline)
       (mas (cdr lista))
       (newline)))))


(mas UnoMas)