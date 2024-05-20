#lang racket
; Actividad Sesión 20 - A00835071
; Mayor elemento en una lista imbricada
(define (mayor L)
  (cond
    [(null? L) null]
    [(null? (cdr L) ) (if (pair? (car L) ) (mayor (car L)) (car L))]
    [(pair? (car L))
     (if (> (mayor (car L)) (mayor (cdr L)))
         (mayor (car L))
         (mayor (cdr L)))]
    [(> (car L) (mayor (cdr L))) (car L)]
    [else (mayor (cdr L))]))

(define lista '(3 7 (5 8) 2 (9 (4) 6)))
(define lista1 '())
(define lista2 '(-3 -10 (-5 -8) -2 (-9 (-4) -6)))
(display (mayor lista)) (newline)
(display (mayor lista1)) (newline)
(display (mayor lista2))

; Funciones para un grupo
(define grupo '(("A01" "Ana" (10 10 8))
                ("A02" "Carlos" (10 8 9))
                ("A03" "Luisa" (9 10 10))
                ("A04" "Juan" (8 8 8))
                ("A05" "Juan" (9 7 8))  ))
(define (extraer-nombres lista)
  (map cadr lista))
; Buscar por matricula (verdadero o falso) 
(define (buscar-matricula matricula lista)
  (cond
    ((null? lista) #f)
    ((equal? matricula (caar lista)) #t)
    (else (buscar-matricula matricula (cdr lista)))))
; Listar los promedios
(define (promedios lista)
   (map (lambda (persona) (/ (apply + (caddr persona)) (length (caddr persona)))) lista))
; Regresa datos por matricula, devolviendo el resto de la tupla
(define (datos matricula lista)
  (cond
    ((null? lista) '())
    ((null? matricula) lista)
    (cdr (assoc matricula lista))))
(newline)
(extraer-nombres grupo)
(newline)
(buscar-matricula "A02" grupo)
(buscar-matricula "" grupo)
(buscar-matricula "1" grupo)
(buscar-matricula "A08" grupo)
(buscar-matricula "A03" grupo)
(newline)
(promedios grupo)
(newline)
(datos "A02" grupo)
(datos "A04" grupo)
(datos "A03" grupo)
(datos "1" grupo)
(datos "A03" '())