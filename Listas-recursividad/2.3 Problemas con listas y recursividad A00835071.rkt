#lang racket
; Aleksandra Stupiec A00835071
; 2.3 Problemas con listas y recursividad

; Ejercicio 1 - Promedio de lista
(define Promedio '( 23 34 45 56 67 78 89 90))
(define Promedio1 '(10 10 10 10))
(define Promedio2 '(90 1 2 3 4))
(define Promedio3 '())
(define (average lista)
  (define (sum lista) ; primero calculamos la suma de los valores
    (if (null? lista)
        0
        (+ (car lista) (sum (cdr lista)))))
  (define (count lista) ; calculamos la cantidad de elementos
    (if (null? lista)
        0
        (+ 1 (count (cdr lista)))))
  (if (null? lista)
      "La lista se encuentra sin valores!"
      (/ (sum lista) (count lista)))) ; el calculo del promedio

(average Promedio) ; aplicamos la lista a la funcion creada 
(average Promedio1)
(average Promedio2)
(average Promedio3)

; Ejercicio 2 - El mayor de la lista
(define Mayor '(1 2 3 4 5 4 3 2 1))
(define Mayor1 '(10 1 2 3))
(define Mayor2 '())
(define Mayor3 '(1 2 3 4 5 6 7 8 9 11))

(define (max lista)
  (define (max-helper lista max-val)
    (if (null? lista)
        max-val
        (max-helper (cdr lista) (if (> (car lista) max-val) (car lista) max-val))))
  (if (null? lista)
      "Lista sin vaores"
      (max-helper (cdr lista) (car lista))))
(newline)
(max Mayor)
(max Mayor1)
(max Mayor2)
(max Mayor3)

; Ejercicio 3 - Lista contiene un elemento especifico #t - true o #f - false
(define elemento '())
(define elemento1 '( "n" "n" "m" "d" "j" "a"))
(define elemento2 '("mm" "mm" "mm" "mm" "mm" "ma" "mn"))
(define elemento3 '(6 7 8 6 7 8 6 7 8 6 7 8 6 7 8 9))

(define (tiene-elemento elem lista)
  (cond
    ((null? lista) #f)
    ((equal? elem(car lista)) #t) 
    (else (tiene-elemento elem (cdr lista)))))
(newline)
(tiene-elemento 1 elemento)
(tiene-elemento "d" elemento1)
(tiene-elemento "mn" elemento2)
(tiene-elemento 2 elemento3)

; Ejercicio 4 - Contar el numero de ocurrencias de un elemento especifico
(define ocurrencias '( "n" "n" "n" "n" "a" "d" "g" "h" "j"))
(define ocurrencias1 '(1 3 4 6 7 4 4 2 3 3 1 3 3))
(define ocurrencias2 '(6 7 8 6 7 8 6 7 8 6 7 8 6 7 8 9))
(define ocurrencias3 '("mm" "mm" "mm" "mm" "mm" "ma" "mn"))

(define (count-occurrences element lista)
  (cond
    ((null? lista) 0)
    ((equal? element (car lista))
     (add1 (count-occurrences element (cdr lista)))) 
    (else
     (count-occurrences element (cdr lista)))))
(newline)
(count-occurrences "n" ocurrencias)
(count-occurrences 5 ocurrencias1)
(count-occurrences 9 ocurrencias2)
(count-occurrences "mm" ocurrencias3)

; Ejercicio 5 - Lista con n veces 0
(define (nueva-lista n value)
  (if (zero? n)
      '()
      (cons value (nueva-lista (- n 1) value))))
(newline)
(nueva-lista 6 0)
(nueva-lista 0 0)
(nueva-lista 16 0)

; Ejercicio 6 - Generar lista con valores de 1 a n
(define (lista1-n n)
  (define (build-list current-n)
    (if (= current-n n)
        (list current-n)
        (cons current-n (build-list (+ current-n 1)))))
  
  (if (zero? n)
      '()
      (build-list 1)))
(newline)
(lista1-n 5)
(lista1-n 10)
(lista1-n 0)

; Ejercicio 7 - Generar lista con valores de n a 1
(define (listan-1 n)
  (define (build-list current-n)
    (if (= current-n 1)
        (list current-n)
        (cons current-n (build-list (- current-n 1)))))
  (if (zero? n)
      '()
      (build-list n)))

(newline)
(listan-1 5)
(listan-1 15)
(listan-1 0)

; Ejercicio 8 - Lista con los n primeros valores de la secuencia de Fibonacci
(define (fibonacci-lista n)
  (define (fibonacci a b count)
    (if (= count 0)
        '()
        (cons a (fibonacci b (+ a b) (- count 1)))))
  (if (< n 1)
      '()
      (fibonacci 0 1 n)))
(newline)
(fibonacci-lista 20)
(fibonacci-lista 0)
(fibonacci-lista 1)

; Ejercicio 9 - Lista con numeros pares en un rango
(define (mod a b) 
  (if (< a b)
      a
      (mod (- a b) b)))

(define (pares-lista n)
    (define (even x)
    (= (mod x 2) 0))
  (define (build-lista current-n)
    (if (zero? current-n)
        '()
        (if (even current-n)
            (cons current-n (build-lista (- current-n 1)))
            (build-lista (- current-n 1)))))
  (if (zero? n)
      '(0)
      (build-lista n)))
(newline)
(pares-lista 2)
(pares-lista 0)
(pares-lista 22)

; Ejercicio 10 - Sumar dos listas de la misma longitud
(define (suma-listas lista1 lista2)
  (if (null? lista1)
      '()
      (cons (+ (car lista1) (car lista2)) (suma-listas (cdr lista1) (cdr lista2)))))
(newline)
(suma-listas '(1 2 3 4 5) '(6 7 8 9 10))
(suma-listas '(-1 -2 -10) '(-10 -2 10))
(suma-listas '(0) '(0))
(suma-listas '() '())

; Ejercicio 11 - Concatenar dos listas (SIN APPEND)
(define (concatenacion lista1 lista2)
  (if (null? lista1)
      lista2
      (if (null? lista2)
      lista1
      (cons (car lista1) (concatenacion (cdr lista1) lista2)))))

(newline)
(concatenacion '(1 2 3) '(1 2 3))
(concatenacion '(-4 -3 -2 -1 0) '(1 2 3 4))
(concatenacion '(1 2 3 4 5 6 7 8 9) '(4 5 6))
(concatenacion '(1 2 3) '())
(concatenacion '() '(1 2 3))
(concatenacion '() '())

; Ejercicio 12 - Invertir el orden de una lista (SIN REVERSE)
(define Desarreglada '(0 9 8 7 6 5 4 3 2 1))
(define Desarreglada1 '(3 4 5 6))
(define Desarreglada2 '())
(define (extract-rev elem)
  (define (helper lista resultado)
  (if (null? lista)
      resultado
    (helper (cdr lista) (cons (car lista) resultado))))
  (helper elem '()))

(newline)
(define Arreglada (extract-rev Desarreglada))
Arreglada
(define Arreglada1 (extract-rev Desarreglada1))
Arreglada1
(define Arreglada2 (extract-rev Desarreglada2))
Arreglada2

; Ejercicio 13 - Insertar un valor ordenado en una lista
(define (insert-ordenado n lista)
  (cond
    ((not (number? n)) "No es posible, el valor no es un numero real")
    ((null? lista) (list n))
    ((<= n (car lista)) (cons n lista))
    (else (cons (car lista) (insert-ordenado n (cdr lista))))))
(newline)
(insert-ordenado 4 '(3 7 9))
(insert-ordenado -2 '(-10 -3 0 2 3))
(insert-ordenado "sos" '(1 2 3))
(insert-ordenado 100 '())

; Ejercicio 14 - Ordenar una lista (SIN USAR SORT PREDEFINIDOS)
(define (insertion-sort lista)
  (define (insertar n sorted-lista)
    (if (null? sorted-lista)
        (list n)
        (if (<= n (car sorted-lista))
            (cons n sorted-lista)
            (cons (car sorted-lista) (insertar n (cdr sorted-lista))))))
  (define (insertion-helper unsorted sorted-lista)
    (if (null? unsorted)
        sorted-lista
        (insertion-helper (cdr unsorted) (insertar (car unsorted) sorted-lista))))
  (insertion-helper lista '()))
(newline)
(insertion-sort '(3 5 0 2 9 1))
(insertion-sort '(-1 -5 10 -10 -3 4 -6 8))
(insertion-sort '(8 3 5 11 3400 484 290))
        
; Ejercicio 15 - Sumar dos listas de longitud variable
(define (sumar-listas lista1 lista2)
  (define (sumar-listas-helper lista1 lista2)
    (cond
      ((null? lista1) lista2)  ; Si lista1 es vacía, devuelve lista2
      ((null? lista2) lista1)  ; Si lista2 es vacía, devuelve lista1
      (else
        (cons (+ (car lista1) (car lista2))
              (sumar-listas-helper (cdr lista1) (cdr lista2))))))

  (sumar-listas-helper lista1 lista2))

(newline)
(sumar-listas '(1 2 3) '(1 2 3 4))
(sumar-listas '(0) '(1 2 3 4))
(sumar-listas '(1209384 12983 123 156 178) '(1 2 3 4))

