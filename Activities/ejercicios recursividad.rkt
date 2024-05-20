#lang racket
;De la clase
; Cuadrado
(define (cuadrado x)
  (* x x))
;(cuadrado 4)

; Evaluacion n^2 /2
(define (eval n)
  (+(* n n)(/ n 2)) )
;(eval 126)

; Distancia
(define (dist x1 x2 y1 y2)
  (sqrt (+ (cuadrado (- x1 x2)) (cuadrado (- y1 y2)))))
;(dist 10 10 10 20)


;Factorial
(define (factorial f)
  (if (= 0 f) 1
      (* f (factorial (- f 1))
      )))
;(factorial 5)

; Fibonacci
(define (fibonacci x) (* x x))

;Potencia con recursividad
(define (potencia n m)
  (* n m))


; Hola
(define (string_multiply s n)
  (string-append*(make-list n s)))
(string_multiply "hola" 2)

; Sumatoria acumulada
(define (sumatoria_ac sa)
   (if (= 0 sa) 0
      (+ sa (sumatoria_ac (- sa 1))
      )))
(sumatoria_ac 12874)

; Secuencia de n a 1
(define (sec n)
  (cond
    ((<= n 1) (display n))
    (else
     (display n)
     (display " ")
     (sec (- n 1))))
    )
(sec 100)

; Secuencia de 1 a n
(define (sec_arriba m)
  (define (ayuda i)
  (cond
    ((> i m)(newline) )
    (else
     (display i)
     (when (< i m) (display " "))
     (ayuda (+ i 1)))))
  (ayuda 1))
(sec_arriba 50)


; Calculo de las coordenadas
(define (triangulo a b c)
  (cond
    ((or (<= (+ a b) c) (<= (+ b c) a) (<= (+ a c) b)) "NO ES VALIDO")
    ((and (= a b) (= b c)) "Equilatero")
    ((or (= a b) (= b c) (= a c)) "Triangulo Isosceles")
    (else "Escaleno")))
(triangulo 4 6 3)


