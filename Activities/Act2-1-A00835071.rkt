#lang racket
;Author: Aleksandra Stupiec A00835071
;Date: 20/04/2024
; Problema 1 - Fahrenheit to Celsius
(string #\F #\a #\h #\r #\e #\n #\h #\e #\i #\t)
(define (fah-to-celsius f)
  (/ (* 5(- f 32)) 9))
(fah-to-celsius 212.0)
(fah-to-celsius 32.0)
(fah-to-celsius -40.0)

; Problema 2 - Sign
(string #\S #\i #\g #\n)
(define (sign x)
  (if (< x 0 )-1 1 )
  (if (= x 0) 0 1))
(sign -5)
(sign 10)
(sign 0)

;Problema 3 - Roots
(string #\R #\o #\o #\t #\s)
(define (roots a b c)
(/ (+ (* b -1)(sqrt(-(* b b)(* 4 a c)))) (* 2 a))
   )
(roots 2 4 2)
(roots 1 0 0)
(roots 4 5 1)



 

  