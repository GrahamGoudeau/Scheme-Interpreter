(define fibo (n)
  (if (< n 1) 
    0
    (if (< n 2)
      1
      (+ (fibo (- n 1)) (fibo (- n 2))))))

(check-expect (fibo 0) 0)
(check-expect (fibo 1) 1)
(check-expect (fibo 2) 1)
(check-expect (fibo 3) 2)
(check-expect (fibo 12) 144)
(check-expect (fibo 22) 17711)
