(define find-divisor (target n)
  (if (= target n)
    target
    (if (= (mod target n) 0)
      n
      (find-divisor target (+ n 1)))))

(define prime (n)
  (or (= n 2) (= n (find-divisor n 2))))

(check-expect (find-divisor 3 2) 3)
(check-expect (prime 2) #t)
(check-expect (prime 3) #t)
(check-expect (prime 45) #f)
(check-expect (prime 189) #f)
