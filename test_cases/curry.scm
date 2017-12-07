(define curry (f)
  (lambda (x)
    (lambda (y)
      (f x y))))

(val x ((curry +) 5))
(check-expect (x 3) 8)
