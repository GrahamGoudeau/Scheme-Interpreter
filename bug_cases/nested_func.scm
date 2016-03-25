(define curry (f) (lambda (x) (lambda (y) (f x y))))

(val x ((curry +) 4))
