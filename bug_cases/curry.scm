(define curry (f) (lambda (x) (lambda (y) (f x y))))

(val x (curry +))
(val y (x 5))
(val z (y 10))
