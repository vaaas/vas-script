(console.log (+ 1 2 3))
(macro (test_macro x) `(console.log ,x))
(test_macro 891374)
((if true console.log console.error) "swag")
(define test "123")
(const rest 1234)
(let fest (when true (lambda (x) "swag")))
(const subtract
	(lambda (a b) (- a b)))
(progn
	1
	2
	(+ 3 5))
(if (>= 2 1)
	(console.log "yay")
	(console.log "nay"))
(define (main a b c)
	(console.log a b c))
(set (.. document body (getFirstElementChild 1 2 3)) (array 1 2 3))