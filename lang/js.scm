(define (js/+ . body)
	(list
		(serialise (list-ref 0 body))
		(serialise (list-ref 1 body))
		(serialise (list-ref 2 body))))
