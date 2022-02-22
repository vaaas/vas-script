(use-modules
	(vas-script compiler)
	(vas-script parser)
	(vas-script util))

(define (main args)
	(define lang #f)
	(define file #f)
	(ewhile args
		(let ((x (pop args)))
		(cond
			((string= x "-lang") (set! lang (string->symbol (pop args))))
			(#t (set! file x)))))
	(load-lang lang)
	(parse (open-input-file file))
	(for-each
		(lambda (x) (let ((y (serialise lang x))) (when y (display y) (newline))))
		(parse (open-input-file file))))

(main (cdr (command-line)))
