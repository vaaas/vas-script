(use-modules (ice-9 ftw))

(define-macro (push x xs)
	`(set! ,xs (cons ,x ,xs)))

(define (main args)
	(define lang #f)
	(define file #f)
	(define lines '())
	(let ((head args))
		(while (not (null? head))
			(cond
				((string= (car head) "-lang")
					(set! lang (car (cdr head)))
					(set! head (cddr head)))
				(#t
					(set! file (car head))
					(set! head (cdr head))))))
	(set! lines (file-lines file))
	(load (string-append "./lang/" lang ".scm"))

	(for-each (lambda (x) (display x) (newline))
		(map (lambda (x) (serialise x lang)) lines)))

(define (file-lines path)
	(let*
		((file (open-input-file path))
		(line (read file))
		(lines '()))
	(while (not (eof-object? line))
		(push line lines)
		(set! line (read file)))
	(reverse lines)))

(define (serialise x lang)
	(cond
		((list? x) (serialise-list x lang))
		((symbol? x) (symbol->string x))
		((number? x) (number->string x))
		((string? x) x)))

(define (serialise-list x lang)
	(let ((first (car x)) (rest (cdr x)))
	(cond
		((bound? first)
			(primitive-eval
					(list (string->symbol (string-append lang "/" first)))
					rest))
))

(main (cdr (command-line)))
