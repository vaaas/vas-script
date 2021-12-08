(use-modules (ice-9 ftw))
(use-modules (ice-9 pretty-print))

(define-macro (push x xs)
	`(set! ,xs (cons ,x ,xs)))

(define-macro (each x xs . body)
	`(let ((head ,xs))
	(while (not (null? head))
		(let ((,x (car head)))
		,@body
		(set! head (cdr head))))))

(define (intersperse s xs)
	(let ((r (cons (car xs) '())))
	(each x (cdr xs)
		(push s r)
		(push x r))
	(reverse r)))

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
		((string? x) (primitive-eval (list (lang-proc lang "/escape-string") x)))))

(define (lang-proc lang name)
	(string->symbol (string-append lang "/" name)))

(define (serialise-list x lang)
	(let
		((first (car x))
		(rest (cdr x)))
	(cond
		((list? first)
			(primitive-eval
				`(,(lang-proc lang "/nested") (quote ,first) (quote ,rest))))
		((defined? (lang-proc lang (symbol->string first)))
			(primitive-eval
				`(,(lang-proc lang (symbol->string first)) (quote ,rest))))
		(#t
			(primitive-eval
				`(,(lang-proc lang "/call-user-function") (quote ,x)))))))

(main (cdr (command-line)))
