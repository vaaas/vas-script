(use-modules (ice-9 ftw))

(define-macro (push x xs) `(set! ,xs (cons ,x ,xs)))

(define-macro (eif cond then else) `(if (null? ,cond) ,else ,then))

(define-macro (ewhen cond . then) `(if (null? ,cond) '() (begin ,@then)))

(define-macro (ewhile cond . body) `(while (not (null? ,cond)) ,@body))

(define-macro (pop x) `(ewhen ,x (let ((head (car ,x))) (set! ,x (cdr ,x)) head)))

(define (intersperse s x)
	(ewhen x (let ((head (car x)) (tail (cdr x)))
		(cons head (ewhen tail (cons s (intersperse s tail)))))))

(define (lang-proc lang name)
	(string->symbol (string-append lang "/" name)))

(define (lang-eval lang fname . args)
	(primitive-eval (append
		(list (lang-proc lang fname))
		(map (lambda (x) (list 'quote x)) args))))

(define (serialise x lang)
	(cond
		((list? x) (serialise-list x lang))
		((symbol? x) (symbol->string x))
		((number? x) (number->string x))
		((string? x) (lang-eval lang "/escape-string" x))))

(define (serialise-list x lang)
	(let
		((first (car x))
		(rest (cdr x)))
	(cond
		((list? first)
			(lang-eval lang "/nested" first rest))
		((defined? (lang-proc lang (symbol->string first)))
			(lang-eval lang (symbol->string first) rest))
		(#t
			(lang-eval lang "/call-user-function" x)))))

(define (file-lines file)
	(let ((line (read file)))
	(if (eof-object? line)
		'()
		(cons line (file-lines file)))))

(define (main args)
	(define lang #f)
	(define file #f)
	(ewhile args
		(let ((x (pop args)))
		(cond
			((string= x "-lang") (set! lang (pop args)))
			(#t (set! file x)))))
	(load (string-append "./lang/" lang ".scm"))
	(for-each
		(lambda (x) (display (serialise x lang)) (newline))
		(file-lines (open-input-file file))))

(main (cdr (command-line)))
