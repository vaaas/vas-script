(use-modules (ice-9 ftw))

(define-macro (push x xs) `(set! ,xs (cons ,x ,xs)))

(define-macro (eif cond then else) `(if (null? ,cond) ,else ,then))

(define-macro (ewhen cond . then) `(if (null? ,cond) '() (begin ,@then)))

(define-macro (ewhile cond . body) `(while (not (null? ,cond)) ,@body))

(define-macro (pop x) `(ewhen ,x (let ((head (car ,x))) (set! ,x (cdr ,x)) head)))

(define (quote-all x) (map (lambda (x) (list 'quote x)) x))

(define (intersperse s x)
	(ewhen x (let ((head (car x)) (tail (cdr x)))
		(cons head (ewhen tail (cons s (intersperse s tail)))))))

(define (lang-proc lang name)
	(string->symbol (string-append lang "/" (symbol->string name))))

(define (lang-eval lang fname . args)
	(primitive-eval (cons (lang-proc lang fname) (quote-all args))))

(define (user-macro name)
	(string->symbol (string-append "macro//" (symbol->string name))))

(define (add-user-macro list)
	(let*
		((head (car list))
		(name (car head))
		(safe-name (user-macro name))
		(args (cdr head))
		(body (cdr list)))
	(primitive-eval `(define-macro (,safe-name ,@args) ,@body))))

(define (user-macro? name) (defined? (user-macro name)))

(define (user-macro-expand name args)
	(primitive-eval (cons (user-macro name) (quote-all args))))

(define (lang-proc? lang name) (defined? (lang-proc lang name)))

(define (serialise x lang)
	(cond
		((list? x) (serialise-list x lang))
		((symbol? x) (symbol->string x))
		((number? x) (number->string x))
		((string? x) (lang-eval lang '/escape-string x))))

(define (serialise-list x lang)
	(let
		((first (car x))
		(rest (cdr x)))
	(cond
		((list? first) (lang-eval lang '/nested first rest))
		((eq? 'macro first) (add-user-macro rest) "")
		((user-macro? first) (serialise (user-macro-expand first rest) lang))
		((lang-proc? lang first) (lang-eval lang first rest))
		(#t (lang-eval lang '/call-user-function x)))))

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
