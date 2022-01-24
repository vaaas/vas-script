(use-modules
	(srfi srfi-1)
	(ice-9 receive))

(define nil (list))

(define-macro (push x xs) `(set! ,xs (cons ,x ,xs)))

(define-macro (ewhen cond . then) `(if (null? ,cond) nil (begin ,@then)))

(define-macro (ewhile cond . body) `(while (not (null? ,cond)) ,@body))

(define-macro (pop x) `(ewhen ,x (let ((head (car ,x))) (set! ,x (cdr ,x)) head)))

(define-macro (partial f . args) `(lambda (X) (,f ,@args X)))

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
	(primitive-eval `(define (,safe-name ,@args) ,@body))))

(define (user-macro-expand x)
	(if (list? x)
		(let ((name (car x)) (args (cdr x)))
		(if (and (symbol? name) (defined? (user-macro name)))
			(primitive-eval (cons (user-macro name) args))
			x))
		x))

(define (lang-proc? lang name) (defined? (lang-proc lang name)))

(define (serialise lang x)
	(let ((x (user-macro-expand x)))
	(cond
		((list? x) (serialise-list lang x))
		((symbol? x) (symbol->string x))
		((number? x) (number->string x))
		((string? x) (lang-eval lang '/escape-string x)))))

(define (serialise-list lang x)
	(let ((first (car x)) (rest (cdr x)))
	(cond
		((list? first) (lang-eval lang '/nested (serialise-list lang first) rest))
		((eq? 'macro first) (add-user-macro rest) #f)
		((lang-proc? lang first) (lang-eval lang first rest))
		(#t (lang-eval lang '/call-user-function x)))))

(define (file-lines file)
	(let ((line (read file)))
	(if (eof-object? line)
		nil
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
		(lambda (x) (let ((y (serialise lang x))) (when y (display y) (newline))))
		(file-lines (open-input-file file))))

(main (cdr (command-line)))
