(define-module (vas-script compiler)
	#:export (serialise))

;public
(define (serialise lang x)
	(let ((x (user-macro-expand x)))
	(if (list? x)
		(serialise-list lang x)
		(serialise-symbol x))))

;private
(use-modules (vas-script util) (ice-9 regex))

(define (lang-proc? lang name) (defined? (symbol-append lang '/ name)))

(define (lang-eval lang name . args)
	(primitive-eval (cons (symbol-append lang '/ name) (map (lambda (x) (list 'quote x)) args))))

(define (user-macro name) (symbol-append 'macro// name))

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
			(primitive-eval
				(cons
					(user-macro name)
					(map (lambda (x) (list 'quote x)) args)
				))
			x))
		x))

(define (serialise-list lang x)
	(let ((first (car x)) (rest (cdr x)))
	(cond
		((list? first) (lang-eval lang 'nested-function (serialise-list lang first) rest))
		((eq? 'macro first) (add-user-macro rest) #f)
		((lang-proc? lang first) (lang-eval lang first rest))
		(#t (lang-eval lang 'call-user-function x)))))

(define (serialise-symbol x)
	(define s (symbol->string x))
	(cond
		((string-match "^[0-9.]+" s) s)
		(#t s)))
