(define-module (vas-script compiler)
	#:export (
		load-lang
		lang-proc?
		lang-eval
		user-macro
		add-user-macro
		user-macro-expand
		serialise
		serialise-list
		file-lines))

(use-modules (vas-script util))

(define (load-lang x)
	(module-use! (current-module) (resolve-interface (list 'vas-script 'lang x) #:prefix x)))

(define (lang-proc? lang name) (defined? (symbol-append lang name)))

(define (lang-eval lang fname . args)
	(primitive-eval (cons (symbol-append lang fname) (map (lambda (x) (list 'quote x)) args))))

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
			(primitive-eval (cons (user-macro name) args))
			x))
		x))

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
		((list? first) (lang-eval lang '/nested-function (serialise-list lang first) rest))
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
			((string= x "-lang") (set! lang (string->symbol (pop args))))
			(#t (set! file x)))))
	(load-lang lang)
	(for-each
		(lambda (x) (let ((y (serialise lang x))) (when y (display y) (newline))))
		(file-lines (open-input-file file))))

(main (cdr (command-line)))
