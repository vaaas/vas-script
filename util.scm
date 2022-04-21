(define-module (vas-script util)
	#:export (
		load-lang
		nil
		push
		ewhen
		ewhile
		pop
		partial
		->
		C
		$
		intersperse
		parens
		brackets
		braces
		spaces
		newlines
		infix
		plist->alist
		sanitise-string))

(use-modules ((vas-script compiler) #:select (serialise)))

(define (load-lang x)
	(module-use! (current-module) (resolve-interface (list 'vas-script 'lang x) #:prefix x)))

(define nil (list))

; push an item on top of a list, modifying the list
(define-macro (push x xs) `(set! ,xs (cons ,x ,xs)))

(define-macro (ewhen cond . then) `(if (null? ,cond) nil (begin ,@then)))

(define-macro (ewhile cond . body) `(while (not (null? ,cond)) ,@body))

; take the first item off a list, modifying the list
(define-macro (pop x) `(ewhen ,x (let ((head (car ,x))) (set! ,x (cdr ,x)) head)))

; partially apply a function
(define-macro (partial f . args) `(lambda (X) (,f ,@args X)))

; pipeline operator
(define-macro (-> x . fs)
	(define (help x fs)
		(if (null? fs) x
			(help
				(if (list? (car fs)) (append (car fs) (list x)) (list (car fs) x))
				(cdr fs))))
	(help x fs))

; flip the last 2 arguments
(define-macro (C . xs)
	(define (help x)
		(if (= 3 (length x))
			(list (car x) (caddr x) (cadr x))
			(list (car x) (help (cdr x)))))
	(if (< (length xs) 3) xs) (help xs))

; place the last argument in the position of #:$
(define-macro ($ . xs)
	(let*
		((len (length xs))
		(last (list-ref xs (- len 1)))
		(head (list-head xs (- len 1))))
	(map (lambda (x) (if (eq? #:$ x) last x)) head)))

; place element S between every element of a list
(define (intersperse s x)
	(ewhen x (let ((head (car x)) (tail (cdr x)))
		(cons head (ewhen tail (cons s (intersperse s tail)))))))

(define (parens x) (string-append "(" x ")"))
(define (brackets x) (string-append "[" x "]"))
(define (braces x) (string-append "{" x "}"))
(define (newlines x) (string-append "\n" x "\n"))
(define (spaces x) (string-append " " x " "))
(define (infix lang i x) (string-join (map (partial serialise lang) x) (string-append " " i " ")))

; turns a plist into an alist
(define (plist->alist x)
	(if (null? x) x
		(cons (cons (car x) (cadr x))
			(plist->alist (cddr x)))))

(define (sanitise-string x)
	(-> x
		string->list
		(map sanitise-char)
		(list->string)))

(define (sanitise-char x)
	(case x
		((#\-) #\_)
		((#\?) #\A)
		((#\/) #\B)
		((#\!) #\C)
		((#\:) #\D)
		((#\;) #\E)
		((#\\) #\F)
		((#\@) #\G)
		((#\#) #\H)
		((#\%) #\I)
		((#\^) #\J)
		((#\&) #\K)
		((#\*) #\L)
		((#\=) #\M)
		((#\+) #\N)
		((#\|) #\O)
		((#\.) #\P)
		((#\,) #\Q)
		((#\<) #\R)
		((#\>) #\S)
		((#\`) #\U)
		((#\~) #\V)
		(else x)
	))
