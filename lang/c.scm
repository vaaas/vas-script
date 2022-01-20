(define (c//call-user-function xs)
	(let
		((name (car xs))
		(args (cdr xs)))
	(string-append
		(string-append (serialise "js" name) "(")
		(string-join (map (partial serialise "js") args) ", ")
		")")))

(define (c//nested first rest)
	(string-append
		"(" first ")"
		"(" (string-join (map (partial serialise "js") rest) ", ") ")"))

(define (c//escape-string x)
	(string-append "\"" x "\""))

(define (c/include x)
	(string-append "include " (serialise "c" (car x))))

(define (c//type x)
	(string-join
		(map (lambda (x) (serialise "c" (if (eq? 'ptr x) '* x))) x)
		" "))

(define (c/define x)
	(string-append
		(c//type (car x))
		" "
		(serialise "c" (cadr x))
		" = "
		(serialise "c" (caddr x))
		";"))

(define (c/function x)
	(string-append
		(c//type (car x))
		" "
		(serialise "c" (cadr x))
		" ("
		(c//function-args (caddr x))
		") {"
		(c//function-body (cddr x))
		"}"))
