(define-module (vas-script lang c)
	#:export (
		/call-user-function
		/nested-function
		/escape-string
		/include
		/define
		/function))

;private
(use-modules (vas-script util))
(use-modules ((vas-script compiler) #:select serialise))

(define (type x)
	(string-join
		(map (lambda (x) (serialise 'c (if (eq? 'ptr x) '* x))) x)
		" "))

;public
(define (/call-user-function xs)
	(let
		((name (car xs))
		(args (cdr xs)))
	(string-append
		(string-append (serialise 'c name) "(")
		(string-join (map (partial serialise 'c) args) ", ")
		")")))

(define (/nested-function first rest)
	(string-append
		"(" first ")"
		"(" (string-join (map (partial serialise 'c) rest) ", ") ")"))

(define (/escape-string x)
	(string-append "\"" x "\""))

(define (/include x)
	(string-append "include " (serialise 'c (car x))))

(define (/define x)
	(string-append
		(type (car x))
		" "
		(serialise 'c (cadr x))
		" = "
		(serialise 'c (caddr x))
		";"))

(define (/function x)
	(string-append
		(type (car x))
		" "
		(serialise 'c (cadr x))
		" ("
		(c//function-args (caddr x))
		") {"
		(c//function-body (cddr x))
		"}"))
