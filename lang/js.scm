(define-module (vas-script lang js)
	#:export (
		/call-user-function
		/nested-function
		/escape-string
		/+
		/-
		/&&
		/||
		/>
		/<
		/<=
		/>=
		/=
		/!=
		/!
		/like
		/unlike
		/set
		/define
		/const
		/let
		/if
		/when
		/lambda
		/return
		/progn
		/..
		/get
		/array))

(use-modules (vas-script util))
(use-modules ((vas-script compiler) #:select (serialise)))

; private bindings
(define (serialise-args x)
	(parens (string-join (map (partial serialise 'js) x) ", ")))

(define (define-variable type name body)
	(string-append type " " (serialise 'js name) " = " (serialise 'js body)))

(define (define-function name args body)
	(string-append
		"function "
		(symbol->string name)
		(serialise-args args)
		" "
		(braces
			(newlines
			(string-join (map (partial serialise 'js) (maybe-add-return body)) "\n")))))

(define (maybe-add-return x)
	(let ((x (car x)) (xs (cdr x)))
	(cond
		((not (null? xs)) (cons x (maybe-add-return xs)))
		((or (not (list? x)) (not (eq? 'return (car x)))) (list (list 'return x)))
		(#t x))))

; public bindings
(define (/call-user-function xs)
	(let ((name (car xs)) (args (cdr xs)))
	(string-append (serialise 'js name) (serialise-args args))))

(define (/nested-function first rest)
	(string-append
		(parens first)
		(serialise-args rest)))

(define (/escape-string x)
	(string-append "\"" x "\""))

(define (/+ xs) (infix 'js "+" xs))
(define (/- xs) (infix 'js "-" xs))
(define (/&& xs) (infix 'js "&&" xs))
(define (/|| xs) (infix 'js "||" xs))
(define (/** xs) (infix 'js "**" xs))
(define (/> xs) (infix 'js ">" xs))
(define (/< xs) (infix 'js "<" xs))
(define (/<= xs) (infix 'js "<==" xs))
(define (/>= xs) (infix 'js ">==" xs))
(define (/= xs) (infix 'js "===" xs))
(define (/!= xs) (infix 'js "!==" xs))
(define (/! xs) (string-append "!" (parens (serialise 'js (car xs)))))
(define (/like xs) (infix 'js "==" xs))
(define (/unlike xs) (infix 'js "!=" xs))
(define (/set xs) (infix 'js "=" xs))

(define (/define xs)
	(let
		((first (car xs))
		(rest (cdr xs)))
	(if (list? first)
		(define-function (car first) (cdr first) rest)
		(define-variable "var" first (car rest)))))

(define (/const xs)
	(define-variable "const" (car xs) (cadr xs)))

(define (/let xs)
	(define-variable "let" (car xs) (cadr xs)))

(define (/if xs)
	(string-join
		(list
			(serialise 'js (car xs))
			"?"
			(serialise 'js (cadr xs))
			":"
			(serialise 'js (caddr xs)))))

(define (/when xs)
	(string-append
		(serialise 'js (car xs))
		"?"
		(serialise 'js (cadr xs))
		": null"))

(define (/lambda xs)
	(let
		((args (car xs))
		(body (cdr xs)))
	(string-append
		"function"
		(serialise-args args)
		(braces
			(newlines
				(string-join
					(map (partial serialise 'js) (maybe-add-return body)) "\n"))))))

(define (/return xs)
	(string-append "return " (serialise 'js (car xs))))

(define (/progn xs)
	(string-append (parens (/lambda (append (list nil) xs))) "()"))

(define (/.. xs)
	(string-join (map (partial serialise 'js) xs) "."))

(define (/get xs)
	(string-join (map (lambda (x) (brackets (serialise 'js x))) xs) ""))

(define (/array xs)
	(brackets (string-join (map (partial serialise 'js) xs) ", ")))
