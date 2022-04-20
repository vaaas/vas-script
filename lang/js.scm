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
		/array
		/object
		/Set
		/Map))

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

; basic features
(define (/call-user-function xs)
	(let ((name (car xs)) (args (cdr xs)))
	(string-append (serialise 'js name) (serialise-args args))))

(define (/nested-function first rest)
	(string-append
		(parens first)
		(serialise-args rest)))

(define (/escape-string x)
	(string-append "\"" x "\""))

(define (/.. xs)
	(string-join (map (partial serialise 'js) xs) "."))

(define (/get xs)
	(string-join (map (lambda (x) (brackets (serialise 'js x))) xs) ""))

; infix operators
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

; variable and function declarations
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

; lambda
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

; flow control
(define (/if xs)
	(string-append
		(serialise 'js (car xs))
		" ? "
		(serialise 'js (cadr xs))
		" : "
		(serialise 'js (caddr xs))))

(define (/when xs)
	(string-append
		(serialise 'js (car xs))
		" ? "
		(serialise 'js (cadr xs))
		" : null"))

(define (/return xs)
	(string-append "return " (serialise 'js (car xs))))

(define (/progn xs)
	(string-append (parens (/lambda (append (list nil) xs))) "()"))

; data structure literals
(define (/array xs)
	(-> xs
		(map (partial serialise 'js))
		(C string-join ", ")
		brackets))

(define (/object xs)
	(-> xs
		plist->alist
		(map (lambda (x) (string-append (serialise 'js (car x)) ": " (serialise 'js (cdr x)))))
		(C string-join ", ")
		braces))

(define (/Set xs)
	(-> xs
		/array
		parens
		(string-append "new Set")))

(define (/Map xs)
	(-> xs
		plist->alist
		(map (lambda (x) (/array (list (car x) (cdr x)))))
		(C string-join ", ")
		brackets
		parens
		(string-append "new Map")))
