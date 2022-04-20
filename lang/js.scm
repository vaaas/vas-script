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
		/progn
		/..
		/get
		/array
		/object
		/Set
		/Map
		/from
		/return
		/new
		/async
		/await
		/throw))

(use-modules (vas-script util))
(use-modules ((vas-script compiler) #:select (serialise)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; private bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (serialise-args x)
	(parens (string-join (map (partial serialise 'js) x) ", ")))

(define (declare-variable type name body)
	(string-append
		type
		" "
		(sanitise-string (serialise 'js name))
		" = "
		(serialise 'js body)))

(define (declare-function name args body)
	(string-append
		"function "
		(sanitise-string (symbol->string name))
		(serialise-args args)
		" "
		(-> body
			maybe-add-return
			(map (partial serialise 'js))
			(C string-join "\n")
			newlines
			braces)))

(define (maybe-add-return x)
	(let ((x (car x)) (xs (cdr x)))
	(cond
		((not (null? xs)) (cons x (maybe-add-return xs)))
		((or (not (list? x)) (not (eq? 'return (car x)))) (list (list 'return x)))
		(#t x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; public bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

; declarations
(define (/define xs)
	(let
		((first (car xs))
		(rest (cdr xs)))
	(if (list? first)
		(declare-function (car first) (cdr first) rest)
		(declare-variable "var" first (car rest)))))

(define (/const xs)
	(declare-variable "const" (car xs) (cadr xs)))

(define (/let xs)
	(declare-variable "let" (car xs) (cadr xs)))

(define (/from xs)
	(let ((len (length xs)))
	(cond
		((= 1 len) (string-append "import " (serialise 'js (car xs)))) ; side-effect import
		((and (= 2 len) (list? (cadr xs)) ; import named exports
			(string-append
				"import "
				(-> xs
					cadr
					(map (lambda (x)
						(if (list? x)
							(string-append (serialise 'js (car x)) " as " (serialise 'js (cadr x)))
							(serialise 'js x))))
					(C string-join ", ")
					spaces
					braces)
				" from "
				(serialise 'js (car xs)))))
		(#t (string-append ; import default export
			"import "
			(serialise 'js (cadr xs))
			" from " (serialise 'js (car xs)))))))

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

; keywords
(define (/return xs) (string-append "return " (serialise 'js (car xs))))
(define (/async xs) (string-append "async " (serialise 'js (car xs))))
(define (/await xs) (parens (string-append "await " (serialise 'js (car xs)))))
(define (/new xs) (string-append "new " (serialise 'js (car xs))))
(define (/throw xs) (string-append "throw " (serialise 'js (car xs))))
