(define-module (vas-script lang css)
	#:export (
		/call-user-function
		/nested-function
		/escape-string
		/rule
		/px
		/em
		/rem
		/vh
		/vw
		/%
		/important
		/>
		/~
		/+
		/hex
		/rgba
		/linear-gradient
		/var
		/is
		/:
		/::
		/c
		/or
		/=
		/not
		/id))

(use-modules (vas-script util) (vas-script compiler))

; private functions
(define (declarations->alist x)
	(if (null? x)
		x
		(cons
			(cons (user-macro-expand (car x)) (user-macro-expand (cadr x)))
			(declarations->alist (cddr x)))))

(define (declarations x) (string-join (map declaration x) "\n"))

(define (declaration x)
	(let ((property (car x)) (value (cdr x)))
	(string-append
		(serialise 'css property)
		": "
		(string-join (map (partial serialise 'css) value) " ")
		";")))

(define (function f x) (string-append f "(" (map (partial serialise 'css) x) ")"))

(define (unit u x) (string-append (serialise 'css x) u))

(define (prefix p x) (string-append p (serialise 'css x)))

; public functions
(define (/serialise-symbol x) (serialise-symbol x))

(define (/nested-function x) #f)

(define (/call-user-function x) #f)

(define (/rule x)
	(string-append
		(serialise 'css (car x))
		" {\n"
		(declarations (declarations->alist (cdr x)))
		"\n}"))

(define (/px x) (unit "px" (car x)))

(define (/em x) (unit "em" (car x)))

(define (/rem x) (unit "rem" (car x)))

(define (/vh x) (unit "vh" (car x)))

(define (/vw x) (unit "vw" (car x)))

(define (/% x) (unit "%" (car x)))

(define (/important x) (unit " !important" (car x)))

(define (/> x)
	(string-join
		(map (lambda (x) (string-append "> " (serialise 'css x))) x)))

(define (/~ x)
	(string-join
		(map (lambda (x) (string-append "~ " (serialise 'css x))) x)))

(define (/+ x)
	(string-join
		(map (lambda (x) (string-append "+ " (serialise 'css x))) x)))

(define (/hex x) (prefix "#" (car x)))

(define (/rgba x) (function "rgba" x))

(define (/linear-gradient x) (function "linear-gradient" x))

(define (/var x) (function "var" x))

(define (/is x) (function ":is" x))

(define (/: x) (string-append (serialise 'css (cadr x)) ":" (car x)))

(define (/:: x) (string-append (serialise 'css (cadr x)) "::" (car x)))

(define (/c x) (string-join (map (partial serialise 'css) x) " "))

(define (/or x) (string-join (map (partial serialise 'css) x) ", "))

(define (/= x)
	(string-append
		(serialise 'css (caddr x))
		"["
		(serialise 'css (cadr x))
		"="
		(serialise 'css (car x))
		"]"))

(define (/not x) (string-append ":not(" (serialise 'css (car x)) ")"))

(define (/id x) (string-append "#" (serialise 'css (car x))))
