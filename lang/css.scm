(define (css//escape-string x) (string-append "\"" x "\""))

(define (css//nested x) #f)

(define (css//call-user-function x) #f)

(define (css//declarations->alist x)
	(if (null? x)
		x
		(cons
			(cons (car x) (cadr x))
			(css//declarations->alist (cddr x)))))

(define (css/rule x)
	(string-append
		(serialise "css" (car x))
		" {\n"
		(css//declarations (css//declarations->alist (cdr x)))
		"\n}"))

(define (css//declarations x)
	(string-join (map css//declaration x) "\n"))

(define (css//declaration x)
	(let ((property (car x)) (value (cdr x)))
	(string-append
		(serialise "css" property)
		": "
		(string-join (map (partial serialise "css") value) " ")
		";")))

(define (css//unit u x) (string-append (serialise "css" x) u))

(define (css//prefix p x) (string-append p (serialise "css" x)))

(define (css//function f x) (string-append f "(" (map (partial serialise "css") x) ")"))

(define (css/px x) (css//unit "px" (car x)))

(define (css/em x) (css//unit "em" (car x)))

(define (css/rem x) (css//unit "rem" (car x)))

(define (css/vh x) (css//unit "vh" (car x)))

(define (css/vw x) (css//unit "vw" (car x)))

(define (css/% x) (css//unit "%" (car x)))

(define (css/important x) (css//unit " !important" (car x)))

(define (css/> x)
	(string-join
		(map (lambda (x) (string-append "> " (serialise "css" x))) x)))

(define (css/~ x)
	(string-join
		(map (lambda (x) (string-append "~ " (serialise "css" x))) x)))

(define (css/+ x)
	(string-join
		(map (lambda (x) (string-append "+ " (serialise "css" x))) x)))

(define (css/hex x) (css//prefix "#" (car x)))

(define (css/rgba x) (css//function "rgba" x))

(define (css/linear-gradient x) (css//function "linear-gradient" x))

(define (css/var x) (css//function "var" x))

(define (css/is x) (css//function ":is" x))

(define (css/: x) (string-append (serialise "css" (cadr x)) ":" (car x)))

(define (css/:: x) (string-append (serialise "css" (cadr x)) "::" (car x)))

(define (css/c x) (string-join (map (partial serialise "css") x) " "))

(define (css/or x) (string-join (map (partial serialise "css") x) ", "))

(define (css/= x)
	(string-append
		(serialise "css" (caddr x))
		"["
		(serialise "css" (cadr x))
		"="
		(serialise "css" (car x))
		"]"))

(define (css/not x) (string-append ":not(" (serialise "css" (car x)) ")"))

(define (css/id x) (string-append "#" (serialise "css" (car x))))
