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
	(let ((rule (car x)) (declarations (css//declarations->alist (cdr x))))
	(string-append
		(string-join (map (partial serialise "css") rule) " ")
		" {\n"
		(string-join (map css/declaration declarations) ";\n")
		"\n}")))

(define (css/declaration x)
	(let ((property (car x)) (value (cadr x)))
	(string-append
		(serialise "css" property)
		": "
		(map (partial serialise "css") value))))

(define (css//unit unit x) (string-append (serialise "css" x) unit))

(define (css//prefix p x) (string-append p (serialise "css" x)))

(define (css//function f x) (string-append f "(" (map (partial serialise "css") x) ")"))

(define (css/px x) (css//unit (car x) "px"))

(define (css/em x) (css//unit (car x) "em"))

(define (css/rem x) (css//unit (car x) "rem"))

(define (css/vh x) (css//unit (car x) "vh"))

(define (css/vw x) (css//unit (car x) "vw"))

(define (css/% x) (css//unit (car x) "%"))

(define (css/> x) (css//prefix "> " (car x)))

(define (css/~ x) (css//prefix "~ " (car x)))

(define (css/+ x) (css//prefix "+ " (car x)))

(define (css/hex x) (css//prefix "#" (car x)))

(define (css/important x) (css//unit " !important" (car x)))

(define (css/rgba x) (css//function "rgba" x))

(define (css/linear-gradient x) (css//function "linear-gradient" x))

(define (css/var x) (css//function "var" x))

(define (css/is x) (css//function ":is" x))

(define (css/: x) (string-append (serialise "css" (cadr x)) ":" (car x)))

(define (css/: x) (string-append (serialise "css" (cadr x)) "::" (car x)))

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
