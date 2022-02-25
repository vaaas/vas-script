(define-module (vas-script lang tjs)
	#:export (
		/call-user-function
		/nested-function
		/escape-string
		/type))

; public
(define (/call-user-function xs) #f)
(define (/escape-string x) (string-append "\"" x "\""))
(define (/nested-function x) (throw 'unsupported "nested functions are unsupported"))

(define (/type xs)
	(let ((first (car xs)) (rest (cdr xs)))
	(cond
		((null? rest) (self-defined-type first))
		((list? first) (dynamic-type first (car rest)))
		((symbol? first) (static-type first (car rest)))))
	"")

; private
(define (static-type a b)
	(if (symbol? b) (set! b (list 'quote b)))
	(primitive-eval `(define ,a ,b)))

(define (dynamic-type a b)

	#f)

(define (self-defined-type x)
	(if (symbol? x)
		(primitive-eval `(define ,x (quote ,x)))
		(if (= 1 (length x))
			(primitive-eval `(define (,(car x) . xs) (append (list (quote ,(car x))) xs)))
			(primitive-eval `(define (,(car x) ,@(cdr x)) (list (quote ,(car x)) ,@(cdr x)))))))
