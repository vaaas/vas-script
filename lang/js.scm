(define (js//call-user-function xs)
	(let ((name (car xs)) (args (cdr xs)))
	(string-append (serialise "js" name) (js//serialise-args args))))

(define (js//nested first rest)
	(string-append
		(js//parens first)
		(js//serialise-args rest)))

(define (js//escape-string x)
	(string-append "\"" x "\""))

(define (js//infix-operator symbol xs)
	(string-join
	(intersperse symbol
	(map (partial serialise "js") xs))))

(define (js//parens x) (string-append "(" x ")"))
(define (js//brackets x) (string-append "[" x "]"))
(define (js//braces x) (string-append "{" x "}"))
(define (js//newlines x) (string-append "\n" x "\n"))

(define (js/+ xs) (js//infix-operator "+" xs))
(define (js/- xs) (js//infix-operator "-" xs))
(define (js/&& xs) (js//infix-operator "&&" xs))
(define (js/|| xs) (js//infix-operator "||" xs))
(define (js/** xs) (js//infix-operator "**" xs))
(define (js/> xs) (js//infix-operator ">" xs))
(define (js/< xs) (js//infix-operator "<" xs))
(define (js/<= xs) (js//infix-operator "<==" xs))
(define (js/>= xs) (js//infix-operator ">==" xs))
(define (js/= xs) (js//infix-operator "===" xs))
(define (js/!= xs) (js//infix-operator "!==" xs))
(define (js/! xs) (string-append "!" (js//parens (serialise "js" (car xs)))))
(define (js/like xs) (js//infix-operator "==" xs))
(define (js/unlike xs) (js//infix-operator "!=" xs))
(define (js/set xs) (js//infix-operator "=" xs))

(define (js//serialise-args x)
	(js//parens (string-join (map (partial serialise "js") x) ", ")))

(define (js//define-variable type name body)
	(string-append type " " (serialise "js" name) " = " (serialise "js" body)))

(define (js//define-function name args body)
	(string-append
		"function "
		(symbol->string name)
		(js//serialise-args args)
		" "
		(js//braces
			(js//newlines
			(string-join (map (partial serialise "js") (js//maybe-add-return body)) "\n")))))

(define (js/define xs)
	(let
		((first (car xs))
		(rest (cdr xs)))
	(if (list? first)
		(js//define-function (car first) (cdr first) rest)
		(js//define-variable "var" first (car rest)))))

(define (js/const xs)
	(js//define-variable "const" (car xs) (cadr xs)))

(define (js/let xs)
	(js//define-variable "let" (car xs) (cadr xs)))

(define (js/if xs)
	(string-join
		(list
			(serialise "js" (car xs))
			"?"
			(serialise "js" (cadr xs))
			":"
			(serialise "js" (caddr xs)))))

(define (js/when xs)
	(string-join
		(list
			(serialise "js" (car xs))
			"?"
			(serialise "js" (cadr xs))
			":"
			"null")))

(define (js//maybe-add-return x)
	(let ((x (car x)) (xs (cdr x)))
	(cond
		((not (null? xs)) (cons x (js//maybe-add-return xs)))
		((or (not (list? x)) (not (eq? 'return (car x)))) (list (list 'return x)))
		(#t x))))

(define (js/lambda xs)
	(let
		((args (car xs))
		(body (cdr xs)))
	(string-append
		"function"
		(js//serialise-args args)
		(js//braces
			(js//newlines
				(string-join
					(map (partial serialise "js") (js//maybe-add-return body)) "\n"))))))

(define (js/return xs)
	(string-append "return " (serialise "js" (car xs))))

(define (js/progn xs)
	(string-append (js//parens (js/lambda (append (list nil) xs))) "()"))

(define (js/.. xs)
	(string-join (map (partial serialise "js") xs) "."))

(define (js/get xs)
	(string-join (map (lambda (x) (js//brackets (serialise "js" x))) xs) ""))

(define (js/array xs)
	(js//brackets (string-join (map (partial serialise "js") xs) ", ")))
