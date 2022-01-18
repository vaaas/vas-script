(define (js//call-user-function xs)
	(let
		((name (car xs))
		(args (cdr xs)))
	(string-append
		(string-append (serialise name "js") "(")
		(string-join (map (lambda (x) (serialise x "js")) args) ", ")
		")")))

(define (js//nested first rest)
	(string-append
		"(" (serialise first "js") ")"
		"(" (string-join (map (lambda (x) (serialise x "js")) rest) ", ") ")"))

(define (js//escape-string x)
	(string-append "\"" x "\""))

(define (js//infix-operator symbol xs)
	(string-join
	(intersperse symbol
	(map (lambda (x) (serialise x "js")) xs))))

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
(define (js/! xs) (string-append "!(" (serialise (car x) "js") ")"))
(define (js/like xs) (js//infix-operator "==" xs))
(define (js/unlike xs) (js//infix-operator "!=" xs))

(define (js//define-variable type name body)
	(string-join
		(list
			type
			(symbol->string name)
			"="
			(serialise body "js"))))

(define (js//define-function name args body)
	(string-append
		"function "
		(symbol->string name)
		" ("
		(string-join (map symbol->string args) ", ")
		") "
		"{\n"
		(string-join (map (lambda (x) (serialise x "js")) (js//maybe-add-return body)) "\n")
		"\n}"))

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
			(serialise (car xs) "js")
			"?"
			(serialise (cadr xs) "js")
			":"
			(serialise (caddr xs) "js"))))

(define (js/when xs)
	(string-join
		(list
			(serialise (car xs) "js")
			"?"
			(serialise (cadr xs) "js")
			":"
			"null")))

(define (js//maybe-add-return xs)
	(let ((last nil) (r nil))
	(for-each
		(lambda (x) (ewhen last (push r last)) (set! last x))
		xs)
	(cond
		((not (list? last)) (push (list 'return last) r))
		((not (eq? 'return (car last))) (push (list 'return last) r))
		(#t (push last r)))
	(reverse r)))

(define (js/lambda xs)
	(let
		((args (car xs))
		(body (cdr xs)))
	(string-join
		(list
			"function("
			(string-join (map (lambda (x) (serialise x "js")) args) ", ")
			") { "
			(string-join (map (lambda (x) (serialise x "js")) (js//maybe-add-return body)) "\n")
			" }")
		"")))

(define (js/return xs)
	(string-join
		(list
			"return"
			(serialise (car xs) "js"))))

(define (js/progn xs)
	(string-append "(" (js/lambda (append (list nil) xs)) ")()"))
