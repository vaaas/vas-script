(define-module (vas-script lang sql)
	#:export (
		/call-user-function
		/nested-function
		/escape-string
		/query
		/select
		/where
		/or
		/and
		/=
		/>=
		/>
		/<
		/<=
		/is
		/isnt
		/from
		/as))

(use-modules (vas-script util))
(use-modules ((vas-script compiler) #:select (serialise)))

(define (/escape-string x) (string-append "'" x "'"))

(define (/nested-function x) #f)

(define (/call-user-function xs)
	(let
		((name (car xs))
		(args (cdr xs)))
	(string-append
		(string-append (serialise 'sql name) "(")
		(infix 'sql ", " args)
		")")))

(define (/query x) (infix 'sql "\n" x))
(define (/select x) (string-append "select " (infix 'sql ", " x)))
(define (/where x) (string-append "where " (infix 'sql " AND " x)))
(define (/or x) (infix 'sql " OR " x))
(define (/and x) (infix 'sql " AND " x))
(define (/= x) (infix 'sql " = " x))
(define (/>= x) (infix 'sql " >= " x))
(define (/> x) (infix 'sql " > " x))
(define (/< x) (infix 'sql " < " x))
(define (/<= x) (infix 'sql " <= " x))
(define (/!= x) (infix 'sql " != " x))
(define (/is x) (infix 'sql " is " x))
(define (/isnt x) (infix 'sql " is not " x))

(define (/from x) (string-append "from " (serialise 'sql (car x))))

(define (/as x)
	(string-append
		(serialise 'sql (cadr x))
		" as "
		(serialise 'sql (car x))))
