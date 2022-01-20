(define (sql//escape-string x) (string-append "'" x "'"))

(define (sql//nested x) #f)

(define (sql//call-user-function xs)
	(let
		((name (car xs))
		(args (cdr xs)))
	(string-append
		(string-append (serialise "sql" name) "(")
		(sql//infix ", " args)
		")")))

(define (sql//infix i x)
	(string-join (map (partial serialise "sql") x) i))

(define (sql/query x) (sql//infix "\n" x))

(define (sql/select x) (string-append "select " (sql//infix ", " x)))

(define (sql/where x) (string-append "where " (sql//infix " AND " x)))

(define (sql/or x) (sql//infix " OR " x))

(define (sql/and x) (sql//infix " AND " x))

(define (sql/= x) (sql//infix " = " x))

(define (sql/>= x) (sql//infix " >= " x))

(define (sql/> x) (sql//infix " > " x))

(define (sql/< x) (sql//infix " < " x))

(define (sql/<= x) (sql//infix " <= " x))

(define (sql/!= x) (sql//infix " != " x))

(define (sql/is x) (sql//infix " is " x))

(define (sql/isnt x) (sql//infix " is not " x))

(define (sql/from x) (string-append "from " (serialise "sql" (car x))))

(define (sql/as x)
	(string-append
		(serialise "sql" (cadr x))
		" as "
		(serialise "sql" (car x))))
