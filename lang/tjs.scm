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

;; (define (/type xs)
;; 	(let ((first (car xs)) (rest (cdr xs)))
;; 	(cond
;; 		((null? rest) (self-defined-type first))
;; 		((list? first) (dynamic-type first (car rest)))
;; 		((symbol? first) (static-type first (car rest)))))
;; 	"")

;; ; private
;; (define (static-type a b)
;; 	(if (symbol? b) (set! b (list 'quote b)))
;; 	(primitive-eval `(define ,a ,b)))

;; (define (dynamic-type a b)

;; 	#f)

;; (define (self-defined-type x)
;; 	(if (symbol? x)
;; 		(primitive-eval `(define ,x (quote ,x)))
;; 		(if (= 1 (length x))
;; 			(primitive-eval `(define (,(car x) . xs) (append (list (quote ,(car x))) xs)))
;; 			(primitive-eval `(define (,(car x) ,@(cdr x)) (list (quote ,(car x)) ,@(cdr x)))))))

(define (plist-map f x)
	(if (null? x) x
		(cons (car x)
			(cons (f (cadr x))
			(plist-map f (cddr x))))))

(define (infer-type x)
	(cond
		((integer? x) 'Integer)
		((rational? x) 'Number)
		((string? x) 'String)
		((boolean? x) 'Boolean)
		((or (eq? 'null x) (eq? 'undefined x)) 'Nothing)
		((list? x) (infer-type-list x))))

(define (infer-type-list x)
	(define first (car x))
	(define rest (cdr x))
	(cond
		((eq? 'Array first) (list 'Array (length rest) (fold type-unite '() (map (lambda (x) (infer-type x)) rest))))
		((eq? 'Object first) (append (list 'Object) (plist-map (lambda (x) (infer-type x)) rest)))))

(define (type-unite a b)
	(cond
		((null? a) b)
		((equal? a b) b)
		((and (list? a) (list? b) (equal? 'Array (car a)) (equal? 'Array (car b)) (equal? (caddr a) (caddr b)))
			(list 'Array 'Any (caddr a)))
		(#t (throw 'type-mismatch "Cannot match types"))))
