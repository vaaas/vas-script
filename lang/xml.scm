(define-module (vas-script lang xml)
	#:export (
		/call-user-function
		/nested-function
		/serialise-symbol))

(use-modules
	(vas-script util)
	(ice-9 regex)
	((vas-script compiler) #:select (serialise)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; private bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (string-like? x)
	(let ((c (string-ref (symbol->string x) 0)))
	(or (eq? #\' c) (eq? #\" c))))

(define (serialise-attrs xs)
	(-> xs
		(map (lambda (x)
			(string-append
				(serialise 'xml (car x))
				"="
				(quotes (serialise 'xml (cdr x))))))
		string-join))

(define (serialise-xml-node xs)
	(let ((name (pop xs))
		(attrs nil)
		(children nil))
	(ewhile xs (let ((x (pop xs)))
		(cond
			((list? x) (push x children))
			((string-like? x) (push x children))
			(#t (push (cons x (pop xs)) attrs)))))
	(string-append
		"<" (serialise 'xml name)
		(if (not (null? attrs))
			(string-append " " (serialise-attrs (reverse attrs)))
			"")
		(if (not (null? children))
			(string-append
				">"
				(string-join (map (partial serialise 'xml) (reverse children)) "")
				"</"
				(serialise 'xml name)
				">")
			"/>"))))

(define (escape-string x)
	(-> x
		string->list
		(map (lambda (x) (case x
			((#\') "&apos;")
			((#\") "&quot;")
			((#\&) "&amp;")
			((#\<) "&lt;")
			((#\>) "&gt;")
			(else (list->string (list x))))))
		(C string-join "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; public bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; basic features
(define (/call-user-function xs)
	(serialise-xml-node xs))

(define (/nested-function first rest)
	(throw 'unsupported "Nested functions are unsupported in xml."))

(define (/serialise-symbol x)
	(let*
		((s (symbol->string x))
		(len (string-length s)))
	(cond
		((string-match "^(\"|').*(\"|')$" s) (escape-string (substring s 1 (- len 1))))
		((string-match "^'.*$" s) (escape-string (substring s 1 len)))
		(#t s))))

(define (/?xml) "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
