(define (html//escape-string x) (string-append "\"" x "\""))

(define (html//nested x) #f)

(define (html/doctype _) "<!DOCTYPE html>")

(define (html//call-user-function xs)
	(let
		((name (symbol->string (pop xs)))
		(attrs nil)
		(children nil)
		(tokens nil))
	(ewhile xs
		(let ((x (pop xs)))
		(cond
			((symbol? x) (push (cons (symbol->string x) (pop xs)) attrs))
			((list? x) (push (serialise "html" x) children))
			((string? x) (push x children)))))
	(set! attrs (reverse attrs))
	(set! children (reverse children))
	(push "<" tokens)
	(push name tokens)
	(ewhile attrs
		(push " " tokens)
		(let* ((kv (pop attrs)) (attr (car kv)) (value (cdr kv)))
		(push attr tokens)
		(push "=" tokens)
		(push (html//escape-string value) tokens)))
	(if (null? children)
		(push "/>" tokens)
		(begin
			(push ">" tokens)
			(ewhile children
				(let ((x (pop children)))
				(push (if (list? x) (serialise "html" x) x) tokens)))
			(push "</" tokens)
			(push name tokens)
			(push ">" tokens)))
	(string-join (reverse tokens) "")))
