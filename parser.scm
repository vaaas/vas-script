(define-module (vas-script parser) #:export (parse))

(use-modules (vas-script util) (ice-9 textual-ports))

(define comment-char (make-parameter #\;))
(define string-char (make-parameter #\"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; public bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse port)
	(define x (peek-char port))
	(define xs nil)
	(while (not (eof-object? x))
		(cond
			((char=? x #\() (push (parse-list port) xs))
			((char=? x (comment-char)) (skip-comment port))
			((char=? x #\)) (throw 'unexpected "Unexpected symbol: )"))
			((whitespace? x) (get-char port)) ; do nothing/skip
			(#t (push (parse-atom port) xs)))
		(set! x (peek-char port)))
	(reverse xs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; private bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-list port)
	(get-char port)
	(let ((xs nil) (x (peek-char port)))
	(while (not (or (eof-object? x) (char=? x #\))))
		(cond
			((char=? x #\() (push (parse-list port) xs))
			((char=? x (comment-char)) (skip-comment port))
			((whitespace? x) (get-char port)) ; do nothing/skip
			(#t (push (parse-atom port) xs)))
		(set! x (peek-char port)))
	(get-char port)
	(reverse xs)))

(define (parse-atom port)
	(if (char=? (peek-char port) (string-char))
		(parse-string port)
		(parse-symbol port)))

(define (parse-string port)
	(define xs (list (string-char)))
	(define x nil)
	(get-char port)
	(set! x (get-char port))
	(while (not (or (eof-object? x) (char=? x (string-char))))
		(push x xs)
		(set! x (get-char port)))
	(when (eof-object? x) (throw 'unexpected "Unexpected end of file while reading string."))
	(push x xs)
	(-> xs reverse list->string string->symbol))

(define (parse-symbol port)
	(define xs nil)
	(define x (get-char port))
	(while (not (or (eof-object? x) (char=? x #\() (char=? x #\)) (char=? x (comment-char)) (whitespace? x)))
		(push x xs)
		(set! x (get-char port)))
	(when (not (eof-object? x)) (unget-char port x))
	(-> xs reverse list->string string->symbol))

(define (skip-comment port)
	(define x (get-char port))
	(while (not (or (eof-object? x) (char=? x #\newline)))
		(set! x (get-char port))))

(define (whitespace? x) (char-set-contains? char-set:whitespace x))
