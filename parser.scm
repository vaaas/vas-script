(define-module (vas-script parser) #:export (parse))

(use-modules (vas-script util) (ice-9 textual-ports))

(define comment-char (make-parameter #\;))
(define strings (make-parameter #f))

;public
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

;private
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
