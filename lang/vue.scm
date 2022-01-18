(load (string-append "js" ".scm"))
(load (string-append "html" ".scm"))

(define (vue//escape-string x) (string-append "\"" x "\""))

(define (vue//nested x) #f)

(define (vue//call-user-function x) #f)

(define (vue/template x)
	(string-append
		"<template>"
		(string-join
			(map (lambda (x) (serialise x "html")) x)
			"\n")
		"</template>"))

(define (vue/style x)
	(string-append
		"<style>"
		(string-join x "\n")
		"</style>"))

(define (vue/script x)
	(string-append
		"<script>"
		(string-join
			(map (lambda (x) (serialise x "js")) x)
			"\n")
		"</script>"))
