(load-lang "js")
(load-lang "html")
(load-lang "css")

(define (vue//escape-string x) (string-append "\"" x "\""))

(define (vue//nested x) #f)

(define (vue//call-user-function x) #f)

(define (vue/template x)
	(string-append
		"<template>"
		(string-join
			(map (partial serialise "html") x)
			"\n")
		"</template>"))

(define (vue/style x)
	(string-append
		"<style>"
		(string-join
            (map (partial serialise "css") x)
            "\n")
		"</style>"))

(define (vue/script x)
	(string-append
		"<script>"
		(string-join
			(map (partial serialise "js") x)
			"\n")
		"</script>"))
