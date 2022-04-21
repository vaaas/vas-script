(load-lang "js")
(load-lang "html")
(load-lang "css")

(define (/serialise-symbol x) (serialise-symbol x))

(define (/nested-function x) #f)

(define (/call-user-function x) #f)

(define (/template x)
	(string-append
		"<template>"
		(string-join
			(map (partial serialise "html") x)
			"\n")
		"</template>"))

(define (/style x)
	(string-append
		"<style>"
		(string-join
            (map (partial serialise "css") x)
            "\n")
		"</style>"))

(define (/script x)
	(string-append
		"<script>"
		(string-join
			(map (partial serialise "js") x)
			"\n")
		"</script>"))
