# Vas Script

This is a Scheme to anything compiler. It provides a unified, coherent lisp serialisation for various programming languages and serialisation formats, and also macro support.

# Requirements

- guile (>= 2.2.7)

# Examples

## Macros

Macros are functions that are declared, executed, and exist only at compile time. Regardless of language mode, macros are declared as follows:

```scheme
(macro (my-macro-name a b c) (' (a b c)))
```

You can also declare variadic macros:

```scheme
(macro (variadic-macro . xs) (` (array (@ xs)))) ; (variadic-macro 1 2 3) -> (array 1 2 3)
```

You can use any scheme function within a macro, and macros can refer to and use other macros.

Additionally, the following special symbols are provided:

- `'` is a synonym for `quote`
- `&#96;` is a synomym for `quasiquote`
- `,` is a synomym for `unquote`
- `@` is a synomym for `unquote-splicing`

## Javascript

```scheme
(define (fibonacci x)
	(if (< x 1) 0
	(if (<= x 2) 1
	(+ (fibonacci (- x 1)) (fibonacci (- x 2))))))
```

## HTML

```scheme
(doctype)
(html lang "en-gb"
	(head
		(link rel "stylesheet" href "/style.css"))
	(body
		(header "Hello, world!")
		(script src "/script.js")))
```

## CSS

```scheme
(rule (body)
	margin auto
	padding (px 0))

(rule ((+ p p))
	margin-bottom (em 1))
```

## SQL

```scheme
(query
	(select (as "name" username) email password)
	(from users)
	(where (> joindate "2020-01-01")))
```
