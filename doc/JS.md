# Javascript

## Literals

Numbers:

- `1`
- `2`
- `43.151`

Strings:

- `'Hello'`
- `"Hello"`
- `'Hello` (turns into `"Hello"`)

Arrays:

`(array 1 2 'hello)`

Objects:

`(object 'name "Bob McBob" 'age 43)`

Maps:

`(Map "Bob" (object 'age 43) "Alice" (object 'age 44))`

Sets:

`(Set 1 2 3 "Bob McBob" 'Alice)`

## Operators

There are no infix operators. Everything is prefix. For example, addition:

`(+ 1 2 3) ; 1+2+3`

- `+`
- `-`
- `&&`
- `||`
- `**`
- `>`
- `<`
- `<=`
- `>=`
- `=` (strict equality with `===`)
- `!=` (strict inequality with `!==`)
- `like` (coersive equality with `==`)
- `unlike` (coersive inequality with `!=`)
- `set` (assignment)

## Fetching properties

Dot-chaining:

`(. document body) ; document.body`

Can be used for method chaining as well

`(. document body (querySelector '#app) (querySelectorAll 'p))
; document.body.querySelector("#app").querySelectorAll("p")`

Array syntax is also provided

`(get document "body" "chidren" 0)
; document["body"]["children"][0]`

## Keywords

All keywords are available with function-like syntax. For example:

`(new (Date))
; new Date()`

- `return`
- `yield`
- `async`
- `await`
- `new`
- `throw`
- `export`
- `default-export`

## Declarations

Variable definition:

`(define variable-name 1)`

Function definition:

`(define (add a b) (+ a b))`

Async function:

`(async (define (test x) (. x (then (lambda (x) (+ 1 x))))))`

Generator function is any function with a yield.

Names can have characters illegal in Javascript, like hyphens. They are replaced by legal characters on compilation. For functions, explicit return isn't necessary; the last statement or expression is automatically returned.

Anonymous function:

`(lambda (a b c) (* (+ a b) c))`

Const variables:

`(const variable-name 1)`

Let variables:

`(let variable-name 1)`

IIFE:

`(progn (. console (log 123)) "Hello, world!")
; (function() { console.log(123) ; return "Hello, world!" })()`

Static imports:

- for side-effects: `(from "module.js")`
- import default export: `(from "module.js" MyModule)`
- import all exports: `(from "module.js" * MyModule)`
- import some exports: `(from "module.js" (one two three))`
- renaming exports: `(from "module.js" (one (two four) three))`

Dynamic imports:

`(import "module.js")`
