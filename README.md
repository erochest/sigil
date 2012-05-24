
# Plan for Stack-Based Language for Genetic Programming

## Types

### Boolean

### Double

### Int

### Symbol

### List

### Vector

### Quotation

## Core Functions

### Stack Functions

* `dip`: This pops a quotation off the stack, temporarily removes the next
  item, executes the quotation against the stack, and pushes the "next item"
  back on the stack.  For example, `2 3 1 [ + ] dip` results in `5 1`.
* `dup`
* `pop`
* `swap`

These four are primitives. `if`, for example, can be created from them.

Other functions.

* `shove`
* `depth`
* `yank`
* `yankdup`
* `bi`: `1 [ 5 + ] [ 10 + ] bi` becomes `6 11`.

### Num Functions

How to handle coercion and dispatching is TBD.

* `%`
* `*`
* `/`
* `+`
* `-`
* `<`
* `<=`
* `>`
* `>=`
* `cos`
* `sin`
* `tan`
* `max`
* `min`

### Quotation Functions

* `[ ... ]`: Quotation.
* `call`: This pops the quotation off the top of the stack and executes it.
* `execute`: This pops a word off the top of the stack and executes the code it
  represents.
* `curry`: `5 [ + ] curry` becomes `[5 + ]`.
* `compose`: `[+] [*] compose` becomes `[+ *]`.

### Guard Functions

These are internal functions that introspect the stack without changing it and
only execute a block if the guards pass.

### List Functions

* `filter`
* `fold`, `foldr`
* `append`
* `cons`
* `uncons`
* `length`
* `car`
* `cdr`
* `member`
* `empty`
* `nth`

### Vector Functions

* `narray`: `1 2 3 3 narray` becomes `<1 2 3>`.
* `dim`

### Boolean Functions

* `and`
* `not`
* `or`

### Other Functions

* `==`
* `coerce` (Will need versions of this for every target type.)
* `rand` (Will need versions of this for every target type.)
* `define` (Defines a name to a quotation.)
* `.`: pop and log the top of the stack.

## Syntax

* `[ ... ]`: Quotation. This creates a function and pushes it onto the stack.
* `{ ... }`: List
* `#< ... >`: Vector

## Operations

All operations are of type:

    Stack a s -> Stack a s 

`a` is the type of the top of the stack, and `s` is the type of the rest of the
stack. `Stack` is a GADT.

Programs are a composition of those functions, which can then be compiled.

## Resources

* http://www.codecommit.com/blog/cat/the-joy-of-concatenative-languages-part-3
* http://alaska-kamtchatka.blogspot.com/2009/01/essence-of-concatenative-languages.html

