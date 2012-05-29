
# Plan for Stack-Based Language for Genetic Programming

## Types

### GpWord

`GpWord` is the basic value type for Sigil.

```haskell
newtype Symbol = Text

data GpWord x where
    B :: Bool -> GpWord (s -> (Stack Bool s))
    I :: Int -> GpWord (s -> (Stack Int s))
    D :: Double -> GpWord (s -> (Stack Double s))
    S :: Symbol -> GpWord (s -> s')
    L :: [GpWord y] -> GpWord (s -> (Stack [GpWord y] s))
    BV :: (Vector Bool) -> GpWord (s -> (Stack (Vector Bool) s))
    IV :: (Vector Int) -> GpWord (s -> (Stack (Vector Int) s))
    DV :: (Vector Double) -> GpWord (s -> (Stack (Vector Double) s))
    Q :: [GpWord y] -> GpWord (s -> (Stack [GpWord y] s))
  implements (Eq, Ord)

instance Show GpWord where
    ...
class ToWord a where
    toGpWord :: a -> GpWord
class FromWord a where
    fromGpWord :: GpWord -> Maybe a
```

### Stack

The `Stack v s` type is the basic row type for everything.

```haskell
data Stack v s where
    Empty :: 
```

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
* `bi`: `1 [ 5 + ] [ 10 + ] bi` becomes `6 11`. This could be defined `[rot dup
  rot call rot rot swap call swap]`. That's a mouthful.

A a (A a -> A e) (A a -> A d)
  rot  -> A (A a -> A e) (A a -> A d) a
  dup  -> A (A a -> A e) (A a -> A d) a a
  rot  -> A (A a -> A e) a a (A a -> A d)
  call -> A (A a -> A e) a d
  rot  -> A a d (A a -> A e)
  rot  -> A d (A a -> A e) a
  swap -> A d a (A a -> A e)
  call -> A d e
  swap -> A e d

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
* http://lambda-the-ultimate.org/node/1899#comment-23169

