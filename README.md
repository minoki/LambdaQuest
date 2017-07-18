# LambdaQuest

This is an implementation of System F and System Fsub with
- primitive types: `Int`, `Real`, `Bool`, `Unit`
- primivite values: integer literals, floating point literals, `True`, `False`, `unit`
- built-in functions:
```
negateInt : Int -> Int
negateReal : Real -> Real
intToReal : Int -> Real
addInt, subInt, mulInt : Int -> Int -> Int
ltInt, leInt, equalInt : Int -> Int -> Bool
addReal, subReal, mulReal, divReal : Real -> Real -> Real
ltReal, leReal, equalReal : Real -> Real -> Bool
```
- non-trivial subtyping relation of primitives: `Int <: Real` [Fsub only]

## Example
- [readline](https://hackage.haskell.org/package/readline) is required to build REPL program.

Play with System F:
```
$ stack build
$ stack exec SystemF-repl
This is System F REPL.
Press Ctrl-D to exit.
> \x: Int. x
Type is Int -> Int.
Evaluation:
\x:Int. x
--> \x:Int. x.
> (\x: Int. x) 123
Type is Int.
Evaluation:
(\x: Int. x) 123
--> 123.
> (?a. (\x: a. x)) [Int] 123
Type is Int.
Evaluation:
(?a. \x:a. x) [Int] 123
--> (\x:Int. x) 123
--> 123.
> ^D
Bye!
```

Play with System Fsub:
```
$ stack exec SystemFsub-repl
This is System F REPL.
Press Ctrl-D to exit.
> (\x: Int. addReal x 1.0) 3
Type is Real.
Evaluation:
(\x:Int. addReal x 1.0) 3
--> addReal 3 1.0
--> 4.0.
> (?a<:Real. \x:a. addReal x x) [Int] 5
Type is Real.
Evaluation:
(?a<:Real. \x:a. addReal x x) [Int] 5
--> (\x:Int. addReal x x) 5
--> addReal 5 5
--> 10.0.
> (\f:Int -> Real. f 4) negateInt
Type is Real.
Evaluation:
(\f:Int -> Real. f 4) negateInt
--> negateInt 4
--> -4.
> ^D
Bye!
```

Church numerals:
```
> type CNat = forall a. (a -> a) -> a -> a
CNat := forall a. (a -> a) -> a -> a.
> let cnat2int = \n:CNat. n [Int] (\x:Int. addInt x 1) 0
cnat2int : (forall a. (a -> a) -> a -> a) -> Int.
Evaluation:
\n:CNat. n [Int] (\x:Int. addInt x 1) 0
--> \n:CNat. n [Int] (\x:Int. addInt x 1) 0.
> let csucc = \n:CNat. ?a. \s:a->a. \z:a. s (n [a] s z)
csucc : (forall a. (a -> a) -> a -> a) -> (forall a. (a -> a) -> a -> a).
Evaluation:
\n:CNat. ?a. \s:a -> a. \z:a. s (n [a] s z)
--> \n:CNat. ?a. \s:a -> a. \z:a. s (n [a] s z).
> let cplus = \n:CNat. \m:CNat. ?a. \s:a->a. \z:a. m [a] s (n [a] s z)
cplus : (forall a. (a -> a) -> a -> a) -> (forall a. (a -> a) -> a -> a) -> (forall a. (a -> a) -> a -> a).
Evaluation:
\n:CNat. \m:CNat. ?a. \s:a -> a. \z:a. m [a] s (n [a] s z)
--> \n:CNat. \m:CNat. ?a. \s:a -> a. \z:a. m [a] s (n [a] s z).
> let c0 = ?a. \s:a->a. \z:a. z
c0 : forall a. (a -> a) -> a -> a.
Evaluation:
?a. \s:a -> a. \z:a. z
--> ?a. \s:a -> a. \z:a. z.
> let c1 = ?a. \s:a->a. \z:a. s z
c1 : forall a. (a -> a) -> a -> a.
Evaluation:
?a. \s:a -> a. \z:a. s z
--> ?a. \s:a -> a. \z:a. s z.
> cnat2int (csucc c0)
Type is Int.
Evaluation:
cnat2int (csucc c0)
...
--> 1.
> cnat2int (cplus c1 c1)
Type is Int.
Evaluation:
cnat2int (cplus c1 c1)
...
--> 2.
```

## Syntax
```
type       ::= 'forall' <type variable> '.' type                 -- universal quantified (forall) type
             | 'forall' <type variable> '<:' arrowType '.' type  -- bounded universal quantified type [Fsub only]
             | arrowType
arrowType  ::= simpleType '->' arrowType                         -- function type
             | simpleType
simpleType ::= 'Int' | 'Real' | 'Bool' | 'Unit'
             | 'Top'                                             -- the universal supertype [Fsub only]
             | <type variable>
             | '(' type ')'

term       ::= '\' <variable> ':' arrowType '.' term        -- lambda abstraction (function)
             | '?' <type variable> '.' term                 -- type abstraction
             | '?' <type variable> '<:' arrowType '.' term  -- bounded type abstraction [Fsub only]
             | 'if' term 'then' term 'else' term            -- conditional
             | appTerm
appTerm    ::= appTerm simpleTerm       -- function application
             | appTerm '[' type ']'     -- type application
             | appTerm 'as' arrowType   -- type coercion [Fsub only]
             | simpleTerm
simpleTerm ::= <integer literal>
             | <floating point literal>
             | 'True' | 'False' | 'unit'
             | <variable>
             | <built-in function>
             | '(' term ')'
```

