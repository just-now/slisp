**A simple lisp interpreter**

Consists of the following components:
 - lex: parses parentheses-tree into `{List|Atom}`-tree:

```
(foo 1 (bar 2)) --> List([Atom("foo"),
                          Atom("1"),
                          List([Atom("bar"), Atom("2")])])
```

 - ast: converts `{List|Atom}`-tree into `{Seq|While|If|Var|...}`-tree.

```
List([List([Atom('let'), Atom('b'), Atom('0')]),
      List([Atom('let'), Atom('a'), Atom('10'),
      List([Atom('while'), Atom('a'),
                           List([List([Atom('let'), Atom('a'),
                                 List([Atom('-'), Atom('a'), Atom('1')])]),
                                 List([Atom('let'), Atom('b'),
                                 List([Atom('+'), Atom('b'), Atom('2')])])])])])])
|
V
Seq([Assign('b', Const(0)),
     Assign('a', Const(10)),
     While(Var('a'),
         Seq([Assign('a', Op('-', Var('a'), Const(1))),
              Assign('b', Op('+', Var('b'), Const(2)))]))])
```

 - ast: intrp() calculates variable values in the heap.
 - future features: strings, ffi.
 - uses python3.10 mostly to try patternmatching.
 - WARN: has bugs in favour of simplicity.