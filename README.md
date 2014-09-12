TaPL
====

Implementation of the Interpreters / Type Checkers in Benjamin Pierce's
*Types and Programming Languages*. We use Parsec, and applicative style
parsing whenever possible. Addtionally we first lex into a list of tokens,
and then parse the tokens. We try to find a middle ground between using the
full power of Parsec and rolling our own parsers. The evaluators try to
stay as close to the OCaml implementations as possible.

Completed so far
----------------

* Arith
