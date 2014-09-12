TaPL
====

Implementation of the Interpreters / Type Checkers in Benjamin Pierce's
*Types and Programming Languages*. We use Parsec, and applicative style
parsing whenever possible. Addtionally we first lex into a list of tokens,
and then parse the tokens. We try to find a middle ground between using the
full power of Parsec and rolling our own parsers.

Completed so far
----------------

    * Arith
