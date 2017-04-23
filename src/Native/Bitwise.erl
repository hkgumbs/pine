-module('Bitwise').

-compile(export_all).


shiftRightZfBy(Offset, Val) -> Val bsr Offset.


'or'(X, Y) -> X bor Y.
'xor'(X, Y) -> X bxor Y.
'and'(X, Y) -> X band Y.
