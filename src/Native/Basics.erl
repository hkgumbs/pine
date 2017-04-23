-module('Basics').

-compile(export_all).


add(X, Y) -> X + Y.
sub(X, Y) -> X - Y.
mul(X, Y) -> X * Y.
'div'(X, Y) -> X div Y.


lt(X, Y) -> X < Y.
gt(X, Y) -> X > Y.
le(X, Y) -> X =< Y.
ge(X, Y) -> X >= Y.


eq(X, Y) -> X =:= Y.
