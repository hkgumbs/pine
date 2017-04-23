-module('List').

-compile(export_all).


foldr(F, A, List) ->
    lists:foldr(fun (X, Y) -> 'Utils':apply(F, X, Y) end, A, List).


cons(Head, Tail) ->
    [Head|Tail].
