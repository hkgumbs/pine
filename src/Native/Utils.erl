-module('Utils').

-compile(export_all).


toString(_) ->
    <<"Utils.toString">>.


append(X, Y) when is_list(X) -> X ++ Y;
append(X, Y) -> <<X/binary, Y/binary>>.
