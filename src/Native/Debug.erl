-module('Debug').

-compile(export_all).


log(Msg, Value) ->
    io:fwrite("~s\n~p", [Msg, Value]),
    Value.
