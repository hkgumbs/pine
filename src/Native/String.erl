-module('String').

-compile(export_all).


join(_, []) -> <<"">>;
join(Sep, [First|Rest]) ->
    F = fun (Acc, Next) ->
                <<Acc/binary, Sep/binary, Next/binary>>
        end,
    lists:foldl(F, First, Rest).

isEmpty(<<"">>) -> true;
isEmpty(_) -> false.
