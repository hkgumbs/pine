-module('Utils').

-compile(export_all).


toString(_) ->
    <<"Utils.toString">>.


append(X, Y) when is_list(X) -> X ++ Y;
append(X, Y) -> <<X/binary, Y/binary>>.



%% CODEGEN


apply(F, A) when is_function(F, 1) -> F(A);
apply(F, A) -> function(erlang:fun_info(F, arity), F, [A]).

apply(F, A1, A2) when is_function(F, 2) -> F(A1, A2);
apply(F, A1, A2) -> function(erlang:fun_info(F, arity), F, [A1, A2]).

apply(F, A1, A2, A3) when is_function(F, 3) -> F(A1, A2, A3);
apply(F, A1, A2, A3) -> function(erlang:fun_info(F, arity), F, [A1, A2, A3]).

apply(F, A1, A2, A3, A4) when is_function(F, 4) -> F(A1, A2, A3, A4);
apply(F, A1, A2, A3, A4) -> function(erlang:fun_info(F, arity), F, [A1, A2, A3, A4]).

apply(F, A1, A2, A3, A4, A5) when is_function(F, 5) -> F(A1, A2, A3, A4, A5);
apply(F, A1, A2, A3, A4, A5) -> function(erlang:fun_info(F, arity), F, [A1, A2, A3, A4, A5]).

apply(F, A1, A2, A3, A4, A5, A6) when is_function(F, 6) -> F(A1, A2, A3, A4, A5, A6);
apply(F, A1, A2, A3, A4, A5, A6) -> function(erlang:fun_info(F, arity), F, [A1, A2, A3, A4, A5, A6]).

apply(F, A1, A2, A3, A4, A5, A6, A7) when is_function(F, 7) -> F(A1, A2, A3, A4, A5, A6, A7);
apply(F, A1, A2, A3, A4, A5, A6, A7) -> function(erlang:fun_info(F, arity), F, [A1, A2, A3, A4, A5, A6, A7]).

apply(F, A1, A2, A3, A4, A5, A6, A7, A8) when is_function(F, 8) -> F(A1, A2, A3, A4, A5, A6, A7, A8);
apply(F, A1, A2, A3, A4, A5, A6, A7, A8) -> function(erlang:fun_info(F, arity), F, [A1, A2, A3, A4, A5, A6, A7, A8]).

apply(F, A1, A2, A3, A4, A5, A6, A7, A8, A9) when is_function(F, 9) -> F(A1, A2, A3, A4, A5, A6, A7, A8, A9);
apply(F, A1, A2, A3, A4, A5, A6, A7, A8, A9) -> function(erlang:fun_info(F, arity), F, [A1, A2, A3, A4, A5, A6, A7, A8, A9]).


function({ arity, N }, F, Args) ->
    case N - length(Args) of
        1 ->
            fun (A1) ->
                    erlang:apply(F, Args ++ [A1])
            end;
        2 ->
            fun (A1, A2) ->
                    erlang:apply(F, Args ++ [A1, A2])
            end;
        3 ->
            fun (A1, A2, A3) ->
                    erlang:apply(F, Args ++ [A1, A2, A3])
            end;
        4 ->
            fun (A1, A2, A3, A4) ->
                    erlang:apply(F, Args ++ [A1, A2, A3, A4])
            end;
        5 ->
            fun (A1, A2, A3, A4, A5) ->
                    erlang:apply(F, Args ++ [A1, A2, A3, A4, A5])
            end;
        6 ->
            fun (A1, A2, A3, A4, A5, A6) ->
                    erlang:apply(F, Args ++ [A1, A2, A3, A4, A5, A6])
            end;
        7 ->
            fun (A1, A2, A3, A4, A5, A6, A7) ->
                    erlang:apply(F, Args ++ [A1, A2, A3, A4, A5, A6, A7])
            end;
        8 ->
            fun (A1, A2, A3, A4, A5, A6, A7, A8) ->
                    erlang:apply(F, Args ++ [A1, A2, A3, A4, A5, A6, A7, A8])
            end;
        9 ->
            fun (A1, A2, A3, A4, A5, A6, A7, A8, A9) ->
                    erlang:apply(F, Args ++ [A1, A2, A3, A4, A5, A6, A7, A8, A9])
            end
    end.
