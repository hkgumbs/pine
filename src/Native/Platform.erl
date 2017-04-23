-module('Platform').

-compile(export_all).


batch(List) ->
    { 'node', List }.


program(_Impl) ->
    {}.
