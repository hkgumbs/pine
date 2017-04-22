-module('Platform').

-compile(export_all).


batch(List) ->
    { 'node', List }.


program(_Impl) ->
	fun (_FlagDecoder) ->
		fun (_Object, _ModuleName) ->
                {}
        end
    end.
