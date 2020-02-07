-module(map).
-export([new/0, update/3, reachable/2, all_nodes/1]).


new() -> 
	[].

update(Node, Links, Map) ->
	AuxMap = lists:keydelete(Node, 1, Map),
	[{Node, Links} | AuxMap].

reachable(Node, Map) ->
	case lists:keyfind(Node, 1, Map) of
		false -> 
			[];
		{_, Links} ->
			Links
		end.

all_nodes(Map) ->
	AuxMap = lists:flatmap(fun({Node, Links}) -> [Node | Links] end, Map),
	lists:usort(AuxMap).