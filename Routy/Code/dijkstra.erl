-module(dijkstra).
-export([table/2, route/2]).

entry(Node, Sorted) ->
	case lists:keyfind(Node, 1, Sorted) of
		false -> 0;
		{_, Length, _} -> Length
	end.

replace(Node, N, Gateway, Sorted) ->
	io:format(" replace Node: ~w ~n", [Node]),
	io:format(" replace Sortede: ~w ~n", [Sorted]),
	io:format(" replace Gateway: ~w ~n", [Gateway]),
	io:format(" replace N: ~w ~n", [N]),
	AuxList = lists:keydelete(Node, 1, Sorted),
	lists:keysort(2, [{Node, N, Gateway} | AuxList]).

update(Node, N, Gateway, Sorted) -> 
	Length = entry(Node, Sorted),

	if
		N < Length ->
			replace(Node, N, Gateway, Sorted);
		true ->
			Sorted
	end.


iterate(Sorted, Map, Table) ->
	case Sorted of
		[] ->
			Table;
		[{_, inf, _} | _] ->
			Table;
		[Entry | Tail] ->
			{Node, Length, Gateway} = Entry,
			
			case lists:keyfind(Node, 1, Map) of
				{_, Reachables} ->
				    NewList = lists:foldl(
				    			fun(Element, Sorted) -> update(Element, Length + 1, Gateway, Sorted) end, 
							  Tail, Reachables);
				false ->
				    NewList = Tail
			end,

			iterate(NewList, Map, [{Node, Gateway} | Table])
	end.

table(Gateways, Map) ->
	Nodes = map:all_nodes(Map),
	MockedNodes = lists:map(fun(Node) ->
								case lists:member(Node, Gateways) of
								    true ->
										io:format(" table true: ~w ~n", [Node]),
										{Node, 0, Node};
								    false ->
										io:format(" table false: ~w ~n", [Node]),
										{Node, inf, unknown}
								end
							end, Nodes),

	InitialSortedList = lists:keysort(2, MockedNodes),
	iterate(InitialSortedList, Map, []).


route(Node, Table) ->
	case lists:keyfind(Node, 1, Table) of
		{Dest, Gateway} ->
	    	{ok, Gateway};
		false ->
	    	notfound
    end.