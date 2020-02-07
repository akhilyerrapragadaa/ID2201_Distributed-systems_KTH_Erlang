-module(hist).
-export([new/1, update/3]).

new(Name) ->
	Dict = dict:new(),
	dict:append(Name, inf, Dict). 


update(Node, N, History) ->
	case dict:find(Node, History) of
		{ok, [Value | _ ]} ->
			if
				N > Value ->
					AuxDict = dict:erase(Node, History),
					Updated = dict:append(Node, N, AuxDict),
					{new, Updated};
				true ->
					old
			end;
		error ->
			{new, dict:append(Node, N, History)}
	end.