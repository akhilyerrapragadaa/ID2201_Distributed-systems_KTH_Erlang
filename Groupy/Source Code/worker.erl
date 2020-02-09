-module(worker).
-export([start/4, start/5]).
-define(change, 20).
-define(color, {0,0,0}).

start(Id, Module, Rnd, Sleep) ->
	spawn(fun() -> init(Id, Module, Rnd, Sleep) end).
init(Id,  Module, Rnd, Sleep) ->
	Test1 = apply(Module, start, [Id]),
	Color = ?color,
	init_cont(Id, Rnd, Test1, Color, Sleep).

start(Id, Module, Rnd, Peer, Sleep) ->
	spawn(fun() -> init(Id, Module, Rnd, Peer, Sleep) end).

init(Id, Module, Rnd, Peer, Sleep) ->
	Test2 = apply(Module, start, [Id, Peer]),
	{ok, Color} = join(Id, Test2),
	init_cont(Id, Rnd, Test2, Color, Sleep).

join(Id, Test) ->
	receive
		{view, _} ->
		Ref = make_ref(),
		Test ! {mcast, {state_request, Ref}},
		state(Id, Ref);
		{error, Reason} ->
		{error, Reason}
	end.

state(Id, Ref) ->
	receive
		{state_request, Ref} ->
		receive
			{state, Ref, Color} ->
			{ok, Color}
		end;
		_Ignore ->
			state(Id, Ref)
	end.

init_cont(Id, Rnd, Test, Color, Sleep) ->
	random:seed(Rnd, Rnd, Rnd),
	Title = "Worker: " ++ integer_to_list(Id),
	Gui = gui:start(Title, self()),
	Gui ! {color, Color},
	worker(Id, Test, Color, Gui, Sleep),
	Test ! stop,
	Gui ! stop.

worker(Id, Test, Color, Gui, Sleep) ->
	Wait = wait(Sleep),
	receive
		{change, N} ->
%% io:format("worker ~w change ~w~n", [Id, N]),
			Color2 = change_color(N, Color),
			Gui ! {color, Color2},
			worker(Id, Test, Color2, Gui, Sleep);

		{state_request, Ref} ->
			Test ! {mcast, {state, Ref, Color}},
			worker(Id, Test, Color, Gui, Sleep);

		{state, _, _} ->
			worker(Id, Test, Color, Gui, Sleep);
		{join, Peer, Gms} ->    Test ! {join, Peer, Gms},
			worker(Id, Test, Color, Gui, Sleep);
		{view, _} ->
			worker(Id, Test, Color, Gui, Sleep);
		freeze ->    frozen(Id, Test, Color, Gui, Sleep);
		{sleep, Slp} ->
			worker(Id, Test, Color, Gui, Slp);
		stop ->    ok;

		{send, Msg} ->    Test !  {mcast, Msg},
			worker(Id, Test, Color, Gui, Sleep);
		Error ->
			io:format("strange message: ~w~n", [Error]),
			worker(Id, Test, Color, Gui, Sleep)
	after Wait ->
%% Ok, let's propose a change of colors    %% io:format("worker ~w mcast message~n", [Id]),
		Test !  {mcast, {change, random:uniform(?change)}},
		worker(Id, Test, Color, Gui, Sleep)
	end.

frozen(Id, Test, Color, Gui, Sleep) ->
	receive go ->    worker(Id, Test, Color, Gui, Sleep);
		stop ->    ok;
%% Someone from above wants us to multicast a message.
		{send, Msg} ->
			Test !  {mcast, Msg},
			frozen(Id, Test, Color, Gui, Sleep)
	end.


wait(Sleep) ->
	if
		Sleep == 0 ->
			0;
		true ->
			random:uniform(Sleep)
	end.

change_color(N, {R,G,B}) ->
	{G, B, ((R+N) rem 256)}.