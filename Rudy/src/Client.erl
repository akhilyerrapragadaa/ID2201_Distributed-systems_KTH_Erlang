%%%-------------------------------------------------------------------
%%% @author Akhil
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Feb 2020 01:07
%%%-------------------------------------------------------------------
-module('Client').
-author("Akhil").

-export([bench/2]).

bench(Host, Port) ->
  Start = erlang:system_time(micro_seconds),
  run(150, Host, Port),
  Finish = erlang:system_time(micro_seconds),
  Finish - Start.
run(N, Host, Port) ->
  if N == 0 -> ok;
    true ->
      request(Host, Port),
      run(N-1, Host, Port)
  end.

request(Host, Port) ->
  Opt = [list, {active, false}, {reuseaddr, true}],
  {ok, Server} = gen_tcp:connect(Host, Port, Opt),
  {ok, Server1} = gen_tcp:connect(Host, Port, Opt),
  gen_tcp:send(Server, 'HTTPParser':get("pingpong")),
  Recv = gen_tcp:recv(Server, 0),
  io:fwrite("RECV is:~w~n", [Recv]),
  gen_tcp:send(Server1, 'HTTPParser':get("dingpong")),

  Recv1 = gen_tcp:recv(Server1, 0),

  io:fwrite("RECV1 is:~w~n", [Recv1]),

  case Recv of {ok, _} -> ok;
    {error, Error} -> io:format("test: error: ~w~n", [Error]) end,
  gen_tcp:close(Server).
