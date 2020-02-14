-module(test).
-compile(export_all).
-define(Timeout, 1000).

start(Module) ->
  Id = key:generate(),
  apply(Module, start, [Id]).

start(Module, P) ->
  Id = key:generate(),
  apply(Module, start, [Id,P]).

start(_, 0, _) ->
  ok;

start(Module, N, P) ->
  start(Module, P),
  start(Module, N-1, P).

add(Key, Value , P) ->
  Q = make_ref(),
  P ! {add, Key, Value, Q, self()},
  receive {Q, ok} ->
    ok
  after ?Timeout ->
    {error, "timeout"}
  end.

lookup(Key, Node) ->
  Q = make_ref(),
  Node ! {lookup, Key, Q, self()},
  receive
    {Q, Value} ->
      Value
  after ?Timeout ->
    {error, "timeout"}
  end.

keys(N,Node) ->
   Key = lists:map(fun(_) -> key:generate() end, lists:seq(1,N)),
   add(Key, Node).
add(Keys, P) ->
  lists:foreach(fun(K) ->
    add(K, gurka, P) end, Keys),
  check(Keys, P).
  check(Keys, P) ->
  T1 = now(),
  io:fwrite("~w incomingg!", [T1]),
  {Failed, Timeout} = check(Keys, P, 0, 0),
  T2 = now(),
  io:fwrite("~w outgoingg!", [T2]),
  Done = (timer:now_diff(T2, T1)div 1000),
  io:format("~w lookup operation in ~w ms ~n", [length(Keys), Done]),
  io:format("~w lookups failed, ~w caused a timeout ~n", [Failed, Timeout]).
check([], _, Failed, Timeout) ->
 io:fwrite("I'm out!"),
  {Failed, Timeout};
check([Key|Keys], P, Failed, Timeout) ->
  case lookup(Key,P) of{Key, _} ->
    io:fwrite("I'm In!!"),
    check(Keys, P, Failed, Timeout);
    {error, _} ->
    check(Keys, P, Failed, Timeout+1);
    false ->
    check(Keys, P, Failed+1, Timeout)
  end.