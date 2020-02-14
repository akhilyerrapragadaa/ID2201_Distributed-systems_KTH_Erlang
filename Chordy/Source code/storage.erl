-module(storage).
-compile(export_all).

create()->
  [].

add(Key,Value,Store)->
  [{Key,Value}|lists:keydelete(Key, 1, Store)].

lookup(Key,Store)->
  lists:keyfind(Key, 1, Store).

split(Key,Store)->
  lists:partition(fun(A)->{B,_}=A , B>Key end, Store).

merge(Store1,Store2)->
  lists:append(Store1, Store2).