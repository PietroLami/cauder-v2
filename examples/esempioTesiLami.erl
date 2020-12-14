-module(esempioTesiLami).
-export([main/0]).


main() ->
  register(parent, self()),
  Child = spawn(?MODULE, child, []),
  register(child, Child),
  child ! {hello},
  receive
    hello2 ->
      unregister(parent),
      ok
  end.

child() ->
  receive
    {hello} ->
      parent ! hello2
  end,
  unregister(child),
  main().
