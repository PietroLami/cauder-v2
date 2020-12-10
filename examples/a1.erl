-module(a1).
-export([main/0]).

main() ->
  register(pippo, self()),
  register(pippo, self()).
