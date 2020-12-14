-module(esempioTesiLami2).
-export([main/0]).

main() ->
  S = spawn(?MODULE, server, []),
  register(s,S),
  spawn(?MODULE, client, []),
  client().

server() ->
  receive
    {P, _} ->
      P ! ack,
      server()
  end.

client() ->
  P = whereis(s),
  P ! {self(), req},
  receive
    ack -> ok
  end.
