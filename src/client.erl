%%%-------------------------------------------------------------------
%%% @author Victor
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. 九月 2016 14:15
%%%-------------------------------------------------------------------
-module(client).
-author("Victor").

%% API
-export([start/0,start/2,scan_input/0]).

start() ->
  register(client_socket,spawn(?MODULE, start, ["localhost", 2345])).

start(Ip, Port) ->
  {ok, Socket} = gen_tcp:connect(Ip, Port, [binary, {packet,2}]),
  register(scanner, spawn(?MODULE, scan_input, [])),
  loop(Socket).

loop(Socket) ->
  receive
    {message, Message} ->
      ok = gen_tcp:send(Socket, term_to_binary(re:replace(Message,"^\\s*|\\s*$", "",[{return,list},global]))),
      loop(Socket);

    {tcp,Socket,Bin} ->
      Val = binary_to_term(Bin),
      case Val of
        "quit" ->
          gen_tcp:close(Socket),
          scanner ! stop;
        _Any ->
          io:format("Client Result = ~p~n", [Val]),
          scanner ! continue,
          loop(Socket)
      end
  end.



scan_input() ->
  receive
    stop ->
      void;
    continue ->
      Message = io:get_line(""),
      client_socket ! {message, Message},
      scan_input()
  end.





