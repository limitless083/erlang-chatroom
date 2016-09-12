%%%-------------------------------------------------------------------
%%% @author Victor
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. 九月 2016 10:22
%%%-------------------------------------------------------------------
-module(parallel_server).
-author("Victor").

%% API
-export([start/1,reset_tables/0,query_message_item/1]).

-record(message, {time,from_name,to_name,msg}).
-include_lib("stdlib/include/qlc.hrl").

control(CurrentSocketsMap) ->
  receive
    {add, FromNode, From, UserName, Socket} ->
      io:format("controller receive ~p~n", [{add, FromNode, From, UserName, Socket}]),
      io:format("length add: ~p~n", [CurrentSocketsMap]),
      NameSocketMap = lists:filter(
        fun({_Node, Name, _ToSocket}) ->
          Name =:= UserName
        end, CurrentSocketsMap
      ),
      io:format("NameSocketMap : ~p~n", [NameSocketMap]),
      if
        length(NameSocketMap) > 0 ->
          if
            FromNode =:= node() -> From ! {add, failed};
            true -> void
          end,
          control(CurrentSocketsMap);
        true ->
          if
            FromNode =:= node() -> From ! {add, ok};
            true -> void
          end,
          control([{FromNode, UserName, Socket} | CurrentSocketsMap])
      end;
    {delete, FromNode, UserName, Socket} ->
      io:format("length del: ~p~n", [CurrentSocketsMap]),
      control(CurrentSocketsMap -- [{FromNode, UserName, Socket}]);
    {broadcast, _FromNode, ExceptNames, Message} ->
      io:format("controller receive ~p~n", [{broadcast, _FromNode, ExceptNames, Message}]),
      lists:foreach(
        fun({ToNode, _Name, Socket}) ->
          if
            ToNode =:= node() -> gen_tcp:send(Socket, term_to_binary(Message));
            true -> void
          end
        end, double_filter(ExceptNames, CurrentSocketsMap)
      ),
      control(CurrentSocketsMap);
    {send_to_self, FromNode, Socket, Message} ->
      io:format("controller receive ~p~n", [{send_to_self, FromNode, Socket, Message}]),
      gen_tcp:send(Socket, term_to_binary(Message)),
      control(CurrentSocketsMap);
    {send_to_other, From, Socket, ToName, Message} ->
      io:format("controller receive ~p~n", [{send_to_other, From, Socket, ToName, Message}]),
      NameSocketMap = lists:filter(
        fun({_Node, UserName, _ToSocket}) ->
          UserName =:= ToName
        end, CurrentSocketsMap
      ),
      if
        length(NameSocketMap) > 0 ->
          [{ToNode, _UserName, ToSocket}] = NameSocketMap,
          if
            ToSocket =/= Socket ->
              From ! {send_to_other, ok},
              {controller, ToNode} ! {send_to_self, ToNode, ToSocket, Message };
            true ->
              From ! {send_to_other, failed},
              gen_tcp:send(Socket, term_to_binary("you can't talk to yourself!"))
          end;
        true ->
          From ! {send_to_other, other},
          gen_tcp:send(Socket, term_to_binary(ToName ++ " not on line!"))
      end,
      control(CurrentSocketsMap);
    {who, _FromNode, Socket} ->
      lists:foreach(
        fun({_Node, Name, _Socket}) ->
          gen_tcp:send(Socket, term_to_binary(Name))
        end, CurrentSocketsMap
      ),
      gen_tcp:send(Socket, term_to_binary("Total online user: " ++ integer_to_list(length(CurrentSocketsMap)))),
      control(CurrentSocketsMap);
    _Any ->
      io:format("controller receive balala=~p~n", [_Any]),
      control(CurrentSocketsMap)
  end.

%% server
start(Port) ->
  %% 连接所有节点
  net_adm:world(),
  register(controller, spawn(fun() -> control([]) end)),
  {ok, Listen} = gen_tcp:listen(Port, [binary, {packet, 2},
    {reuseaddr, true},
    {active, true}]),
  do_this_once([node()|nodes()]),
  mnesia:start(),
  seq_loop(Listen).

seq_loop(Listen) ->
  {ok, Socket} = gen_tcp:accept(Listen),
  spawn(fun() -> seq_loop(Listen) end),
  Nodes = [node()|nodes()],
  send_message_to_other_server([node()],{send_to_self, node(), Socket, "Please login!\n"}),
  loop(Nodes, Socket, "", false).

loop(Nodes, Socket, FromName, IsLogined) ->
  receive
    {tcp, Socket, Bin} ->
      Message = binary_to_term(Bin),
      io:format("controller received = ~p~n", [Message]),
      [Command | Tail] = re:split(Message, "\\s+", [{return, list}]),
      case string:span(Command, "/") of
        0 ->
          %% broadcast message
          if
            IsLogined ->
              send_message_to_other_server([node()], {send_to_self, node(), Socket, "you say:" ++ Message}),
              send_message_to_other_server(Nodes, {broadcast, node(), [FromName],  FromName ++ " says " ++ Message});

            true ->
              send_message_to_other_server([node()], {send_to_self, node(), Socket, "Invalid command"})
          end,
          loop(Nodes, Socket, FromName, IsLogined);
        1 ->
          %% execute command
          case re:replace(Command, "/+", "", [{return,list}]) of
            "login" ->
              if
                length(Tail) > 0 ->
                  [UserName | _] = Tail;
                true ->
                  UserName = "bianguangkuo"
              end,
              if
                IsLogined ->
                  send_message_to_other_server([node()],{send_to_self, node(), Socket, "Name exist, please choose anthoer name.."}),
                  loop(Nodes, Socket, FromName, IsLogined);
                true ->
                  send_message_to_other_server(Nodes,{add, node(), self(), UserName, Socket}),
                  receive
                    {add, ok} ->
                      send_message_to_other_server([node()],{send_to_self, node(), Socket, "Login sucess!"}),
                      send_message_to_other_server(Nodes,{broadcast, node(), [UserName], UserName ++ " has logined!"}),
                      loop(Nodes, Socket, UserName, true);
                    {add, failed} ->
                      send_message_to_other_server([node()],{send_to_self, node(), Socket, "Name exist, please choose anthoer name.."}),
                      loop(Nodes, Socket, FromName, IsLogined)
                  end
              end;
            "quit" ->
              if
                IsLogined ->
                  send_message_to_other_server([node()],{send_to_self, node(), Socket, "quit"}),
                  send_message_to_other_server(Nodes,{delete, node(), FromName, Socket}),
                  send_message_to_other_server(Nodes,{broadcast, node(), [FromName], FromName ++ " has quit!"});
                true ->
                  send_message_to_other_server([node()],{send_to_self, node(), Socket, "quit"})
              end;
            "to" ->
              if
                IsLogined ->
                  if
                    length(Tail) > 1 ->
                      [_, ToName, ToMessage] = re:split(Message, "\\s+", [{return, list},{parts, 3}]),

                      send_message_to_other_server([node()],{send_to_other, self(), Socket, ToName, FromName ++ " says to you:" ++ ToMessage}),
                      receive
                        {send_to_other, ok} ->
                          send_message_to_other_server([node()],{send_to_self, node(), Socket, "you say to " ++ ToName ++ ":" ++ ToMessage}),
                          add_message_item(calendar:now_to_local_time(erlang:timestamp()), FromName, ToName, ToMessage);
                        {send_to_other, failed} ->
                          void;
                        {send_to_other, other} ->
                          void
                      end;
                    true ->
                      send_message_to_other_server([node()],{send_to_self, node(), Socket, "Invalid command"})
                  end;
                true ->
                  send_message_to_other_server([node()],{send_to_self, node(), Socket, "Invalid command"})
              end,
              loop(Nodes, Socket, FromName, IsLogined);
            "who" ->
              if
                IsLogined ->
                  send_message_to_other_server([node()],{who, node(), Socket});
                true ->
                  send_message_to_other_server([node()],{send_to_self, node(), Socket, "Invalid command"})
              end,
              loop(Nodes, Socket, FromName, IsLogined);
            "history" ->
              MessageList = query_message_item(all),
              lists:foreach(
                fun({message,_Time, Name, ToName, Msg}) ->
                  if
                    Name =:= FromName ->
                      send_message_to_other_server([node()],{send_to_self, node(), Socket, "you say to " ++ ToName ++ ": " ++ Msg});
                    ToName =:= FromName ->
                      send_message_to_other_server([node()],{send_to_self, node(), Socket,  Name ++ " says to you : " ++ Msg});
                    ToName =:= "all" ->
                      send_message_to_other_server([node()],{send_to_self, node(), Socket,  Name ++ " says to all : " ++ Msg});
                    true -> void
                  end
                end,
                MessageList)
          end;
        2 ->
          %% defined message
          if
            IsLogined ->
              if
                length(Tail) > 0 ->
                  [ToName | _] = Tail,
                  DefinedMessage = defined_message({private, Command}),
                  send_message_to_other_server([node()],{send_to_self, node(), Socket, "you say to " ++ ToName ++ ":" ++ DefinedMessage}),
                  send_message_to_other_server([node()],{send_to_other, self(), Socket, ToName, FromName ++ " says to you:" ++ DefinedMessage}),
                  send_message_to_other_server(Nodes,{broadcast, node(), [FromName,ToName], FromName ++ " says to " ++ ToName ++ ":" ++ DefinedMessage}),
                  add_message_item(calendar:now_to_local_time(erlang:timestamp()), FromName, ToName, DefinedMessage);
                true ->
                  DefinedMessage = defined_message({public, Command}),
                  send_message_to_other_server([node()],{send_to_self, node(), Socket, "you say to all:" ++ DefinedMessage}),
                  send_message_to_other_server(Nodes,{broadcast, node(), [FromName], FromName ++ " says to all:" ++ DefinedMessage}),
                  add_message_item(calendar:now_to_local_time(erlang:timestamp()), FromName, "all", DefinedMessage)
              end;
            true ->
              send_message_to_other_server([node()],{send_to_self, node(), Socket, "Invalid command"})
          end,
          loop(Nodes, Socket, FromName, IsLogined);

        _Any ->
          send_message_to_other_server([node()],{send_to_self, node(), Socket, "Invalid command"}),
          loop(Nodes, Socket, FromName, IsLogined)
      end;
    {tcp_closed, Socket} ->
      io:format("Server closed ~n"),
      if
        IsLogined ->
          send_message_to_other_server([node()],{send_to_self, node(), Socket, "quit"}),
          send_message_to_other_server(Nodes,{delete, node(), FromName, Socket}),
          send_message_to_other_server(Nodes,{broadcast, node(), [FromName], FromName ++ " has quit!"});
        true ->
          send_message_to_other_server([node()],{send_to_self, node(), Socket, "quit"})
      end
  end.

%% 预设信息
defined_message({private, Command}) ->
  case re:replace(Command, "/+", "", [{return,list}]) of
    "hi" ->
      "Hi,nice to meet you!";
    "smile" ->
      "Hello, I am happy."
  end;
defined_message({public, Command}) ->
  case re:replace(Command, "/+", "", [{return,list}]) of
    "hi" ->
      "Hi,erveryone, nice to meet you!";
    "smile" ->
      "Hi,everyone, I am happy."
  end.

%% 除去Map当中以List中元素为标签的元素
double_filter(List, Map) ->
  lists:filter(
    fun(L1) ->
      {_, Key, _} = L1,
      lists:all(
        fun(L2) ->
          L2 =/= Key
        end,
        List
      )
    end,
    Map
  ).

send_message_to_other_server(Nodes, Message) ->
  lists:foreach(
    fun(Node) ->
      {controller, Node} ! Message
    end, Nodes
  ).

do_this_once(Nodes) ->
  mnesia:create_schema(Nodes),
  mnesia:start(),
  mnesia:create_table(message, [{disc_copies, Nodes}, {attributes, record_info(fields, message)}]),
  mnesia:stop().

reset_tables() ->
  mnesia:clear_table(message).

add_message_item(Time, FromName, ToName, Msg) ->
  Row = #message{time=Time, from_name=FromName, to_name=ToName, msg=Msg},
  F = fun() ->
        mnesia:write(Row)
      end,
  mnesia:transaction(F).

query_message_item(all) ->
  do(qlc:q([Msg || Msg <- mnesia:table(message)])).

do(Q) ->
  F = fun() ->
        qlc:e(Q)
      end,
  {atomic, Val} = mnesia:transaction(F),
  Val.