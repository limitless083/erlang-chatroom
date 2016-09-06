%%%-------------------------------------------------------------------
%%% @author Victor
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. 九月 2016 14:49
%%%-------------------------------------------------------------------
-module(server).
-author("Victor").

%% API
-export([start_controller/0, start_server/2]).

%% controller node
start_controller() ->
  register(controller, spawn(fun() -> control([]) end)).

control(CurrentSocketsMap) ->
  receive
    {add, FromNode, From, UserName, Socket} ->
      io:format("length : ~p~n", [length(CurrentSocketsMap)+1]),
      NameSocketMap = lists:filter(
        fun({_Node, Name, _ToSocket}) ->
          Name =:= UserName
        end, CurrentSocketsMap
      ),
      if
        length(NameSocketMap) > 0 ->
          rpc:call(FromNode, gen_tcp, send, [Socket, term_to_binary("Name exist, please choose anthoer name..")]),
          From ! {add, failed},
          control(CurrentSocketsMap);
        true ->
          From ! {add, ok},
          control([{FromNode, UserName, Socket} | CurrentSocketsMap])
      end;
    {delete, FromNode, UserName, Socket} ->
      io:format("length : ~p~n", [length(CurrentSocketsMap)-1]),
      control(CurrentSocketsMap -- [{FromNode, UserName, Socket}]);
    {broadcast, _FromNode, ExceptNames, Message} ->
      io:format("controller receive ~p~n", [{{broadcast, ExceptNames, Message}}]),
      lists:foreach(
        fun({ToNode, _Name, Socket}) ->
          rpc:call(ToNode, gen_tcp, send, [Socket, term_to_binary(Message)])
        end, double_filter(ExceptNames, CurrentSocketsMap)
      ),
      control(CurrentSocketsMap);
    {send_to_self, FromNode, Socket, Message} ->
      io:format("controller receive ~p~n", [{send_to_self, Socket, Message}]),
      rpc:call(FromNode, gen_tcp, send, [Socket, term_to_binary(Message)]),
      control(CurrentSocketsMap);
    {send_to_other, FromNode, Socket, ToName, Message} ->
      io:format("controller receive ~p~n", [{send_to_other, Socket, ToName, Message}]),
      NameSocketMap = lists:filter(
        fun({_Node, UserName, _ToSocket}) ->
          UserName =:= ToName
        end, CurrentSocketsMap
      ),
      if
        length(NameSocketMap) > 0 ->
          [{ToNode, _UserName, ToSocket}] = NameSocketMap,
          rpc:call(ToNode, gen_tcp, send, [ToSocket, term_to_binary(Message)]);
        true ->
          rpc:call(FromNode, gen_tcp, send, [Socket, term_to_binary(ToName ++ " not on line!")])
      end,
      control(CurrentSocketsMap);
    {who, FromNode, Socket} ->
      lists:foreach(
        fun({Name, _Socket}) ->
          rpc:call(FromNode, gen_tcp, send, [Socket, term_to_binary(Name)])
        end, CurrentSocketsMap
      ),
      rpc:call(FromNode, gen_tcp, send, [Socket, term_to_binary("Total online user: " ++ integer_to_list(length(CurrentSocketsMap)))]),
      control(CurrentSocketsMap);
    _Any ->
      io:format("controller receive ~p~n", [_Any]),
      control(CurrentSocketsMap)
  end.

%% server
start_server(ControllerNode, Port) ->
  {ok, Listen} = gen_tcp:listen(Port, [binary, {packet, 2},
    {reuseaddr, true},
    {active, true}]),
  seq_loop(ControllerNode, Listen).

seq_loop(ControllerNode, Listen) ->
  {ok, Socket} = gen_tcp:accept(Listen),
  spawn(fun() -> seq_loop(ControllerNode, Listen) end),
  {controller, ControllerNode} ! {send_to_self, node(), Socket, "Please login!\n"},
  loop(ControllerNode, Socket, "", false).

loop(ControllerNode, Socket, FromName, IsLogined) ->
  receive
    {tcp, Socket, Bin} ->
      Message = binary_to_term(Bin),
      io:format("Server received = ~p~n", [Message]),
      [Command | Tail] = re:split(Message, "\\s+", [{return, list}]),
      case string:span(Command, "/") of
        0 ->
          %% broadcast message
          if
            IsLogined ->
              {controller, ControllerNode} ! {send_to_self, node(), Socket, "you say:" ++ Message},
              {controller, ControllerNode} ! {broadcast, node(), [FromName],  FromName ++ " says " ++ Message};
            true ->
              {controller, ControllerNode} ! {send_to_self, node(), Socket, "Invalid command"}
          end,
          loop(ControllerNode, Socket, FromName, IsLogined);
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
                  {controller, ControllerNode} ! {send_to_self, node(), Socket, "Name exist, please choose anthoer name.."},
                  loop(ControllerNode, Socket, FromName, IsLogined);
                true ->
                  {controller, ControllerNode}! {add, node(), self(), UserName, Socket},
                  receive
                    {add, ok} ->
                      {controller, ControllerNode} ! {send_to_self, node(), Socket, "Login sucess!"},
                      {controller, ControllerNode} ! {broadcast, node(), [UserName], UserName ++ " has logined!"},
                      loop(ControllerNode, Socket, UserName, true);
                    {add, failed} ->
                      loop(ControllerNode, Socket, FromName, IsLogined)
                  end
              end;
            "quit" ->
              if
                IsLogined ->
                  {controller, ControllerNode} ! {send_to_self, node(), Socket, "quit"},
                  {controller, ControllerNode} ! {delete, node(), FromName, Socket},
                  {controller, ControllerNode} ! {broadcast, node(), [FromName], FromName ++ " has quit!"};
                true ->
                  {controller, ControllerNode} ! {send_to_self, node(), Socket, "quit"}
              end;
            "to" ->
              if
                IsLogined ->
                  if
                    length(Tail) > 1 ->
                      [_, ToName, ToMessage] = re:split(Message, "\\s+", [{return, list},{parts, 3}]),
                      {controller, ControllerNode} ! {send_to_self, node(), Socket, "you say to " ++ ToName ++ ":" ++ ToMessage},
                      {controller, ControllerNode} ! {send_to_other, node(), Socket, ToName, FromName ++ " says to you:" ++ ToMessage};
                    true ->
                      {controller, ControllerNode} ! {send_to_self, node(), Socket, "Invalid command"}
                  end;
                true ->
                  {controller, ControllerNode} ! {send_to_self, node(), Socket, "Invalid command"}
              end,
              loop(ControllerNode, Socket, FromName, IsLogined);
            "who" ->
              if
                IsLogined ->
                  {controller, ControllerNode} ! {who, node(), Socket};
                true ->
                  {controller, ControllerNode} ! {send_to_self, node(), Socket, "Invalid command"}
              end,
              loop(ControllerNode, Socket, FromName, IsLogined);
            "history" ->
              void
          end;
        2 ->
          %% defined message
          if
            IsLogined ->
              if
                length(Tail) > 0 ->
                  [ToName | _] = Tail,
                  DefinedMessage = defined_message({private, Command}),
                  {controller, ControllerNode} ! {send_to_self, node(), Socket, "you say to " ++ ToName ++ ":" ++ DefinedMessage},
                  {controller, ControllerNode} ! {send_to_other, node(), Socket, ToName, FromName ++ " says to you:" ++ DefinedMessage},
                  {controller, ControllerNode} ! {broadcast, node(), [FromName,ToName], FromName ++ " says to " ++ ToName ++ ":" ++ DefinedMessage};
                true ->
                  DefinedMessage = defined_message({public, Command}),
                  {controller, ControllerNode} ! {send_to_self, node(), Socket, "you say to all:" ++ DefinedMessage},
                  {controller, ControllerNode} ! {broadcast, node(), [FromName], FromName ++ " says to all:" ++ DefinedMessage}
              end;
            true ->
              {controller, ControllerNode} ! {send_to_self, node(), Socket, "Invalid command"}
          end,
          loop(ControllerNode, Socket, FromName, IsLogined);

        _Any ->
          {controller, ControllerNode} ! {send_to_self, node(), Socket, "Invalid command"},
          loop(ControllerNode, Socket, FromName, IsLogined)
      end;
    {tcp_closed, Socket} ->
      io:format("Server closed ~n"),
      if
        IsLogined ->
          {controller, ControllerNode} ! {send_to_self, node(), Socket, "quit"},
          {controller, ControllerNode} ! {delete, node(), FromName, Socket},
          {controller, ControllerNode} ! {broadcast, node(), [FromName], FromName ++ " has quit!"};
        true ->
          {controller, ControllerNode} ! {send_to_self, node(), Socket, "quit"}
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
      "Hi，erveryone, nice to meet you!";
    "smile" ->
      "Hi, everyone, I am happy."
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
