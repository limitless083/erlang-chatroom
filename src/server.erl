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
-export([start_controller/0, start_server/1]).

start_controller() ->
  register(controller, spawn(fun() -> control([]) end)).

control(CurrentSocketsMap) ->
  receive
    {add, From, UserName, Socket} ->
      io:format("length : ~p~n", [length(CurrentSocketsMap)+1]),
      NameSocketMap = lists:filter(
        fun({Name, _ToSocket}) ->
          Name =:= UserName
        end, CurrentSocketsMap
      ),
      if
        length(NameSocketMap) > 0 ->
          controller ! {send_to_self, Socket, "Name exist, please choose anthoer name.."},
          From ! {add, failed},
          control(CurrentSocketsMap);
        true ->
          From ! {add, ok},
          control([{UserName, Socket} | CurrentSocketsMap])
      end;
    {delete, UserName, Socket} ->
      io:format("length : ~p~n", [length(CurrentSocketsMap)-1]),
      control(CurrentSocketsMap -- [{UserName, Socket}]);
    {broadcast, ExceptNames, Message} ->
      lists:foreach(
        fun({_Name, Socket}) ->
          gen_tcp:send(Socket, term_to_binary(Message))
        end, double_filter(ExceptNames, CurrentSocketsMap)
      ),
      control(CurrentSocketsMap);
    {send_to_self, Socket, Message} ->
      gen_tcp:send(Socket, term_to_binary(Message)),
      control(CurrentSocketsMap);
    {send_to_other, Socket, ToName, Message} ->
      NameSocketMap = lists:filter(
        fun({UserName, _ToSocket}) ->
          UserName =:= ToName
        end, CurrentSocketsMap
      ),
      if
        length(NameSocketMap) > 0 ->
          [{_UserName, ToSocket}] = NameSocketMap,
          gen_tcp:send(ToSocket, term_to_binary(Message));
        true ->
          gen_tcp:send(Socket, term_to_binary(ToName ++ " not on line!"))
      end,
      control(CurrentSocketsMap);
    {who, Socket} ->
      lists:foreach(
        fun({Name, _Socket}) ->
          gen_tcp:send(Socket, term_to_binary(Name))
        end, CurrentSocketsMap
      ),
      gen_tcp:send(Socket, term_to_binary("Total online user: " ++ integer_to_list(length(CurrentSocketsMap)))),
      control(CurrentSocketsMap);
    _Any ->
      io:format("controller receive ~p~n", [_Any]),
      control(CurrentSocketsMap)
  end.

start_server(ControllerNode) ->
  {ok, Listen} = gen_tcp:listen(2345, [binary, {packet, 2},
    {reuseaddr, true},
    {active, true}]),
  seq_loop(Listen, ControllerNode).

seq_loop(Listen, ControllerNode) ->
  {ok, Socket} = gen_tcp:accept(Listen),

  spawn(fun() -> seq_loop(Listen, ControllerNode) end),
  {controller,ControllerNode} ! {send_to_self, Socket, "Please login!\n"},
  %% 连接成功以后进行登录，直到登录成功才能进行其他命令
  loop(ControllerNode, Socket, "", false).

loop(ControllerNode, Socket, FromName, IsLogined) ->
  receive
    {tcp, Socket, Bin} ->
      io:format("Server received binary = ~p~n", [Bin]),
      Message = binary_to_term(Bin),
      [Command | Tail] = re:split(Message, "\\s+", [{return, list}]),
      case string:span(Command, "/") of
        0 ->
          %% broadcast message
          if
            IsLogined ->
              {controller, ControllerNode} ! {send_to_self, Socket, "you say:" ++ Message},
              {controller, ControllerNode} ! {broadcast, [FromName],  FromName ++ " says " ++ Message};
            true ->
              {controller, ControllerNode} ! {send_to_self, Socket, "Invalid command"}
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
                  {controller, ControllerNode} ! {send_to_self, Socket, "Name exist, please choose anthoer name.."},
                  loop(ControllerNode, Socket, FromName, IsLogined);
                true ->
                  {controller, ControllerNode} ! {add, self(), UserName, Socket},
                  receive
                    {add, ok} ->
                      {controller, ControllerNode} ! {send_to_self, Socket, "Login sucess!"},
                      {controller, ControllerNode} ! {broadcast, [UserName], UserName ++ " has logined!"},
                      loop(ControllerNode, Socket, UserName, true);
                    {add, failed} ->
                      loop(ControllerNode, Socket, FromName, IsLogined)
                  end
              end;
            "quit" ->
              if
                IsLogined ->
                  {controller, ControllerNode} ! {send_to_self, Socket, "quit"},
                  {controller, ControllerNode} ! {delete, FromName, Socket},
                  {controller, ControllerNode} ! {broadcast, [FromName], FromName ++ " has quit!"};
                true ->
                  {controller, ControllerNode} ! {send_to_self, Socket, "quit"}
              end;
            "to" ->
              if
                IsLogined ->
                  if
                    length(Tail) > 1 ->
                      [_, ToName, ToMessage] = re:split(Message, "\\s+", [{return, list},{parts, 3}]),
                      {controller, ControllerNode} ! {send_to_self, Socket, "you say to " ++ ToName ++ ":" ++ ToMessage},
                      {controller, ControllerNode} ! {send_to_other, Socket, ToName, FromName ++ " says to you:" ++ ToMessage};
                    true ->
                      {controller, ControllerNode} ! {send_to_self, Socket, "Invalid command"}
                  end;
                true ->
                  {controller, ControllerNode} ! {send_to_self, Socket, "Invalid command"}
              end,
              loop(ControllerNode, Socket, FromName, IsLogined);
            "who" ->
              if
                IsLogined ->
                  {controller, ControllerNode} ! {who, Socket};
                true ->
                  {controller, ControllerNode} ! {send_to_self, Socket, "Invalid command"}
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
                  {controller, ControllerNode} ! {send_to_self, Socket, "you say to " ++ ToName ++ ":" ++ DefinedMessage},
                  {controller, ControllerNode} ! {send_to_other, Socket, ToName, FromName ++ " says to you:" ++ DefinedMessage},
                  {controller, ControllerNode} ! {broadcast, [FromName,ToName], FromName ++ " says to " ++ ToName ++ ":" ++ DefinedMessage};
                true ->
                  DefinedMessage = defined_message({public, Command}),
                  {controller, ControllerNode} ! {send_to_self, Socket, "you say to all:" ++ DefinedMessage},
                  {controller, ControllerNode} ! {broadcast, [FromName], FromName ++ " says to all:" ++ DefinedMessage}
              end;
            true ->
              {controller, ControllerNode} ! {send_to_self, Socket, "Invalid command"}
          end,
          loop(ControllerNode, Socket, FromName, IsLogined);

        _Any ->
          ok = gen_tcp:send(Socket, term_to_binary("Invalid command")),
          loop(ControllerNode, Socket, FromName, IsLogined)
      end;
    {tcp_closed, Socket} ->
      io:format("Server closed ~n"),
      {controller, ControllerNode} ! {delete, Socket}
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
      {Key,_Value} = L1,
      lists:all(
        fun(L2) ->
          L2 =/= Key
        end,
        List
      )
    end,
    Map
  ).
