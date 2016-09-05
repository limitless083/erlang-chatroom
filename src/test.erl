%%%-------------------------------------------------------------------
%%% @author Victor
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 九月 2016 12:41
%%%-------------------------------------------------------------------
-module(test).
-author("Victor").

%% API
-export([test/2,double_filter/2]).

test(Tail,IsLogined) ->
  if
    length(Tail) > 0 ->
      [UserName | _] = Tail;
    true ->
      UserName = "bianguangkuo"
  end,
  if
    IsLogined ->
      io:format("login?~p~n",[UserName]);
    true ->
      io:format("login+~p~n",[UserName])
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