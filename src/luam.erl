%%% @doc LuaM, extended function calling
%%%
%%% This module is responsible for high-level function invocations in Lua.
%%%
%%% ## Erlang->Lua type conversion table
%%%     >-------->------>
%%% +------------+----------------+
%%% | Erlang     |      Lua       |
%%% +------------+----------------+
%%% | 'nil'      | nil
%%% | boolean    | boolean        |
%%% | binary     | string         |
%%% | atom       | string         |
%%% | string     | string         |
%%% | number     | number         |
%%% | proplist   | tagged table   |
%%% | tuple      | indexed table  |
%%% +------------+----------------+
%%%
%%% Whereas >--------->----->
%%% +-----------------+------------+
%%% |       Lua       |   Erlang   |
%%% +-----------------+------------+
%%% | nil             | nil        |
%%% | boolean         | boolean    |
%%% | light_user_data | --NA--     |
%%% | number          | float      |
%%% | string          | binary     |
%%% | table           | proplist   |
%%% | function        | --NA--     |
%%% | user_data       | --NA--     |
%%% | thread.         | --NA--     |
%%% +-----------------+------------+
%%%
%%% ## Table to proplist conversion rules
%%% Table key conversion rules are same like for any other data type.
%%%
%%% ## Function return value handling
%%% When function returns with one value, give it plain.
%%% When function returns more values, wrap them to tuple.

-module(luam).

-export([call/4, push_args/2]).

-type arg() :: binary() | % string
               atom()   | % string
               number() | % number
               list()   | % table
               tuple().   % table
-type args() :: [arg(), ...].

-type ret() :: nil       |
               boolean() |
               float()   |
               binary()  |
               list({ret(), ret()}).

-spec call(lua:lua(), string(), args(), pos_integer()) -> ret().
call(L, Fun, Args, 1) ->
    push_args(L, Args),
    lua:call(L, Fun, length(Args), 1),
    pop_results(L).


push_args(L, nil) ->
    lua:pushnil(L);
push_args(L, Arg) when is_boolean(Arg) ->
    lua:pushboolean(L, Arg);
push_args(L, Arg) when is_binary(Arg) ->
    lua:pushlstring(L, Arg);
push_args(L, Arg) when is_number(Arg) ->
    lua:pushnumber(L, Arg);
push_args(L, Args) when is_tuple(Args) ->
    lua:createtable(L, size(Args), 0),
    TPos = lua:gettop(L), % table position we have just created
    Fun = fun({I, Arg}) ->
            lua:pushnumber(L, I),
            push_args(L, Arg),
            lua:settable(L, TPos)
    end,
    lists:foreach(Fun, lists:zip(
            lists:seq(1, size(Args)),
            tuple_to_list(Args))
    ).

pop_results(_) ->
    ok.
