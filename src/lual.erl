%%% @doc LuaL, extended function calling
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

-module(lual).

-export([dostring/2, call/4]).
-export([push_args/2]).

-include("lua.hrl").
-include("lua_api.hrl").

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

dostring(#lua{port=Port}, Code) ->
    port_command(Port, term_to_binary({?ERL_LUAL_DOSTRING, Code})),
    recieve_simple_response().

recieve_simple_response() ->
    receive
        ok ->
            ok;
        error ->
            {error, lua_error};
        {error, Reason} ->
            {error, Reason};
        Other ->
            {other, Other}
    after ?STD_TIMEOUT ->
        {error, timeout}
    end.

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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
push_args_empty_table_test() ->
    {ok, L} = lua:new_state(),
    push_args(L, {}),
    ?assertEqual(1, lua:gettop(L)),
    ?assertEqual(0, lua:objlen(L, 1)).

push_args_table1_test() ->
    {ok, L} = lua:new_state(),
    push_args(L, {true, {}, <<"yadda">>}),
    ?assertEqual(1, lua:gettop(L)),
    ?assertEqual(3, lua:objlen(L, 1)),
    lua:getfield(L, 1, 1),
    ?assertEqual(boolean, lua:type(L, 2)),
    lua:getfield(L, 1, 2),
    ?assertEqual(table, lua:type(L, 3)),
    lua:getfield(L, 1, 3),
    ?assertEqual(string, lua:type(L, 4)).

-endif.
