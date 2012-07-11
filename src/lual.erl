%%% @doc LuaL, extended function calling
%%%
%%% This module is responsible for high-level function invocations in Lua.
%%%
%%% ## Erlang->Lua type conversion table
%%%     >-------->------>
%%% +------------+----------------+
%%% | Erlang     |      Lua       |
%%% +------------+----------------+
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
%%% | boolean         | true|false |
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
call(L, Fun, Args, Num) ->
    ok.
