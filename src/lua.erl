-module(lua).

-export([new_state/0,
         close/1,
         call/3,
         concat/2,
         createtable/3,
         getfield/3,
         getglobal/2,
         gettable/2,
         gettop/1,
         newtable/1,
         next/2,
         objlen/2,
         pushboolean/2,
         pushinteger/2,
         pushlstring/2,
         pushnil/1,
         pushnumber/2,
         remove/2,
         setfield/3,
         setglobal/2,
         toboolean/2,
         tointeger/2,
         tolstring/2,
         tonumber/2,
         settable/2,
         type/2]).


-include("lua.hrl").
-include("lua_api.hrl").

-type lua() :: #lua{}.
-type index() :: abs_index() | rel_index().
-type abs_index() :: pos_integer().
-type rel_index() :: neg_integer().

-export_type([lua/0, index/0, abs_index/0]).

-type lua_type() :: nil | boolean | light_user_data | number |
        string | table | function | user_data | thread | unknown.

-type arg() :: binary()               | % string
               atom()                 | % string
               number()               | % number
               list({arg(), arg()})   | % associative table
               tuple(arg()).            % indexed table
-type ret() :: nil       |
               boolean() |
               float()   |
               binary()  |
               list({ret(), ret()}).
-export_type([arg/0, ret/0]).


-type lua_name() :: string().

-spec new_state() -> {ok, lua()}.
new_state() ->
    {ok, lua_driver:open()}.
    
-spec close(lua()) -> true.
close(L) ->
    lua_driver:close(L).

%% @doc Calls a function.
-spec call(lua(), non_neg_integer(), non_neg_integer()) -> ok.
call(L, Args, Results) ->
    lua_common:command(L, {?ERL_LUA_CALL, Args, Results}),
    lua_common:receive_simple_response().

-spec concat(lua(), index()) -> ok.
concat(L, N) ->
    lua_common:command(L, {?ERL_LUA_CONCAT, N}),
    lua_common:receive_simple_response().

-spec getfield(lua(), global | index(), lua_name()) -> ok.
getfield(L, global, Name) ->
    getglobal(L, Name);
getfield(L, Index, Name) ->
    lua_common:command(L, {?ERL_LUA_GETFIELD, Index, Name}),
    lua_common:receive_simple_response().
    
-spec getglobal(lua(), lua_name()) -> ok.
getglobal(L, Name) ->
    lua_common:command(L, {?ERL_LUA_GETGLOBAL, Name}),
    lua_common:receive_simple_response().

-spec gettable(lua(), index()) -> ok.
gettable(L, Index) ->
    lua_common:command(L, {?ERL_LUA_GETTABLE, Index}),
    lua_common:receive_simple_response().

-spec gettop(lua()) -> abs_index().
gettop(L) ->
    lua_common:command(L, {?ERL_LUA_GETTOP}),
    lua_common:receive_valued_response().

-spec newtable(lua()) -> ok.
newtable(L) ->
    lua_common:command(L, {?ERL_LUA_NEWTABLE}),
    lua_common:receive_simple_response().

-spec next(lua(), index()) -> integer().
next(L, Index) ->
    lua_common:command(L, {?ERL_LUA_NEXT, Index}),
    lua_common:receive_valued_response().

-spec objlen(lua(), index()) -> non_neg_integer().
objlen(L, Index) ->
    lua_common:command(L, {?ERL_LUA_OBJLEN, Index}),
    lua_common:receive_valued_response().

-spec createtable(lua(), non_neg_integer(), non_neg_integer()) -> ok.
createtable(L, Narr, Nrec) ->
    lua_common:command(L, {?ERL_LUA_CREATETABLE, Narr, Nrec}),
    lua_common:receive_simple_response().

-spec pushboolean(lua(), boolean()) -> ok.
pushboolean(L, Bool) ->
    lua_common:command(L, {?ERL_LUA_PUSHBOOLEAN, Bool}),
    lua_common:receive_simple_response().

-spec pushinteger(lua(), integer()) -> ok.
pushinteger(L, Int) when is_integer(Int) ->
    lua_common:command(L, {?ERL_LUA_PUSHINTEGER, Int}),
    lua_common:receive_simple_response().

-spec pushlstring(lua(), binary()) -> ok.
pushlstring(L, String) when is_binary(String) ->
    lua_common:command(L, {?ERL_LUA_PUSHLSTRING, String}),
    lua_common:receive_simple_response().

-spec pushnil(lua()) -> ok.
pushnil(L) ->
    lua_common:command(L, {?ERL_LUA_PUSHNIL}),
    lua_common:receive_simple_response().

-spec pushnumber(lua(), number()) -> ok.
pushnumber(L, Num) when is_number(Num) ->
    lua_common:command(L, {?ERL_LUA_PUSHNUMBER, Num}),
    lua_common:receive_simple_response().

-spec remove(lua(), index()) -> ok.
remove(L, Index) ->
    lua_common:command(L, {?ERL_LUA_REMOVE, Index}),
    lua_common:receive_simple_response().

-spec setfield(lua(), global | index(), string()) -> ok.
setfield(L, global, Name) ->
    setglobal(L, Name);
setfield(L, Index, Name) ->
    lua_common:command(L, {?ERL_LUA_SETFIELD, Index, Name}),
    lua_common:receive_simple_response().

-spec setglobal(lua(), string()) -> ok.
setglobal(L, Name) ->
    lua_common:command(L, {?ERL_LUA_SETGLOBAL, Name}),
    lua_common:receive_simple_response().

-spec toboolean(lua(), index()) -> boolean().
toboolean(L, Index) ->
    lua_common:command(L, {?ERL_LUA_TOBOOLEAN, Index}),
    lua_common:receive_valued_response().

-spec tointeger(lua(), index()) -> integer().
tointeger(L, Index) ->
    lua_common:command(L, {?ERL_LUA_TOINTEGER, Index}),
    lua_common:receive_valued_response().

-spec tolstring(lua(), index()) -> binary().
tolstring(L, Index) ->
    lua_common:command(L, {?ERL_LUA_TOLSTRING, Index}),
    lua_common:receive_valued_response().

-spec tonumber(lua(), index()) -> number().
tonumber(L, Index) ->
    lua_common:command(L, {?ERL_LUA_TONUMBER, Index}),
    binary_to_term(lua_common:receive_valued_response()).

-spec settable(lua(), index()) -> ok.
settable(L, Index) ->
    lua_common:command(L, {?ERL_LUA_SETTABLE, Index}),
    lua_common:receive_simple_response().

-spec type(lua(), index()) -> lua_type().
type(L, Index) ->
    lua_common:command(L, {?ERL_LUA_TYPE, Index}),
    lua_type_to_atom(lua_common:receive_valued_response()).

-spec lua_type_to_atom(non_neg_integer()) -> lua_type().
lua_type_to_atom(0) -> nil;
lua_type_to_atom(1) -> boolean;
lua_type_to_atom(2) -> light_user_data;
lua_type_to_atom(3) -> number;
lua_type_to_atom(4) -> string;
lua_type_to_atom(5) -> table;
lua_type_to_atom(6) -> function;
lua_type_to_atom(7) -> user_data;
lua_type_to_atom(8) -> thread.
