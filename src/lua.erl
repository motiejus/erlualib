-module(lua).

-export([new_state/0,
         close/1,
         call/3,
         concat/2,
         getfield/3,
         getglobal/2,
         gettop/1,
         pushboolean/2,
         pushinteger/2,
         pushstring/2,
         pushnil/1,
         pushnumber/2,
         remove/2,
         setfield/3,
         setglobal/2,
         toboolean/2,
         tointeger/2,
         tolstring/2,
         tonumber/2,
         type/2]).

-include("lua.hrl").
-include("lua_api.hrl").

-type lua() :: #lua{}.
-type index() :: pos_integer() | neg_integer().

-type lua_type() :: nil | boolean | light_user_data | number |
        string | table | function | user_data | thread | unknown.

-export_type([lua/0]).

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
    command(L, {?ERL_LUA_CALL, Args, Results}),
    receive_simple_response().

-spec concat(lua(), index()) -> ok.
concat(L, N) ->
    command(L, {?ERL_LUA_CONCAT, N}),
    receive_simple_response().

-spec getfield(lua(), global | index(), lua_name()) -> ok.
getfield(L, global, Name) ->
    getglobal(L, Name);
getfield(L, Index, Name) ->
    command(L, {?ERL_LUA_GETFIELD, Index, Name}),
    receive_simple_response().
    
-spec getglobal(lua(), lua_name()) -> ok.
getglobal(L, Name) ->
    command(L, {?ERL_LUA_GETGLOBAL, Name}),
    receive_simple_response().

-spec gettop(lua()) -> {ok, non_neg_integer()}.
gettop(L) ->
    command(L, {?ERL_LUA_GETTOP}),
    receive_valued_response().

-spec pushboolean(lua(), boolean()) -> ok.
pushboolean(L, Bool) ->
    command(L, {?ERL_LUA_PUSHBOOLEAN, Bool}),
    receive_simple_response().

-spec pushinteger(lua(), integer()) -> ok.
pushinteger(L, Int) when is_integer(Int) ->
    command(L, {?ERL_LUA_PUSHINTEGER, Int}),
    receive_simple_response().

-spec pushstring(lua(), string()) -> ok.
pushstring(L, String) when is_list(String) ->
    command(L, {?ERL_LUA_PUSHSTRING, String}),
    receive_simple_response().

-spec pushnil(lua()) -> ok.
pushnil(L) ->
    command(L, {?ERL_LUA_PUSHNIL}),
    receive_simple_response().

-spec pushnumber(lua(), number()) -> ok.
pushnumber(L, Num) when is_number(Num) ->
    command(L, {?ERL_LUA_PUSHNUMBER, Num}),
    receive_simple_response().

-spec remove(lua(), index()) -> ok.
remove(L, Index) ->
    command(L, {?ERL_LUA_REMOVE, Index}),
    receive_simple_response().

-spec setfield(lua(), global | index(), string()) -> ok.
setfield(L, global, Name) ->
    setglobal(L, Name);
setfield(L, Index, Name) ->
    command(L, {?ERL_LUA_SETFIELD, Index, Name}),
    receive_simple_response().

-spec setglobal(lua(), string()) -> ok.
setglobal(L, Name) ->
    command(L, {?ERL_LUA_SETGLOBAL, Name}),
    receive_simple_response().

-spec toboolean(lua(), index()) -> {ok, boolean()}.
toboolean(L, Index) ->
    command(L, {?ERL_LUA_TOBOOLEAN, Index}),
    receive_valued_response().

-spec tointeger(lua(), index()) -> {ok, integer()}.
tointeger(L, Index) ->
    command(L, {?ERL_LUA_TOINTEGER, Index}),
    receive_valued_response().

-spec tolstring(lua(), index()) -> {ok, string()}.
tolstring(L, Index) ->
    command(L, {?ERL_LUA_TOLSTRING, Index}),
    receive_valued_response().

-spec tonumber(lua(), index()) -> {ok, number()}.
tonumber(L, Index) ->
    command(L, {?ERL_LUA_TONUMBER, Index}),
    {ok, Value} = receive_valued_response(),
    Value2 = list_to_binary(Value),
    {ok, binary_to_term(Value2)}.

-spec type(lua(), index()) -> lua_type().
type(L, Index) ->
    command(L, {?ERL_LUA_TYPE, Index}),
    {ok, Ret} = receive_valued_response(),
    lua_type_to_atom(Ret).

command(#lua{port=Port}, Data) ->
    port_command(Port, term_to_binary(Data)).

receive_simple_response() ->
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
    
receive_valued_response() ->
    receive
        {ok, Str} ->
            {ok, Str};
        error ->
            {error, lua_error};
        Other ->
            {other, Other}
    after ?STD_TIMEOUT ->
        {error, timeout}
    end.

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
