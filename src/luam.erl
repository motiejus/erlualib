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

-include("lua_api.hrl").
-include_lib("kernel/include/file.hrl").

-export([one_call/3, call/3, multipcall/2, maybe_atom/2, pushterm/2]).
-export([fold/4]).


%% @doc Call FunName with Args. Return all arguments in tuple.
-spec call(lua:lua(), string(), list(lua:arg())) -> lua:ret().
call(L, FunName, Args) ->
    lua:getglobal(L, FunName),
    [pushterm(L, Arg) || Arg <- Args],
    N = multipcall(L, length(Args)),
    lua:gettop(L),
    pop_results(L, N).

%% @doc Call a function once. Initializes Lua, does luam:call, closes Lua
%%
%% Steps:
%% 1. Create a new Lua state
%% 2. Open and load the file
%% 3. luam:call/4
%% 4. Close Lua state
%% 5. Return the result of luam:call
-spec one_call(file:name(),string(),list(lua:arg())) -> lua:ret() | no_return().
one_call(File, FunName, Args) ->
    State = case get(lua_state) of
        undefined ->
            {ok, L} = lua:new_state(),
            put(lua_state, L),
            L;
        L -> L
    end,
    {ok, #file_info{mtime=Mtime}}=file:read_file_info(File),
    LuaMtime = get(lua_mtime), 
    if 
        LuaMtime =/= Mtime ->
            Src = case file:read_file(File) of
                {ok, Bin} -> Bin;
                {error, Err} -> erlang:error({{error_reading_file_in, filename:absname("")},
                            File, Err})
            end,
            ok = lual:dostring(State, Src),
            put(lua_mtime, Mtime);
        true -> do_nothing
    end,
    luam:call(State, FunName, Args).

%% @doc Push arbitrary variable on stack
-spec pushterm(lua:lua(), lua:arg())    -> ok.
pushterm(L, Arg) when is_pid(Arg)       -> lua:pushnil(L); % TODO: UNSUPPORTED
pushterm(L, Arg) when is_reference(Arg) -> lua:pushnil(L); % TODO: UNSUPPORTED
pushterm(L, Arg) when is_port(Arg)      -> lua:pushnil(L); % TODO: UNSUPPORTED
pushterm(L, nil)                        -> lua:pushnil(L);
pushterm(L, Arg) when is_boolean(Arg)   -> lua:pushboolean(L, Arg);
pushterm(L, Arg) when is_atom(Arg)      -> lua:pushlstring(L, a2b(Arg));
pushterm(L, Arg) when is_binary(Arg)    -> lua:pushlstring(L, Arg);
pushterm(L, Arg) when is_number(Arg)    -> lua:pushnumber(L, Arg);
pushterm(L, Args) when is_tuple(Args)   ->
    Proplist = lists:zip(lists:seq(1, size(Args)), tuple_to_list(Args)),
    pushterm(L, Proplist);
pushterm(L, Args) when is_list(Args) ->
    NewArgs = case is_proplist(Args) of
        true -> Args;
        false -> lists:zip(lists:seq(1, length(Args)), Args)
    end,
    lua:createtable(L, length(NewArgs), 0),
    TPos = lua:gettop(L),
    Fun = fun({K, V}) ->
            pushterm(L, K),
            pushterm(L, V),
            lua:settable(L, TPos)
    end,
    lists:foreach(Fun, NewArgs).

%% @doc Pop N results from the stack and return result tuple. [-N, +0]
-spec pop_results(lua:lua(), pos_integer()) -> tuple().
pop_results(L, N) ->
    MapFun = fun(_) ->
            R = toterm(L, lua:gettop(L)),
            lua:remove(L, -1),
            R
    end,
    case lists:reverse(lists:map(MapFun, lists:seq(1, N))) of
        [Ret] -> Ret;
        Ret -> list_to_tuple(Ret)
    end.

%% @doc Returns Nth element on the stack. [-0, +0]
-spec toterm(lua:lua(), lua:index()) -> lua:ret().
toterm(L, N) ->
    case lua:type(L, N) of
        nil -> nil;
        boolean -> lua:toboolean(L, N);
        number -> lua:tonumber(L, N);
        string -> lua:tolstring(L, N);
        user_data ->
            case maybe_atom(L, N) of
                {lua_ok, Atom} -> Atom;
                _ -> erlang:error(badarg)
            end;
        table ->
            F = fun(K, V, Acc) -> [{K, V}|Acc] end,
            lists:reverse(fold(F, [], L, N))
    end.

%% @doc Call Fun over table on absolute index N. [-0, +0].
-spec fold(Fun, Acc0, lua:lua(), N :: lua:abs_index()) -> Acc1 when
      Fun :: fun((lua:ret(), lua:ret(), AccIn) -> AccOut),
      Acc0 :: term(),
      Acc1 :: term(),
      AccIn :: term(),
      AccOut :: term().
fold(Fun, Acc0, L, N) ->
    lua:pushnil(L),
    fold(Fun, Acc0, L, N, lua:next(L, N)).

fold(_Fun, Acc, _L, _N, 0) -> Acc;
fold( Fun, Acc, L,   N, _) ->
    V = toterm(L, lua:gettop(L)), lua:remove(L, -1),
    K = toterm(L, lua:gettop(L)),
    Acc2 = Fun(K, V, Acc),
    fold(Fun, Acc2, L, N, lua:next(L, N)).

%% @doc Call function and retrieve its all arguments. [-(N+1), +M]
%%
%% Equivalent to lua:call(L, N, LUA_MULTRET), but returns how many arguments
%% function returned. Pops function from the stack.
-spec multipcall(lua:lua(), non_neg_integer()) -> non_neg_integer().
multipcall(L, N) ->
    lua_common:command(L, {?ERL_LUAM_MULTIPCALL, N}),
    lua_common:receive_valued_response().

-spec a2b(atom()) -> binary().
a2b(Atom) when is_atom(Atom) ->
    list_to_binary(atom_to_list(Atom)).

%% @doc Return atom() if value on N is atom; false otherwise
-spec maybe_atom(lua:lua(), lua:index()) -> {lua_ok, atom()} | false.
maybe_atom(L, N) ->
    lua_common:command(L, {?ERL_LUAM_MAYBE_ATOM, N}),
    lua_common:receive_valued_response().

%is_string([]) -> false;
%is_string(X) -> io_lib:printable_unicode_list(X).

is_proplist([]) -> true;
is_proplist({_K, _V}) -> true;
is_proplist([X|T]) -> is_proplist(X) andalso is_proplist(T);
is_proplist(_) -> false.
