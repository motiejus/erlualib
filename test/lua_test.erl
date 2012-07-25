-module(lua_test).

-include("proper_utils.hrl").

-define(T, [{numtests, 1000}]).

oh_prop_test_() ->
    [
        {timeout, 3600, {"boolean", ?prop_check(fun boolean_p/0, [])}},
        {timeout, 3600, {"lstring", ?prop_check(fun lstring_p/0, ?T)}},
        {timeout, 3600, {"concat",  ?prop_check(fun concat_p/0,  ?T)}},
        {timeout, 3600, {"float",   ?prop_check(fun float_p/0,   ?T)}},
        {timeout, 3600, {"int32",   ?prop_check(fun int_p/0,     ?T)}}
    ].

boolean_p() -> ?FORALL(X, boolean(), push_to_helper(X, pushboolean, toboolean)).
lstring_p() -> ?FORALL(X, binary(),  push_to_helper(X, pushlstring, tolstring)).
float_p()   -> ?FORALL(X, float(),   push_to_helper(X, pushnumber,   tonumber)).

int_p() ->
    ?FORALL(X, integer(-16#7fffffff, 16#7fffffff - 1),
        push_to_helper(X, pushinteger, tointeger)
    ).

concat_p() ->
    ?FORALL(X, list(binary()),
        begin
                {ok, L} = lua:new_state(),
                [lua:pushlstring(L, B) || B <- X],
                Concated = iolist_to_binary(X),
                ok = lua:concat(L, length(X)),
                R = Concated =:= lua:tolstring(L, -1),
                lua:close(L),
                R
        end).

small_integer_test() -> push_to_helper(1, pushinteger, tointeger).
zero_integer_test() -> push_to_helper(0, pushinteger, tointeger).
small_negative_integer_test() -> push_to_helper(-2, pushinteger, tointeger).
small_number_test() -> push_to_helper(2, pushnumber, tonumber).
small_negative_number_test() -> push_to_helper(-2, pushnumber, tonumber).
zero_number_test() -> push_to_helper(0, pushnumber, tonumber).
big_number_test() -> push_to_helper(5000000000, pushnumber, tonumber).
big_float_number_test() -> push_to_helper(5000000000.234, pushnumber, tonumber).
big_neg_number_test() -> push_to_helper(-5000000000, pushnumber, tonumber).
big_neg_float_test() -> push_to_helper(-5000000000.234, pushnumber, tonumber).
string_test() -> push_to_helper(<<"testing">>, pushlstring, tolstring).
bool_test() -> push_to_helper(false, pushboolean, toboolean).

nil_type_test() -> type_test_helper(pushnil, nil).
boolean_type_test() -> type_test_helper(true, pushboolean, boolean).
num_type_test() -> type_test_helper(1, pushinteger, number).
string_type_test() -> type_test_helper(<<"foo">>, pushlstring, string).
table_type_test() -> type_test_helper(newtable, table).

ns() -> {ok, L} = lua:new_state(), L.
oh_test_() ->
    [
        {"createtable", ?_test(createtable(ns()))},
        {"settable", ?_test(settable(ns()))},
        {"gettable", ?_test(gettable(ns()))},
        {"remove", ?_test(remove(ns()))},
        {"setfield, getfield", ?_test(set_get_field(ns()))},
        {"concat", ?_test(concat(ns()))},
        {"call", ?_test(call(ns()))},
        {"setglobal, getglobal", ?_test(set_get_global(ns()))},
        {"next", ?_test(next(ns()))}
    ].

createtable(L) ->
    ?assertEqual(ok, lua:createtable(L, 0, 2)),
    ?assertEqual(table, lua:type(L, 1)),
    ?assertEqual(0, lua:objlen(L, 1)).

settable(L) ->
    ?assertEqual(ok, lua:newtable(L)),
    lua:pushlstring(L, <<"x">>),
    lua:pushlstring(L, <<"y">>),
    ?assertEqual(3, lua:gettop(L)),
    ?assertEqual(ok, lua:settable(L, 1)),
    ?assertEqual(1, lua:gettop(L)),
    lua:getfield(L, 1, "x"),
    ?assertEqual(<<"y">>, lua:tolstring(L, -1)).

gettable(L) ->
    lua:newtable(L),
    lua:pushnumber(L, 2),
    lua:pushnumber(L, 3),
    lua:settable(L, 1), % t[2] = 3
    lua:pushnumber(L, 2),
    lua:gettable(L, -2), % push t[2] to top
    ?assertEqual(3, lua:tonumber(L, -1)).

remove(L) ->
    lua:pushnumber(L, 1),
    ?assertEqual(1, lua:gettop(L)),
    ?assertEqual(ok, lua:remove(L, 1)),
    ?assertEqual(0, lua:gettop(L)).

set_get_field(L) ->
    lua:newtable(L),
    lua:pushboolean(L, true),
    ?assertEqual(table, lua:type(L, 1)),
    ?assertEqual(boolean, lua:type(L, 2)),
    ?assertEqual(ok, lua:setfield(L, 1, "foo")),
    ?assertEqual(table, lua:type(L, 1)),
    ?assertEqual(ok, lua:getfield(L, 1, "foo")),
    ?assertEqual(true, lua:toboolean(L, 2)).

concat(L) ->
    lua:pushlstring(L, <<"ya">>),
    ?assertEqual(2, lua:objlen(L, 1)),
    lua:pushlstring(L, <<"dda">>),
    ?assertEqual(3, lua:objlen(L, 2)),
    ?assertEqual(ok, lua:concat(L, 2)),
    ?assertEqual(<<"yadda">>, lua:tolstring(L, 1)),
    ?assertEqual(5, lua:objlen(L, 1)).

call(L) ->
    ?assertEqual(ok, lua:getfield(L, global, "type")),
    ?assertEqual(function, lua:type(L, 1)),
    ?assertEqual(ok, lua:pushnumber(L, 1)),
    ?assertEqual(ok, lua:call(L, 1, 1)),
    ?assertEqual(<<"number">>, lua:tolstring(L, 1)).
    
set_get_global(L) ->
    ?assertEqual(ok, lua:pushnumber(L, 23)),
    ?assertEqual(ok, lua:setfield(L, global, "foo")),
    ?assertEqual(ok, lua:getfield(L, global, "foo")),
    ?assertEqual(23, lua:tonumber(L, 1)).

next(L) ->
    createtable(L),
    lua:pushlstring(L, <<"vienas">>),
    lua:pushlstring(L, <<"1">>),
    lua:settable(L, 1),
    lua:pushlstring(L, <<"du">>),
    lua:pushlstring(L, <<"2">>),
    lua:settable(L, 1),
    ?assertEqual(1, lua:gettop(L)),
    lua:pushnil(L),
    ?assertNotEqual(0, lua:next(L, 1)),
    ?assertEqual(<<"2">>, lua:tolstring(L, -1)), lua:remove(L, -1),
    ?assertEqual(<<"du">>, lua:tolstring(L, -1)),
    ?assertNotEqual(0, lua:next(L, 1)),
    ?assertEqual(<<"1">>, lua:tolstring(L, -1)), lua:remove(L, -1),
    ?assertEqual(<<"vienas">>, lua:tolstring(L, -1)),
    ?assertEqual(0, lua:next(L, 1)).


%% =============================================================================
%% Helpers
%% =============================================================================

push_to_helper(Val, Push, To) ->
    {ok, L} = lua:new_state(),
    ?assertEqual(ok, lua:Push(L, Val)),
    ?assertEqual(Val, lua:To(L, 1)),
    % Test luam:pushterm/2
    ?assertEqual(ok, luam:pushterm(L, Val)),
    ?assertEqual(Val, lua:To(L, 2)),
    lua:close(L),
    true.

type_test_helper(PushFun, Type) ->
    {ok, L} = lua:new_state(),
    ?assertEqual(ok, lua:PushFun(L)),
    ?assertEqual(Type, lua:type(L, 1)),
    lua:close(L).

type_test_helper(Value, PushFun, Type) ->
    {ok, L} = lua:new_state(),
    ?assertEqual(ok, lua:PushFun(L, Value)),
    ?assertEqual(Type, lua:type(L, 1)),
    % Test luam:pushterm/2
    ?assertEqual(ok, luam:pushterm(L, Value)),
    ?assertEqual(Type, lua:type(L, 2)),
    lua:close(L).
