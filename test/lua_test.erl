-module(lua_test).

-include_lib("eunit/include/eunit.hrl").

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

createtable_test() ->
    {ok, L} = lua:new_state(),
    ?assertEqual(ok, lua:createtable(L, 0, 2)),
    ?assertEqual(table, lua:type(L, 1)),
    ?assertEqual(0, lua:objlen(L, 1)),
    lua:close(L).

settable_test() ->
    {ok, L} = lua:new_state(),
    ?assertEqual(ok, lua:newtable(L)),
    lua:pushlstring(L, <<"x">>),
    lua:pushlstring(L, <<"y">>),
    ?assertEqual(3, lua:gettop(L)),
    ?assertEqual(ok, lua:settable(L, 1)),
    ?assertEqual(1, lua:gettop(L)),
    lua:getfield(L, 1, "x"),
    ?assertEqual(<<"y">>, lua:tolstring(L, -1)),
    lua:close(L).


remove_test() ->
    {ok, L} = lua:new_state(),
    lua:pushnumber(L, 1),
    ?assertEqual(1, lua:gettop(L)),
    ?assertEqual(ok, lua:remove(L, 1)),
    ?assertEqual(0, lua:gettop(L)),
    lua:close(L).

set_get_field_test() ->
    {ok, L} = lua:new_state(),
    lua:newtable(L),
    lua:pushboolean(L, true),
    ?assertEqual(table, lua:type(L, 1)),
    ?assertEqual(boolean, lua:type(L, 2)),
    ?assertEqual(ok, lua:setfield(L, 1, "foo")),
    ?assertEqual(table, lua:type(L, 1)),
    ?assertEqual(ok, lua:getfield(L, 1, "foo")),
    ?assertEqual(true, lua:toboolean(L, 2)).

concat_test() ->
    {ok, L} = lua:new_state(),
    lua:pushlstring(L, <<"ya">>),
    ?assertEqual(2, lua:objlen(L, 1)),
    lua:pushlstring(L, <<"dda">>),
    ?assertEqual(3, lua:objlen(L, 2)),
    ?assertEqual(ok, lua:concat(L, 2)),
    ?assertEqual(<<"yadda">>, lua:tolstring(L, 1)),
    ?assertEqual(5, lua:objlen(L, 1)).

call_test() ->
    {ok, L} = lua:new_state(),
    ?assertEqual(ok, lua:getfield(L, global, "type")),
    ?assertEqual(function, lua:type(L, 1)),
    ?assertEqual(ok, lua:pushnumber(L, 1)),
    ?assertEqual(ok, lua:call(L, 1, 1)),
    ?assertEqual(<<"number">>, lua:tolstring(L, 1)),
    lua:close(L).
    
set_get_global_test() ->
    {ok, L} = lua:new_state(),
    ?assertEqual(ok, lua:pushnumber(L, 23)),
    ?assertEqual(ok, lua:setfield(L, global, "foo")),
    ?assertEqual(ok, lua:getfield(L, global, "foo")),
    ?assertEqual(23, lua:tonumber(L, 1)),
    lua:close(L).

%% =============================================================================
%% Helpers
%% =============================================================================

push_to_helper(Val, Push, To) ->
    {ok, L} = lua:new_state(),
    ?assertEqual(ok, lua:Push(L, Val)),
    ?assertEqual(Val, lua:To(L, 1)),
    % Test lual:push_args/2
    ?assertEqual(ok, lual:push_args(L, Val)),
    ?assertEqual(Val, lua:To(L, 2)),
    lua:close(L).

type_test_helper(PushFun, Type) ->
    {ok, L} = lua:new_state(),
    ?assertEqual(ok, lua:PushFun(L)),
    ?assertEqual(Type, lua:type(L, 1)),
    lua:close(L).

type_test_helper(Value, PushFun, Type) ->
    {ok, L} = lua:new_state(),
    ?assertEqual(ok, lua:PushFun(L, Value)),
    ?assertEqual(Type, lua:type(L, 1)),
    % Test lual:push_args/2
    ?assertEqual(ok, lual:push_args(L, Value)),
    ?assertEqual(Type, lua:type(L, 2)),
    lua:close(L).

