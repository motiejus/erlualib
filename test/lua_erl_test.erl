-module(lua_erl_test).

-include_lib("eunit/include/eunit.hrl").

example1_test() ->
    {ok, L} = lua:new_state(),
    ?assertEqual({<<"number">>}, lua_erl:call(L, type, [23], 1)),
    lual:dostring(L, <<"function add(a, b, c) return a + b + c end">>),
    ?assertEqual({9}, lua_erl:call(L, add, [2, 3, 4], 1)),
    lua:close(L).

example2_test() ->
    {ok, L} = lua:new_state(),
    ?assertEqual({<<"number">>}, lua_erl:call(L, type, [23])),
    lual:dostring(L, <<"function add(a, b, c) return a + b + c end">>),
    ?assertEqual({9}, lua_erl:call(L, add, [2, 3, 4])),
    lua:close(L).
