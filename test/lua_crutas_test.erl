-module(lua_crutas_test).

-include_lib("eunit/include/eunit.hrl").

put_atom_test() ->
    {ok, L} = lua:new_state(),
    ok = lual:dostring(L, <<"function t(arg) return erlang.atom(arg) end">>),
    lua:getglobal(L, "t"),
    lua:pushlstring(L, <<"a1">>),
    ?assertEqual(false, luam:maybe_atom(L, 1)),
    lua:call(L, 1, 1),
    ?assertEqual(1, lua:gettop(L)),
    ?assertEqual(user_data, lua:type(L, 1)),
    ?assertEqual({ok, a1}, luam:maybe_atom(L, 1)).

luam_call_with_atom_test() ->
    {ok, L} = lua:new_state(),
    ok = lual:dostring(L, <<"function t(arg) return erlang.atom(arg) end">>),
    ?assertEqual(yadda, luam:call(L, "t", [<<"yadda">>])).
