-module(luam_test).

-include_lib("eunit/include/eunit.hrl").

ns() -> {ok, L} = lua:new_state(), L.
oh_test_() ->
    [
        {"fold", ?_test(fold(ns()))},
        {"push arg empty table", ?_test(push_arg_empty_table(ns()))},
        {"push arg table 1", ?_test(push_arg_table1(ns()))},
        {"push arg nested table", ?_test(push_arg_nested_table(ns()))},
        {"multicall 0", ?_test(multicall_0(ns()))},
        {"multicall 1", ?_test(multicall_1(ns()))},
        {"multicall 2", ?_test(multicall_2(ns()))}
    ].

fold(L) ->
    lua:createtable(L, 0, 2),
    lua:pushlstring(L, <<"vienas">>),
    lua:pushlstring(L, <<"1">>),
    lua:settable(L, 1),
    lua:pushlstring(L, <<"du">>),
    lua:pushlstring(L, <<"2">>),
    lua:settable(L, 1),
    ?assertEqual(1, lua:gettop(L)),
    R = luam:fold(fun(K, V, Acc) -> [{K, V}|Acc] end, [], L, 1),
    ?assertEqual(
        lists:sort([{<<"vienas">>, <<"1">>}, {<<"du">>, <<"2">>}]),
        lists:sort(R)
    ),
    ?assertEqual(1, lua:gettop(L)).

push_arg_empty_table(L) ->
    luam:push_arg(L, {}),
    ?assertEqual(1, lua:gettop(L)),
    ?assertEqual(0, lua:objlen(L, 1)).

push_arg_table1(L) ->
    luam:push_arg(L, {true, {}, <<"yadda">>}),
    ?assertEqual(1, lua:gettop(L)),
    ?assertEqual(3, lua:objlen(L, 1)),
    lua:pushnumber(L, 1), lua:gettable(L, 1), % push t[1]
    ?assertEqual(boolean, lua:type(L, 2)),
    lua:pushnumber(L, 2), lua:gettable(L, 1), % push t[2]
    ?assertEqual(table, lua:type(L, 3)),
    lua:pushnumber(L, 3), lua:gettable(L, 1), % push t[3]
    ?assertEqual(string, lua:type(L, 4)).

push_arg_nested_table(L) ->
    luam:push_arg(L, {{true, nil}}),
    ?assertEqual(1, lua:gettop(L)),
    lua:pushnumber(L, 1), lua:gettable(L, 1),
    lua:pushnumber(L, 1), lua:gettable(L, 2),
    ?assertEqual(boolean, lua:type(L, -1)),
    lua:pushnumber(L, 2), lua:gettable(L, 2),
    ?assertEqual(nil, lua:type(L, -1)).

multicall_0(L) ->
    ok = lual:dostring(L, <<"function t(...) local noop end">>),
    lua:getglobal(L, "t"),
    ?assertEqual(0, luam:multicall(L, 0)),
    ?assertEqual(0, lua:gettop(L)).


multicall_1(L) ->
    ok = lual:dostring(L, <<"function t(...) return (1) end">>),
    lua:getglobal(L, "t"),
    ?assertEqual(1, luam:multicall(L, 0)),
    ?assertEqual(1, lua:gettop(L)),
    ?assertEqual(number, lua:type(L, -1)). % return value

multicall_2(L) ->
    ok = lual:dostring(L, <<"function t(...) return 1, 2 end">>),
    lua:getglobal(L, "t"),
    ?assertEqual(2, luam:multicall(L, 0)),
    ?assertEqual(2, lua:gettop(L)),
    ?assertEqual(number, lua:type(L, -1)), % return value
    ?assertEqual(number, lua:type(L, -2)). % return value

luam_call_test_() ->
    [
        {"[nil] -> {nil}", ?_assertEqual({nil}, luam_call([nil]))},
        {"[1] -> {1}", ?_assertEqual({nil}, luam_call([nil]))},
        {"[[{1, 4}]] -> {[{1, 4}]}",
            ?_assertEqual({[{1, 4}]}, luam_call([[{1, 4}]]))}
    ].

%number_test() -> sah([1], {1}).
%nil_test() -> sah([nil], {nil}).
%proplist1_test() -> sah([{1, 1}]).
%proplist2_test() -> sah([{1,1}, {2, 2}]).
%proplist3a_test() -> sah([{1,1},{2,<<"x">>}]).
%proplist3b_test() -> sah([{1,1},{2,'x'}], [{1,1},{2,<<"x">>}]).
%
%tuple1_test() -> sah({1}, [{1, 1}]).
%tuple2_test() -> sah({1,2}, [{1,1}, {1,2}]).
%tuple3a_test() -> sah({1,<<"x">>}, [{1,1},{2,<<"x">>}]).
%tuple3b_test() -> sah({1,'x'}, [{1,1},{2,<<"x">>}]).

%number_test() -> sah(9).
%float_test() -> sah(9.21).
%neg_float_test() -> sah(-9.21).
%binary_test() -> sah(<<"hiho">>).
%atom_test() -> sah(hiho, <<"hiho">>).
%

luam_call(Args) ->
    {ok, L} = lua:new_state(),
    ok = lual:dostring(L, <<"function t(...) return ... end">>),
    R = luam:call(L, "t", Args),
    lua:close(L),
    R.
