-module(lual_test).

-include_lib("eunit/include/eunit.hrl").

number_test() -> sah(9).
float_test() -> sah(9.21).
neg_float_test() -> sah(-9.21).
binary_test() -> sah(<<"hiho">>).
atom_test() -> sah(hiho, <<"hiho">>).

proplist1_test() -> sah([{1, 1}]).
proplist2_test() -> sah([{1,1}, {2, 2}]).
proplist3a_test() -> sah([{1,1},{2,<<"x">>}]).
proplist3b_test() -> sah([{1,1},{2,'x'}], [{1,1},{2,<<"x">>}]).

tuple1_test() -> sah({1}, [{1, 1}]).
tuple2_test() -> sah({1,2}, [{1,1}, {1,2}]).
tuple3a_test() -> sah({1,<<"x">>}, [{1,1},{2,<<"x">>}]).
tuple3b_test() -> sah({1,'x'}, [{1,1},{2,<<"x">>}]).


%% @doc Single Arg Helper
sah(Val) ->
    sah(Val, Val).
sah(Val, Expect) ->
    {ok, L} = lua:new_state(),
    lual:dostring(L, <<"function t(c) return c end">>),
    ?assertEqual(Expect, lual:call(L, "t", [Val], 1)),
    lua:close(L).
