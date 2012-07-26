-module(luam_test).

%-define(PROPER_MODULE_TESTS, 1).
%-include("proper_utils.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

ns() -> {ok, L} = lua:new_state(), L.
oh_test_() ->
    [
        {"fold", ?_test(fold(ns()))},
        {"fold nested", ?_test(fold_nested(ns()))},
        {"push arg empty table", ?_test(pushterm_empty_table(ns()))},
        {"push arg table 1", ?_test(pushterm_table1(ns()))},
        {"push arg nested table", ?_test(pushterm_nested_table(ns()))},
        {"multicall 0", ?_test(multicall_0(ns()))},
        {"multicall 1", ?_test(multicall_1(ns()))},
        {"multicall 2", ?_test(multicall_2(ns()))}
    ].

luam_call_test_() ->
    [
        {"single nil", ?_assertEqual(nil, luam_call(['nil']))},
        {"single atom", ?_assertEqual(<<"x">>, luam_call(['x']))},
        {"1 number", ?_assertEqual(nil, luam_call([nil]))},
        {"number and string", ?_assertEqual(
                {4, <<"bac">>}, luam_call([4, <<"bac">>]))},
        {"numeric proplist", ?_assertEqual(
                [{1, 4}], luam_call([[{1, 4}]]))
        },
        {"string proplist", ?_assertEqual(
                [{<<"x">>, <<"y">>}], luam_call([[{'x', 'y'}]]))},
        {"2 numeric arguments", ?_assertEqual({1, 2}, luam_call([1, 2]))},
        {"number and empty table", ?_assertEqual({1, []}, luam_call([1, []]))},
        {"3 booleans and number", ?_assertEqual(
                {true, false, true, 4}, luam_call([true, false, true, 4]))},
        {"number and table", ?_assertEqual(
                {5, [{<<"x">>, <<"y">>}]}, luam_call([5, [{'x', 'y'}]]))}
    ].

proper_test_() ->
    {timeout, 3600, fun() ->
                proper_utils:run_proper(module,
                    fun() ->
                            ?assertEqual([], proper:module(?MODULE,
                                    [{max_size, 8}, {numtests, 1000}]))
                    end)
        end
    }.

prop_luam_call() ->
    ?FORALL(Args,
        ?SUCHTHAT(Args, list(arg()), valid(Args)
        ),
        begin
                Ret = case luam_call(Args) of
                    X when is_tuple(X) -> tuple_to_list(X);
                    X -> [X]
                end,

                Conv = lists:map(fun arg_to_ret/1, Args),
                %io:format("Arg: ~p, Ret: ~p, Conv: ~p~n", [Args, Ret, Conv]),
                lists:map(fun sorted/1, Ret) =:= lists:map(fun sorted/1, Conv)
        end).


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

fold_nested(L) ->
    lua:createtable(L, 0, 2),
    lua:pushlstring(L, <<"k">>),
    lua:createtable(L, 0, 2),
    lua:pushlstring(L, <<"vienas">>),
    lua:pushlstring(L, <<"1">>),
    lua:settable(L, 3),
    lua:settable(L, 1),
    ?assertEqual(1, lua:gettop(L)),
    R = luam:fold(fun(K, V, Acc) -> [{K, V}|Acc] end, [], L, 1),
    ?assertEqual(
        lists:sort([{<<"k">>, [{<<"vienas">>, <<"1">>}]}]),
        lists:sort(R)
    ),
    ?assertEqual(1, lua:gettop(L)).

pushterm_empty_table(L) ->
    luam:pushterm(L, {}),
    ?assertEqual(1, lua:gettop(L)),
    ?assertEqual(0, lua:objlen(L, 1)).

pushterm_table1(L) ->
    luam:pushterm(L, {true, {}, <<"yadda">>}),
    ?assertEqual(1, lua:gettop(L)),
    ?assertEqual(3, lua:objlen(L, 1)),
    lua:pushnumber(L, 1), lua:gettable(L, 1), % push t[1]
    ?assertEqual(boolean, lua:type(L, 2)),
    lua:pushnumber(L, 2), lua:gettable(L, 1), % push t[2]
    ?assertEqual(table, lua:type(L, 3)),
    lua:pushnumber(L, 3), lua:gettable(L, 1), % push t[3]
    ?assertEqual(string, lua:type(L, 4)).

pushterm_nested_table(L) ->
    luam:pushterm(L, {{true, nil}}),
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

luam_call(Args) ->
    {ok, L} = lua:new_state(),
    ok = lual:dostring(L, <<"function t(...) return ... end">>),
    R = luam:call(L, "t", Args),
    lua:close(L),
    R.

-type arg() :: binary()               | % string
               atom()                 | % string
               number()               | % number
               list({arg(), arg()})   | % associative table
               tuple(arg()).            % indexed table

%% @doc Disallow proplists with non-unique keys
valid(Args) when is_list(Args) ->
    % Check if every argument is valid
    lists:all(fun(Arg) -> valid_arg(arg_to_ret(Arg)) end, Args).

valid_arg(Ret) when is_list(Ret) ->
    NormalizeKeys = lists:map(fun({K, V}) -> {remove_0s(K), V} end, Ret),
    (length(NormalizeKeys) =:= length(proplists:get_keys(NormalizeKeys))) andalso
    lists:all(fun({K, V}) -> valid_arg(K) and valid_arg(V) end, NormalizeKeys);

valid_arg(_Ret) ->
    true.

%% @doc Sort inner proplists for comparison
sorted(List) when is_list(List) ->
    Inner = lists:map(fun({K, V}) -> {sorted(K), sorted(V)} end, List),
    lists:keysort(1, Inner);

sorted(X) ->
    X.


arg_to_ret(A) when A =:= nil; is_boolean(A); is_number(A); is_binary(A) ->
    A;

arg_to_ret(A) when is_atom(A) ->
    list_to_binary(atom_to_list(A));

arg_to_ret(A) when is_tuple(A) ->
    arg_to_ret(lists:zip(lists:seq(1, size(A)), tuple_to_list(A)));

arg_to_ret(A) when is_list(A) ->
    [{arg_to_ret(K), arg_to_ret(V)} || {K, V} <- A].

%% @doc Remove trailing zeroes from binary
remove_0s(Binary) when is_binary(Binary) ->
    F = fun(0) -> true; (_) -> false end,
    RevArr = lists:reverse(binary_to_list(Binary)),
    RetArr = lists:reverse(lists:dropwhile(F, RevArr)),
    list_to_binary(RetArr);

remove_0s(L) ->
    L.
