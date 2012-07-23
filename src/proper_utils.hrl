-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%-define(mem(), ?info("Memory usage", [erlang:memory()])).
-define(mem(), ok).
-define(prop_check(Fun), ?prop_check(Fun, [])).
-define(prop_check(Fun, Opts),
        fun() ->
                run_proper(function,
                    fun() ->
                            R = proper:quickcheck(Fun(), Opts), ?mem(), R
                    end)
        end).

%% @doc Changes group leader to proper, runs tests and gives group leader back
%% to eunit. Needed for IO outputing.
-spec run_proper(module | function, fun()) -> none().
run_proper(What, Fun) ->
    EunitLeader = erlang:group_leader(),
    erlang:group_leader(whereis(user), self()),
    _ExpectedReturn = case What of
        module -> [];
        function -> true
    end,
    Fun(),
    timer:sleep(100),
    erlang:group_leader(EunitLeader, self()).

-ifdef(PROPER_MODULE_TESTS).

proper_test_() ->
    {timeout, 3600, fun() ->
                run_proper(module,
                    fun() ->
                            ?assertEqual([], proper:module(?MODULE, [{max_size, 8}]))
                    end)
        end
    }.

-endif.

-ifdef(PROPER_SPEC_TESTS).

get_module_name(Mod) ->
    list_to_atom(lists:nth(1, string:tokens(atom_to_list(?MODULE), "_"))).

proper_spec_test_() ->
    Mod = get_module_name(?MODULE),
    {timeout, 3600, fun () ->
            run_proper(module, fun() -> proper:check_specs(Mod) end)
        end
    }.

-endif.
