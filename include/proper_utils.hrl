-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%-define(mem(), ?info("Memory usage", [erlang:memory()])).
-define(mem(), ok).
-define(prop_check(Fun), ?prop_check(Fun, [])).
-define(prop_check(Fun, Opts),
        fun() ->
                proper_utils:run_proper(function,
                    fun() ->
                            R = proper:quickcheck(Fun(), Opts), ?mem(), R
                    end)
        end).

-ifdef(PROPER_MODULE_TESTS).

proper_test_() ->
    {timeout, 3600, fun() ->
                proper_utils:run_proper(module,
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
            proper_utils:run_proper(module, fun() -> proper:check_specs(Mod) end)
        end
    }.

-endif.
