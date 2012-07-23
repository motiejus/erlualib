-module(proper_utils).

-export([run_proper/2]).

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
