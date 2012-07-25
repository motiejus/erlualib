-module(lual_test).

-include_lib("eunit/include/eunit.hrl").

failure_test() ->
    {ok, L} = lua:new_state(),
    ?assertMatch({error, _}, lual:dostring(L, <<"syntax @#%^&#@$@ error!!!">>)).
