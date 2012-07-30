-module(lual_test).

-include_lib("eunit/include/eunit.hrl").

failure_test() ->
    {ok, L} = lua:new_state(),
    ?assertThrow(_, lual:dostring(L, <<"syntax @#%^&#@$@ error!!!">>)).
