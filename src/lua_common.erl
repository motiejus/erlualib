-module(lua_common).

-include("lua.hrl").

-export([command/2, receive_valued_response/0, receive_simple_response/0]).

command(#lua{port=Port}, Data) ->
    port_command(Port, term_to_binary(Data)).

receive_simple_response() ->
    receive
        ok ->
            ok;
        {error, Reason} ->
            {error, Reason};
        Other ->
            {other, Other}
    after ?STD_TIMEOUT ->
        {error, timeout}
    end.

receive_valued_response() ->
    receive
        {ok, Str} ->
            Str;
        Other ->
            throw({lua_error, Other})
    after ?STD_TIMEOUT ->
            error(timeout)
    end.

