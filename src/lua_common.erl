-module(lua_common).

-include("lua.hrl").

-export([command/2, receive_valued_response/0]).

command(#lua{port=Port}, Data) ->
    port_command(Port, term_to_binary(Data)).

receive_valued_response() ->
    receive
        ok -> ok;
        {ok, Str} -> Str;
        {throw, Throw} -> throw(Throw);
        Other -> throw({unknown_return, Other})
    after ?STD_TIMEOUT ->
            error(timeout)
    end.

