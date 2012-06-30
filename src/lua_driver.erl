-module(lua_driver).

-export([open/0, close/1]).

-include("lua.hrl").

-spec open() -> {ok, lua:lua()} | {error, string()}.
open() ->
    case load_driver() of
        {ok, Port} -> #lua{port=Port};
        E = {error, _} -> E
    end.

-spec close(lua:lua()) -> true.
close(#lua{port=Port}) ->
    port_close(Port).


%% Private functions
-spec load_driver() -> {ok, port()} | {error, string()}.
load_driver() ->
    SearchDir = filename:join([filename:dirname(code:which(lua_driver)), "..", "priv"]),
    case erl_ddll:load(SearchDir, "liberlua") of
        ok ->
            {ok, open_port({spawn, "liberlua"}, [binary])};
        {error, Error} ->
            {error, erl_ddll:format_error(Error)}
    end.
