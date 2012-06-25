-module(lua_driver).

-export([open/0, close/1]).

-include("lua.hrl").

open() ->
    {ok, L} = load_driver(),
    #lua{port=L}.

close(#lua{port=Port}) ->
    port_close(Port).


%% Private functions
load_driver() ->
    SearchDir = filename:join([filename:dirname(code:which(lua_driver)), "..", "priv"]),
    case erl_ddll:load(SearchDir, liberlua) of
        ok ->
            {ok, open_port({spawn, 'liberlua'}, [binary])};
        {error, Error} ->
            {error, erl_ddll:format_error(Error)}
    end.
