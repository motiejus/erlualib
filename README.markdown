Erl-Lua is a library for embedding Lua into Erlang. It provides a simple
interface that is very similar to the Lua C API.

This is a fork of Ray Morgan's [Erl-Lua] library with:

* Rebar
* *Much* better test coverage (all API except for boilerplate is covered)
* Dialyzer is happy about this project
* Bugfixes
* New low-level commands
* Strings in Lua are Binaries in Erlang (instead of lists of numbers)

Major new feature:
* luam:call/4.

This is planned:
* [Erlang behaviours in Lua]

Example how to use luam:call/4:

    1> {ok, L} = lua:new_state(),
    2> ok = lual:dostring(L, <<"function t(when, tab) return tab[when] end">>),
    3> Args = [noon, [{ morning, breakfast }, { noon, lunch }, {evening, dinner} ] ],
    4> luam:call(L, "t", Args),
    {<<"lunch">>}.

Gist: you can pass (almost) arbitrary Erlang values to the Lua call, and get
(almost) arbitrary values back, deserialized.

This will be especially powerful combined with with [Erlang behaviours in Lua]. As said, stay tuned.

Older examples:

    {ok, L} = lua:new_state().
    lua:getfield(L, global, "print").
    lua:pushlstring(L, <<"Hello from Lua!">>).
    lua:call(L, 1, 0).
    % (Lua) => Hello from Lua!

    lua:getfield(L, global, "type").
    lua:pushnumber(L, 23).
    lua:call(L, 1, 1).
    S = lua:tolstring(L, 1).
    lua:remove(L, 1). % always rebalance the stack.. it is the right thing to do!
    S. % => <<"number">>

For more examples, see the tests.

There is also a simple way to run one off simple Lua code snippets:

    (continued from above)
    lual:dostring(L, <<"print 'Howdy!'">>).
    % (Lua) => Howdy!
    
Testing
=======

Code has 100% non-boilerplate test coverage, some of which are PropErly tested.
To test the whole project, run:

    make test

[Erl-Lua]: https://github.com/raycmorgan/erl-lua/
[Erlang behaviours in Lua]: http://m.jakstys.lt/tech/2012/06/erlang-behaviours-in-lua/
