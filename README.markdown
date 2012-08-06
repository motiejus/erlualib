Erlualib enables us to implement arbitrary Erlang behavious in Lua. This
library makes embedding Lua code to Erlang codebase very easy. How to do it:

1. Create an Erlang module which you want to implement in Lua
2. Add 4 lines:
    1. `-module(my_mod).`
    2. `-behaviour(abitrary_behaviour).`
    3. `-implemented_in({priv, "/module_impl.lua"}). % where to forward calls`
    4. `-compile({parse_transform, lua_behaviour}). % this does the hard work`
3. Compile and use `my_mod` as if it was written in pure Erlang.

Below is an example of simple key-value name server. It has 2 operations:

* `{add_name, Key :: binary(), Value :: binary()} -> ok.`
* `{get_addr, Key :: binary()} -> 'error' | Value :: binary().`

==== `name_server.erl` ====

```erlang
-module(name_server).
-behaviour(gen_server).
-implemented_in({priv, "/name_server.lua"}).
-compile({parse_transform, lua_behaviour}).
```

==== `priv/name_server.lua` ====

```lua
function init()
    return erlang.atom("ok"), {} -- This empty table will be our state
end
-- Forwards the call to function which is specified in req[1]. Returns
-- {reply, Return, NewTable}. Return and NewTable are values from the
-- forwarded module.
function handle_call(req, from, tbl)
    return erlang.atom("reply"), call(tbl, req)
end
-- Adds name to State. Returns {ok, Table}.
function add_name(tbl, name, address)
    tbl[name] = address
    return erlang.atom("ok"), tbl
end
-- Gets name table and current name. Returns: { 'error' | Value, Table}
function get_addr(tbl, name)
    return tbl[name] or erlang.atom("error"), tbl
end
-- Call req[1](tbl, req[2], req[3], ...)
function call(tbl, req) return _G[req[1]](tbl, unpack(req, 2)) end
```

That's it! Compile `name_server.erl` and call it. Alternatively, download the
[example][erlualib_examples] and `make test`.

Performance
===========
`luam:one_call/3` (the "do all" function) consists of 3 parts:

1. `lua:new_state/0`, takes ~220-250µs.
2. `luam:call/4`, takes ~12-15µs.
3. `lua:close/1`, negligible.

For `lua_behaviour`, new Lua state is created on every request, which
adds significant overhead. In `gen_(server|fsm)` case (as well as many
others), it will be possible to reuse the state, and performance will be
much, much better. I just need to create `gen_(server|fsm)` specific
`parse_transform` for it, which is planned for near future.

Tested on Intel Core 2 Q9400. Performance tests are not scientific, in order to
give general overview.  Tests were executed serially, with SMP support. More
scientific benchmarks are upcoming.

Type conversions Lua -> Erlang
==============================
As you already noticed in the Lua example, "erlang" lua library is
available! Now it has only one method `atom`, which takes a string and
returns atom. Analogue `tuple` method is planned (now, when you return
an indexed table in Lua, it is treated as a proplist with numeric
indices, so there is no way to return nested tuples).

Error handling
==============

When Lua code encounters an error in `luam:call/3`, `lua:multipcall/2` or
`lua:one_call/3`, Erlang exception is thrown: `{lua_error, Error}`, where
`Error` is a `string()`, the exception value from Lua.

Some words about erlualib
=========================
erlualib is a library for embedding Lua into Erlang. It provides a
simple interface that is very similar to the Lua C API, as well as some
very useful high-level functions.

This is a fork of Ray Morgan's Erl-Lua library with the following
changes:

* High test coverage (and PropEr tests)
* New low-level commands
* Strings in Lua are Binaries in Erlang (instead of lists of numbers)
* Many bugfixes
* Dialyzer is happy about this project
* Rebarized
* `luam:call/4`.
* arbitrary erlang behaviours in Lua

Erlualib is a nice example how and when to PropErly test things. Tests
for `parse_transforms` are coming soon.

Lower level function examlpes
=============================

Example how to use `luam:call/4`:

    1> {ok, L} = lua:new_state(),
    2> ok = lual:dostring(L, <<"function t(when, tab) return tab[when] end">>),
    3> Args = [noon, [{ morning, breakfast }, { noon, lunch }, {evening, dinner} ] ],
    4> luam:call(L, "t", Args),
    <<"lunch">>.

Gist: you can pass (almost) arbitrary Erlang values to the Lua call, and get
(almost) arbitrary values back, deserialized, after the call. This will be
especially powerful combined with with [Erlang behaviours in Lua]. As said,
stay tuned.

Low level API example:

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

To test the whole project and see eUnit and PropEr in action, run:

    make test

Compatibility
=============

* `liblua 5.1` fully supported
* `liblua 5.2` backport done, but unstable. Has _some issues with memory handling_.

This project is continuously tested in [Travis][Build Status]. Erlualib with
`liblua 5.1` passes all tests, whereas with `liblua 5.2` it sometimes segfaults
while cleaning up the state. Not sure if it is a bug in Lua or Erlualib.
Investigation in progress. Many thanks to [PropEr] for catching this.

[![Build Status](https://secure.travis-ci.org/Motiejus/erlualib.png)]

[erlualib_examples]: https://github.com/Motiejus/erlualib_examples
[Erl-Lua]: https://github.com/raycmorgan/erl-lua/
[Erlang behaviours in Lua]: http://m.jakstys.lt/tech/2012/06/erlang-behaviours-in-lua/
[PropEr]: http://proper.softlab.ntua.gr/
[Build Status]: http://travis-ci.org/Motiejus/erlualib
