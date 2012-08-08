%%% @doc Parse_transform which implements a given behaviour.
%%%
%%% What it does for the module:
%%% 1. Find out which behaviour it implements: {attribute,_,behaviour,Mod}
%%% 2. Call Mod:behaviour_info(callbacks)
%%% Latter function returns a list of functions/arities which must be
%%% implemented in this module. For example: [{add, 2}, {sin, 1}].
%%% 3. Implement every function in that behaviour, so it looks like this:
%%%
%%% add(Arg1, Arg2) ->
%%%     luam:one_call(FileName, "add", [Arg1, Arg2]).
%%% 
%%% FileName is the backing Lua module, in which the module is actually
%%% implemented. It is defined in -implemented_in(String) attribute.
%%%
%%% Assumptions about the parse_transformed module:
%%% 1. Has -behaviour(some_behaviour).
%%% 2. some_behaviour is compiled
%%%     (you can achieve this using erl_first_files in rebar.config)
%%% 3. Has -implemented_in(String | {priv, String}).
%%% 4. String is a single Erlang expression or String (see below).
%%%
%%% Filename is written as abstract form in -implemented_in(String) attribute.
%%% In simple words, String must be 1 Erlang expression.
%%% These Strings are valid (wrapped in -implemented_in):
%%% * "\"crutas_calc.lua\".".
%%% * "code:priv_dir(crutas) ++ \"/crutas_calc.lua\"."
%%% * "fun() -> ok, code:priv_dir(crutas) ++ \"/crutas_calc.lua\" end()."
%%% Note that trailing dots are important.
%%%
%%% A convenient alias for
%%%
%%% -implemented_in("filename:join(filename:dirname(filename:dirname("
%%%                 "code:which(?MODULE))), \"priv\") ++ \"/FILE.lua\".").
%%% Is this:
%%% -implemented_in({priv, "/FILE.lua"}).
%%%
%%% The latter is recommended.
-module(lua_behaviour).

-export([parse_transform/2]).

parse_transform(Ast, _Opts) ->
    {Ret, _Opt} = lists:foldl(fun walk_ast/2, {[], []}, Ast),
    %io:format("Opt: ~p~nRet: ~p~n", [Opt, Ret]),
    lists:reverse(Ret).

walk_ast(N={attribute, _, module, Mod}, {AstOut, PL}) ->
    {[N|AstOut], [{module, Mod}|PL]};

walk_ast(N={attribute, _, behaviour, Mod}, {AstOut, PL}) ->
    {[N|AstOut], [{behaviour, Mod}|PL]};

walk_ast(N={attribute, _, implemented_in, Eval}, {AstOut, PL}) ->
    LuaModABS = get_lua_mod_expr(get_value(module, PL), Eval),
    {[N|AstOut], [{implemented_in, LuaModABS}|PL]};

walk_ast({eof, L}, {AstOut, PL}) ->
    Funs = (get_value(behaviour, PL)):behaviour_info(callbacks),
    Export = {attribute,L,export,Funs},
    {EL, FunNodes} = make_fun_nodes(L, Funs, get_value(implemented_in, PL)),
    {lists:append([[{eof, EL}], FunNodes, [Export], AstOut]), PL};

walk_ast(Node, {AstOut, PL}) ->
    {[Node|AstOut], PL}.

get_value(Key, PL) ->
    Ref = make_ref(),
    case proplists:get_value(Key, PL, Ref) of
        Ref -> error({keyerror, Key, PL});
        Ret -> Ret
    end.

make_fun_nodes(L, Funs, ImplementedIn) ->
    LinesFuns = lists:zip(lists:seq(L+1, L + length(Funs)*2, 2), Funs),
    ABSFuns = [make_fun(Line, Fun, Arity, ImplementedIn)
        || {Line, {Fun, Arity}} <- LinesFuns],

    %io:format("LinesFuns: ~p~n, Abstract funs: ~p~n", [LinesFuns, ABSFuns]),
    {
        L + length(Funs) * 2 + 1,
        lists:reverse(ABSFuns)
    }.

make_fun(L, Name, Arity, LuaModABS) ->
    Args = [list_to_atom("Arg"++integer_to_list(I)) || I <- lists:seq(1,Arity)],
    Header = [{var, L, Arg} || Arg <- Args],
    CallArgs = erl_parse:abstract(Args),
    {function,L,Name,Arity,
        [{clause,L,
                %[{var,8,'Arg1'},{var,8,'Arg2'}],
                Header,
                [],
                [{call,L+1, {remote,L+1, {atom,L+1,luam},{atom,L+1,one_call}},
                        [
                            LuaModABS,
                            {string,L+1,atom_to_list(Name)},
                            %{cons,9,
                            %    {var,9,'Arg1'},
                            %    {cons,9,{var,9,'Arg2'},{nil,9}}}
                            CallArgs
                        ]}]}]}.

%% @doc takes -implemented_in(Eval) and returns abstract form for module
%%
%% Simple string is not enough, since we do not want to hardcode full paths.
%% code:priv_dir/1 rarely works, so we have to do some hacking.
get_lua_mod_expr(ModAtom, Eval) ->
    % Convert shortcut {priv, FileRelativeToPrivDir} to UglyString
    Mod = atom_to_list(ModAtom),
    LuaModExpr = case Eval of
        {priv, File} -> 
            "filename:join(filename:dirname(filename:dirname(code:which("
            ++ Mod ++ "))), \"priv\") ++ \"" ++ File ++ "\".";
        _ when is_list(Eval) ->
            % ?MODULE in parse_transform does not mean anything. Replace
            % to what we have.
            % TODO: make re:replace at word boundaries.
            binary_to_list(iolist_to_binary(re:replace(Eval, "\\?MODULE", Mod)))
    end,

    %io:format("LuaModExpr: ~p~n", [LuaModExpr]),
    {ok, T, _} = erl_scan:string(LuaModExpr),
    case erl_parse:parse_exprs(T) of
        {ok, [M]} -> M;
        {ok, [_|_]} -> error(one_expression_allowed_in_implemented_in);
        {error, E} -> error({syntax_error_in_implemented_in_header, E})
    end.
