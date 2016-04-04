#!/usr/bin/env escript
%% -*- erlang -*-
%% -------------------------------------------------------------------
%%
%% An escript that compiles a lexer into a set of QuickCheck generators
%% for the eqc_grammar module of QuickCheck
%%
%% Create the lexer_compiler_for_eqc command by running:
%%   >./rebar escriptize
%%
%% Usage
%%   >./lexer_compiler_for_eqc path/to/my/lexer.xrl path/to/output.file
%%
%% Copyright (c) 2016 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

main([Filename]) ->
    Root = filename:rootname(Filename),
    {ok, IoDev} = file:open(Filename, [read]),
    {Lexer, Transforms} = read(IoDev, start, [], []),
    io:format("Transforms is ~p~n", [Transforms]),
    ok = file:close(IoDev),
    Tokens = relex_terminals(Lexer, []),
    Transforms2 = [string:strip(X, right, $\n) || X <- Transforms],
    io:format("Transforms2 is ~p~n", [Transforms2]),
    Lookup = relex_transforms(merge_lines(Transforms2, []), []),
    io:format("Lookup is ~p~n", [Lookup]),
    ok = compile(Tokens, Root, [], []),
    ok;
main(_Other) ->
    io:format("Invalid invocation of this escript.~n~nUsage::~n" ++
                  "> ./lexer_compiler_for_eqc.escript path/to/my/lexer.xrl~n"),
    halt(1).

read(Io, Mode, Toks, Trans) ->
    case file:read_line(Io) of
        eof        -> exit("Should not have finished the file");
        {ok, L} -> L2 = string:to_lower(string:strip(string:strip(L, right, $\n))),
                   case L2 of
                       "definitions." -> read(Io, terminals, Toks, Trans);
                       "rules."       -> read(Io, rules, Toks, Trans);
                       "erlang code." -> {lists:reverse(Toks), lists:reverse(Trans)};
                       ""             -> read(Io, Mode, Toks, Trans);
                       _              -> case Mode of
                                             start     -> read(Io, Mode, Toks, Trans);
                                             terminals -> read(Io, Mode, [L | Toks], Trans);
                                             rules     -> read(Io, Mode, Toks, [L | Trans])
                                        end
                   end
    end.

%% you can have line breaks in the lexer - we want to remove them
%% this function merges lines if the first line doens't terminate in
%% a full stop
merge_lines([], Acc) ->
    lists:reverse(Acc);
merge_lines([H | []], Acc) ->
    lists:reverse([H | Acc]);
merge_lines([H1, H2 | T], Acc)->
    case is_terminated(H1) of
        true  -> merge_lines([H2 | T], [H1 | Acc]);
        false -> merge_lines([H1 ++ " " ++ H2 | T], Acc)
    end.

is_terminated(Line) ->
    case hd(lists:reverse(Line)) of
        $. -> true;
        _  -> false
    end.             

relex_transforms([], Acc) ->
    lists:reverse(Acc);
relex_transforms([[$% | _Rest] | T], Acc) ->
    relex_transforms(T, Acc);    
relex_transforms([L | T], Acc) ->
    io:format("L is ~p~n", [L]),
    [Gen | Terminal] = string:tokens(L, ":"),
    Terminal2 =  lists:flatten(Terminal),
    io:format("Gen is ~p Terminal is ~p~n", [Gen, Terminal2]),
    AST = string_to_AST(Terminal2),
    case AST of
        [{tuple, _, [_, {tuple, _, [{atom, _, Terminal3}, _]}]}] ->
            relex_transforms(T, [{strip_gen(Gen), Terminal3} | Acc]);
        _Other ->
            relex_transforms(T, Acc)
    end.

strip_gen(String) ->
    string:strip(string:strip(string:strip(String), left, ${), right, $}).

string_to_AST(String) ->
    {ok, Toks, _} = erl_scan:string(String),
    {ok, AbsForm} = erl_parse:parse_exprs(Toks),
    AbsForm.

relex_terminals([], Acc) ->
    lists:reverse(Acc);
relex_terminals([H | T], Acc) ->
    L = string:strip(string:strip(H, right, $\n)),
    case L of
        [$% | _Rest] -> relex_terminals(T, Acc);
        _            -> [Gen | RegEx] = string:tokens(L, "="),
                        Gen2 = string:strip(Gen),
                        %% some regex's contain equals - only interested in the first one
                        RegEx2 = string:strip(string:join(RegEx, "=")),
                        relex_terminals(T, [{Gen2, RegEx2} | Acc])
    end.

compile([], Root, Exports, Fns) ->
    Base = filename:basename(Root) ++ "_compiler",
    _Mod = string:join([
                       "%% Generated file",
                       "-module(" ++ Base ++ ").",
                       "-exports([" ++ string:join(lists:reverse(Exports), ",") ++ "])."
                      ] ++ lists:reverse(Fns), "~n~n"),
    %%io:format(Mod),
    ok;
compile([{Gen, RegEx} | T], File, Exports, Fns) ->
    Export = make_export(Gen),
    Fn = make_fn(Gen, RegEx),
    compile(T, File, [Export| Exports], [Fn | Fns]).

make_export(Gen) -> "'" ++ Gen ++ "'/0".

make_fn(Gen, RegEx) ->
    "'" ++ Gen ++ "'() ->~n" ++ make_gen(RegEx).

make_gen(RegEx) ->
    %% io:format("RegEx is ~p~n", [RegEx]),
    "erk.".
