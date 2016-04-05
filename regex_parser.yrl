%%% -*- erlang -*-
%%% @doc       Parser for the riak Time Series Query Language.
%%% @author    gguthrie@basho.com
%%% @copyright (C) 2016 Basho

Nonterminals

Generator
Match
Groups
Group

.

Terminals

%dash
open
close
%open_sq
%close_sq
%pipe
%caret
%backslash
%question
%dot
%tilda
%plus
%underscore

stuff

.

Rootsymbol Generator.
Endsymbol '$end'.

Generator -> Match  : finalise(make_gen('$1')).
Generator -> Groups : finalise(make_groups('$1')).

Groups -> Group : ['$1'].
Groups -> Groups Group : '$1' ++ ['$2'].

Group -> open Match close : log("make_gen", make_gen('$2')).

Match -> stuff : ['$1'].
Match -> Match stuff : '$1' ++ ['$2'].
    
Erlang code.

make_gen(Tokens) ->
    io:format("Tokens is ~p~n", [Tokens]),
    {_, Chars} = lists:unzip(Tokens),
    lists:flatten(Chars).

make_groups(Groups) ->
    lists:flatten(Groups).

finalise(String) ->
    String ++ ".".

log(Msg, X) ->
    io:format(Msg, []),
    X.
