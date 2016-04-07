%%% -*- erlang -*-
%%% @doc       Parser for the riak Time Series Query Language.
%%% @author    gguthrie@basho.com
%%% @copyright (C) 2016 Basho

Nonterminals

Generator
Matchs
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
pipe
%caret
%backslash
%question
%dot
%tilda
%plus
%underscore

chars

.

Rootsymbol Generator.
Endsymbol '$end'.

Generator -> Matchs : finalise(make_gen('$1')).
Generator -> Groups : finalise(make_groups('$1')).

Groups -> Group             : ['$1'].
Groups -> Groups Group      : '$1' ++ ['$2'].
Groups -> Groups pipe Group : make_or('$1', ['$3']).

Group -> open Matchs close : log("make_gen~n", make_gen('$2')).

Matchs -> Matchs pipe Matchs : log("Matches (3)~n", make_or('$1', ['$3'])).

Matchs -> Match             : log("Matches (1)~n", ['$1']).
Matchs -> Matchs chars      : log("Matches (2)~n", '$1' ++ ['$2']).

Match -> chars            : '$1'.

Erlang code.

make_or(A, B) ->
    io:format("make an or around ~p ~p~n", [A, B]),
    A ++ B.

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
