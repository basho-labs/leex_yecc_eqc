%%% -*- erlang -*-
%%% @doc       Parser for the riak Time Series Query Language.
%%% @author    gguthrie@basho.com
%%% @copyright (C) 2016 Basho

Nonterminals

Generator
Matchs
Match
MatchOptions
MatchGens
Groups
Group

.

Terminals

backslash
open
close
open_sq
close_sq
dash
pipe
caret
question
dot
tilda
plus
underscore
star

chars

.

Rootsymbol Generator.
Endsymbol '$end'.

Generator -> MatchGens    : finalise('$1').
Generator -> MatchOptions : finalise('$1').
Generator -> Groups       : finalise('$1').

Groups -> Group             : '$1'.
Groups -> Groups Group      : make_and('$1', '$2').
Groups -> Groups pipe Group : make_or('$1', '$3').

Group -> open MatchGens    close : '$2'.
Group -> open MatchOptions close : '$2'.

MatchOptions -> MatchGens pipe MatchGens : make_or('$1', '$3').

MatchGens -> Matchs : make_gen('$1').

Matchs -> Match            : ['$1'].
Matchs -> Matchs Match     : '$1' ++ ['$2'].

Match -> chars : '$1'.

Match -> backslash backslash  : escape('$2').

Match -> backslash open       : escape('$2').
Match -> backslash close      : escape('$2').
Match -> backslash open_sq    : escape('$2').
Match -> backslash close_sq   : escape('$2').
Match -> backslash dash       : escape('$2').
Match -> backslash pipe       : escape('$2').
Match -> backslash caret      : escape('$2').
Match -> backslash question   : escape('$2').
Match -> backslash dot        : escape('$2').
Match -> backslash tilda      : escape('$2').
Match -> backslash plus       : escape('$2').
Match -> backslash underscore : escape('$2').
Match -> backslash star       : escape('$2').

Erlang code.

escape({_, Token}) -> {chars, Token}.

make_or(A, B) ->
    {oneof, [A, B]}.

make_and(A, B) ->
    {and_, [A, B]}.

make_dot() ->
    {dot, []}.

make_gen(Tokens) ->
    {_, Chars} = lists:unzip(Tokens),
    {match, "\"" ++ lists:flatten(Chars) ++ "\""}.

finalise(X) ->
    Flat = flatten(X),
    _Made = make(Flat) ++ ".".

make({and_, Args}) ->
    string:join([make(Arg) || Arg <- Args], " ++ ");
make({oneof, Args}) ->
    "oneof(" ++ string:join([make(Arg) || Arg <- Args], ", ") ++ ")";
make({match, Arg}) ->
    Arg.
  
flatten({Type, [{Type, Args1}, Args2]}) when Type =:= and_  orelse
                                            Type =:= oneof ->
    flatten({and_, lists:flatten([Args1, Args2])});
flatten({match, X}) ->
    {match, X};
flatten({Type, Args}) ->
    {Type, [flatten(X) || X <- Args]}.

%%
%% Debugging
%%
%% log(Msg, X) ->
%%     io:format(Msg, [X]),
%%     X.
