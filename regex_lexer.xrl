%%% -*- mode: erlang -*-
%%% @doc lexer for the quick check generator compiler
%%% @author gguthrie@basho.com
%%% @copyright (C) 2016 Basho

Definitions.


DASH       = \-
OPEN       = (\()
CLOSE      = (\))
OPEN_SQ    = \[
CLOSE_SQ   = \]
PIPE       = \|
CARET      = \^
BACKSLASH  = \\
QUESTION   = \?
DOT        = \.
TILDA      = \~
PLUS       = \+
UNDERSCORE = \_
STAR       = \*

Rules.

{DASH}       : {token, {dash,       TokenChars}}.
{OPEN}       : {token, {open,       TokenChars}}.
{CLOSE}      : {token, {close,      TokenChars}}.
{OPEN_SQ}    : {token, {open_sq,    TokenChars}}.
{CLOSE_SQ}   : {token, {close_sq,   TokenChars}}.
{PIPE}       : {token, {pipe,       TokenChars}}.
{CARET}      : {token, {caret ,     TokenChars}}.
{BACKSLASH}  : {token, {backslash,  TokenChars}}.
{QUESTION}   : {token, {question,   TokenChars}}.
{DOT}        : {token, {dot,        TokenChars}}.
{TILDA}      : {token, {tilda,      TokenChars}}.
{PLUS}       : {token, {plus,       TokenChars}}.
{UNDERSCORE} : {token, {underscore, TokenChars}}.
{STAR}       : {token, {star,       TokenChars}}.

. : {token, {chars, TokenChars}}.

Erlang code.
