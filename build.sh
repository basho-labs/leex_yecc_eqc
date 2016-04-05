#!/bin/bash

set -e
set -u
set -o pipefail

erlc regex_lexer.xrl
erlc regex_lexer.erl
erlc regex_parser.yrl
erlc regex_parser.erl
