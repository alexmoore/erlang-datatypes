-module(balanced_brackets).
-export([balanced_brackets/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%% Public Functions

-spec balanced_brackets(string()) -> boolean().
balanced_brackets(Input) ->
    check_balance(Input, []).

%%% Private Functions

-spec check_balance(string(), [char()]) -> boolean().
check_balance([], Seen) ->
    length(Seen) == 0;

check_balance(_Input = [Next | T], Seen) ->
    case {is_opener(Next), is_closer(Next)} of
        {true, false} -> check_balance(T, [Next | Seen]);
        {false, true} -> check_closer(Next, T, Seen);
        _Else -> check_balance(T, Seen)
    end.

-spec check_closer(char(), string(), [char()]) -> boolean().
check_closer(ClosingBracket, InputTail, Seen) ->
    OpeningBracket = get_matching_opener(ClosingBracket),
    case Seen of
        [OpeningBracket | SeenTail] -> check_balance(InputTail, SeenTail);
        _Else -> false
    end.

-spec is_opener(char()) -> boolean().
is_opener(Char) ->
    Char == $( orelse Char == ${ orelse Char == $[.

-spec is_closer(char()) -> boolean().
is_closer(Char) ->
    Char == $) orelse Char == $} orelse Char == $].

-spec get_matching_opener(char()) -> char().
get_matching_opener(Char) ->
    case Char of
        $) -> $(;
        $} -> ${;
        $] -> $[
    end.

%%% Tests

-ifdef(TEST).

empty_string_test() ->
    ?assert(true =:= balanced_brackets("")).

good_patterns_test() ->
    ?assert(true =:= balanced_brackets("()")),
    ?assert(true =:= balanced_brackets("{}")),
    ?assert(true =:= balanced_brackets("[]")),
    ?assert(true =:= balanced_brackets("(()()()())")),
    ?assert(true =:= balanced_brackets("(((())))")),
    ?assert(true =:= balanced_brackets("([])")),
    ?assert(true =:= balanced_brackets("([]{foo}[{}])")),
    ?assert(true =:= balanced_brackets("(bar[]{}[{}])")),
    ?assert(true =:= balanced_brackets("(ifdef[]{}[{}endif])")).

bad_patterns_test() ->
    ?assert(false =:= balanced_brackets(")(")),
    ?assert(false =:= balanced_brackets("][")),
    ?assert(false =:= balanced_brackets("}{")),
    ?assert(false =:= balanced_brackets("({)})")),
    ?assert(false =:= balanced_brackets("{[}")).
-endif.
