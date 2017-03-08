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
    if
        Next == $(; Next == ${; Next == $[ ->
            check_balance(T, [Next | Seen]);

        Next == $) -> check_closer($(, T, Seen);
        Next == $} -> check_closer(${, T, Seen);
        Next == $] -> check_closer($[, T, Seen);
        true -> check_balance(T, Seen)
    end.

check_closer(OpeningBracket, InputTail, Seen) ->
    case Seen of
        [OpeningBracket | SeenTail] -> check_balance(InputTail, SeenTail);
        _Else -> false
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
