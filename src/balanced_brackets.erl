-module(balanced_brackets).
-export([is_balanced/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%% Public Functions

-spec is_balanced(string()) -> boolean().
is_balanced(Input) ->
    is_balanced(Input, []).

%%% Private Functions

-spec is_balanced(string(), [char()]) -> boolean().
is_balanced([], Seen) ->
    length(Seen) == 0;

is_balanced(_Input = [NextChar | T], Seen) ->
    case NextChar of
        $( -> is_balanced(T, [NextChar | Seen]);
        ${ -> is_balanced(T, [NextChar | Seen]);
        $[ -> is_balanced(T, [NextChar | Seen]);

        $) -> check_closer($(, T, Seen);
        $} -> check_closer(${, T, Seen);
        $] -> check_closer($[, T, Seen);

        _Else -> is_balanced(T, Seen)
    end.

check_closer(OpeningBracket, InputTail, Seen) ->
    case Seen of
        [OpeningBracket | SeenTail] -> is_balanced(InputTail, SeenTail);
        _Else -> false
    end.

%%% Tests

-ifdef(TEST).

empty_string_test() ->
    ?assert(true =:= is_balanced("")).

good_patterns_test() ->
    ?assert(true =:= is_balanced("()")),
    ?assert(true =:= is_balanced("{}")),
    ?assert(true =:= is_balanced("[]")),
    ?assert(true =:= is_balanced("(()()()())")),
    ?assert(true =:= is_balanced("(((())))")),
    ?assert(true =:= is_balanced("([])")),
    ?assert(true =:= is_balanced("([]{foo}[{}])")),
    ?assert(true =:= is_balanced("(bar[]{}[{}])")),
    ?assert(true =:= is_balanced("(ifdef[]{}[{}endif])")).

bad_patterns_test() ->
    ?assert(false =:= is_balanced(")(")),
    ?assert(false =:= is_balanced("][")),
    ?assert(false =:= is_balanced("}{")),
    ?assert(false =:= is_balanced("({)})")),
    ?assert(false =:= is_balanced("{[}")).
-endif.
