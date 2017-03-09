-module(balanced_brackets_b).
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

is_balanced([Next | T], Seen) when Next =:= $( orelse Next =:= $[ orelse Next =:= ${ ->
    is_balanced(T, [Next | Seen]);

is_balanced([Next | T], Seen) when Next =:= $) orelse Next =:= $] orelse Next =:= $} ->
    check_closer(Next, T, Seen);

is_balanced([_Next | T], Seen) ->
    is_balanced(T, Seen).

check_closer(Next, InputTail, [LastSeen | SeenTail]) when Next =:= $) andalso LastSeen =:= $( ->
    is_balanced(InputTail, SeenTail);

check_closer(Next, InputTail, [LastSeen | SeenTail]) when Next =:= $] andalso LastSeen =:= $[ ->
    is_balanced(InputTail, SeenTail);

check_closer(Next, InputTail, [LastSeen | SeenTail]) when Next =:= $} andalso LastSeen =:= ${ ->
    is_balanced(InputTail, SeenTail);

%% Catchall for mismatches, or the Seen stack is empty.
check_closer(_,_,_) ->
    false.

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
