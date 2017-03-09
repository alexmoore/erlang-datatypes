-module(balanced_abstracts).
-export([is_balanced/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type char_pairs() :: [char_pair()].
-type char_pair() :: {char(), char()}.

%%% Public Functions

-spec is_balanced(string(), char_pairs()) -> boolean().
is_balanced(Input, CharPairs) ->
    Lookup = create_lookup(CharPairs),
    check_balance(Input, Lookup, []).

%%% Private Functions

-type opener_list() :: [char()].
-type closer_dict() :: [{char(), char()}].
-type pairs_lookup() :: {opener_list(), closer_dict()}.

-spec check_balance(string(), pairs_lookup(), [char()]) -> boolean().
check_balance([], _Lookups, Seen) ->
    length(Seen) == 0;

check_balance(_Input = [Next | T], Lookups, Seen) ->
    case {is_opener(Next, Lookups), is_closer(Next, Lookups)} of
        {true, false} -> check_balance(T, Lookups, [Next | Seen]);
        {false, true} -> check_closer(Next, T, Lookups, Seen);
        _Else -> check_balance(T, Lookups, Seen)
    end.

-spec check_closer(char(), string(), pairs_lookup(), [char()]) -> boolean().
check_closer(CloserChar, InputTail, Lookups, Seen) ->
    OpeningChar = get_matching_opener(CloserChar, Lookups),
    case Seen of
        [OpeningChar | SeenTail] -> check_balance(InputTail, Lookups, SeenTail);
        _Else -> false
    end.

-spec is_opener(char(), pairs_lookup()) -> boolean().
is_opener(Char, {Openers, _Closers}) ->
    lists:member(Char, Openers).

-spec is_closer(char(), pairs_lookup()) -> boolean().
is_closer(Char, {_Openers, Closers}) ->
    orddict:is_key(Char, Closers).

-spec get_matching_opener(char(), pairs_lookup()) -> char().
get_matching_opener(Char, {_Openers, Closers}) ->
    orddict:fetch(Char, Closers).

-spec create_lookup(char_pairs()) -> pairs_lookup().
create_lookup(CharPairs) ->
    Openers = [Opener || {Opener, _} <- CharPairs],
    Closers = lists:foldl(fun insert_closer_dict/2, orddict:new(), CharPairs),
    {Openers, Closers}.

-spec insert_closer_dict(char_pair(), closer_dict()) -> closer_dict().
insert_closer_dict({Opener, Closer}, CloserDict) ->
    orddict:store(Closer, Opener, CloserDict).


%%% Tests

-ifdef(TEST).

brackets() ->
    [{$(, $)}, {${, $}}, {$[, $]}].

create_lookup_test() ->
    Expected =
    {
      [
        $(,
        ${,
        $[
      ],
      [
        { $), $( },
        { $], $[ },
        { $}, ${ }
      ]
    },

    Actual = create_lookup(brackets()),
    ?assert(Expected =:= Actual).

empty_string_test() ->
    ?assert(is_balanced("", brackets())).

good_patterns_test() ->
    Lookup = brackets(),
    ?assert(true =:= is_balanced("()", Lookup)),
    ?assert(true =:= is_balanced("{}", Lookup)),
    ?assert(true =:= is_balanced("[]", Lookup)),
    ?assert(true =:= is_balanced("(()()()())", Lookup)),
    ?assert(true =:= is_balanced("(((())))", Lookup)),
    ?assert(true =:= is_balanced("([])", Lookup)),
    ?assert(true =:= is_balanced("([]{foo}[{}])", Lookup)),
    ?assert(true =:= is_balanced("(bar[]{}[{}])", Lookup)),
    ?assert(true =:= is_balanced("(ifdef[]{}[{}endif])", Lookup)).

bad_patterns_test() ->
    Lookup = brackets(),
    ?assert(false =:= is_balanced(")(", Lookup)),
    ?assert(false =:= is_balanced("][", Lookup)),
    ?assert(false =:= is_balanced("}{", Lookup)),
    ?assert(false =:= is_balanced("({)})", Lookup)),
    ?assert(false =:= is_balanced("{[}", Lookup)).
-endif.
