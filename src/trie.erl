-module(trie).
-export([new/0, new/2, add/3, is_member/2, find/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type trie() :: {trie_value(), [trie_child()]}.
-type trie_child() :: {trie_key(), trie()}.
-type trie_key() :: char().
-type trie_value() :: term() | undefined.

-spec new() -> trie().
new() ->
    create_blank_trie().

-spec new(nonempty_string(), trie_value()) -> trie().
new(Word, Value) when is_list(Word) andalso length(Word) > 0 ->
    Trie = create_blank_trie(),
    add(Trie, Word, Value).

-spec add(trie(), nonempty_string(), trie_value()) -> trie().
add(Trie, Word, Value) when is_list(Word) andalso length(Word) > 0 ->
    insert(Trie, string:to_lower(Word), Value).

-spec is_member(trie(), nonempty_string()) -> boolean().
is_member(Trie, Word) when is_list(Word) andalso length(Word) > 0 ->
    find(Trie, Word) /= undefined.

find(Trie, Word) when is_list(Word) andalso length(Word) > 0 ->
    find_value(Trie, string:to_lower(Word)).


%%% - Private Functions

-spec create_blank_trie() -> trie().
create_blank_trie() ->
    {undefined, orddict:new()}.


-spec insert(trie(), string(), trie_value()) -> trie().
insert({_OldValue, Children}, [], Value) ->
    {Value, Children};

insert(Trie, [H|T], Value) ->
    ChildTrie = get_subtrie(Trie, H),
    NewChild = insert(ChildTrie, T, Value),
    update_child_trie(Trie, H, NewChild).


-spec get_subtrie(trie(), char()) -> trie().
get_subtrie({_Value, Children}, Key) ->
    case orddict:find(Key, Children) of
        {ok, SubTrie} -> SubTrie;
        error -> create_blank_trie()
    end.

-spec update_child_trie(trie(), char(), trie()) -> trie().
update_child_trie({Value, Children}, Key, Child) ->
    {Value, orddict:store(Key, Child, Children)}.


-spec find_value(trie(), string()) -> trie_value().
find_value({Value, _Children}, []) ->
    Value;

find_value(Trie, [H | T]) ->
    case contains_key(Trie, H) of
        true ->
            SubTrie = get_subtrie(Trie, H),
            find_value(SubTrie, T);
        false -> undefined
    end.


-spec contains_key(trie(), trie_key()) -> boolean().
contains_key({_Value, Trie}, Key) ->
    orddict:is_key(Key, Trie).

%%% Tests

-ifdef(TEST).

abc_trie() ->
    {undefined,[{$a,{undefined,[{$b,{undefined,[{$c,{1,[]}}]}}]}}]}.

new_test() ->
    NewTrie = trie:new("abc", 1),
    ?assert(abc_trie() =:= NewTrie).

insert_at_end_test() ->
    UpdatedTrie = trie:add(abc_trie(), "abcd", 2),
    ExpectedTrie = {undefined,[{$a,{undefined,[{$b,{undefined,[{$c,{1,[{$d,{2,[]}}]}}]}}]}}]},
    ?assert(ExpectedTrie =:= UpdatedTrie).

insert_at_beginning_test() ->
    UpdatedTrie = trie:add(abc_trie(), "zbc", 3),
    ExpectedTrie = {undefined,[{$a,{undefined,[{$b,{undefined,[{$c,{1,[]}}]}}]}}, {$z,{undefined,[{$b,{undefined,[{$c,{3,[]}}]}}]}}]},
    ?assert(ExpectedTrie =:= UpdatedTrie).

contains_partial_match_test() ->
    ?assert(false =:= is_member(abc_trie(), "a")).

contains_full_test() ->
    ?assert(true =:= is_member(abc_trie(), "abc")).

contains_too_long_test() ->
    ?assert(false =:= is_member(abc_trie(), "abcd")).

contains_no_match_test() ->
    ?assert(false =:= is_member(abc_trie(), "zyxw")).

find_test() ->
    ?assert(1 =:= find(abc_trie(), "abc")).

not_found_test() ->
    ?assert(undefined =:= find(abc_trie(), "abcd")).

-endif.
