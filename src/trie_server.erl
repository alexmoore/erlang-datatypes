-module(trie_server).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start/0, add/2, is_member/1, find/1]).

start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
add(Word, Value) -> gen_server:call(?MODULE, {add, Word, Value}).
is_member(Word) -> gen_server:call(?MODULE, {is_member, Word}).
find(Word) -> gen_server:call(?MODULE, {find, Word}).

init([]) ->
	Trie = trie:new(),
	{ok, Trie}.

handle_call({add, Word, Value}, _From, Trie) ->
	UpdatedTrie = trie:add(Trie, Word, Value),
	{reply, ok, UpdatedTrie};

handle_call({is_member, Word}, _From, Trie) ->
	Response = trie:is_member(Trie, Word),
	{reply, Response, Trie};

handle_call({find, Word}, _From, Trie) ->
	Response = trie:find(Trie, Word),
	{reply, Response, Trie};

handle_call(_Message, _From, Trie) ->
	{reply, error, Trie}.

handle_cast(_Msg, Trie) -> {noreply, Trie}.
handle_info(_Msg, Trie) -> {noreply, Trie}.
terminate(_Reason, _Library) -> ok.
code_change(_OldVersion, Trie, _Extra) -> {ok, Trie}.
