-module(binmap).
-export([get_hash_by_index/1, get_content_by_hash/1]).


%% ------------------------------------------------------------------
%% API calls
%% ------------------------------------------------------------------

get_hash_by_index(Index) ->
    ok.

get_content_by_hash(Hash) ->
    ok.


%% ------------------------------------------------------------------
%% Functions
%% ------------------------------------------------------------------
%% Here we take the chunks (C0,C1 ...) and store their corresponding hashes
%% (H0,H1...) in a list at index denoted by their bin numbers
%%
%%                        7
%%                       / \
%%                     /     \
%%                   /         \
%%                 /             \
%%                3              11
%%               / \             / \
%%              /   \           /   \
%%             /     \         /     \
%%            1       5       9      13
%%           / \     / \     / \     / \
%%          0   2   4   6   8  10   12  14
%%         H0  H1  H2  H3  H4  H5   H6  H7
%%         C0  C1  C2  C3  C4  C5   C6  C7

empty() ->
    [].

%% creates a Hash List : [H0, undef, H1, undef, H2, undef] and parse it
%% through the binmap tree to calculate the root hash
add(Element, List) ->
    lists:append(List, [hash(Element), undefined]).

delete(Element, List) ->
    lists:delete(hash(Element), List).

%% Parses the list to calculate the root hash
parseList(Hashes) ->
    %% TODO pad the Hash list if there are not enough hashes
    recompute(Hashes, []).

recompute([], [RootHash, undefined]) ->
    RootHash;
recompute([], Acc) ->
    recompute(Acc, []);
recompute([N1,_,N2,_ | Tail], Acc) ->
    recompute(Tail , lists:append(Acc, [merge(N1,N2), undefined]));
recompute([N,undefined], Acc) ->
    recompute([] , lists:append(Acc, [crypto:sha(term_to_binary(N))]) ).

hash(Element) ->
    crypto:sha(term_to_binary(Element)).

merge(Hash1, Hash2) ->
    crypto:sha(term_to_binary([Hash1,Hash2])).

