-module(merkle).
-export([hash/1]).

-include("../include/ppspp.hrl").

%% specify the chunk size to be read of the file
-define(CHUNK, 1024).

%% specify the hash algorithm
-define(HASH_ALGO, sha).

%%
%% create merkle hash tree of a file
%%
hash(Path) ->

    %% extract the filename from the path
    %FileName = filename:basename(Path),

    %% open file in read mode
    %% TODO read file in binary mode
    {ok, Fdr} = file:open(Path, [read]),

    %% read data of the file in 1Kb and hash it
    FileHash = read_file(Fdr, []),

    %% close the file descripter
    file:close(Fdr),

    %% add leaves to complete the pairs in hash tree
    MerkleTree = padding(FileHash),
    io:format("~p~n", MerkleTree),

    %% calculate root hash for the tree
    rootHash(FileHash, []).

%%
%% calculate root hash for the leaves of the merkle tree
%%
rootHash([], HashAcc) ->
    %% if the HashAcc has a size of 1 means that we have
    %% reached of the tree
    if
        erlang:length(HashAcc) =:= 1 ->
            HashAcc;
        true ->
            rootHash(HashAcc, [])
    end,
    ok;

rootHash([HashChunk1, HashChunk2 | T], HashAcc) ->

    %% join the hashChunks using space as separator
    NewChunk  = binary_join([HashChunk1, HashChunk2], <<"">>),

    %% calculate the hash for this new chunk
    ChunkHash = crypto:hash(?HASH_ALGO, NewChunk),

    %% add the hashed chunk to the accumulator
    Acc       = lists:append([HashAcc, ChunkHash]),

    %% get the remaining list
    rootHash(T, Acc).

%%
%% read the file in 64kB chucks
%%
read_file(Fd, FileHash) ->

    case file:read(Fd, ?CHUNK) of
        {ok, Data} ->

            %% calculate sha hash for the file chunk
            ChunkHash = crypto:hash(?HASH_ALGO, Data),

            %% append the new has to the origianl list
            New       = lists:append([ FileHash, [ChunkHash] ]),
            read_file(Fd, New );

        eof ->
            FileHash
    end.

%%
%% Helper function : join binary list
%%
binary_join([], _Sep) ->
  <<>>;
binary_join([Part], _Sep) ->
  Part;
binary_join(List, Sep) ->
  lists:foldr(fun (A, B) ->
    if
      bit_size(B) > 0 -> <<A/binary, Sep/binary, B/binary>>;
      true -> A
    end
  end, <<>>, List).

%%
%% Helper function : add padding
%%
padding(HashList) ->
    %% Calulate the padding required for the hash tree
    Size    = erlang:length(HashList),
    Padding = math:pow(2, ceiling(math:log(Size))) - Size,

    %% generate string (all zeros) of chunk size
    NullSring  = string:copies("0", ?CHUNK),

    %% hash the null string
    NullHash   = crypto:hash(?HASH_ALGO, NullSring),

    %% add the hash of null string to the Hash tree
    %% (a total of n=Padding null hashes are added)
    MerkleTree = HashList ++ lists:duplicate(erlang:round(Padding), erlang:list_to_binary(NullHash)),
    MerkleTree.

%%
%% Helper function : calculate ceiling 
%%
ceiling(X) when X < 0 ->
    trunc(X);
ceiling(X) ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T + 1
    end.
