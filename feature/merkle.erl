-module(merkle).
-export([hash/1]).

%% TODO either add padding to the leaf nodes for completing
%% the hash tree or use a different approach

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
    {ok, Fdr} = file:open(Path, [read]),

    %% read data of the file in 1Kb and hash it
    FileHash = read_file(Fdr, []),

    %% add leaves to complete the pairs in hash tree
    padding(FileHash),

    %% calculate root hash for the tree
    rootHash(FileHash, []),

    %% close the file descripter
    file:close(Fdr),
    ok.

%%
%% calculate root hash for the leaves of the merkle tree
%%
rootHash([], HashAcc) ->
    if
        erlang:length(HashAcc) =:= 1 ->
            HashAcc;
        true ->
            rootHash(HashAcc, [])
    end,
    ok;

rootHash([HashChunk1, HashChunk2 | T], HashAcc) ->

    %% join the hashChunks using space as separator
    %% NOTE SPACE IS BEING ADDED INTO THE HASHES
    NewChunk   = binary_join([HashChunk1, HashChunk2], <<"">>),

    %% calculate the hash for this new chunk
    ChunkHash  = crypto:hash(?HASH_ALGO, NewChunk),

    %% add the hashed chunk to the accumulator
    Acc = lists:append([HashAcc, ChunkHash]),

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
    Size    = erlang:length(HashList),
    Padding = math:pow(2, ceiling(math:log(Size))) - Size,

    %%% TODO write code to add padding leaves to the merkle tree
    %% generate string (all zeros) of chunk size
    NullSring = string:copies("0", ?CHUNK),
    ok.

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
