-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).

%% N is a replication value, it means that data stored in a bucket will
%% be replicated across N different nodes [1].
-define(N, 3).
%% R is a read quorum value, at least R nodes will need to respond to
%% queries for reading data [1].
-define(R, 2).
%% W is a write quorum value, at least W nodes will need to respond to
%% queries for writing data [1].
-define(W, 2).

%% [1] docs.basho.com/riak/latest/references/appendices/concepts/Replication/
