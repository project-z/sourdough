-module(mock_vnode).

-export([
    mock_write/3
    ]).

%% this is meant as a simple "acknowledgment" mock function for mimicking the
%% write funciton of sdgh_vnode (which will become `sdgh_write_vnode`, soon).
mock_write(_Client, {_Bucket,_Key}, _Data) ->
    %% pretend the function did something! A-OK!
    ok.