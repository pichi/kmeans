-module(kmeans).
-export([run/3]).
-on_load(on_load/0).

run(Xs, N, Iters) ->
    InitCentroids = lists:sublist(Xs, N),
    Step = fun(_, Centroids) ->
                   element(1, step(Xs, Centroids))
           end,
    lists:foldl(Step, InitCentroids, lists:seq(1, Iters)).

on_load() ->
    ok = erlang:load_nif("./kmeans", 0).

step(_, _) ->
    erlang:nif_error(nif_not_loaded).
