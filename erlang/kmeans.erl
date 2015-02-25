-module(kmeans).
-export([run/3]).
-export([points2bin/1]).
-export([bin2points/2]).
-on_load(on_load/0).

run(Xs, N, Iters) ->
    InitCentroids = lists:sublist(Xs, N),
    XsBin = points2bin(Xs),
    CBin = points2bin(InitCentroids),
    Size = tuple_size(hd(Xs)),
    Step = fun(_, Centroids) ->
                   element(1, step(Size, XsBin, Centroids))
           end,
    bin2points(Size, lists:foldl(Step, CBin, lists:seq(1, Iters))).

on_load() ->
    ok = erlang:load_nif("./kmeans", 0).

points2bin(_) ->
    erlang:nif_error(nif_not_loaded).
bin2points(_, _) ->
    erlang:nif_error(nif_not_loaded).
step(_, _, _) ->
    erlang:nif_error(nif_not_loaded).
