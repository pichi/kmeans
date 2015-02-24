-module(kmeans).
-export([run/3]).

-compile(inline).

run(Xs, N, Iters) ->
    Parent = self(),
    Worker = fun() ->
                     InitCentroids = lists:sublist(Xs, N),
                     Step = fun(_, Centroids) ->
                                    [average(X) || X <- clusters(Xs, Centroids)]
                            end,
                     Result = lists:foldl(Step, InitCentroids, lists:seq(1, Iters)),
                     Parent ! {self(), Result}
             end,
    Pid = spawn_link(Worker), % spawn worker for abusing process dictionary
    receive {Pid, Result} -> Result end.

dist({X1, Y1}, {X2, Y2})
  when is_float(X1), is_float(X2), is_float(Y1), is_float(Y2)->
    DX = X1-X2,
    DY = Y1-Y2,
    DX*DX+DY*DY.

average(Q) ->
    average(length(Q), Q, 0.0, 0.0).

average(L, [], X, Y) -> {X/L, Y/L};
average(L, [{X, Y}|T], XAcc, YAcc)
  when is_float(X), is_float(Y) ->
    average(L, T, XAcc+X, YAcc+Y).

closest(P, Centroids) ->
    element(2, lists:min([{dist(P, C), C} || C <- Centroids])).

clusters(Xs, Centroids) ->
    groupBy(Xs, fun(X) -> closest(X, Centroids) end).

groupBy(L, Fn) ->
    [add(Fn(X), X) || X <- L],
    [ C || {_, C} <- erase() ].

add(K, V) ->
    T = case get(K) of
            L when is_list(L) -> L;
            undefined -> []
        end,
    put(K, [V | T]).
