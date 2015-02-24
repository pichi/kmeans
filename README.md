This benchmark is born to compare the performance of Pharo 3 in executing a simple machine learning algorithm with a reference implementation in Python and Scala. Since then, it got a little out of hand, and a few other implementations are available.

Rules
=====

The implementations should all follow the same algorithm, and be optimized for idiomatic code and not for speed. The example is intended to compare time of execution for a typical machine learning algorithm, ideally during an interactive session, instead of highly optimized production code. As such, it is important that the code is straightforward and that there is no separate phase to prepare the caches.

The points are in `points.json`, and are to be grouped into 10 clusters, using 15 iterations of kmeans. The initial centroids are initialized to the first 10 points, and we take an average over 100 runs.

Results
=======

Time for running on my laptop are available under `results`. A few surprises:

* Writing a working Rust implementation was surprisingly difficult; writing one that would perform decently even more so. I had to rely frequently on help from people online.
* PyPy is able to outperform Scala
* Factor is pretty impressive, given that it is a fairly small project with a dedicated VM. With an implementation in 8 (!) lines, we get the a fairly performing dynamic language
* Nim was also quite impressive: my first implementation was as easy as Python, and it was just behind Rust; when an unnecessary copy was removed, it turned out to be the fastest.

How to run
==========

**Clojure**: `lein with-profile uberjar run`

**Erlang**:

    erl
    1> c(main).
    2> c(kmeans).
    3> main:run().

Use `c(kmeans, native).` for native compiled version.

**Factor**:

    USE: kmeans.benchmark
    100 "../points.json" kmeans-benchmark

**Julia**:

    julia -e 'Pkg.add("JSON")'
    julia kmeans.jl

**Lua**: download [this JSON library](http://dkolf.de/src/dkjson-lua.fsl/home) and put it in the same folder as the main file. Then run

    lua kmeans.lua
    luajit kmeans.lua

**Nim**:

    nim c -d:release benchmark
    ./benchmark

**Node**: `node kmeans.js`

**Parasail**: assume `plc.csh` is on `$PATH`. Then

    pslc.csh -O3 point.psl kmeans.psl benchmark.psl -o benchmark
    ./benchmark

**Pharo3**: install `NeoJSON` and file-in `Kmeans.st`, then open a workspace and write something like

    | path points kmeans |

    path := '../points.json'.

    kmeans := KMeans new
      iterations: 15;
      clusters: 10;
      yourself.

    StandardFileStream readOnlyFileNamed: path
      do: [ :stream |
        points := (NeoJSONReader on: stream) next collect: [ :each |
          (each first) @ (each second)
        ].
      ].

    kmeans benchmark: points repeating: 100

**Python**:

    python kmeans.py
    pypy kmeans.py

**Rust**:

    cargo run --release

**Scala**: `sbt run`