FROM haskell:9.4.8 AS builder

RUN cabal update

# This caches the dependencies as a layer to avoid spurious rebuilds
COPY word-counter.cabal word-counter.cabal
RUN cabal build --only-dependencies

COPY . .
RUN cabal install exe:word-counter --installdir=.

# Use multi-stage build to minimize the size of the final image
FROM debian:buster

COPY --from=builder word-counter ./

ENTRYPOINT ["./word-counter"]
