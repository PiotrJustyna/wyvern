#!/bin/bash

cabal update
cabal install alex
cabal install happy
cabal install hlint
cabal install hindent

exec "$@"
