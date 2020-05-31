# Setting Up

## On CSE

GHC and `cabal` are already setup on CSE. To use the same set of dependencies, type `3141` on the CSE terminal.

## On Home Machine

Install stack build tool for Haskell

``` sh
curl -sSL https://get.haskellstack.org/ | sh
# or
wget -qO- https://get.haskellstack.org/ | sh
```

Install entire compiler tool chain with `stack setup`

`stack repl` opens `ghci`  
`stack exec ghc` runs `ghc`
