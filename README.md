shake-install is a tool for building a set of related Haskell packages that are built with shake.

If you are building a system containing multiple cabal files you may find it useful.

cabal is an excellent tool for building a stand-alone library.
With dependency freezing, it works pretty well to build an application that is contained in a single cabal file.
However, things get tedious when you have a system of 10 local cabal files with dependencies on each other.
When one is changed, the others must have their dependencies updated.
You already have the dependencies declared in your cabal file, but your tooling does not automatically connect the dots.

## Declaring dependencies

shake-install expects to find a file `Shakefile.hs`. That file indicates what it builds via `sources` and what its build should effect via `children`.

``` haskell
children = []
sources = ["project.cabal"]
```

## installation requirements

The `shake` library in your user db

    cabal install shake
    
Use the cabal executable that GHC uses

```
mkdir old-cabal
cabal sandbox init
cabal install cabal-1.18.0.8 # ghc 7.8.3
export PATH=$PWD/.cabal-sandbox/bin:$PATH
```

The shake-install executable `shake`

```
git clone git@github.com:AlphaHeavy/shake-install
cd shake-install
cabal sandbox init
cabal install
export PATH=$PWD/.cabal-sandbox/bin:$PATH
```

## Using sandboxes

You will need the package db

   /Source/shake-install/dist/build/shake/shake configure --package-db "/Source/shake-install/.cabal-sandbox/x86_64-osx-ghc-7.10.1-packages.conf.d"
 
Note that anything added with `add-source` that has not be installed will not be known about.
