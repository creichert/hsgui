
Haskell Gui Project Template
============================

About:
-----

Use this code as a starting point for a gtk2hs application. Included getOpts support
for handling command line arguments. Ui included uses the glade xml format.

A large portion of this code was inspired by examples in "Real World Haskell" and
other various resources found in the Haskell Platform documentation.

Building:
--------

    $ cabal configure --prefix=$HOME --user
    $ cabal build
    $ ./dist/build/gui/gui

