# DrIFT [![Build Status](https://travis-ci.org/ajhc/drift.png)](https://travis-ci.org/ajhc/drift)

This package contains a source distribution of DrIFT, a tool for automatic
derivation of Haskell class instances. DrIFT was formerly known as Derive.

The original homepage is at
[http://repetae.net/john/computer/haskell/DrIFT/](http://repetae.net/john/computer/haskell/DrIFT/).
But this DrIFT package has some changes with original DrIFT.

## Contents of this package:

* src:         Directory with the source files of the DrIFT tool
* example:     Directory with examples of using the DrIFT tool
* docs:        Documentation

## How to install

To configure and install DrIFT from the source tarball

    $ autoreconf -i
    $ ./configure
    $ make cabal-install
