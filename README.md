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

## For developing

First, you should use the arafura branch, rather than master branch.

* [master branch](https://github.com/ajhc/drift/tree/master): Do not touch me, mirror of [jhc's darcs repository](http://repetae.net/dw/darcsweb.cgi?r=DrIFT;a=summary).
* [arafura branch](https://github.com/ajhc/drift/tree/arafura): For developing Ajhc's DrIFT.

You should send patch to original DrIFT, if testing on arafura branch is good.

    $ darcs get http://repetae.net/repos/DrIFT
    $ cd DrIFT/
    $ patch -p1 < ~/yourfile.patch
    $ darcs record -a
    $ darcs send

The command [darcs send](http://darcs.net/Using/Send) sends email the patch to
jhc author (= [John Meacham](http://repetae.net/)).
