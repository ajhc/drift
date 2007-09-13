------------------------------------------------------------------------------

			     DrIFT

This package contains a source distribution of DrIFT, a tool for automatic
derivation of Haskell class instances. DrIFT was formerly known as Derive.

the current homepage is at
    http://repetae.net/john/computer/haskell/DrIFT/


------------------------------------------------------------------------------

Contents of this package:

  - src         Directory with the source files of the DrIFT tool
  - example     Directory with examples of using the DrIFT tool
  - docs        Documentation

------------------------------------------------------------------------------


To configure and install DrIFT from the source tarball

  ./configure --prefix=<dir>
  make install

Where <dir> is the directory in which you want to install the tool. Note that
a haskell compiler (nhc98 or ghc) must be in your path. Otherwise, supply the
location of a Haskell compiler as follows:

  ./configure --prefix=<dir> --with-hc=<haskell compiler>
  make install


If you have pulled the source from the darcs repository, you must first build
the autotools scripts by running

 autoreconf -i


------------------------------------------------------------------------------


You may optionally sset DERIVEPATH to the list of directories you wish to search for
modules.

as an example:

DERIVEPATH=/users/grad/nww/share/hugs/lib:/users/grad/nww/share/hugs/lib/hugs


to run DrIFT, simply pass the haskell file name as an argument. you may run
DrIFT --help to get a summary of the various command line options

