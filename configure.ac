
AC_INIT(src/DrIFT.hs)
AM_INIT_AUTOMAKE(DrIFT,2.1.2)

AC_CANONICAL_HOST
AC_PROG_CC

dnl Let aclocal use macros in ./config directory
ACLOCAL="${ACLOCAL} -I ./ac-macros"




AC_PROGRAM_REQUIRE(hc,ghc nhc98,              [  --with-hc=<haskell compiler>    Specify location of nhc or ghc.])
AC_ARG_WITH(hcflags, 
            [  --with-hcflags=HCFLAGS  specify flags for Haskell compiler],
	    [HCFLAGS=$withval])


AC_PROG_INSTALL
AC_PATH_PROGS(SH, sh)


AC_SUBST(HC)
AC_SUBST(HCFLAGS)

AC_OUTPUT( 
	Makefile 
	src/Makefile 
	docs/Makefile
	example/Makefile
	src/Version.hs
        
        DrIFT.spec
        drift-ghc
)
