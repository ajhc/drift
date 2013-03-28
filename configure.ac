AC_INIT([DrIFT],[2.2.3],john@repetae.net, [DrIFT])
AC_CONFIG_SRCDIR(src/DrIFT.hs)
AC_CONFIG_MACRO_DIR(ac-macros)
AC_CONFIG_AUX_DIR(ac-macros)
AM_INIT_AUTOMAKE([foreign no-dependencies subdir-objects])

AC_CANONICAL_HOST

dnl Let aclocal use macros in ./config directory
dnl ACLOCAL="${ACLOCAL} -I ./ac-macros"

AC_PROGRAM_REQUIRE(hc,ghc,[  --with-hc=<haskell compiler>    Specify location of ghc.])
AC_ARG_WITH(hcflags,
            [  --with-hcflags=HCFLAGS  specify flags for Haskell compiler],
	    [HCFLAGS=$withval])

GHC=$HC
GHCFLAGS=
GHC_CHECK_MODULE([System.IO],[],[],[],[HCFLAGS="$HCFLAGS -hide-all-packages"])

AC_PROG_INSTALL
AC_PATH_PROGS(SH, sh)

AC_SUBST(HC)
AC_SUBST(HCFLAGS)
AC_SUBST(PACKAGE)

AC_OUTPUT(
	Makefile
	src/Makefile
	example/Makefile
	src/Version.hs

        DrIFT.spec
        drift-ghc
)
