SRCDIR	= $(top_srcdir)/example

EXTRA_DIST = \
	README\
	TestTerm.hs TestTerm.out.correct\
	Artifical.hs BTree.hs Example.hs Foo.lhs Xref.hs

#TESTS	= TestTerm_check

%_check: Makefile
	(\
	echo "#! /bin/sh"; \
	echo "set -e"; \
	echo "DERIVEPATH=$(SRCDIR) $(top_builddir)/src/DrIFT $(SRCDIR)/$*.hs | sed -e '/{-#/d' > $*.out"; \
	echo "diff $*.out $(SRCDIR)/$*.out.correct"; \
	echo "$(RM) $*.out"; \
	) > $@
	chmod +x $@

CLEANFILES = *_check *.out
