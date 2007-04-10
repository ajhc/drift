
AUTOMAKE_OPTIONS = no-dependencies foreign subdir-objects

RULES=  Rules/Arbitrary.hs Rules/Binary.hs Rules/BitsBinary.hs Rules/FunctorM.hs \
	Rules/Generic.hs Rules/GhcBinary.hs Rules/Monoid.hs Rules/Standard.hs \
	Rules/Utility.hs Rules/Xml.hs

bin_PROGRAMS = DrIFT
DrIFT_SOURCES = ${RULES} ChaseImports.hs CommandP.hs DataP.lhs DrIFT.hs GetOpt.hs \
   Unlit.hs ParseLib2.hs PreludData.hs Pretty.lhs RuleUtils.hs Version.hs GenUtil.hs

# DrIFT_static_SOURCES = $(DrIFT_SOURCES)
# DrIFT_static_LINK = $(DrIFT_LINK) -static -optl-static -ldl

EXTRA_DIST = Rules.hs collect_rules.sh
# EXTRA_PROGRAMS = DrIFT_static

BUILT_SOURCES=Rules.hs


DrIFT_LINK = $(HC) $(HCFLAGS) -o $@

Rules.hs: collect_rules.sh
	sh ./collect_rules.sh > $@

SUFFIXES = .hs .lhs .o .hi

# COMPILE = $(HC) $(HCFLAGS)

# all: DrIFT
DrIFT: $(DrIFT_SOURCES) $(nodist_DrIFT_SOURCES)
	$(HC) $(HCFLAGS) -i. -i@srcdir@ -hidir . -odir . -o $@ --make @srcdir@/DrIFT.hs

#.hs.o:
#	$(HC) -O -i. -i@srcdir@ $(HCFLAGS) -hidir . -o $@ -c $<
#.lhs.o:
#	$(HC) -O -i. -i@srcdir@ $(HCFLAGS) -hidir . -o $@ -c $<
#%.hi: %.o
#	@:

clean:
	rm -f -- *.hi *.o *_hsc.c *_hsc.h

distclean-local:
	rm -f -- *.hi *.o *_hsc.c *_hsc.h config.log config.status

#depend:
#	ghc -M -optdep-f -optdepdepend.make DrIFT.hs

#depend.make:
#	ghc -M -optdep-f -optdepdepend.make DrIFT.hs


BINDISTNAME=$(PACKAGE)-$(VERSION)-`uname -s`-`uname -m`

# $(PACKAGE)_static
bin-dist: $(PACKAGE)
	$(RM) -rf -- $(BINDISTNAME)
	mkdir $(BINDISTNAME)
	strip -- $(PACKAGE) || true
#strip -- $(PACKAGE)_static || true
	cp -- $(PACKAGE) $(BINDISTNAME)
#cp -- $(PACKAGE)_static $(BINDISTNAME)
	tar cvf $(BINDISTNAME).tar $(BINDISTNAME)
	gzip -f -- $(BINDISTNAME).tar
	$(RM) -rf -- $(BINDISTNAME)

CLEANFILES = Rules/*.o Rules/*.hi

#-include depend.make

#.PHONY: depend
