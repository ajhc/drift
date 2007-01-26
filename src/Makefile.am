
AUTOMAKE_OPTIONS = no-dependencies foreign

bin_PROGRAMS = DrIFT
DrIFT_SOURCES = ChaseImports.hs CommandP.hs DataP.lhs DrIFT.hs GetOpt.hs \
   Literate.lhs ParseLib2.hs PreludData.hs Pretty.lhs RuleUtils.hs \
   StandardRules.hs UserRuleBinary.hs UserRulesGeneric.hs \
   UserRules.hs UserRuleXml.hs Version.hs GenUtil.hs UserRuleGhcBinary.hs \
   RuleUtility.hs RuleFunctorM.hs RuleMonoid.hs UserRuleArbitrary.hs \
   UserRuleBitsBinary.hs

# DrIFT_static_SOURCES = $(DrIFT_SOURCES)
# DrIFT_static_LINK = $(DrIFT_LINK) -static -optl-static -ldl

EXTRA_DIST = depend.make
# EXTRA_PROGRAMS = DrIFT_static


DrIFT_LINK = $(HC) $(HCFLAGS) -o $@


SUFFIXES = .hs .lhs .o .hi

# COMPILE = $(HC) $(HCFLAGS)

# all: DrIFT

.hs.o:
	$(HC) -O -i. -i@srcdir@ $(HCFLAGS) -hidir . -o $@ -c $<
.lhs.o:
	$(HC) -O -i. -i@srcdir@ $(HCFLAGS) -hidir . -o $@ -c $<
%.hi: %.o
	@:

clean:
	rm -f -- *.hi *.o *_hsc.c *_hsc.h

distclean-local:
	rm -f -- *.hi *.o *_hsc.c *_hsc.h config.log config.status

depend:
	ghc -M -optdep-f -optdepdepend.make DrIFT.hs

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

include depend.make
