AUTOMAKE_OPTIONS = no-dependencies foreign
ACLOCAL_AMFLAGS=-I ac-macros

info_TEXINFOS = docs/drift.texi

docs/drift.html: docs/drift.texi
	texi2html -monolithic $< -o $@


SUBDIRS	= src example

EXTRA_DIST = configure.ac \
             Changelog README.old drift-ghc.in code \
             code/FunctorM.hs code/GhcBinary.hs code/README.txt \
	     ac-macros/acincludepackage.m4 LICENSE docs/drift.html \
	     DrIFT.spec

bin_SCRIPTS = drift-ghc


PUBLISH_DIR = /home/john/public_html/computer/haskell/DrIFT
PUBLISH_FILES = docs/drift.html docs/drift.ps  drift-list.txt

BINDISTNAME=@PACKAGE_NAME@-@VERSION@-`uname -s`-`uname -m`
bin-dist:
	(cd ./src; $(MAKE) bin-dist)
	mv ./src/$(BINDISTNAME).tar.gz .

drift-list.txt: src/DrIFT
	src/DrIFT -l > drift-list.txt

publish: drift-list.txt docs bin-dist dist  # rpm
	rm -f -- $(PUBLISH_DIR)/drop/$(PACKAGE_NAME)-*-*-*.tar.gz
#	rm -f -- $(PUBLISH_DIR)/drop/$(PACKAGE)-*.rpm
	rm -f -- "$(PUBLISH_DIR)/drop/$(PACKAGE_NAME)-$(VERSION).tar.gz"
	cp -- "$(BINDISTNAME).tar.gz" "$(PUBLISH_DIR)/drop"
	cp -- "$(PACKAGE_NAME)-$(VERSION).tar.gz" "$(PUBLISH_DIR)/drop"
	cp -- $(PUBLISH_FILES) $(PUBLISH_DIR)
#	cp -- "$(HOME)/redhat/RPMS/i386/$(PACKAGE)-$(VERSION)-1.i386.rpm" "$(PUBLISH_DIR)/drop"
#	cp -- "$(HOME)/redhat/SRPMS/$(PACKAGE)-$(VERSION)-1.src.rpm" "$(PUBLISH_DIR)/drop"
	cp -- Changelog $(PUBLISH_DIR)
	make -C $(PUBLISH_DIR) || true


src/Rules.hs:
	make -C src Rules.hs

cabal-install: src/Rules.hs docs/drift.info
	cabal install

cabal-dist: src/Rules.hs docs/drift.info
	runhaskell Setup.hs sdist

rpm: depend $(PACKAGE_NAME).spec dist
	cp $(PACKAGE_NAME)-$(VERSION).tar.gz $(HOME)/var/rpm/SOURCES/
	rpmbuild -ba $(PACKAGE_NAME).spec

depend:
	$(MAKE) -C src depend

docs: docs/drift.html docs/drift.ps

.PHONY: docs depend rpm publish bin-dist
