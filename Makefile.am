AUTOMAKE_OPTIONS = no-dependencies foreign




SUBDIRS	= ac-macros src docs example

EXTRA_DIST = configure.in \
             reconf Changelog TODO README.old drift-ghc.in code \
             code/FunctorM.hs code/GhcBinary.hs code/README.txt

bin_SCRIPTS = drift-ghc


PUBLISH_DIR = /home/john/public_html/computer/haskell/DrIFT
PUBLISH_FILES = docs/drift.html docs/drift.ps  drift-list.txt

BINDISTNAME=$(PACKAGE)-$(VERSION)-`uname -s`-`uname -m`
bin-dist:
	(cd ./src; $(MAKE) bin-dist)
	mv ./src/$(BINDISTNAME).tar.gz .

drift-list.txt: src/DrIFT
	src/DrIFT -l > drift-list.txt

publish: drift-list.txt docs depend bin-dist dist  # rpm 
	rm -f -- $(PUBLISH_DIR)/drop/$(PACKAGE)-*-*-*.tar.gz
#	rm -f -- $(PUBLISH_DIR)/drop/$(PACKAGE)-*.rpm
	rm -f -- "$(PUBLISH_DIR)/drop/$(PACKAGE)-$(VERSION).tar.gz"
	cp -- "$(BINDISTNAME).tar.gz" "$(PUBLISH_DIR)/drop"
	cp -- "$(PACKAGE)-$(VERSION).tar.gz" "$(PUBLISH_DIR)/drop"
	cp -- $(PUBLISH_FILES) $(PUBLISH_DIR)
#	cp -- "$(HOME)/redhat/RPMS/i386/$(PACKAGE)-$(VERSION)-1.i386.rpm" "$(PUBLISH_DIR)/drop"
#	cp -- "$(HOME)/redhat/SRPMS/$(PACKAGE)-$(VERSION)-1.src.rpm" "$(PUBLISH_DIR)/drop"
	cp -- Changelog $(PUBLISH_DIR)
	make -C $(PUBLISH_DIR) || true



rpm: depend $(PACKAGE).spec dist
	cp $(PACKAGE)-$(VERSION).tar.gz $(HOME)/redhat/SOURCES/
	rpmbuild -ba $(PACKAGE).spec

depend:
	$(MAKE) -C src depend

docs:
	$(MAKE) -C docs drift.html drift.ps

.PHONY: docs depend rpm publish bin-dist
