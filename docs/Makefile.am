

info_TEXINFOS = drift.texi
EXTRA_DIST = drift.html



%.html: %.texi
	texi2html -monolithic $<

# 
# 
# %.dvi: %.texinfo
# 	texi2dvi $<
# 
# %.pdf: %.dvi
# 	dvipdf $< $@ 
# 
# %.info: %.texinfo
# 	makeinfo $<
# 
# 
# clean:
# 	rm -f -- *.html *.dvi *.pdf *.log *.aux *.toc *.pg *.fn *.cp *.ky *.tp *.vr *.info
# 
# 
