
all:
	xsltproc --stringparam library-version `perl -ne 'print "$$1\n" if (/\(defvar \*drakma-version-string\* "(.*)"/)' ../drakma.asd` clixdoc.xsl index.xml > index.html
