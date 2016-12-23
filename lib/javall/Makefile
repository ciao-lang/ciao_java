CIAOCMD ?= ciao

# TODO: Use ciao_builder instead

# TODO: build from java.hooks.pl
PROLOGSERVER=plserver

all: build docs

build:
	$(CIAOCMD) comp -x $(PROLOGSERVER)
	cd CiaoJava && $(MAKE) all

docs:
	cd javadoc && $(MAKE) all

clean realclean:
	if [ -x $(CIAOCMD) ]; then $(CIAOCMD) clean_tree "."; fi
	cd CiaoJava && $(MAKE) clean
	cd javadoc && $(MAKE) clean

distclean: realclean
	/bin/rm -f *~ plserver
	cd CiaoJava && $(MAKE) distclean
	cd javadoc && $(MAKE) distclean
