CIAOCMD ?= ciao

# TODO: Use ciao_builder instead

all: build

build:
	cd agenda && $(MAKE) all
	cd java2pl && $(MAKE) all
	cd pl2java && $(MAKE) all

clean realclean:
	cd agenda && $(MAKE) clean
	cd java2pl && $(MAKE) clean
	cd pl2java && $(MAKE) clean

distclean: realclean
	cd agenda && $(MAKE) distclean
	cd java2pl && $(MAKE) distclean
	cd pl2java && $(MAKE) distclean
