CIAOCMD ?= ciao

# TODO: Use ciao_builder instead

JAVASOURCES=*.java
PROLOGSOURCES=*.pl
JAVACLASSES=*.class
JAVAC=javac
JAVAEXECUTE=java
# TODO: ask 'ciao' about the class paths
JAVALL=../../lib/javall
JAVAFLAGS=-cp $(JAVALL):./
JAVACFLAGS=-classpath $(JAVALL)
LOGFILES=plserver.out plserver.err
PROLOGSERVER=$(JAVALL)/plserver

all: classes prolog

run_queens:
	$(JAVAEXECUTE) $(JAVAFLAGS) queens $(PROLOGSERVER)
run_example0:
	$(JAVAEXECUTE) $(JAVAFLAGS) example0 $(PROLOGSERVER)
run_example1:
	$(JAVAEXECUTE) $(JAVAFLAGS) example1 $(PROLOGSERVER)

classes:
	-$(JAVAC) $(JAVACFLAGS) $(JAVASOURCES)

prolog:
	$(CIAOCMD) comp -c $(PROLOGSOURCES)

clean realclean:
	if [ -x $(CIAOCMD) ]; then $(CIAOCMD) clean_tree "."; fi
	/bin/rm -f $(JAVACLASSES)
	/bin/rm -f $(LOGFILES)

distclean: realclean
	/bin/rm -f *~ 


