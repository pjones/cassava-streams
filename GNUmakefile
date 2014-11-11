################################################################################
# From util/haskell.mk (git submodule update --init)
CABAL_FLAGS = --enable-tests -fmaintainer -ftutorial
include util/haskell.mk

################################################################################
CSV_HEADER     = test/header.csv
CSV_MEGA       = test/massive.csv
TUTORIAL       = dist/build/tutorial/tutorial todo
PROF_OPTS      = +RTS -sstderr -hc -pa -xc

################################################################################
clean::
	rm -f $(CSV_MEGA)
	rm -f $(foreach e,aux hp pdf prof ps,tutorial.$(e))

################################################################################
profile:: $(CSV_MEGA)
	$(TUTORIAL) $(PROF_OPTS) < $(CSV_MEGA) > profile.stdout
	hp2ps -M -b -c tutorial.hp
	ps2pdf tutorial.ps

################################################################################
# Generate a very large CSV file for profiling.
$(CSV_MEGA): $(CSV_HEADER)
	sh test/make-huge-csv.sh $@ $(CSV_HEADER)
	du -h $@
