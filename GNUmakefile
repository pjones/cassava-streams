################################################################################
# From util/haskell.mk (git submodule update --init)
CABAL_FLAGS = --enable-tests -fmaintainer -ftutorial
include util/haskell.mk

################################################################################
CSV_HEADER = test/header.csv
CSV_MEGA   = test/massive.csv
RAND_ROW   = openssl rand -base64 1048576 | paste -sd/
TUTORIAL   = dist/build/tutorial/tutorial todo
PROF_OPTS  = +RTS -sstderr -hc -pa -xc

################################################################################
clean::
	rm -f $(CSV_MEGA)
	rm -f $(foreach e,aux hp pdf prof ps,tutorial.$(e))

################################################################################
profile:: $(CSV_MEGA)
	$(TUTORIAL) $(PROF_OPTS) < $(CSV_MEGA) > /dev/null
	hp2ps -M -b -c tutorial.hp
	ps2pdf tutorial.ps

################################################################################
# Generate a very large CSV file for profiling.
$(CSV_MEGA): $(CSV_HEADER)
	cp $(CSV_HEADER) $@
	for n in `seq 1024`; do (printf "TODO,"; $(RAND_ROW)) >> $@; printf .; done
	@ echo # print a newline
	du -h $@
