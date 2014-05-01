################################################################################
# From util/haskell.mk (git submodule update --init)
CABAL_FLAGS = --enable-tests -fmaintainer -ftutorial
include util/haskell.mk

################################################################################
CSV_HEADER     = test/header.csv
CSV_MEGA       = test/massive.csv
TUTORIAL       = dist/build/tutorial/tutorial todo
PROF_OPTS      = +RTS -sstderr -hc -pa -xc
PROF_ROW_SIZE  = 512
PROF_ROW_COUNT = 10000 # Actual rows will be this number * 100
RAND_ROW       = openssl rand -base64 $(PROF_ROW_SIZE) | paste -sd/


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
	for i in `seq $(PROF_ROW_COUNT)`; do \
          row=`printf "TODO,"; $(RAND_ROW)`; \
          for j in `seq 100`; do echo "$$row" >> $@; done; \
          printf .; \
        done
	@ echo # print a newline
	du -h $@
