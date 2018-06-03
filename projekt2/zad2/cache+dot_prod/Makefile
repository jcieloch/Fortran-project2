PFUNIT = /home/kuba/pfunit
F90_VENDOR = GNU
F90=gfortran

include $(PFUNIT)/include/base.mk

FFLAGS += -std=f2008 -Wall -pedantic -I$(PFUNIT)/mod
LIBS = $(PFUNIT)/lib/libpfunit$(LIB_EXT)

PFS = $(wildcard *.pf)
OBJS = $(PFS:.pf=.o)


%.F90: %.pf
	$(PFUNIT)/bin/pFUnitParser.py $< $@

%.o: %.F90
	$(F90) $(FFLAGS) -c $<

test: 
	$(F90) $(FFLAGS) -c zad1.F90
	make tests.x
tests.x: testSuites.inc testy.o $(OBJS)
	$(F90) -o $@ -I. -I$(PFUNIT)/mod -I$(PFUNIT)/include $(PFUNIT)/include/driver.F90 ./*$(OBJ_EXT) $(LIBS) $(FFLAGS)

clean:
	rm -f *.o *.mod
	rm -f *.x
