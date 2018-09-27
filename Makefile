default: libfregex.a

OBJS = lib/objs
MODS = lib/mods

FC = ifort

ifeq "$(FC)" "gfortran"
    FFLAGS = -Dgfortran
else
ifeq "$(FC)" "ifort"
    FFLAGS = -Difort -cpp
endif
endif


fregex.o: src/fregex.f90 pcre_constants.o
	$(FC) -o $@ -c $< $(FFLAGS)

fmatch.o: src/fmatch.f90 pcre_constants.o
	$(FC) -o $@ -c $< $(FFLAGS)

pcre_constants.o: src/pcre_constants.f90
	$(FC) -o $@ -c $< $(FFLAGS)

libfregex.a: fmatch.o fregex.o pcre_constants.o
	mkdir -p lib/objs
	mkdir -p lib/mods
	ar cr lib/$@ $^
	mv *.mod $(MODS)
	mv *.o $(OBJS)

clean:
	rm -rf lib/
