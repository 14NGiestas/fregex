SRC = ../src

FC = ifort
FFLAGS = -g -traceback

default: main

fregex.o: $(SRC)/fregex.f90
	$(FC) -o $@ -c $< $(FFLAGS)
	
fmatch.o: $(SRC)/fmatch.f90 pcre_constants.o
	$(FC) -o $@ -c $< $(FFLAGS)

pcre_constants.o: $(SRC)/pcre_constants.f90
	$(FC) -o $@ -c $< $(FFLAGS)

main: fmatch.o fregex.o pcre_constants.o
	$(FC) -o $@ simple_example.f90 $^ -lpcre $(FFLAGS)

clean:
	rm -f *.o *.mod main
