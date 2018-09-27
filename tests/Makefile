SRC = ../src

FC = ifort
FFLAGS = -g -traceback

default: main


libfregex.a:
	$(MAKE) -C ../

main: ../lib/libfregex.a
	$(FC) -o $@ simple_example.f90 $^ -lpcre $(FFLAGS) -I../lib/mods

clean:
	rm -f *.o *.mod main
