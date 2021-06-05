FC = gfortran
FFLAGS = 
LIB_DIR = \
-L../src/

LIBS = -lforlab

test: test.o
	$(FC) -o test test.o $(LIB_DIR) $(LIBS) -I../src/

test.o: test.f90
	$(FC) -c test.f90 $(LIB_DIR) $(LIBS) -I../src/
.PHONY:clean
clean:
	$(RM) *.o *.mod
