EXE = main
F90 = gfortran
OPT = -O0 -Wall -pedantic
OBJ = mod_variables.o mod_erreur.o  mod_LU.o mod_BFGS.o mod_recuit.o $(EXE).o

$(EXE)	: $(OBJ)
	$(F90) $(OPT) -o $(EXE) $^

%.o	: %.f90
	$(F90) $(OPT) -c $<

clean	:
	rm *.o *.mod *.dat $(EXE)

exe	: $(EXE)
	./$(EXE)

anim 	:
	python3 animation.py

exeplot :
	make exe && make plot

exeanim :
	make exe && make anim
