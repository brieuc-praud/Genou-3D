EXE = main
F90 = gfortran
OPT = -O0 -Wall -pedantic
OBJ = variables.o fonction.o jacobienne.o mod_LU.o Newton.o $(EXE).o

$(EXE)	: $(OBJ)
	$(F90) $(OPT) -o $(EXE) $^

%.o	: %.f90
	$(F90) $(OPT) -c $<

clean	:
	rm *.o *.mod $(EXE)

exe	: $(EXE)
	./$(EXE)

anim 	:
	python3 animation.py

exeplot :
	make exe && make plot

exeanim :
	make exe && make anim
