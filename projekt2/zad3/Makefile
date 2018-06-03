all:
	gfortran -std=f2008 -O2 -Wall -pedantic -fimplicit-none -c zad1.F90 cache.F90 cache+dot_prod.F90 dot_prod.F90
	gfortran -std=f2008 -O2 -Wall -pedantic -fimplicit-none -c main.F90
	gfortran -std=f2008 -O2 -Wall -pedantic -fimplicit-none zad1.o cache.o cache+dot_prod.o dot_prod.o main.o -o program
	make clean
clean:
	rm -f *.o
	rm -f *.mod
	rm -f *.mod0
