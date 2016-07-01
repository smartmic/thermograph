# Basic Makefile for the tortoise package.
CC = gcc

CFLAGS = -Wall -Werror -fpic
LIBS = -L../libs -lsteam97 -lgsl -lgslcblas -lm 
INC = -I. 

.PHONY: clean build run 

build: thermograph

clean:
	rm -f thermograph *.o 

run: thermograph
	./thermograph

thermograph: thermograph.o model_f.o model_df.o model_fdf.o if97dna.o
	$(CC) *.o -o $@ $(LIBS) 

.c.o:
	$(CC) -c $< -o $@ $(CFLAGS) $(INC)


