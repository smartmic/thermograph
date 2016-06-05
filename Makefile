# Basic Makefile for the tortoise package.
CC = gcc

CFLAGS = -Wall -O
LIBS = -L. -lsteam97 -L/home/martin/ressources/freesteam-2.1 -rdynamic -lgsl -lgslcblas -lm -lfreesteam -Wl,-rpath,/home/martin/ressources/freesteam-2.1
INC = -I. -I/home/martin/ressources/freesteam-2.1

.PHONY: clean build run prepare

build: thermograph

clean:
	rm -f thermograph thermograph.o 

run: thermograph
	./thermograph

thermograph: thermograph.o
	$(CC) -pg $< -o $@ $(LIBS) 

thermograph.o: thermograph.c
	$(CC) -pg -c $< -o $@ $(CFLAGS) $(INC)

thermograph.c: model_f.h ws_table.h

