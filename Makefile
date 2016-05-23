# Basic Makefile for the tortoise package.
CC = clang

CFLAGS = -Wall -O
LIBS = -L/home/martin/ressources/freesteam-2.1 -rdynamic -lgsl -lgslcblas -lm -lfreesteam -Wl,-rpath,/home/martin/ressources/freesteam-2.1
INC = -I. -I/home/martin/ressources/freesteam-2.1

.PHONY: clean build run

build: thermograph

clean:
	rm -f thermograph thermograph.o

run: thermograph
	./thermograph

thermograph: thermograph.o
	$(CC) $< -o $@ $(LIBS) 

thermograph.o: thermograph.c
	m4 eqnbuilder.m4 > model_f.h
	$(CC) -c $< -o $@ $(CFLAGS) $(INC)
