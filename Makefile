# Basic Makefile for the tortoise package.
CC = gcc

CFLAGS = -Wall -O
LIBS = -L. -lsteam97 -lgsl -lgslcblas -lm 
INC = -I. 

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

