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
	$(CC) $< -o $@ $(LIBS) 

thermograph.o: thermograph.c
	$(CC) -c $< -o $@ $(CFLAGS) $(INC)

thermograph.c: model_fdf.h ws_table.h

