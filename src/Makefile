# Example Makefile for Thermograph
# 
# This file is part of Thermograph.
# 
# Thermograph is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# Thermograph is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with Thermograph. If not, see <http://www.gnu.org/licenses/>.
# 
# Copyright 2016 Martin Michel

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


