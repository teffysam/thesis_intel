**************************************************************************

 amatos
 adaptive mesh generator for atmospheric and oceanic simulations

**************************************************************************

DIRECTORY STRUCTURE:

.			- this directory
./data			- directory with data for testing
./irix			- directory with Makefile for IRIX
./linux			- directory with Makefile for Linux
./matlab		- MATLAB script for vizualization
./src/test		- directory with sources for test drivers
./src/gridgen/serial	- directory with the serial mesh generator
./src/system/nag-f90	- directory with system dependent routines for NAG F90
./src/system/std-f90	- directory with system dependent routines for standard F90
./src/timing		- directory with timing support
./solaris		- directory with Makefile for Solaris

**************************************************************************

IN ORDER TO CREATE AN EXECUTABLE:

1. Change into directory ./irix (on SGI IRIX machines only!)
2. Type "make all"

**************************************************************************

EXECUTION:

unix> AMATOS3D

prints the following output:

 USAGE: AMATOS3D        [-i|-b] {-d} {-f name} {-h} {-l} {-o} {-r}
        -i: interactive input mode
        -b: batch input mode (read from file)
        -d: switch on diagnostics
        -f: input filename is << name >>
        -h: help information (this output)
        -l: switch on log file output
        -o: redirect standard output to a file
        -r: release information
 Copyright (c) 1996, 1997, 1998, Jörn Behrens 
 STOPPED ... hope this made it clear

Use AMATOS3D in the following way:
- copy data files to the working directory by "make datacopy"
- run AMATOS3D by the following command:
      unix> AMATOS3D -b -f Parameters.dat

The program anwers with something like:

 ***** ***** ***** ***** ***** ***** ***** ***** ***** *****
 ***** PROGRAM:   AMATOS3D                             *****
 ***** VERSION:   00.01.00                             *****
 *****            Started in BATCH input mode          *****
 ***** INPUTFILE: Parameters.dat                       *****
 ***** ***** ***** ***** ***** ***** ***** ***** ***** *****

 ***** ***** ***** ***** ***** ***** ***** ***** ***** *****
 ***** Longest edge in triangulation:      0.11180E+01 *****
 ***** Shortest edge in triangulation:     0.25000E+00 *****
 ***** ***** ***** ***** ***** ***** ***** ***** ***** *****

AMATOS3D creates the following files:

  AMATOS3D_matlab.nnnn
  AMATOS3D_gmv.nnnn

where 'nnnn' is an integer (with preceeding zeroes). These files can be examined
by either MATLAB (see below for instructions) or GMV (see the GMV Homepage
http://laws.lanl.gov:80/XCM/gmv/GMVHome.html for details).

Visualization with MATLAB:

- copy one of the files AMATOS3D_matlab.nnnn to directory ./matlab
- change into ./matlab
- rename AMATOS3D_matlab.nnnn to infile.dat
- start MATLAB (unix> matlab)
- execute MATLAB script poly3D.m (>> poly3D)

**************************************************************************

QUESTIONS TO:

J. Behrens, behrens@ma.tum.de, http://www-m3.ma.tum.de/m3/behrens/

**************************************************************************

- [Back to top level README](../README.md)