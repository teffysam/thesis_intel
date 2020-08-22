##################################################################
#   FLASH90                                                      #
#   FLexible Adaptive Semi-Lagrangian Hack                       #
#   written in Fortran 90                                        #
##################################################################
# makefile to build FLASH                                        #
# j. behrens 2/95, 3/96, 8/98                                    #
# --- this is for linux ---                                      #
##################################################################

# MACHINE
MACHINE = macosx_gfortran

# SYSTEM
SYSTEM = gfortran

# SET MAKETHING CORRESPONDING TO MACHINE:
MAKETHING= FLASH3D

# OPTIMIZATION SETTINGS [debug|norm|opt]
MODE := debug

# SET MAIN DIRECTORY PATH
# !! This has to be alterd by user !!
ROOTDIR = $(HOME)/Documents/Development/Gerwing_Flash

# SET atlas/blas DIRECTORY PATH
# !! This has to be alterd by user !!
BLASDIR =  
BLASLIB = 

# SET LAPACK DIRECTORY PATH
# !! This has to be alterd by user !!
LAPACKDIR = 
LAPACKLIB = -framework Accelerate

# SET VISNET DIRECTORY PATH
# !! This has to be alterd by user !!
VISNETDIR = #$(ROOTDIR)/lib/3DSINGLE
VISNETLIB = #-lvisnet4flash -lm3tv -ldelaunay \
	    #-ldetri -llia -lsos -lbasic \
	    #$(VISNETDIR)/fortify.o \
	    #-lglut -lGLU -lGL -ltiff 

# SET NETCDF PATHS
# !! This has to be alterd by user !!
LIBNETCDF = -L/sw2/lib -lnetcdff -lnetcdf
INCNETCDF = -I/sw2/include

# SET C++ DIRECTORY PATH
# !! This has to be alterd by user !!
CCLIBDIR = -L/usr/lib/
CCLIB = -lpthread -lstdc++

# SET MORE DIRECTORY PATHS
LIBDIR  = $(ROOTDIR)/amatos3d/lib/$(MACHINE)
INCDIR  = $(ROOTDIR)/amatos3d/include/$(MACHINE)
MODDIR  = $(INCDIR)
MAINDIR = $(ROOTDIR)/flash3d

SRCDIR  = $(MAINDIR)/src/flash
OPTDIR  = $(MAINDIR)/src/options
SYSDIR  = $(MAINDIR)/src/system/$(SYSTEM)
TIMDIR  = $(MAINDIR)/src/timing
DATDIR  = $(MAINDIR)/data
BUILDIR = $(MAINDIR)/compile/$(MACHINE)

AMATOS    = $(INCDIR)/GRID_API.mod

# SET THE ENDING OF MODULE FILES
MODEND  = mod

#----------------------------------------------------------------#
# FLAGS FOR LINUX                                                #
#----------------------------------------------------------------#

F90 	= gfortran
cc 	= gcc
CC 	= g++
LOADER  = gfortran
# --------------------- next are for debugging ------------------#
ifeq ($(strip $(MODE)),debug)
  FFLAGS  = -fbounds-check -ggdb -fpic #-funderscoring # -C
  CFLAGS  = -ggdb -fpic
  LDFLAGS = -ggdb
endif
# --------------------- next are for optimized debugging --------#
ifeq ($(strip $(MODE)),optdebug)
  FFLAGS  = -fPIC -funderscoring # -C
  CFLAGS  = -g -fpic
  LDFLAGS = -g
  SHFLAGS = -fPIC -shared
endif
# --------------------- next are for normal compilation ---------#
ifeq ($(strip $(MODE)),norm)
  FFLAGS  = -fPIC -funderscoring
  CFLAGS  = -fpic
  LDFLAGS =
endif
# --------------------- next are for optimization ---------------#
ifeq ($(strip $(MODE)),opt)
  FFLAGS  = -O3 -fPIC -funderscoring
  CFLAGS  = -O3 -fpic
  LDFLAGS = -O3
endif

# -------- next flag for using dummy graphics library -----------#
LIBS = -L$(LIBDIR) -lamatos3d  \
	          $(LAPACKDIR) $(LAPACKLIB) $(BLASDIR) $(BLASLIB)\
		      $(LIBNETCDF) $(CCLIBDIR) $(CCLIB)\
        	    -lm
INCS = $(INCNETCDF) -I$(INCDIR) -I$(MODDIR)

##################################################################
# AFTER THIS LINE, NO CHANGES SHOULD BE NECESSARY                #
##################################################################

#----------------------------------------------------------------#
# OBJECTS                                                        #
#----------------------------------------------------------------#
MAINOBJ = \
FLASH_parameters.o \
MISC_timing.o \
MISC_system.o \
IO_utils.o \
IO_matlabplot.o \
IO_gmvplot.o \
IO_vtu.o \
IO_vtuplot.o \
SLM_initial.o \
SLM_errorestimate.o \
ADV_wind.o \
date_time.o \
ADV_rhs.o \
SLM_simple.o \
SLM_advanced.o \
ADV_semilagrange.o \
Flash90.o

#----------------------------------------------------------------#
# COMPILE STEP                                                   #
#----------------------------------------------------------------#

.SUFFIXES: .F90 .f90 $(SUFFIXES)

.F90.o:
	@echo "make: Building object module from "$<
	$(F90) $(FFLAGS) $(INCS) -c $<

.f90.o:
	@echo "make: Building object module from "$<
	$(F90) $(FFLAGS) $(INCS) -c $<

.c.o:
	@echo "make: Building object module from "$<
	$(CC) $(CFLAGS) $(INCS) -D$(MACHINE) -c $<

clearsrc::
	@rm -f *.f90 *.h *.F90

clearex::
	@rm -f $(MAKETHING) Flash3D* fort.*

cleardat::
	@rm -f *.dat *.gmv *.vtu

clean::
	@rm -f *.o *.$(MODEND) core *.gmv *.vtu

tidy::
	make clean
	make clearex
	make cleardat
	make clearsrc

#----------------------------------------------------------------#
# THIS COPIES REQUIRED DATA FILES                                #
#----------------------------------------------------------------#

datacopy::
	@cp $(DATDIR)/Domain.dat .
	@cp $(DATDIR)/Triang.dat .
	@cp $(DATDIR)/Parameters.dat .


#----------------------------------------------------------------#
# THIS CREATES PREDEFINED OPTIONS                                #
#----------------------------------------------------------------#

maincopy::
	@cp $(SRCDIR)/*.f90 .
	@cp $(SRCDIR)/*.F90 .
	@cp $(SYSDIR)/*.f90 .
	@cp $(TIMDIR)/*.f90 .

Clip::
	@cp $(OPTDIR)/ADV_semilagrange.sim ADV_semilagrange.f90
	@cp $(OPTDIR)/SLM_simple.clip SLM_simple.f90

Dual::
	@cp $(OPTDIR)/ADV_semilagrange.adv ADV_semilagrange.f90
	@cp $(OPTDIR)/SLM_simple.clip SLM_simple.f90
	@cp $(OPTDIR)/SLM_advanced.dual SLM_advanced.f90

Pinatubo::
	@make maincopy
	@cp $(OPTDIR)/ADV_wind.pinatubo ADV_wind.f90
	@cp $(OPTDIR)/ADV_rhs.pinatubo ADV_rhs.f90
	@make datacopy
	@cp $(DATDIR)/Wind_pinatubo.nc Windparam.dat
	@cp $(DATDIR)/Triang_pinatubo.dat Triang.dat

Small_Pinatubo::
	@make maincopy
	@cp $(OPTDIR)/ADV_wind.pinatubo ADV_wind.f90
	@cp $(OPTDIR)/ADV_rhs.pinatubo_climaticphase ADV_rhs.f90
	@make datacopy
	@cp $(DATDIR)/Wind_pinatubo_climaticphase.nc Windparam.dat
	@cp $(DATDIR)/Triang_pinatubo_1cube.dat Triang.dat

Quadrature::
	@cp $(OPTDIR)/ADV_semilagrange.adv ADV_semilagrange.f90
	@cp $(OPTDIR)/SLM_simple.clip SLM_simple.f90
	@cp $(OPTDIR)/SLM_advanced.quadrature SLM_advanced.f90

MPSLM::
	@cp $(OPTDIR)/ADV_semilagrange.adv ADV_semilagrange.f90
	@cp $(OPTDIR)/SLM_simple.clip SLM_simple.f90
	@cp $(OPTDIR)/SLM_advanced.mpslm SLM_advanced.f90

ConvWind::
	@cp $(OPTDIR)/ADV_wind.conv ADV_wind.f90
	@cp $(OPTDIR)/SLM_initial.conv SLM_initial.f90

DiagWind::
	@cp $(OPTDIR)/ADV_wind.diag ADV_wind.f90
	@cp $(OPTDIR)/SLM_initial.diag SLM_initial.f90

Kaeser::
	@cp $(OPTDIR)/ADV_wind.kaeser ADV_wind.f90
	@cp $(OPTDIR)/SLM_initial.kaeser SLM_initial.f90

Circular::
	@cp $(OPTDIR)/ADV_wind.circ ADV_wind.f90
	@cp $(OPTDIR)/SLM_initial.circ SLM_initial.f90

#----------------------------------------------------------------#
# THIS COMPILES THE MAIN PROGRAM                                 #
#----------------------------------------------------------------#

executable: $(MAINOBJ)
	@echo "make: Linking object modules and libraries"
	$(LOADER) $(LDFLAGS) -o $(MAKETHING) $(MAINOBJ) $(LIBS) 

$(MAKETHING)::
	@make maincopy
	@make executable

all::
	@make maincopy
	@make executable
	@make clearsrc
	@make clean

#----------------------------------------------------------------#
# DEPENDENCIES ON INCLUDE FILES                                  #
#----------------------------------------------------------------#
