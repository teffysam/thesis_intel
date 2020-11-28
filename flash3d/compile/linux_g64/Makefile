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
MACHINE = linux_g64

# SET MAKETHING CORRESPONDING TO MACHINE:
MAKETHING= FLASH3D

# OPTIMIZATION SETTINGS [debug|norm|opt]
MODE := debug

# SET MAIN DIRECTORY PATH
# !! This has to be alterd by user !!
ROOTDIR = $(HOME)/thesis_intel

# SET atlas/blas DIRECTORY PATH
# !! This has to be alterd by user !!
#BLASDIR =  /home/teffy/lapack/lapack-3.9.0/build/lib
BLASDIR = /home/teffy/spack/opt/spack/linux-arch-skylake/intel-19.1.1.217/openblas-0.3.10-hfycupkeohg5d5rbocnv35hoh6li25yh/include
BLASLIB = 

# SET LAPACK DIRECTORY PATH
# !! This has to be alterd by user !!
#LAPACKDIR = /home/teffy/lapack/lapack-3.9.0/build/lib
#LAPACKDIR = /home/teffy/spack/opt/spack/linux-arch-skylake/intel-19.1.1.217/openblas-0.3.10-hfycupkeohg5d5rbocnv35hoh6li25yh/lib
LAPACKDIR = /opt/intel/mkl/include/
LAPACKLIB = -mkl -lpthread -lc 

# SET VISNET DIRECTORY PATH
# !! This has to be alterd by user !!
VISNETDIR = #$(ROOTDIR)/lib/3DSINGLE
VISNETLIB = #-lvisnet4flash -lm3tv -ldelaunay \
	    #-ldetri -llia -lsos -lbasic \
	    #$(VISNETDIR)/fortify.o \
	    #-lglut -lGLU -lGL -ltiff 

# SET C++ DIRECTORY PATH
# !! This has to be alterd by user !!
# CCLIBDIR = /usr/lib/x86_64-linux-gnu/
CCLIBDIR = /usr/lib#/gcc/x86_64-pc-linux-gnu/4.8.5/
CCLIB = -pthread -lstdc++

# SET MORE DIRECTORY PATHS
LIBDIR  = $(HOME)/thesis_intel/amatos3d/lib/$(MACHINE)
INCDIR  = $(HOME)/thesis_intel/amatos3d/include/$(MACHINE)
MODDIR  = $(INCDIR)
MAINDIR = $(ROOTDIR)/flash3d

SRCDIR  = $(MAINDIR)/src/flash
OPTDIR  = $(MAINDIR)/src/options
SYSDIR  = $(MAINDIR)/src/system/std-f90
TIMDIR  = $(MAINDIR)/src/timing
DATDIR  = $(MAINDIR)/data
BUILDIR = $(MAINDIR)/$(MACHINE)

# SET LIBRARY PATHS
# LIBNETCDF = 
#LIBNETCDF = -L/usr/local/lib -lnetcdff #-L/sw/wheezy-x64/netcdf-4.3.2-gccsys/lib -lnetcdf
LIBNETCDF = -L/home/teffy/spack/opt/spack/linux-arch-skylake/intel-19.1.1.217/netcdf-fortran-4.5.2-3mzij622xh7vjfixz7qo4xmy4vnz3la3/lib -lnetcdff

# SET INCLUDE PATHS
# INCNETCDF =
INCNETCDF = -I/home/teffy/spack/opt/spack/linux-arch-skylake/intel-19.1.1.217/netcdf-fortran-4.5.2-3mzij622xh7vjfixz7qo4xmy4vnz3la3/include
AMATOS    = $(INCDIR)/grid_api.mod

# SET THE ENDING OF MODULE FILES
MODEND  = mod

#----------------------------------------------------------------#
# FLAGS FOR LINUX                                                #
#----------------------------------------------------------------#

F90 	= ifort
cc 	= icc
CC 	= icc
LOADER  = ifort
# --------------------- next are for debugging ------------------#
ifeq ($(strip $(MODE)),debug)
  FFLAGS  = -O0 -CB -ggdb#-fbounds-check-fpic -funderscoring # -C
  CFLAGS  = -ggdb -fpic
  LDFLAGS = -ggdb
endif
# --------------------- next are for optimized debugging --------#
ifeq ($(strip $(MODE)),optdebug)
  FFLAGS  = -fast #-fPIC -funderscoring # -C
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
	           -L$(LAPACKDIR) $(LAPACKLIB) -L$(BLASDIR) $(BLASLIB)\
		      $(LIBNETCDF) -L$(CCLIBDIR) $(CCLIB)\
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
	@rm -f ../../../*.dat ../../../*.gmv ../../../*.vtu

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
	@cp $(DATDIR)/Domain.dat ../../../
	@cp $(DATDIR)/Triang.dat ../../../
	@cp $(DATDIR)/Parameters.dat ../../../


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
	@cp $(SRCDIR)/ADV_sp_semilagrange.f90 ADV_semilagrange.f90
	@cp $(SRCDIR)/SLM_simple_small_pinatubo.f90 SLM_simple.f90
	@cp $(SRCDIR)/SLM_advanced_small_pinatubo.f90 SLM_advanced.f90
	@make datacopy
	@cp $(DATDIR)/Wind_pinatubo_climaticphase.nc ../../../Windparam.dat
	@cp $(DATDIR)/Triang_pinatubo_1cube.dat ../../../Triang.dat

Agulhas::
	@make maincopy
	@cp $(OPTDIR)/ADV_wind.agulhas.tstep.f90 ADV_wind.f90
	@cp $(OPTDIR)/ADV_rhs.agulhas.f90 ADV_rhs.f90
	@make datacopy
	@ln -s /pool/c_grid_agulhas.nc ../../../Windparam.dat
	@cp $(DATDIR)/Triang_agulhas_test_cuboid.dat ../../../Triang.dat

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

Berlin::
	@cp $(OPTDIR)/ADV_wind.berlin ADV_wind.f90
	@cp $(OPTDIR)/SLM_initial.berlin SLM_initial.f90

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
