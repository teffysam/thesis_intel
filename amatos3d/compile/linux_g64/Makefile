##################################################################
#   SPLASH/fe                                                    #
#   Semi-Lagrangian Parallel and Locally Adaptive                #
#   SHallow-water model/ with Finite Elements                    #
##################################################################
# makefile to build SPLASH                                       #
# j. behrens 2/95, 3/96, 5/97                                    #
# --- this is for linux ---                                      #
##################################################################

# MACHINE
MACHINE = linux_g64

# SYSTEM
SYSTEM = std-f90

# SET MAKETHING CORRESPONDING TO MACHINE:
MAKETHING= AMATOS3D
AMATFLAG = $(MF)$(MAKETHING)

# PARALLELISM SETTINGS
OMP := no
MPI := no

# LIBRARY SETTINGS [yes|no]
# Useage: make IO_EMIT=yes IO_PGM=yes
IO_EMIT := no
IO_PGM := no

SAVETHING= SAVETEST

# OPTIMIZATION SETTINGS [debug|opt|norm|opt]
MODE := debug


# ADDITIONAL MACRO DEFS
MACROS =

# SET MAIN DIRECTORY PATH
# !! This has to be alterd by user !!
ROOTDIR = $(HOME)/thesis_intel

# SET atlas/blas DIRECTORY PATH
# !! This has to be alterd by user !!
BLASDIR = /home/teffy/spack/opt/spack/linux-arch-skylake/intel-19.1.1.217/openblas-0.3.10-hfycupkeohg5d5rbocnv35hoh6li25yh/lib
BLASLIB = -lblas #comment for intel

# SET LAPACK DIRECTORY PATH
# !! This has to be alterd by user !!
# LAPACKDIR =
#LAPACKDIR = /home/teffy/lapack/lapack-3.9.0/build/lib
LAPACKDIR =  /opt/intel/mkl/include/#/home/teffy/spack/opt/spack/linux-arch-skylake/intel-19.1.1.217/openblas-0.3.10-hfycupkeohg5d5rbocnv35hoh6li25yh/include #/opt/intel/mkl/include
LAPACKLIB =  -mkl #-lpthread -lc #-mkl

# SET IO_EMIT LIBRARY PATH
EMITDIR =  /usr/lib/# gcc/x86_64-pc-linux-gnu/4.8.5/
EMITLIB =  -static-stdc++ #-lresolv -lsocket -lnsl -lCstd

# SET BITMAP LIBRARY PATH (not relevant for amatos3d)
BITMAPDIR = 
BITMAPLIB = -lnetpbm


# SET MORE DIRECTORY PATHS
MAINDIR = $(ROOTDIR)/amatos3d


BUILDIR = $(MAINDIR)/compile/$(MACHINE)
LIBDIR  = $(MAINDIR)/lib/$(MACHINE)
INCDIR  = $(MAINDIR)/include/$(MACHINE)
DATDIR  = $(MAINDIR)/data
SRCDIR = $(MAINDIR)/src

GRIDDIR = $(SRCDIR)/gridgen
TSTDIR  = $(SRCDIR)/test
SYSDIR  = $(SRCDIR)/system/$(SYSTEM)
TIMDIR  = $(SRCDIR)/timing

GRIDLIB = libamatos3d.a
GRDSLIB = libamatos3d.so
MODEND  = mod
GRDMOD  = *.mod

#----------------------------------------------------------------#
# FLAGS FOR LINUX                                                #
#----------------------------------------------------------------#

F90    = ifort
cc     = icc
CC     = icc
LOADER = ifort
AR     = ar
CP     = cp
ARFLAGS = vru
# Compiler flag for macros
MF = -D 

# --------------------- next are for debugging ------------------#
ifeq ($(strip $(MODE)),debug)
  FFLAGS  = -fbounds-check -ggdb -fpic -funderscoring # -C
  CFLAGS  = -ggdb -fpic
  LDFLAGS = -ggdb
  SHFLAGS = -fpic -shared
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
  SHFLAGS = -fPIC -shared
endif
# --------------------- next are for optimization ---------------#
ifeq ($(strip $(MODE)),opt)
  FFLAGS  = -O3 -fPIC -funderscoring
  CFLAGS  = -O3 -fpic
  LDFLAGS = -O3
  SHFLAGS = -fPIC -shared
endif

# -------- next flag for using dummy graphics library -----------#
LIB_SH  = -L$(LAPACKDIR) $(LAPACKLIB)
LIBS =  -L$(LIBDIR) -lamatos3d libamatos3d.so -L$(LAPACKDIR) $(LAPACKLIB) -L$(BLASDIR) $(BLASLIB) -L.
INCPATH   = -I$(INCDIR) -I.
SHLIBPATH = #-L$(LIBDIR) -L$(LAPACKDIR) -L$(BLASDIR) -L.
INCPATH_CC = -I$(BITMAPDIR)

#----------------------------------------------------------------#
# OBJECTS                                                        #
#----------------------------------------------------------------#
OBJMAIN = \
MAIN_parameters.o \
MISC_system.o \
MISC_timing.o \
MISC_functions.o \
ERROR_est.o \
IO_gmvplot.o \
IO_matlabplot.o \
IO_utils.o \
MISC_datafun.o \
MISC_poisson.o \
Amatos3d.o

SEROBJ = \
QSHEP3D.o \
MISC_globalparam.o \
MISC_error.o \
FEM_param.o \
FEM_define.o \
MISC_utils.o \
FEM_handle.o \
FEM_basis.o \
FEM_utils.o \
FEM_create.o \
FEM_gridmanag.o \
FEM_gridgen.o \
FEM_inittriang.o \
FEM_interpolation.o \
FEM_saveset.o \
FEM_signature.o \
MISC_wrapper.o \
FEM_dataretrieve.o \
GRID_api.o
ifeq ($(strip $(SYSTEM)),std-f90)
  SEROBJ += MISC_wrapper.o
endif

#----------------------------------------------------------------#
# common stuff                                                   #
#----------------------------------------------------------------#

include $(MAINDIR)/compile/Makefiles/Makefile.common


#----------------------------------------------------------------#
# copy source files                                              #
#----------------------------------------------------------------#

include $(MAINDIR)/compile/Makefiles/Makefile.cpsrc


#----------------------------------------------------------------#
# COMPILE STEP (CONVENTIONS)                                     #
#----------------------------------------------------------------#
# see $(MAINDIR)/compile/Makefiles/Makefile.common


#----------------------------------------------------------------#
# MAKE EVERYTHING (CREATE THE EXECUTABLE, INSTALL, CLEAN UP)     #
#----------------------------------------------------------------#
# see $(MAINDIR)/compile/Makefiles/Makefile.common


#----------------------------------------------------------------#
# THIS CREATES THE GRID GENERATOR LIBRARIES                      #
#----------------------------------------------------------------#
# see also $(MAINDIR)/compile/Makefiles/Makefile.common


$(GRDSLIB): $(GRIDLIB)
	@echo "make: Creating dynamic shared object (dso) library" $(SHFLAGS)
	$(LOADER) $(SHFLAGS) -o $(GRDSLIB) $(SEROBJ) $(LIB_SH)


#----------------------------------------------------------------#
# THIS CREATES THE MAIN PROGRAM                                  #
#----------------------------------------------------------------#
# see also $(MAINDIR)/compile/Makefiles/Makefile.common

$(SAVETHING): $(OBJSAVE)
	@echo "make: Linking object modules and libraries"
	@$(LOADER) $(LDFLAGS) -o $(SAVETHING) $(OBJSAVE) $(SHLIBPATH) $(LIBS)

exsave: $(SAVETHING)

SAVETEST: $(SAVETHING) clearsrc

#----------------------------------------------------------------#
# THIS COPIES REQUIRED DATAFILES                                 #
#----------------------------------------------------------------#

datacopy:
	@echo "make: Copy required data files"
	$(CP) $(CPFLAGS) $(DATDIR)/Triang.dat Triang.dat
	$(CP) $(CPFLAGS) $(DATDIR)/Parameters.dat Parameters.dat
	$(CP) $(CPFLAGS) $(DATDIR)/*.ftf .


executable: $(OBJMAIN)
	@echo "make: Linking object modules and libraries"
	@$(LOADER) $(LDFLAGS) -o $(MAKETHING) $(OBJMAIN) $(SHLIBPATH) $(LIBS)


#----------------------------------------------------------------#
# THIS RUNS THE PROGRAMS                                         #
#----------------------------------------------------------------#

test:	$(MAKETHING) $(SAVETHING)
	@echo "make: Run tests"
	@$(MAKETHING) -b -f Parameters.dat
	@$(SAVETHING) $(MAKETHING)_save.0010


#----------------------------------------------------------------#
# CLEAN UP							 #
#----------------------------------------------------------------#
# see $(MAINDIR)/compile/Makefiles/Makefile.common


#----------------------------------------------------------------#
# DEPENDENCIES ON INCLUDE FILES                                  #
#----------------------------------------------------------------#
