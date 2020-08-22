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
MACHINE = linux_ia32

# SET MAKETHING CORRESPONDING TO MACHINE:

# PARALLELISM SETTINGS
OMP := no
MPI := no

# LIBRARY SETTINGS [yes|no]
# Useage: make IO_EMIT=yes IO_PGM=yes
IO_EMIT := no
IO_PGM := no
MAKETHING= AMATOS3D
AMATFLAG = $(MF)$(MAKETHING)

SAVETHING= SAVETEST

# OPTIMIZATION SETTINGS [debug|opt|norm|opt]
MODE := norm


# ADDITIONAL MACRO DEFS
MACROS =


# SET MAIN DIRECTORY PATH
# !! This has to be alterd by user !!
ROOTDIR = $(HOME)/Development

# SET MAIN DIRECTORY PATH
# !! This has to be alterd by user !!
ROOTDIR = $(HOME)/Development

# SET atlas/blas DIRECTORY PATH
# !! This has to be alterd by user !!
BLASDIR = /usr/lib/
BLASLIB = 

# SET LAPACK DIRECTORY PATH
# !! This has to be alterd by user !!
LAPACKDIR =
#LAPACKDIR = /usr/lib/
LAPACKLIB = -llapack -lpthread -lc	#-lmkl_ia32 -lguide 

# SET IO_EMIT LIBRARY PATH
EMITDIR = 
EMITLIB =  -lstdc++ #-lresolv -lsocket -lnsl -lCstd

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
SYSDIR  = $(SRCDIR)/system/std-f90
TIMDIR  = $(SRCDIR)/timing

GRIDLIB = libamatos3d.a
GRDSLIB = libamatos3d.so
MODEND  = mod
GRDMOD  = *.$(MODEND)

#----------------------------------------------------------------#
# FLAGS FOR LINUX                                                #
#----------------------------------------------------------------#

F90    = ifort
cc     = gcc
CC     = g++	
LOADER = ifort -shared-intel
AR     = ar
CP     = cp
LIB_SH  = -L$(LAPACKDIR) $(LAPACKLIB)
ARFLAGS = vru
# Compiler flag for macros
MF = -D 

# --------------------- next are for debugging ------------------#
ifeq ($(strip $(MODE)),debug)
  FFLAGS = -g -fpic -w90 # -C
  CFLAGS = -g
  LDFLAGS = -g
  SHFLAGS = -fpic -shared -Vaxlib
endif
# --------------------- next are for optimized debugging --------#
ifeq ($(strip $(MODE)),optdebug)
  FFLAGS = -g -fPIC -w90 # -C
  CFLAGS = -g
  LDFLAGS = -g
  SHFLAGS = -fpic -shared -Vaxlib
endif
# --------------------- next are for normal compilation ---------#
ifeq ($(strip $(MODE)),norm)
  FFLAGS = -fPIC -w90 # -C
  CFLAGS =
  LDFLAGS = 
  SHFLAGS = -fpic -shared -Vaxlib
endif
# --------------------- next are for optimization ---------------#
ifeq ($(strip $(MODE)),opt)
  FFLAGS = -O3 -fPIC -w90 # -C
  CFLAGS = -O3
  LDFLAGS = -O3 -nofor_main
  SHFLAGS = -fpic -shared -Vaxlib
endif

# -------- next flag for using dummy graphics library -----------#
LIBS =  -lamatos3d $(LAPACKLIB) $(BLASLIB)
INCPATH   = -I$(INCDIR) -I.
SHLIBPATH = -L$(LIBDIR) -L$(LAPACKDIR) -L$(BLASDIR)
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
GRID_api.o

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
