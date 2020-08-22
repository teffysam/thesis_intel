##################################################################
#   SPLASH/fe                                                    #
#   Semi-Lagrangian Parallel and Locally Adaptive                #
#   SHallow-water model/ with Finite Elements                    #
##################################################################
# makefile to build SPLASH                                       #
# j. behrens 2/95, 3/96, 5/97                                    #
# --- this is for solaris ---                                    #
##################################################################

# MACHINE
MACHINE = solaris

# SET MAKETHING CORRESPONDING TO MACHINE:
MAKETHING= AMATOS3D

# SET MAIN DIRECTORY PATH
# !! This has to be alterd by user !!
ROOTDIR = $(HOME)/Development

# SET MORE DIRECTORY PATHS
LIBDIR  = $(ROOTDIR)/lib/3DSINGLE
INCDIR  = $(ROOTDIR)/include/3DSINGLE
MAINDIR = $(ROOTDIR)/amatos3d

BUILDIR = $(MAINDIR)/$(MACHINE)
DATDIR  = $(MAINDIR)/data
SRCDIR  = $(MAINDIR)/src

GRIDDIR = $(SRCDIR)/gridgen
TSTDIR  = $(SRCDIR)/test
SYSDIR  = $(SRCDIR)/system/std-f90
TIMDIR  = $(SRCDIR)/timing

GRIDLIB = libamatos3d.a
GRDSLIB = libamatos3d.so
MODEND  = mod
GRDMOD  = grid_api.$(MODEND)

#----------------------------------------------------------------#
# FLAGS FOR SGI                                                  #
#----------------------------------------------------------------#

F90    = f90
CC     = cc
LOADER = f90
AR     = ar

# --------------------- next three for debugging ----------------#
FFLAGS  = -g -KPIC
CFLAGS  = -g
LDFLAGS = -g -z nodefs
SHFLAGS = -G -KPIC
LIB_SH  =
ARFLAGS = vru
# --------------------- next three with optimization ------------#
# FFLAGS  = -O -Kpic
# CFLAGS  = -O
# LDFLAGS = -O -z nodefs
# SHFLAGS = -O -Kpic
# LIB_SH  =
# ARFLAGS = vru
# -------- next flag for using dummy graphics library -----------#
LIBS      = -lamatos3d
INCPATH   = -M$(INCDIR)
SHLIBPATH = -L$(LIBDIR) -R$(LIBDIR)

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
# COMPILE STEP (CONVENTIONS)                                     #
#----------------------------------------------------------------#

.f90.o:
	@echo "make: Building object module from "$<
	@$(F90) $(FFLAGS) $(AMATFLAG) $(INCPATH) -c $<

.f.o:
	@echo "make: Building object module from "$<
	@$(F90) $(FFLAGS) $(AMATFLAG) $(INCPATH) -c $<

.c.o:
	@echo "make: Building object module from "$<
	@$(CC) $(CFLAGS) $(AMATFLAG) $(INCPATH) -D$(MACHINE) -c $<

clearsrc::
	@rm -f *.f90 *.f *.h

cleardat::
	@rm -f $(MAKETHING)_* *.dat

clearex::
	@rm -f $(MAKETHING) $(GRIDLIB) $(GRDSLIB)

clearlib::
	@rm -f $(LIBDIR)/$(GRIDLIB)  $(LIBDIR)/$(GRDSLIB) $(INCDIR)/$(GRDMOD)

clean::
	@rm -f *.o *.$(MODEND) core so_locations

tidy::
	make clean
	make clearex
	make clearsrc
	make clearlib
	make cleardat

#----------------------------------------------------------------#
# THIS CREATES THE GRID GENERATOR LIBRARIES                      #
#----------------------------------------------------------------#

$(GRIDLIB): $(SEROBJ)
	@echo "make: Creating an archive from object modules"
	@$(AR) $(ARFLAGS) $(GRIDLIB) $(SEROBJ)

$(GRDSLIB): $(GRIDLIB)
	@echo "make: Creating dynamic shared object (dso) library"
	@$(LOADER) $(SHFLAGS) -o $(GRDSLIB) $(SEROBJ) $(LIB_SH)

gridcopy::
	@cp $(GRIDDIR)/*.*90 $(GRIDDIR)/*.f .

gridclear::
	@make clearsrc
	@rm -f *.o core

grid::
	@make gridcopy
	@make $(GRIDLIB)
	@make $(GRDSLIB)
	@make gridclear

install::
	@echo "make: Installing libraries and module file in lib directory"
	@mv $(GRIDLIB) $(GRDSLIB) $(LIBDIR)
	@mv $(GRDMOD) $(INCDIR)

lib::
	@make grid
	@make install
	@make clean

#----------------------------------------------------------------#
# THIS CREATES THE MAIN PROGRAM                                  #
#----------------------------------------------------------------#

maincopy::
	@cp $(TSTDIR)/*.*90 .
	@cp $(SYSDIR)/*.f90 .
	@cp $(TIMDIR)/*.f90 .

executable: $(OBJMAIN)
	@echo "make: Linking object modules and libraries"
	@$(LOADER) $(LDFLAGS) -o $(MAKETHING) $(OBJMAIN) $(SHLIBPATH) $(LIBS)

$(MAKETHING)::
	@make maincopy
	@make executable
	@make clearsrc

#----------------------------------------------------------------#
# THIS COPIES REQUIRED DATAFILES                                 #
#----------------------------------------------------------------#

datacopy::
	@echo "make: Copy required data files"
	@cp $(DATDIR)/Triang.dat Triang.dat
	@cp $(DATDIR)/Parameters.dat Parameters.dat

#----------------------------------------------------------------#
# MAKE EVERYTHING (CREATE THE EXECUTABLE, INSTALL, CLEAN UP)     #
#----------------------------------------------------------------#

all::
	make lib
	make $(MAKETHING)
	make datacopy
	make clean

#----------------------------------------------------------------#
# MAKE EVERYTHING FOR DEBUGGING, LEAVE .O AND .F90 FILES         #
#----------------------------------------------------------------#

debug::
	@make gridcopy
	@make $(GRIDLIB)
	@make $(GRDSLIB)
	@make install
	@make maincopy
	@make executable
	
#----------------------------------------------------------------#
# DEPENDENCIES ON INCLUDE FILES                                  #
#----------------------------------------------------------------#
