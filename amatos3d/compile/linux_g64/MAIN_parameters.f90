!*****************************************************************
!
! MODULE NAME:
!	MAIN_parameters
! FUNCTION:
!	defines global control structure
! CONTAINS:
!
! PUBLIC:
!	all
! COMMENTS:
!
! LIBRARIES:
!
! REFERENCES:
!
! VERSION(S):
!	1. original version		j. behrens	11/2000
!
!*****************************************************************
	MODULE MAIN_parameters
            USE FEM_define

	IMPLICIT NONE
	INTEGER (KIND = GRID_SI), PARAMETER :: io_fillen=48

!---------- structure for the command line

	TYPE cmdline_param
	  LOGICAL                   :: l_interactive  ! interactive input mode
	  LOGICAL                   :: l_diagnostics  ! switch on diagnostics
	  CHARACTER (len=io_fillen) :: c_batchfile    ! input file name
	  LOGICAL                   :: l_output       ! redirect std output
	  LOGICAL                   :: l_logging      ! enable logging (verbose)
	END TYPE cmdline_param

!---------- structure for the i/o behaviour

	TYPE io_param
	  LOGICAL                   :: l_matlab       ! write matlab (offline) output
	  LOGICAL                   :: l_gmv          ! write matlab (offline) output
	  CHARACTER (len=io_fillen) :: c_triangfile   ! file with initial triangulation
	END TYPE io_param

!---------- structure for global physical and steering parameters

	TYPE phys_param
	  INTEGER (KIND = GRID_SI)            :: i_crslevel      ! coarsest requested level
	  INTEGER (KIND = GRID_SI)            :: i_reflevel      ! finest requested level
	END TYPE phys_param

!---------- global control structure

	TYPE control_struct
	  TYPE (phys_param)         :: phy
	  TYPE (cmdline_param)      :: cmd
	  TYPE (io_param)           :: io
	END TYPE control_struct
	TYPE (control_struct)       :: p_contr

	END MODULE MAIN_parameters
