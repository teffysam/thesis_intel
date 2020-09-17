!*****************************************************************
!
! MODULE NAME:
!	FLASH_parameters
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
!	1. original version		j. behrens	12/2000
!
!*****************************************************************
	MODULE FLASH_parameters

	IMPLICIT NONE
	INTEGER, PARAMETER :: io_fillen=48
	INTEGER, PARAMETER :: i_redirout=8
	INTEGER, PARAMETER :: i_redirlog=7

!---------- structure for the command line

	TYPE cmdline_param
          SEQUENCE
	  LOGICAL                   :: l_interactive  ! interactive input mode
	  LOGICAL                   :: l_diagnostics  ! switch on diagnostics
          LOGICAL                   :: l_batchmode    ! read params from file
	  CHARACTER (len=io_fillen) :: c_batchfile    ! input file name
	  LOGICAL                   :: l_output       ! redirect std output
	  LOGICAL                   :: l_logging      ! enable logging (verbose)
	END TYPE cmdline_param

!---------- structure for the i/o behaviour

	TYPE io_param
	  SEQUENCE
	  LOGICAL                   :: l_matlab       ! write matlab (offline) output
	  LOGICAL                   :: l_gmv          ! write gmv (offline) output
          !-- BEGIN added for visnetplot [flo]:
	  LOGICAL                   :: l_visnet       ! plot visnet (online) output
          LOGICAL                   :: l_visnet_tiff  ! enable visnet screenshots
          !-- END
	  CHARACTER (len=io_fillen) :: c_domainfile   ! file with definitions for domain
	  CHARACTER (len=io_fillen) :: c_triangfile   ! file with initial triangulation
	  CHARACTER (len=io_fillen) :: c_windfile     ! file with wind information
	  INTEGER                   :: i_plotoffset   ! timesteps between plots
	  INTEGER                   :: i_saveoffset   ! timesteps between savesets
	  INTEGER                   :: i_savelast     ! indicator for last step saving
	END TYPE io_param

!---------- structure for global physical and steering parameters

	TYPE phys_param
	  SEQUENCE
	  REAL               :: r_deltatime     ! timestep length [s]
	  REAL               :: r_reftolerance  ! tolerance for refinement
	  REAL               :: r_crstolerance  ! tolerance for coarsening
	  REAL               :: r_refwatermark  ! watermark for refinement
	  REAL               :: r_crswatermark  ! watermark for coarsening
	  REAL               :: r_totalmass     ! mass of the field (conserved)
	  INTEGER            :: i_experiment    ! current experiment identification
	  INTEGER            :: i_crslevel      ! coarsest requested level
	  INTEGER            :: i_reflevel      ! finest requested level
	  INTEGER            :: i_frsttimestep  ! first timestep of experiment
	  INTEGER            :: i_lasttimestep  ! last timestep of experiment
	  INTEGER            :: i_adviterations ! iterations in trajectory estimation
	END TYPE phys_param

!---------- global control structure

	TYPE control_struct
	  SEQUENCE
	  TYPE (phys_param)         :: phy
	  TYPE (cmdline_param)      :: cmd
	  TYPE (io_param)           :: io
	END TYPE control_struct
	TYPE (control_struct)       :: p_contr

!---------- structure for runtime information

	TYPE rt_info
	  REAL               :: r_modeltime
	  INTEGER            :: i_step
	  INTEGER            :: i_adapit
	  LOGICAL            :: l_saved
	  LOGICAL            :: l_ploted
	END TYPE rt_info
	TYPE (rt_info)       :: p_timestepinfo

	END MODULE FLASH_parameters
