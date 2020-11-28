!*****************************************************************
!
! PROJECT:
!	FLASH90 means
!	        FLexible Adaptive Semi-Lagrangian Hack
!	        written in Fortran 90
! NAME:
!	Flash90
! FUNCTION:
!	main program (driver routine for the adaptive SLM)
! SYNTAX:
!	flash90 [options]
! ON INPUT:
!
! ON OUTPUT:
!
! CALLS:
!
! COMMENTS:
!
! LIBRARIES:
!	USEs several fortran 90 modules
! REFERENCES:
!	this is a complete reimplementation of STASL
! VERSION(S):
!	1. original version		j. behrens	11/96
!	2. tidied up a little		j. behrens	7/97
!	3. new control struct		j. behrens	12/97
!	4. compliant to amatos 1.0	j. behrens	12/2000
!
!*****************************************************************
	PROGRAM flash90

!---------- modules

	USE FLASH_parameters
	USE IO_utils
        !-- added for visnetplot [flo]:
!	USE IO_visnetplot
	USE GRID_api
	USE ADV_semilagrange

!---------- local declarations

	IMPLICIT NONE
	
	INTEGER             :: i_dummy

!---------- set FLASH description in global datastruct

	GRID_parameters%program_name= 'Flash3D                                         '
	GRID_parameters%version= 0
	GRID_parameters%subversion= 2
	GRID_parameters%patchversion= 0
	GRID_parameters%datemonth= 12
	GRID_parameters%dateyear= 2003

!---------- read command line options

	CALL io_getcmdline(p_contr)

!---------- initialize grid generator

	IF(p_contr%cmd%l_output) THEN
	  IF(p_contr%cmd%l_logging) THEN
	    CALL grid_initialize(i_output=i_redirout, i_logging=i_redirlog)
	  ELSE
	    CALL grid_initialize(i_output=i_redirout)
	  END IF
	ELSE
	  IF(p_contr%cmd%l_logging) THEN
	    CALL grid_initialize(i_logging=i_redirlog)
	  ELSE
	    CALL grid_initialize
	  END IF
	END IF

!---------- read user input

        CALL io_initparams(p_contr)
        batch_input: IF(p_contr%cmd%l_batchmode) THEN
  	  CALL io_getbatchinput(p_contr)
  	END IF batch_input
	interactive_input: IF(p_contr%cmd%l_interactive) THEN
	  CALL io_getinterinput(p_contr)
	END IF interactive_input

!---------- print global parameters

	CALL io_putparameters(p_contr)

!---------- set up advection starting conditions, initialize grid, etc.

	CALL slm_initialize(p_grid, p_contr)

!-- BEGIN added for visnetplot [flo]:
!---------- initialize graphics
!	i_dummy= visnet_plot(p_contr, p_grid(i_timeplus), &
!	          c_action='init')
!-- END

!---------- call the (major) routine for timestepping

	CALL slm_timestepping(p_grid, p_contr)

!---------- terminate the SLM (gracefully free memory, terminate grid, etc.)


		CALL slm_finish(p_grid, p_contr)

!---------- terminate grid generator

	CALL grid_terminate

	STOP
	END PROGRAM flash90
