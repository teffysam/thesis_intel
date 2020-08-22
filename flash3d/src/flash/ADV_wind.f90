!*****************************************************************
!
! MODULE NAME:
!	ADV_wind
! FUNCTION:
!	calculate the windfield for the advection problem
! CONTAINS:
!-----------------------------------------------------------------
!
! NAME:
!	slm_windfield
! FUNCTION:
!	calculate the advecting force for simple advection
! SYNTAX:
!	real.arr= slm_windfield(real.arr, real)
! ON INPUT:
!	r_coord: coordinates of point		real
!	r_time:  time coordinate (optional)	real
! ON OUTPUT:
!	r_field: windfield			real
! CALLS:
!
! COMMENTS:
!
!-----------------------------------------------------------------
!
! PUBLIC:
!
! COMMENTS:
!
! USES:
!	MISC_globalparam, MISC_error
! LIBRARIES:
!
! REFERENCES:
!
! VERSION(S):
!	1. original version		j. behrens	12/97
!	2. compliant to amatos 1.0	j. behrens	12/2000
!
!*****************************************************************
	MODULE ADV_wind
	  USE FLASH_parameters
	  USE GRID_api
	  PRIVATE
	  PUBLIC :: slm_windfield, slm_windinit, slm_windquit
	  CONTAINS
!*****************************************************************
	  FUNCTION slm_windfield(r_coord, r_time) RESULT (r_field)

!---------- local declarations

	  IMPLICIT NONE

	  REAL, DIMENSION(GRID_dimension), INTENT(in) :: r_coord
	  REAL, INTENT(in), OPTIONAL                  :: r_time
	  REAL, DIMENSION(GRID_dimension)             :: r_field
	  REAL                                        :: r_fac=.3636e-4
	  REAL					      :: r_diag= .1e-3
	  REAL                                        :: r_tim
	  REAL					      :: r_dist
	  REAL, DIMENSION(GRID_dimension), PARAMETER  :: r_cntr=(/ 0.5, 0.5, 0.5 /)
	  REAL, DIMENSION(GRID_dimension), PARAMETER  :: r_convpoint=(/ 0.75, 0.75, 0.5 /)
	  REAL, DIMENSION(GRID_dimension)             :: r_tmp

!---------- set time

	  IF(present(r_time)) THEN
	    r_tim= r_time
	  ELSE
	    r_tim= 0.0
	  END IF

!---------- calculate the advection at (x,y) (velocity increasing)



!---------- circulating wind in x,y-plane
!
	  r_tmp= r_coord- r_cntr
	  r_field(1)= -r_tmp(2)* r_fac
	  r_field(2)=  r_tmp(1)* r_fac
!	  r_field(3)=  r_tmp(2)* r_fac
	  r_field(3)=  0.0


!---------- diagonal wind without shearing in the x,y-plane
!
!	  r_field(1)= r_fac
!	  r_field(2)= r_fac
!	  r_field(3)= 0.



!---------- point-convergent wind in the x,y-plane
!
!	  r_tmp = r_coord - r_convpoint
!	  r_dist = sqrt( r_tmp(1)*r_tmp(1) + r_tmp(2)*r_tmp(2) )
!	  IF (r_dist < 0.01) THEN 
!	  	r_field = 0.
!	  ELSE
!	       	r_field(1)= -r_tmp(1) * r_fac / r_dist
!	  	r_field(2)= -r_tmp(2) * r_fac / r_dist
!	  	r_field(3)= 0.
!	  ENDIF
!

	
	  RETURN
	  END FUNCTION slm_windfield

!*****************************************************************
	  SUBROUTINE slm_windinit(p_control)

!---------- local declarations

	  IMPLICIT NONE
	  TYPE (control_struct)     :: p_control

	  RETURN
	  END SUBROUTINE slm_windinit

!*****************************************************************
	  SUBROUTINE slm_windquit

!---------- local declarations

	  IMPLICIT NONE

	  RETURN
	  END SUBROUTINE slm_windquit

!*****************************************************************
	END MODULE ADV_wind
