!*****************************************************************
!
! MODULE NAME:
!	ADV_rhs
! FUNCTION:
!	calculate the (nonhomogeneous) right hand side
! CONTAINS:
!-----------------------------------------------------------------
!
! NAME:
!	slm_righthand
! FUNCTION:
!	calculate the rhs of the advection equation
! SYNTAX:
!	real= slm_righthand(real.arr, real)
! ON INPUT:
!	r_coord: coordinates of point		real
!	r_time:  time coordinate (optional)	real
! ON OUTPUT:
!	r_rhs:   right hand side value		real
! CALLS:
!
! COMMENTS:
!
!-----------------------------------------------------------------
!
! PUBLIC:
!
! COMMENTS:
!	this is the homogeneous case!
! USES:
!	MISC_globalparam, MISC_error
! LIBRARIES:
!
! REFERENCES:
!
! VERSION(S):
!	1. original version		j. behrens	2/98
!	2. compliant to amatos 1.0	j. behrens	12/2000
!
!*****************************************************************
	MODULE ADV_rhs
	  USE GRID_api
	  PRIVATE
	  PUBLIC :: slm_righthand
	  CONTAINS
!*****************************************************************
	  FUNCTION slm_righthand(r_coord, r_time) RESULT (r_rhs)

!---------- local declarations

	  IMPLICIT NONE

	  REAL, DIMENSION(GRID_dimension), INTENT(in) :: r_coord
	  REAL, INTENT(in), OPTIONAL                  :: r_time
	  REAL                                        :: r_rhs
	  REAL                                        :: r_tim
	  REAL                                        :: r_rad, r_dist, r_high
	  REAL, DIMENSION(GRID_dimension)             :: r_centr,r_vec

!---------- set time

	  IF(present(r_time)) THEN
	    r_tim= r_time
	  ELSE
	    r_tim= 0.0
	  END IF


!---------- calculate the advection at (x,y) (velocity increasing)
	    r_rhs= 0.0

	
	  RETURN
	  END FUNCTION slm_righthand

!*****************************************************************
	END MODULE ADV_rhs
