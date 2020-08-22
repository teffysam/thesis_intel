!*****************************************************************
!
! MODULE NAME:
!	MISC_functions
! FUNCTION:
!	miscellaneaus test functions
! CONTAINS:
!	f1, ... , f5
!-----------------------------------------------------------------
!
! NAME:
!	fi (i=1,...,5)
! FUNCTION:
!	calculate value for 3D-coordinate
! SYNTAX:
!	real= fi(real.arr)
! ON INPUT:
!	r_coord:  Coordinate array			REAL (KIND = GRID_SR)
! ON OUTPUT:
!	r_return: Return value				REAL (KIND = GRID_SR)
! CALLS:
!
! COMMENTS:
!
!-----------------------------------------------------------------
!
! PUBLIC:
!	f1, f2, f3, f4, f5
! COMMENTS:
!
! USES:
!	MISC_globalparam
!	GRID_api
! LIBRARIES:
!
! REFERENCES:
!
! VERSION(S):
!	1. original version		j. behrens	5/2000
!
!*****************************************************************
	MODULE MISC_functions
          USE FEM_define
	  USE GRID_api
	  CONTAINS
!*****************************************************************
	  FUNCTION f1(r_coord) RESULT (r_return)

!---------- local declarations

	  IMPLICIT NONE
	  REAL (KIND = GRID_SR), DIMENSION(GRID_dimension) :: r_coord
	  REAL (KIND = GRID_SR)                            :: r_return

	  r_return= cos(r_coord(1))* sin(r_coord(2))* r_coord(3)

	  RETURN
	  END FUNCTION f1
!*****************************************************************
	  FUNCTION f2(r_coord) RESULT (r_return)

!---------- local declarations

	  IMPLICIT NONE
	  REAL (KIND = GRID_SR), DIMENSION(GRID_dimension) :: r_coord
	  REAL (KIND = GRID_SR)                            :: r_return

	  r_return= r_coord(1)+ r_coord(2)+ r_coord(3)

	  RETURN
	  END FUNCTION f2
!*****************************************************************
	  FUNCTION f3(r_coord) RESULT (r_return)

!---------- local declarations

	  IMPLICIT NONE
	  REAL (KIND = GRID_SR), DIMENSION(GRID_dimension) :: r_coord
	  REAL (KIND = GRID_SR)                            :: r_return

	  r_return= r_coord(1)* r_coord(2)* r_coord(3)

	  RETURN
	  END FUNCTION f3
!*****************************************************************
	  FUNCTION f4(r_coord) RESULT (r_return)

!---------- local declarations

	  IMPLICIT NONE
	  REAL (KIND = GRID_SR), DIMENSION(GRID_dimension) :: r_coord
	  REAL (KIND = GRID_SR)                            :: r_return

	  r_return= sin(r_coord(1))* sin(r_coord(2))* sin(r_coord(3))

	  RETURN
	  END FUNCTION f4
!*****************************************************************
	  FUNCTION f5(r_coord) RESULT (r_return)

!---------- local declarations

	  IMPLICIT NONE
	  REAL (KIND = GRID_SR), DIMENSION(GRID_dimension) :: r_coord
	  REAL (KIND = GRID_SR)                            :: r_return, r_tmp
	  REAL (KIND = GRID_SR)                            :: r_third=1./3.
	  REAL (KIND = GRID_SR), DIMENSION(GRID_dimension) :: r_centr=(/ 0.5, 0.5, 0.5 /)
	  REAL (KIND = GRID_SR), DIMENSION(GRID_dimension) :: r_3dtmp

	  r_return= 0.0
	  r_3dtmp = r_coord-r_centr
	  r_tmp= dot_product(r_3dtmp,r_3dtmp)
	  r_return=1.0-r_tmp
	  IF(r_return < 0.0) r_return = 0.0

	  RETURN
	  END FUNCTION f5
  
	END MODULE MISC_functions
