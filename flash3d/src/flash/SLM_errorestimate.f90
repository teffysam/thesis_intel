!*****************************************************************
!
! MODULE NAME:
!	SLM_errorestimate
! FUNCTION:
!	provide an error estimator for the adaptive SLM scheme
! CONTAINS:
!-----------------------------------------------------------------
!
! NAME:
!	slm_error
! FUNCTION:
!	estimate the error elementwise
! SYNTAX:
!	real= slm_error(real.arr)
! ON INPUT:
!	r_v:       values at element nodes	REAL
! ON OUTPUT:
!	r_esterr:  estimated error		REAL
! CALLS:
!
! COMMENTS:
!	this is only a simple estimator based on local gradients
!-----------------------------------------------------------------
!
! NAME:
!	slm_errorest
! FUNCTION:
!	this hides the loop structure from the advection part
! SYNTAX:
!	CALL slm_errorest(grid, int, real.arr)
! ON INPUT:
!	p_ghand: handle for the grid		TYPE (grid_handle)
!	i_siz:   size of array for local errors	INTEGER
! ON OUTPUT:
!	i_siz:   size of array for local errors	INTEGER
!	r_arr:   estimated error		REAL
! CALLS:
!
! COMMENTS:
!
!-----------------------------------------------------------------
!
! PUBLIC:
!	slm_errorest
! COMMENTS:
!
! USES:
!	MISC_globalparam, MISC_error, GRID_api
! LIBRARIES:
!
! REFERENCES:
!
! VERSION(S):
!	1. original version		j. behrens	11/96
!	2. nodal values time depend.	j. behrens	1/97
!	3. FEM_handle added		j. behrens	7/97
!	4. version with hashing		j. behrens	7/97
!	5. changed to use GRID_api	j. behrens	11/97
!	6. compliant to amatos 1.0	j. behrens	12/2000
!
!*****************************************************************
	MODULE SLM_errorestimate
	  USE FLASH_parameters
	  USE GRID_api
	  PRIVATE
	  PUBLIC :: slm_errorest
	  CONTAINS
!*****************************************************************
	  FUNCTION slm_error(r_v) RESULT (r_esterr)

!---------- local declarations

	  IMPLICIT NONE
	  REAL, DIMENSION(GRID_tetranodes)   :: r_v
	  REAL                               :: r_esterr
	  REAL, PARAMETER                    :: r_1o4= (1.0/4.0)
	  REAL, DIMENSION(GRID_tetranodes)   :: r_d
	  INTEGER                            :: i_cnt, i_cntp1

!---------- calculate differences

	  DO i_cnt=1,GRID_tetranodes
	    i_cntp1   = mod(i_cnt,GRID_tetranodes)+ 1
	    r_d(i_cnt)=  abs(r_v(i_cnt)- r_v(i_cntp1))
	  END DO

!---------- this is the estimated error

	  r_esterr= SUM(r_d)* r_1o4

	  RETURN
	  END FUNCTION slm_error

!*****************************************************************
	  SUBROUTINE slm_errorest(p_ghand, i_siz, r_arr)

!---------- local declarations

	  IMPLICIT NONE
	  TYPE (grid_handle), INTENT(in)  :: p_ghand
	  INTEGER, INTENT(in)             :: i_siz
	  REAL, DIMENSION(:), INTENT(out) :: r_arr
	  REAL, DIMENSION(:,:), ALLOCATABLE :: r_aux
	  INTEGER                         :: i_cnt, i_size, i_alct

!---------- allocate auxilliary array

	  i_size= p_ghand%i_tnumfine
	  IF(i_siz /= i_size) THEN
	    CALL grid_error(48)
	  END IF

	  ALLOCATE(r_aux(GRID_tetranodes,i_size), stat= i_alct)
	  IF(i_alct /= 0) THEN
	    CALL grid_error(49)
	  END IF

!---------- get values

	  CALL grid_getinfo(p_ghand, i_size, l_finelevel= .TRUE., i_valpoint= GRID_tracer, &
	                    r_tetravalues= r_aux)

!---------- loop through all elements of finest loop

	  elmt_loop: DO i_cnt=1,i_siz
	    r_arr(i_cnt)= slm_error(r_aux(:,i_cnt))
	  END DO elmt_loop

!---------- deallocate and exit

	  DEALLOCATE(r_aux)

	  RETURN
	  END SUBROUTINE slm_errorest

	END MODULE SLM_errorestimate
