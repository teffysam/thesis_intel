!*****************************************************************
!
! MODULE NAME: ERROR_est
!
! FUNCTION:
!
! CONTAINS:
!-----------------------------------------------------------------
!
! NAME:
!
! FUNCTION:
!
! SYNTAX:
!
! ON INPUT:
!
! ON OUTPUT:
!
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
!
! LIBRARIES:
!
! REFERENCES:
!
! VERSION(S):
!	1. original version	j. behrens	/96
!
!*****************************************************************
	MODULE ERROR_est

!---------- uses

	  USE GRID_api
	  USE MAIN_parameters

!---------- local declarations

	  PRIVATE

	  PUBLIC	:: error_estimate, error_coarse

	  CONTAINS

	SUBROUTINE error_estimate( p_ghand )

	IMPLICIT NONE

        REAL (KIND = GRID_SR), ALLOCATABLE, DIMENSION(:,:,:)		:: r_nodes
        INTEGER (KIND = GRID_SI)                                   :: i_tmp, i_err, i_cnt, i_alct
        REAL (KIND = GRID_SR), SAVE                                :: r_rad=0.15, r_rfc=0.8
	TYPE (grid_handle)			  :: p_ghand
        INTEGER (KIND = GRID_SI), DIMENSION(:), ALLOCATABLE        :: i_mark
        REAL (KIND = GRID_SR), DIMENSION(GRID_dimension)           :: r_xy
        REAL (KIND = GRID_SR)                                      :: r_fourth=1./4.

	

!---------- mark some elements for refinement

      ALLOCATE(r_nodes(GRID_dimension, GRID_tetranodes, p_ghand%i_tnumfine), &
               stat= i_alct)
      IF(i_alct /= 0) THEN
        WRITE(*,*) 'ERROR: Temporary array not allocated, stopped ...'
        STOP
      END IF
      ALLOCATE(i_mark(p_ghand%i_tnumfine), stat= i_alct)
      IF(i_alct /= 0) THEN
        WRITE(*,*) 'ERROR: Temporary array not allocated, stopped ...'
        STOP
      END IF
      i_mark= 0

      CALL grid_getinfo(p_ghand, p_ghand%i_tnumfine, &
        l_finelevel= .TRUE., r_tetracoordinates= r_nodes)


      DO i_cnt=1, p_ghand%i_tnumfine
        r_xy(1)= SUM(r_nodes(1,:,i_cnt))* r_fourth- 0.5
        r_xy(2)= SUM(r_nodes(2,:,i_cnt))* r_fourth- 0.5
        r_xy(3)= SUM(r_nodes(3,:,i_cnt))* r_fourth- 0.5
        IF(DOT_PRODUCT(r_xy,r_xy) <= r_rad) THEN
          i_mark(i_cnt)= GRID_pleaserefine
        END IF
      END DO
      r_rad= r_rad* r_rfc

      CALL grid_putinfo(p_ghand, p_ghand%i_tnumfine, &
        l_finelevel= .TRUE., i_tetrastatus= i_mark)

      IF(ALLOCATED(r_nodes)) DEALLOCATE(r_nodes)
      IF(ALLOCATED(i_mark)) DEALLOCATE(i_mark)

	END SUBROUTINE error_estimate

!*****************************************************************

	SUBROUTINE error_coarse( p_ghand )

	IMPLICIT NONE

        REAL (KIND = GRID_SR), ALLOCATABLE, DIMENSION(:,:,:)		:: r_nodes
        INTEGER (KIND = GRID_SI)                                   :: i_tmp, i_err, i_cnt, i_alct
        REAL (KIND = GRID_SR), SAVE                                :: r_rad=0.15, r_rfc=0.8
	TYPE (grid_handle)			  :: p_ghand
        INTEGER (KIND = GRID_SI), DIMENSION(:), ALLOCATABLE        :: i_mark
        REAL (KIND = GRID_SR), DIMENSION(GRID_dimension)           :: r_xy
        REAL (KIND = GRID_SR)                                      :: r_fourth=1./4.

	

!---------- mark some elements for refinement

      ALLOCATE(r_nodes(GRID_dimension, GRID_tetranodes, p_ghand%i_tnumfine), &
               stat= i_alct)
      IF(i_alct /= 0) THEN
        WRITE(*,*) 'ERROR: Temporary array not allocated, stopped ...'
        STOP
      END IF
      ALLOCATE(i_mark(p_ghand%i_tnumfine), stat= i_alct)
      IF(i_alct /= 0) THEN
        WRITE(*,*) 'ERROR: Temporary array not allocated, stopped ...'
        STOP
      END IF
      i_mark= 0

      CALL grid_getinfo(p_ghand, p_ghand%i_tnumfine, &
        l_finelevel= .TRUE., r_tetracoordinates= r_nodes)


      DO i_cnt=1, p_ghand%i_tnumfine
        r_xy(1)= SUM(r_nodes(1,:,i_cnt))* r_fourth- 0.5
        r_xy(2)= SUM(r_nodes(2,:,i_cnt))* r_fourth- 0.5
        r_xy(3)= SUM(r_nodes(3,:,i_cnt))* r_fourth- 0.5
        IF(DOT_PRODUCT(r_xy,r_xy) <= r_rad) THEN
          i_mark(i_cnt)= GRID_pleasecoarse
        END IF
      END DO
      r_rad= r_rad* r_rfc

      CALL grid_putinfo(p_ghand, p_ghand%i_tnumfine, &
        l_finelevel= .TRUE., i_tetrastatus= i_mark)

      IF(ALLOCATED(r_nodes)) DEALLOCATE(r_nodes)
      IF(ALLOCATED(i_mark)) DEALLOCATE(i_mark)

	END SUBROUTINE error_coarse

	END MODULE ERROR_est
