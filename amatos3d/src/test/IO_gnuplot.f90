!*****************************************************************
!
! MODULE NAME:
!	IO_gnuplot
! FUNCTION:
!	perform rendering into a gnuplot compatible file 
! CONTAINS:
!-----------------------------------------------------------------
!
! NAME:
!	plot_gnuplot
! FUNCTION:
!	create output for visualization by gnuplot
! SYNTAX:
!	call plot_gnuplot(grid, int)
! ON INPUT:
!	p_handle:grid handle for the linked lists   (required)	type (grid_handle)
!	i_time: time stamp for file naming          (optional)	integer
! ON OUTPUT:
!
! CALLS:
!
! COMMENTS:
!
!-----------------------------------------------------------------
!
! PUBLIC:
!	plot_gnuplot
! COMMENTS:
!
! USES:
!	MISC_error, GRID_api
! LIBRARIES:
!
! REFERENCES:
!
! VERSION(S):
!	1. original version		n. rakowsky     03/2001
!
!*****************************************************************
	MODULE IO_gnuplot
	  USE GRID_api
	  PRIVATE
	  INTEGER (KIND = GRID_SI), SAVE :: i_timecounter= 0
	  PUBLIC :: plot_gnuplot
	  CONTAINS
!*****************************************************************
	  SUBROUTINE plot_gnuplot(p_handle, i_time)

!---------- local declarations

	  IMPLICIT NONE

	  TYPE (grid_handle), INTENT(in)       :: p_handle
	  INTEGER (KIND = GRID_SI), OPTIONAL, INTENT(in)        :: i_time
	  INTEGER (KIND = GRID_SI)                              :: i_io1, i_cnt, i_tim, i_alct, i_enum
	  INTEGER (KIND = GRID_SI)                              :: i_tcnt, i_fst
	  REAL (KIND = GRID_SR), DIMENSION(:,:,:), ALLOCATABLE  :: r_xytmp
	  REAL (KIND = GRID_SR), DIMENSION(:,:), ALLOCATABLE    :: r_val
	  CHARACTER (len=32)                   :: c_gnufile
	  CHARACTER (len=28)                   :: c_tmp

!---------- file handling (open)

	  IF(present(i_time)) THEN
	    i_tcnt= i_time
	    i_timecounter= i_timecounter+1
	  ELSE
	    i_tcnt= i_timecounter
	    i_timecounter= i_timecounter+1
	  END IF
	  write(c_tmp,*) trim(GRID_parameters%program_name(1:23)), '_gnuplot.'
	  write(c_gnufile,1010) trim(c_tmp), i_tcnt
	  c_gnufile= adjustl(c_gnufile)
	  i_io1= 15
	  OPEN(i_io1, file= c_gnufile, form= 'formatted', iostat= i_fst)
	  IF(i_fst /= 0) THEN
	    RETURN
	  END IF

!---------- set time tag

	  i_tim= p_handle%i_timetag
	  i_enum= p_handle%i_enumfine
	    
!---------- allocate arrays and extract data

	  allocate(r_xytmp(GRID_dimension,GRID_elementnodes,i_enum), & 
                   r_val(GRID_elementnodes,i_enum),                  &
                   stat=i_alct)
	  IF (i_alct /= 0) CALL grid_error(c_error='[plot_gnuplot] could not allocate aux. arrays')
	 
	  CALL grid_getinfo(p_handle, i_enum, l_finelevel=.TRUE., &
	                    r_elementcoordinates= r_xytmp,        &
	                    r_elementvalues= r_val)

!---------- print values

	  DO i_cnt=1,i_enum
             write(i_io1,1000) r_xytmp(:,1,i_cnt), r_val(1,i_cnt)
             write(i_io1,1000) r_xytmp(:,2,i_cnt), r_val(2,i_cnt)
             write(i_io1,1000) r_xytmp(:,3,i_cnt), r_val(3,i_cnt)
             write(i_io1,1000) r_xytmp(:,1,i_cnt), r_val(1,i_cnt)
             write(i_io1,'(a)') "" 
	  END DO 

!---------- close file

	  CLOSE(i_io1)

	  RETURN
 1000	  FORMAT(4f15.6)
 1010	  FORMAT(a28,i4.4)

        END SUBROUTINE plot_gnuplot


      END MODULE IO_gnuplot
