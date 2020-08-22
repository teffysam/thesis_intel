!*****************************************************************
!
! MODULE NAME:
!	IO_matlabplot
! FUNCTION:
!	perform rendering into a matlab compatible file 
! CONTAINS:
!-----------------------------------------------------------------
!
! NAME:
!	plot_matlab
! FUNCTION:
!	create output for visualization by matlab
! SYNTAX:
!	call plot_matlab(grid, int, int, char)
! ON INPUT:
!	p_handle:grid handle for the linked lists   (required)	type (grid_handle)
!	i_time: time stamp for file naming          (optional)	integer
!	c_attrib: plot attribute into element       (optional)	character
! ON OUTPUT:
!
! CALLS:
!
! COMMENTS:
!	the following attributes are supported:
!	  'flag': plot flag of triangle
!	  'levl': plot refinement level as color
!	  'vals': plot values as color
!
!-----------------------------------------------------------------
!
! PUBLIC:
!	plot_matlab
! COMMENTS:
!
! USES:
!	MISC_error, GRID_api
! LIBRARIES:
!
! REFERENCES:
!	this module is based on the original implementations in the
!	splash/fe project (fortran 77 version), but wildly modified!
! VERSION(S):
!	1. original version		j. behrens	1/2000
!	2. amatos-1.0 compliant		j. behrens	11/2000
!
!*****************************************************************
	MODULE IO_matlabplot
	  USE GRID_api
	  PRIVATE
	  INTEGER, SAVE :: i_timecounter= 0
	  PUBLIC :: plot_matlab
	  CONTAINS
!*****************************************************************
	  SUBROUTINE plot_matlab(p_handle, i_time, c_action)

!---------- local declarations

	  IMPLICIT NONE

	  TYPE (grid_handle), INTENT(in)       :: p_handle
	  INTEGER, OPTIONAL, INTENT(in)        :: i_time
	  CHARACTER (len=4), OPTIONAL          :: c_action
	  INTEGER                              :: i_io1, i_cnt, i_opt, i_tim, i_alct, i_enum
	  INTEGER                              :: i_tcnt, i_fst
	  REAL, DIMENSION(:,:,:), ALLOCATABLE  :: r_xytmp
	  REAL, DIMENSION(:,:), ALLOCATABLE    :: r_val
	  INTEGER, DIMENSION(:), ALLOCATABLE   :: i_val
	  REAL, PARAMETER                      :: r_1o3=(1./3.)
	  REAL                                 :: r_tmp
	  CHARACTER (len=32)                   :: c_matfile
	  CHARACTER (len=28)                   :: c_tmp
	  LOGICAL                              :: l_flag, l_levl, l_vals

!---------- file handling (open)

	  IF(present(i_time)) THEN
	    i_tcnt= i_time
	    i_timecounter= i_timecounter+1
	  ELSE
	    i_tcnt= i_timecounter
	    i_timecounter= i_timecounter+1
	  END IF
	  write(c_tmp,*) trim(GRID_parameters%program_name(1:23)), '_matlab.'
	  write(c_matfile,1010) trim(c_tmp), i_tcnt
	  c_matfile= adjustl(c_matfile)
	  i_io1= 15
	  OPEN(i_io1, file= c_matfile, form= 'formatted', iostat= i_fst)
	  IF(i_fst /= 0) THEN
	    RETURN
	  END IF

!---------- set time tag

	  i_tim= p_handle%i_timetag
	  i_enum= p_handle%i_enumfine

!---------- check input for optional parameters

	  l_flag= .FALSE.
	  l_levl= .FALSE.
	  l_vals= .FALSE.
	  action_present: IF (present(c_action)) THEN
	    flag: IF(c_action == 'flag') THEN
	      l_flag= .TRUE.
	    END IF flag
	    levl: IF(c_action == 'levl') THEN
	      l_levl= .TRUE.
	    END IF levl
	    vals: IF(c_action == 'vals') THEN
	      l_vals= .TRUE.
	    END IF vals
	  END IF action_present
	    
!---------- allocate arrays and extract data

	  allocate(r_xytmp(GRID_dimension,GRID_elementnodes,i_enum), stat=i_alct)
	  IF(i_alct /= 0) THEN
	    CALL grid_error(16)
	  END IF

	  CALL grid_getinfo(p_handle, i_enum, l_finelevel=.TRUE., &
	                    r_elementcoordinates= r_xytmp)

	  IF(l_flag) THEN
	    allocate(i_val(i_enum), stat=i_alct)
	    IF(i_alct /= 0) THEN
	      CALL grid_error(17)
	    END IF
	    CALL grid_getinfo(p_handle, i_enum, l_finelevel=.TRUE., &
	                      i_elementstatus= i_val)
	  ELSE IF(l_levl) THEN
	    allocate(i_val(i_enum), stat=i_alct)
	    IF(i_alct /= 0) THEN
	      CALL grid_error(18)
	    END IF
	    CALL grid_getinfo(p_handle, i_enum, l_finelevel=.TRUE., &
	                      i_elementlevel= i_val)
	  ELSE IF(l_vals) THEN
	    allocate(r_val(GRID_elementnodes,i_enum), stat=i_alct)
	    IF(i_alct /= 0) THEN
	      CALL grid_error(19)
	    END IF
	    CALL grid_getinfo(p_handle, i_enum, l_finelevel=.TRUE., &
	                      i_valpoint=GRID_tracer, r_elementvalues= r_val)
	  END IF

!---------- print values

	  poly_loop: DO i_cnt=1,i_enum
	    IF(l_flag .OR. l_levl) THEN
	      i_opt= i_val(i_cnt)
	    ELSE IF (l_vals) THEN
	      r_tmp= (r_val(1,i_cnt)+ r_val(2,i_cnt)+ r_val(3,i_cnt))* r_1o3
	      i_opt= anint(r_tmp)
	    ELSE
	      i_opt= -99999
	    END IF
	    write(i_io1,1000) r_xytmp(1,1,i_cnt), r_xytmp(1,2,i_cnt), r_xytmp(1,3,i_cnt), &
	                      r_xytmp(2,1,i_cnt), r_xytmp(2,2,i_cnt), r_xytmp(2,3,i_cnt), &
	                      r_xytmp(3,1,i_cnt), r_xytmp(3,2,i_cnt), r_xytmp(3,3,i_cnt), &
	                      i_opt
	  END DO poly_loop

!---------- close file

	  CLOSE(i_io1)

	  RETURN
 1000	  FORMAT(9f15.6,i8)
 1010	  FORMAT(a28,i4.4)
	  END SUBROUTINE plot_matlab


	END MODULE IO_matlabplot
