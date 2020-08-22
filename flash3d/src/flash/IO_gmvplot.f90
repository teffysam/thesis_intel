!*****************************************************************
!
! MODULE NAME:
!	IO_gmvplot
! FUNCTION:
!	print data in GMV compatible file format
! CONTAINS:
!-----------------------------------------------------------------
!
! NAME:
!	plot_netcdf
! FUNCTION:
!	create output fin netcdf file format
! SYNTAX:
!	call plot_netcdf(handle, char.string)
! ON INPUT:
!	p_handle: grid handle for the linked lists  (required)	type (grid_handle)
!	c_filename: file name for plot file          (optional)	character
! ON OUTPUT:
!
! CALLS:
!
! COMMENTS:
!
!-----------------------------------------------------------------
!
! PUBLIC:
!	plot_gmv
! COMMENTS:
!
! USES:
!	MISC_globalparam, MISC_error, GRID_api
! LIBRARIES:
!
! REFERENCES:
!
! VERSION(S):
!	1. original version		j. behrens	4/2000
!	2. amatos-1.0 and 2D compliant	j. behrens	11/2000
!
!*****************************************************************
	MODULE IO_gmvplot
	  USE GRID_api
	  USE FLASH_parameters
	  PRIVATE
	  INTEGER, SAVE :: i_timecounter= 0
	  PUBLIC :: plot_gmv
	  CONTAINS
!*****************************************************************
	  SUBROUTINE plot_gmv(p_handle, i_time)

!---------- local declarations

	  IMPLICIT NONE
	  TYPE (grid_handle), INTENT(in)       :: p_handle
	  INTEGER, OPTIONAL, INTENT(in)        :: i_time
	  INTEGER                              :: i_io1
	  INTEGER                              :: i_cnt, j_cnt, &
	    i_alct, i_nnum, i_tnum, i_dim, i_tcnt, i_fst
	  REAL, DIMENSION(:), ALLOCATABLE      :: r_ttmp
	  REAL, DIMENSION(:,:), ALLOCATABLE    :: r_cox
	  INTEGER, DIMENSION(:,:), ALLOCATABLE :: i_tets
	  INTEGER, DIMENSION(:), ALLOCATABLE   :: i_ids
	  CHARACTER (len=32)                   :: c_mfile
	  CHARACTER (len=28)                   :: c_tmp

!---------- file handling (open)

	  IF(present(i_time)) THEN
	    i_tcnt= i_time
	    i_timecounter= i_timecounter+1
	  ELSE
	    i_tcnt= i_timecounter
	    i_timecounter= i_timecounter+1
	  END IF
	  write(c_tmp,*) trim(GRID_parameters%program_name(1:23)), '_'
	  write(c_mfile,10101) trim(c_tmp), i_tcnt,'.gmv'
	  c_mfile= adjustl(c_mfile)
	  i_io1= 15
	  OPEN(i_io1, file= c_mfile, form= 'formatted', iostat= i_fst)
	  IF(i_fst /= 0) THEN
	    RETURN
	  END IF

!---------- write header

	  WRITE(i_io1,1000)
	  WRITE(i_io1,1001)
	  WRITE(i_io1,1002) c_mfile
	  WRITE(i_io1,1003) GRID_parameters%program_name, GRID_parameters%version, GRID_parameters%subversion, GRID_parameters%patchversion
	  WRITE(i_io1,1004) GRID_parameters%author_name, GRID_parameters%author_email
	  WRITE(i_io1,1005)

!---------- the nodes

	  i_nnum = p_handle%i_nnumber
	  WRITE(i_io1,1010) i_nnum

!---------- extract nodal grid data

	  ALLOCATE(r_cox(GRID_dimension, i_nnum), stat=i_alct)
	  IF(i_alct /= 0) THEN
	    CALL grid_error(11)
	  END IF
	  CALL grid_getinfo(p_handle, i_nnum, r_nodecoordinates= r_cox)
	  DO i_dim=1,GRID_dimension
	    WRITE(i_io1,1011) (r_cox(i_dim,i_cnt), i_cnt=1,i_nnum)
	  END DO
	  DEALLOCATE(r_cox)

!---------- the elements

	  i_tnum = p_handle%i_tnumfine
	  WRITE(i_io1,1020) i_tnum

!---------- extract cells grid data

	  ALLOCATE(i_tets(GRID_tetraelements,i_tnum), stat=i_alct)
	  IF(i_alct /= 0) THEN
	    CALL grid_error(24)
	  END IF
	  CALL grid_getinfo(p_handle, i_tnum, l_finelevel=.TRUE., &
	    l_relative=.TRUE., i_tetranodes= i_tets)
	  tetra_loop: DO j_cnt=1,i_tnum
	    WRITE(i_io1,1021)
	    WRITE(i_io1,1022) (i_tets(i_cnt,j_cnt), i_cnt=1,GRID_tetraelements)
	  END DO tetra_loop
	  DEALLOCATE(i_tets)

!---------- the element levels

	  ALLOCATE(i_ids(i_tnum), stat=i_alct)
	  IF(i_alct /= 0) THEN
	    CALL grid_error(13)
	  END IF

	  CALL grid_getinfo(p_handle, i_tnum, l_finelevel=.TRUE., &
	    i_tetralevel= i_ids)
	    
!---------- write modeltime
	
	  WRITE(i_io1,1009) p_timestepinfo%r_modeltime

!---------- start variable output
	  
	  WRITE(i_io1,1040)
	  WRITE(i_io1,1041)
	  WRITE(i_io1,1022) (i_ids(i_cnt), i_cnt=1,i_tnum)
	  DEALLOCATE(i_ids)

!---------- the element status

	  ALLOCATE(i_ids(i_tnum), stat=i_alct)
	  IF(i_alct /= 0) THEN
	    CALL grid_error(14)
	  END IF

	  CALL grid_getinfo(p_handle, i_tnum, l_finelevel=.TRUE., &
	    i_tetrastatus= i_ids)
	  WRITE(i_io1,1043)
	  WRITE(i_io1,1022) (i_ids(i_cnt), i_cnt=1,i_tnum)
	  DEALLOCATE(i_ids)

!---------- the tracer concentration

	  ALLOCATE(r_ttmp(i_nnum), stat=i_alct)
	  IF(i_alct /= 0) THEN
	    CALL grid_error(15)
	  END IF
	  CALL grid_getinfo(p_handle, i_nnum, i_valpoint=GRID_tracer, &
	    r_nodevalues= r_ttmp)
	  WRITE(i_io1,1042)
	  WRITE(i_io1,1011) (r_ttmp(i_cnt), i_cnt=1,i_nnum)

!---------- the u wind component

	  CALL grid_getinfo(p_handle, i_nnum, i_valpoint=GRID_ucomp, &
	    r_nodevalues= r_ttmp)
	  WRITE(i_io1,1044)
	  WRITE(i_io1,1011) (r_ttmp(i_cnt), i_cnt=1,i_nnum)

!---------- the v wind component

	  CALL grid_getinfo(p_handle, i_nnum, i_valpoint=GRID_vcomp, &
	    r_nodevalues= r_ttmp)
	  WRITE(i_io1,1045)
	  WRITE(i_io1,1011) (r_ttmp(i_cnt), i_cnt=1,i_nnum)

!---------- the w wind component

	  CALL grid_getinfo(p_handle, i_nnum, i_valpoint=GRID_wcomp, &
	    r_nodevalues= r_ttmp)
	  WRITE(i_io1,1046)
	  WRITE(i_io1,1011) (r_ttmp(i_cnt), i_cnt=1,i_nnum)
	  WRITE(i_io1,1049)
	  DEALLOCATE(r_ttmp)

!---------- close file

	  WRITE(i_io1,1100)
	  CLOSE(i_io1)

	  RETURN
  1000	  FORMAT('gmvinput ascii')
  1001	  FORMAT('comments')
  1002	  FORMAT(' File: ',a48)
  1003	  FORMAT(' Written by: ',a15,' Version ',i1,'.',i1,'.',i1)
  1004	  FORMAT(' Author: ',a48,' <',a48,'>')
  1005	  FORMAT('endcomm')
  1009    FORMAT('probtime ',f15.1)
  1010	  FORMAT('nodes ',i8)
  1011	  FORMAT(10f15.5)
  1020	  FORMAT('cells ',i8)
  1021	  FORMAT('tet 4')
  1022	  FORMAT(10i8)
  1031	  FORMAT('cellids')
  1040	  FORMAT('variable')
  1041	  FORMAT('level 0')
  1043	  FORMAT('status 0')
  1042	  FORMAT('tracr 1')
  1044	  FORMAT('uwind 1')
  1045	  FORMAT('vwind 1')
  1046	  FORMAT('wwind 1')
  1049	  FORMAT('endvars')
  1100	  FORMAT('endgmv')
 10101	  FORMAT(a24,i4.4,a4)
	  END SUBROUTINE plot_gmv

	END MODULE IO_gmvplot
