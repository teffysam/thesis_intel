!*****************************************************************
!
!> @file IO_vtuplot.F90
!> @brief includes module IO_vtuplot
!
!*****************************************************************
!
! VERSION(S):
!  1. original version                        j. behrens    04/2000
!  2. amatos-1.0 and 2D compliant             j. behrens    11/2000
!  3. adapted to flash2d                      j. behrens    12/2014
!  4. adjusted to 3D                          e. gerwing    03/2015
!
!*****************************************************************
! MODULE DESCRIPTION:
!> prints data in VTU compatible file format
!
MODULE IO_vtuplot
  USE FLASH_parameters
!  USE MISC_outputdata
  USE GRID_api
  USE IO_vtu
  PRIVATE
  INTEGER, SAVE :: i_timecounter= 0
  PUBLIC :: generate_vtu
  CONTAINS

!*****************************************************************
! DESCRIPTION of [SUBROUTINE generate_vtu]:
!> @brief creates output in GMV compatible file format
!>
!> @param[in]       p_handle      grid handle for the linked lists
!> @param[in]       i_time        time step number
!> @param[in]       i_newslevel   the time level
!>
!> @note ON OUTPUT: a number of VTU compatible ascii files for each plotted time step
!
  SUBROUTINE generate_vtu(p_handle, p_control, i_time, i_newslevel)

    IMPLICIT NONE

!---------- local declarations

    TYPE (grid_handle), INTENT(in)                        :: p_handle
    TYPE (control_struct)                                 :: p_control
    INTEGER (KIND = GRID_SI), OPTIONAL, INTENT(in)        :: i_time
    INTEGER (KIND = GRID_SI), OPTIONAL, INTENT(in)        :: i_newslevel
    INTEGER (KIND = GRID_SI)                              :: i_alct, i_nnum, i_tnum
    INTEGER (KIND = GRID_SI)                              :: i_nwl, i_elen, i_tcnt
    REAL (KIND = GRID_SR), DIMENSION(:,:), POINTER    :: r_val, r_val1, r_val2, r_val3
    REAL (KIND = GRID_SR), DIMENSION(:,:), POINTER    :: r_velo
    REAL (KIND = GRID_SR), DIMENSION(:,:), POINTER    :: r_sta, r_lvl
    INTEGER (KIND = GRID_SI), DIMENSION(:), ALLOCATABLE   :: i_ids, i_sta
    CHARACTER (len=32)                                    :: c_mfile
    CHARACTER (len=26)                                    :: c_tmp
    LOGICAL                                               :: l_news
    INTEGER, PARAMETER                                    :: i_vallen=5
    INTEGER, DIMENSION(i_vallen)                          :: i_valind
	REAL, PARAMETER                                       :: r_earth=40075000. ! earth circumference
	REAL                                                  :: r_scalx, r_scaly, r_scalz, r_deg, r_dt

    TYPE(t_vtu_data), DIMENSION(2)                        :: celldata
    TYPE(t_vtu_data), DIMENSION(4)                        :: nodedata

!---------- file handling (open)

    IF(present(i_time)) THEN
      i_tcnt= i_time
      i_timecounter= i_timecounter+1
    ELSE
      i_tcnt= i_timecounter
      i_timecounter= i_timecounter+1
    END IF
    write(c_tmp,*) trim(GRID_parameters%program_name(1:20))
    write(c_mfile,10101) trim(c_tmp), '_', i_tcnt
    c_tmp = adjustl(c_mfile)
    write(c_mfile,*) TRIM(c_tmp), '.vtu'
    c_mfile= adjustl(c_mfile)

!---------- handle news level

    IF(present(i_newslevel)) THEN
      l_news=.TRUE.
      i_nwl= i_newslevel
    ELSE
      l_news=.FALSE.
    END IF

!---------- the nodes

    i_nnum = p_handle%i_nnumber

!---------- extract nodal grid data and nodal values

	ALLOCATE(r_val(1,i_nnum),r_val1(1,i_nnum), r_val2(1,i_nnum), r_val3(1,i_nnum), stat=i_alct)
	IF(i_alct /= 0) THEN
	  CALL grid_error(c_error='[generate_vtu]: could not allocate values arrays')
	END IF


!----------  allocate velocity array

    ALLOCATE(r_velo(GRID_dimension,i_nnum), stat=i_alct)
	IF(i_alct /= 0) THEN
	  CALL grid_error(c_error='[generate_vtu]: could not allocate velocity array')
	END IF

!---------- convert velocities back to m/s

    r_deg = r_earth/360.        ! degree to m

	r_scalx = r_deg
	r_scaly = r_deg
	r_scalz = 1000.             ! km to m
	
	r_dt = p_control%phy%r_deltatime

!---------- Extract u-, v- and w-velocity data

	CALL grid_getinfo(p_handle,i_nnum, i_valpoint=GRID_ucomp, r_nodevalues= r_val)
	r_velo(1,:) = r_val(1,:)*r_scalx/r_dt*(-1.)

	CALL grid_getinfo(p_handle,i_nnum, i_valpoint=GRID_vcomp, r_nodevalues= r_val)
	r_velo(2,:) = r_val(1,:)*r_scaly/r_dt*(-1.)

	CALL grid_getinfo(p_handle,i_nnum, i_valpoint=GRID_wcomp, r_nodevalues= r_val)
	r_velo(3,:) = r_val(1,:)*r_scalz/r_dt*(-1.)

    nodedata(1)%c_name = 'velocity'
    nodedata(1)%i_size = 3
    nodedata(1)%p_vdata => r_velo

!---------- Extract tracer concentration

	  CALL grid_getinfo(p_handle, i_nnum, i_valpoint=GRID_tracer, r_nodevalues= r_val1)

    nodedata(2)%c_name = 'tracer'
    nodedata(2)%i_size = 1
    nodedata(2)%p_vdata => r_val1

!---------- Extract height data (?????????)

	  CALL grid_getinfo(p_handle, i_nnum, i_valpoint=GRID_phi, r_nodevalues= r_val2)

    nodedata(3)%c_name = 'height'
    nodedata(3)%i_size = 1
    nodedata(3)%p_vdata => r_val2

!---------- Extract vorticity (?????????????????)

	  CALL grid_getinfo(p_handle, i_nnum, i_valpoint=GRID_zeta, r_nodevalues= r_val3)

    nodedata(4)%c_name = 'vorticity'
    nodedata(4)%i_size = 1
    nodedata(4)%p_vdata => r_val3

!---------- extract cells grid data

	i_tnum= p_handle%i_enumfine
	ALLOCATE(i_ids(i_tnum), i_sta(i_tnum), r_sta(1,i_tnum), r_lvl(1,i_tnum), stat=i_alct)
	IF(i_alct /= 0) THEN
	  CALL grid_error(c_error='[generate_vtu]: could not allocate cell value arrays')
	END IF

!	IF(l_news) THEN
!	  CALL grid_getinfo(p_handle,i_tnum, i_newsdepth= i_nwl, i_elength= i_elen, &
!	    i_elementlevel= i_ids, i_elementstatus= i_sta)
!	  i_tnum= i_elen
!	ELSE
	  CALL grid_getinfo(p_handle, i_tnum, l_finelevel=.TRUE., i_tetrastatus= i_sta)
	  CALL grid_getinfo(p_handle, i_tnum, l_finelevel=.TRUE., i_tetralevel= i_ids)
!	END IF

    r_sta(1, :) = REAL(i_sta, GRID_SR)
    r_lvl(1, :) = REAL(i_ids, GRID_SR)

    ! Assign all values to driver
    celldata(1)%c_name = 'status'
    celldata(1)%i_size = 1
    celldata(1)%p_vdata => r_sta

    celldata(2)%c_name = 'level'
    celldata(2)%i_size = 1
    celldata(2)%p_vdata => r_lvl
    
    ! plot the vtu data
    CALL plot_vtu(p_handle, c_mfile, i_nodedata = 4, p_nodedata = nodedata, &
                  i_celldata = 2, p_celldata = celldata)

    DEALLOCATE(i_ids, i_sta, r_sta, r_lvl, &
               r_val,r_val1, r_val2, r_val3, r_velo)

    RETURN

!---------- FORMAT
 10101    FORMAT(a19,a1,i6.6)

  END SUBROUTINE generate_vtu

END MODULE IO_vtuplot
