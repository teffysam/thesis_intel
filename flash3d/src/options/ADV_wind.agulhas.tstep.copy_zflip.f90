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
!	2. bug fix concerning interp.	j. behrens	2/98
!	3. compliant to amatos 1.0	j. behrens	12/2000
!	4. simple data reading and interpolation(similar to 2D-Case)    e.gerwing   03/2015
!
!*****************************************************************
	MODULE ADV_wind
		  USE GRID_api
	  USE FLASH_parameters
	  PRIVATE
	  INTEGER, PARAMETER    :: i_ioerr=0
	  REAL :: r_intervallen, r_scalfacx, r_scalfacy, &
	                           r_readlast, r_scalinvx, r_scalinvy
	  REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: r_flowx
	  REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: r_flowy
	  REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: r_flowz

	  !REAL, DIMENSION(100,50,23,1) :: r_flowx, r_flowy, r_flowz

	!   REAL, DIMENSION(501)  :: r_latu, r_latv, r_latw
	!   REAL, DIMENSION(1101) :: r_lonu, r_lonv, r_lonw
	!   REAL, DIMENSION(23) :: r_z, r_zw
	!   REAL				  :: r_time
	  REAL, DIMENSION(:), ALLOCATABLE     :: r_latu
	  REAL, DIMENSION(:), ALLOCATABLE     :: r_lonu
	  REAL, DIMENSION(:), ALLOCATABLE     :: r_latv
	  REAL, DIMENSION(:), ALLOCATABLE     :: r_lonv
	  REAL, DIMENSION(:), ALLOCATABLE     :: r_latw
	  REAL, DIMENSION(:), ALLOCATABLE     :: r_lonw
	  REAL, DIMENSION(:), ALLOCATABLE     :: r_z
	  REAL, DIMENSION(:), ALLOCATABLE     :: r_zw
	  REAL, DIMENSION(:), ALLOCATABLE     :: r_time
      INTEGER                            :: i_lon, i_lat, i_z, i_timesteps
	  INTEGER                            :: i_timeinterval
	  
	  PUBLIC                    :: slm_windfield, slm_windinit, slm_windquit,r_field, read_netcdf_currents
	  
	  CONTAINS
!*****************************************************************
	  FUNCTION slm_windfield(p_control,r_coord, r_time) RESULT (r_field)

!---------- local declarations

	  IMPLICIT NONE

	  TYPE (control_struct), INTENT(in)     :: p_control
	  REAL, DIMENSION(GRID_dimension), INTENT(in) :: r_coord
	  REAL, INTENT(in), OPTIONAL :: r_time
	  REAL, DIMENSION(GRID_dimension)             :: r_field
	  REAL                       :: r_tim
	  CHARACTER (LEN=67)                          :: c_xfile, c_yfile
	  CHARACTER (LEN=35)                          :: c_tmp
	  CHARACTER (LEN=3)                           :: c_num
	  INTEGER                                     :: i_iost, i_cnt, j_cnt
	  
	!   CHARACTER (LEN=io_fillen) :: c_name


	!   c_name= p_control%io%c_windfile
!---------- set time

	  IF(present(r_time)) THEN
	    r_tim= r_time
	  ELSE
	    r_tim= 0.0
	  END IF
	  

	  !!!! i_timeinterval needs to PREDEFINED to ONE! This is passed into data_interpol
	  i_timeinterval=1
!---------- decide, if data has to be read
	  data_read: IF(r_readlast <= r_tim) THEN

		   !!!!! Try reading here
	  CALL read_netcdf_currents(r_tim)
!---------- update values for next open
	  
	    r_readlast = r_readlast+ r_intervallen
	
	
	
		!  This NEEDS to be SWITCHED OFF
		!    i_timeinterval= i_timeinterval+ 1
	  END IF data_read
!---------- interpolate to coordinate
	!   write(*,*) r_coord
	  r_field= data_interpol(r_coord,i_timeinterval) !!TODO check here for flipping															!!Welcome back :)
	  RETURN
 1000	  FORMAT(i3.3)
	  END FUNCTION slm_windfield

!*****************************************************************
	  SUBROUTINE read_netcdf_currents(r_tim)

!---------- local declarations

	  IMPLICIT NONE
      INCLUDE "netcdf.inc"
      
      
!---------- input parameters      
	  CHARACTER (LEN=80) :: c_filename
!---------- local variables
	  INTEGER         :: i_alct, i_ncstat
	  INTEGER         :: i_fileid, i_dimid, i_varid, countA(1),startA(1),startB(4),countB(4)
	  INTEGER         :: i_tm, i_ln, i_lt, i_lz
	  REAL :: r_tim
	  REAL :: r_ttim
	  character(256) :: my_errmsg
	  
!---------- open current file
	  
    
! !---------- determine lon/lat/height/time dimension sizes (grid size of currents field)

!       i_ncstat= nf_inq_dimid(i_fileid, 'lon_u', i_dimid)
! 	  IF(i_ncstat /= NF_NOERR) &
! 	    CALL grid_error(c_error='[read_netcdf_currents]: could not identify lon dimension')
!       i_ncstat= nf_inq_dimlen(i_fileid, i_dimid, i_lon)
! 	  IF(i_ncstat /= NF_NOERR) &
! 	    CALL grid_error(c_error='[read_netcdf_currents]: could not read lon dimension')
      
!       i_ncstat= nf_inq_dimid(i_fileid, 'lat_u', i_dimid)
! 	  IF(i_ncstat /= NF_NOERR) &
! 	    CALL grid_error(c_error='[read_netcdf_currents]: could not identify lat dimension')
!       i_ncstat= nf_inq_dimlen(i_fileid, i_dimid, i_lat)
! 	  IF(i_ncstat /= NF_NOERR) &
! 	    CALL grid_error(c_error='[read_netcdf_currents]: could not read lat dimension')
	    
! 	    i_ncstat= nf_inq_dimid(i_fileid, 'dep', i_dimid)
! 	  IF(i_ncstat /= NF_NOERR) &
! 	    CALL grid_error(c_error='[read_netcdf_currents]: could not identify z dimension')
!       i_ncstat= nf_inq_dimlen(i_fileid, i_dimid, i_z)
! 	  IF(i_ncstat /= NF_NOERR) &
! 		CALL grid_error(c_error='[read_netcdf_currents]: could not read z dimension')
		
! !      i_ncstat= nf_inq_dimid(i_fileid, 'time', i_dimid)
! !	  IF(i_ncstat /= NF_NOERR) &
! !	    CALL grid_error(c_error='[read_netcdf_currents]: could not identify time dimension')
! !      i_ncstat= nf_inq_dimlen(i_fileid, i_dimid, i_timesteps)
! i_timesteps=1
! 	  IF(i_ncstat /= NF_NOERR) &
! 	    CALL grid_error(c_error='[read_netcdf_currents]: could not read time dimension')
    
! !---------- allocate lat/long/height/time coordinate arrays
! !	  ALLOCATE( r_latu(i_lat), r_lonv(i_lon), r_lonw(i_lon), &
! !	    		r_lonu(i_lon), r_latv(i_lat), r_latw(i_lat), &
! !	    		r_z(i_z), r_zw(i_z), r_time(i_timesteps), stat= i_alct)
! !	 !rite(*,*) i_alct 
! !      IF(i_alct /= 0) &
! !	    CALL grid_error(c_error='[read_netcdf_currents]: could not allocate lat/lon/z/time field')
! !---------- read lat/long/height/time coordinate values

!       i_ncstat= nf_inq_varid(i_fileid, 'lon_u', i_varid)
! 	  IF(i_ncstat /= NF_NOERR) &
! 	    CALL grid_error(c_error='[read_netcdf_currents]: could not determine lon varid')
!       i_ncstat= nf_get_var_real(i_fileid, i_varid, r_lonu)
! 	  IF(i_ncstat /= NF_NOERR) &
! 	    CALL grid_error(c_error='[read_netcdf_currents]: could not read lon data')

!       i_ncstat= nf_inq_varid(i_fileid, 'lat_u', i_varid)
! 	  IF(i_ncstat /= NF_NOERR) &
! 	    CALL grid_error(c_error='[read_netcdf_currents]: could not determine lat varid')
!       i_ncstat= nf_get_var_real(i_fileid, i_varid, r_latu)
! 	  IF(i_ncstat /= NF_NOERR) &
! 	    CALL grid_error(c_error='[read_netcdf_currents]: could not read lat data')

!       i_ncstat= nf_inq_varid(i_fileid, 'lon_v', i_varid)
! 	  IF(i_ncstat /= NF_NOERR) &
! 	    CALL grid_error(c_error='[read_netcdf_currents]: could not determine lon varid')
!       i_ncstat= nf_get_var_real(i_fileid, i_varid, r_lonv)
! 	  IF(i_ncstat /= NF_NOERR) &
! 	    CALL grid_error(c_error='[read_netcdf_currents]: could not read lon data')

!       i_ncstat= nf_inq_varid(i_fileid, 'lat_v', i_varid)
! 	  IF(i_ncstat /= NF_NOERR) &
! 	    CALL grid_error(c_error='[read_netcdf_currents]: could not determine lat varid')
!       i_ncstat= nf_get_var_real(i_fileid, i_varid, r_latv)
! 	  IF(i_ncstat /= NF_NOERR) &
! 	    CALL grid_error(c_error='[read_netcdf_currents]: could not read lat data')

!       i_ncstat= nf_inq_varid(i_fileid, 'lon_w', i_varid)
! 	  IF(i_ncstat /= NF_NOERR) &
! 	    CALL grid_error(c_error='[read_netcdf_currents]: could not determine lon varid')
!       i_ncstat= nf_get_var_real(i_fileid, i_varid, r_lonw)
! 	  IF(i_ncstat /= NF_NOERR) &
! 	    CALL grid_error(c_error='[read_netcdf_currents]: could not read lon data')

!       i_ncstat= nf_inq_varid(i_fileid, 'lat_w', i_varid)
! 	  IF(i_ncstat /= NF_NOERR) &
! 	    CALL grid_error(c_error='[read_netcdf_currents]: could not determine lat varid')
!       i_ncstat= nf_get_var_real(i_fileid, i_varid, r_latw)
! 	  IF(i_ncstat /= NF_NOERR) &
! 	    CALL grid_error(c_error='[read_netcdf_currents]: could not read lat data')

! 	  i_ncstat= nf_inq_varid(i_fileid, 'dep', i_varid)
! 	  IF(i_ncstat /= NF_NOERR) &
! 	    CALL grid_error(c_error='[read_netcdf_currents]: could not determine height varid')
!       i_ncstat= nf_get_var_real(i_fileid, i_varid, r_z)
! 	  IF(i_ncstat /= NF_NOERR) &
! 	    CALL grid_error(c_error='[read_netcdf_currents]: could not read height data')

! 	  i_ncstat= nf_inq_varid(i_fileid, 'depw', i_varid)
! 	  IF(i_ncstat /= NF_NOERR) &
! 	    CALL grid_error(c_error='[read_netcdf_currents]: could not determine height varid')
!       i_ncstat= nf_get_var_real(i_fileid, i_varid, r_zw)
! 	  IF(i_ncstat /= NF_NOERR) &
! 		CALL grid_error(c_error='[read_netcdf_currents]: could not read height data')

!Ensure memory is freed
	  IF(ALLOCATED(r_flowx))	  DEALLOCATE(r_flowx)
	  IF(ALLOCATED(r_flowy))	  DEALLOCATE(r_flowy)
	  IF(ALLOCATED(r_flowz))	  DEALLOCATE(r_flowz)
	  
!---------- allocate current data arrays
	

	  ALLOCATE( r_flowx(i_lon, i_lat, i_z, i_timesteps), r_flowy(i_lon, i_lat, i_z, i_timesteps),&
	    & r_flowz(i_lon, i_lat, i_z, i_timesteps), stat= i_alct, errmsg=my_errmsg)
	! write(*,*) i_alct, my_errmsg
     IF(i_alct /= 0) &
		CALL grid_error(c_error='[read_netcdf_currents]: could not allocate currents fields')
		


	  !READ TIME
	  c_filename='/pool/testu'
      i_ncstat= nf_open(c_filename,NF_NOWRITE,i_fileid)
	  IF(i_ncstat /= NF_NOERR) &
	    CALL grid_error(c_error='[read_netcdf_currents]: could not open currents data file')

      i_ncstat= nf_inq_varid(i_fileid, 'TIME1', i_varid)
	  IF(i_ncstat /= NF_NOERR) &
		CALL grid_error(c_error='[read_netcdf_currents]: could not determine time varid')
	
		r_ttim=0
	  IF(r_tim .ne. 0) THEN 
		r_ttim=r_tim/86400
	  END IF
	!   write(*,*) r_tim,r_ttim
startA(1)=r_ttim+1
countA(1)=i_timesteps
!write(*,*) startA, countA 
	  i_ncstat= nf_get_vara_real(i_fileid, i_varid, startA, countA, r_time)
!write(*,*) i_ncstat
	  IF(i_ncstat /= NF_NOERR) &
	    CALL grid_error(c_error='[read_netcdf_currents]: could not read time data')

!---------- read x-/y-/z-direction data of currents


startB(1)=1
startB(2)=1
startB(3)=1
startB(4)=r_ttim+1

countB(1)=i_lon
countB(2)=i_lat
countB(3)=i_z
countB(4)=i_timesteps

		!OPEN UVEL
      i_ncstat= nf_inq_varid(i_fileid, 'UVEL', i_varid)
	  IF(i_ncstat /= NF_NOERR) &
	    CALL grid_error(c_error='[read_netcdf_currents]: could not determine varid of var131')
	 
		i_ncstat= nf_get_vara_real(i_fileid, i_varid, startB, countB, r_flowx)
! write(*,*) startB, countB, i_ncstat, r_flowx(9,9,3,1)
	  IF(i_ncstat /= NF_NOERR) &
		CALL grid_error(c_error='[read_netcdf_currents]: could not read var131 data')
		
		i_ncstat= nf_close(i_fileid)
		IF(i_ncstat /= NF_NOERR) &
		  CALL grid_error(c_error='[read_netcdf_currents]: could not close currents data file')



		!   OPEN VVEL
		c_filename='/pool/testv'
      i_ncstat= nf_open(c_filename,NF_NOWRITE,i_fileid)
	  IF(i_ncstat /= NF_NOERR) &
		CALL grid_error(c_error='[read_netcdf_currents]: could not open currents data file')
		
      i_ncstat= nf_inq_varid(i_fileid, 'VVEL', i_varid)
	  IF(i_ncstat /= NF_NOERR) &
	    CALL grid_error(c_error='[read_netcdf_currents]: could not determine varid of var132')
	  i_ncstat= nf_get_vara_real(i_fileid, i_varid, startB, countB, r_flowy)
	  IF(i_ncstat /= NF_NOERR) &
		CALL grid_error(c_error='[read_netcdf_currents]: could not read var132 data')

		i_ncstat= nf_close(i_fileid)
	IF(i_ncstat /= NF_NOERR) &
	  CALL grid_error(c_error='[read_netcdf_currents]: could not close currents data file')




	!   OPEN WVEL
		c_filename='/pool/testw'
      i_ncstat= nf_open(c_filename,NF_NOWRITE,i_fileid)
	  IF(i_ncstat /= NF_NOERR) &
	    CALL grid_error(c_error='[read_netcdf_currents]: could not open currents data file')

      i_ncstat= nf_inq_varid(i_fileid, 'WVEL', i_varid)
	  IF(i_ncstat /= NF_NOERR) &
	    CALL grid_error(c_error='[read_netcdf_currents]: could not determine varid of var135')
	  i_ncstat= nf_get_vara_real(i_fileid, i_varid, startB, countB, r_flowz)
	  IF(i_ncstat /= NF_NOERR) &
		CALL grid_error(c_error='[read_netcdf_currents]: could not read var135 data')
		
		i_ncstat= nf_close(i_fileid)
	IF(i_ncstat /= NF_NOERR) &
	  CALL grid_error(c_error='[read_netcdf_currents]: could not close currents data file')



!---------- Fix mask values
	DO i_tm=1,i_timesteps
	  	DO i_lt= 1,i_lat
	  	  DO i_ln= 1,i_lon
	  	    DO i_lz= 1,i_z
	  	      IF(r_flowx(i_ln,i_lt,i_lz,i_tm) <= -8.99E+33) r_flowx(i_ln,i_lt,i_lz,i_tm)= 0.
	  	      IF(r_flowy(i_ln,i_lt,i_lz,i_tm) <= -8.99E+33) r_flowy(i_ln,i_lt,i_lz,i_tm)= 0.
	  	      IF(r_flowz(i_ln,i_lt,i_lz,i_tm) <= -8.99E+33) r_flowz(i_ln,i_lt,i_lz,i_tm)= 0.
	  	    END DO
	  	  END DO
		END DO
	END DO
!-----------set timeinterval length between winddata (Timeformat here: yymmdd,ddd)

	  !r_intervallen= (r_time(2) - r_time(1))*24*60*60
	  r_intervallen= 86400 !r_time(2) - r_time(1)

!---------- close currents file

	


	  END SUBROUTINE read_netcdf_currents

!*****************************************************************
	  SUBROUTINE time_interpol(p_control)

!---------- local declarations

	  IMPLICIT NONE

	  TYPE (control_struct)                                :: p_control
	  REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: r_fx, r_fy, r_fz
	  REAL                                :: r_t, r_dt
	  INTEGER                             :: i_tstart, i_tend, i_tint, i_tstep, i_alct

!---------- initialize

	  r_dt = p_control%phy%r_deltatime
	  i_tstart = p_control%phy%i_frsttimestep
	  i_tend = p_control%phy%i_lasttimestep

!---------- allocate interpolated windfields
	  ALLOCATE(r_fx(i_lon,i_lat,i_z,i_tend-i_tstart+1),r_fy(i_lon,i_lat,i_z,i_tend-i_tstart+1),&
	    &r_fz(i_lon,i_lat,i_z,i_tend-i_tstart+1), stat= i_alct)
      IF(i_alct /= 0) &
	    CALL grid_error(c_error='[time_interpol]: could not allocate windfields r_fx, r_fy, r_fz')

!---------- initialize 

	  i_tint = i_timeinterval+1
	  r_t = 0.0

!---------- Linear Interpolation for every timestep (only valid for equidistant timeintervals in Winddata)

	  DO i_tstep = i_tstart,i_tend
	    IF (r_t > r_intervallen) THEN      ! Use Winddata from next timestep
	        r_t = 0
	        i_tint = i_tint +1
	        IF (i_tint == i_timesteps) THEN  ! Reached last Winddata
	            i_tint = i_timesteps -1
	            r_t = r_intervallen
	        END IF
	    END IF
	  
	    r_fx(:,:,:,i_tstep) = ( r_flowx(:,:,:,i_tint)*(r_intervallen-r_t) + r_flowx(:,:,:,i_tint+1)*(r_t-0) )/r_intervallen
	    r_fy(:,:,:,i_tstep) = ( r_flowy(:,:,:,i_tint)*(r_intervallen-r_t) + r_flowy(:,:,:,i_tint+1)*(r_t-0) )/r_intervallen
	    r_fz(:,:,:,i_tstep) = ( r_flowz(:,:,:,i_tint)*(r_intervallen-r_t) + r_flowz(:,:,:,i_tint+1)*(r_t-0) )/r_intervallen
	  
	    r_t = r_t + r_dt
	  END DO

!---------- Reallocate Windfields with new shape

	!  DEALLOCATE(r_flowx, r_flowy, r_flowz)
	!  ALLOCATE(r_flowx(i_lon,i_lat,i_z,i_tend-i_tstart+1),r_flowy(i_lon,i_lat,i_z,i_tend-i_tstart+1),&
	!    &r_flowz(i_lon,i_lat,i_z,i_tend-i_tstart+1) ,stat= i_alct)
      IF(i_alct /= 0) &
	    CALL grid_error(c_error='[time_interpol]: could not allocate windfields r_flowx, r_flowy, r_flowz')

!---------- Use interpolated windfields and deallocate temporary Windfiels r_fx,r_fy
 
	  r_flowx = r_fx
	  r_flowy = r_fy
	  r_flowz = r_fz
	  DEALLOCATE(r_fx, r_fy, r_fz)

!----------- new timeinterval between interpolated Windfields
	  r_intervallen= r_dt

	  END SUBROUTINE time_interpol

!*****************************************************************
	  SUBROUTINE slm_windinit(p_control)

!---------- local declarations

	  IMPLICIT NONE
	  INCLUDE "netcdf.inc"


	  
	!  TYPE (rt_info), INTENT(inout)	    :: p_tinfo
	  

!---------- initialize

	  
	 ! write (*,*) p_tinfo%i_step

!---------- read current data from NetCDF file

! This is moved into windfield into the IF loop
!	  CALL read_netcdf_currents(c_name,p_tinfo)

	  !!!! Moved vars that are constant from read_netcdf_currents
	  
      
      
      
!---------- input parameters  
	  TYPE (control_struct)     :: p_control    
	  CHARACTER (LEN=80)  :: c_filename
!---------- local variables
	  INTEGER         :: i_alct, i_ncstat
	  INTEGER         :: i_fileid, i_dimid, i_varid, countA(1),startA(1),startB(4),countB(4)
	  
	  
	  

	  c_filename= '/pool/agulhas_grid.cdf'
!---------- open current file

      i_ncstat= nf_open(c_filename,NF_NOWRITE,i_fileid)
	  IF(i_ncstat /= NF_NOERR) &
	    CALL grid_error(c_error='[read_netcdf_currents]: could not open currents data file')
    
!---------- determine lon/lat/height/time dimension sizes (grid size of currents field)
! check ncdump for agulhas_grid LON_U is stored by (X_T3150_4250 , Y_T1245_1745) or (1101,501)
      i_ncstat= nf_inq_dimid(i_fileid, 'X_T3150_4250', i_dimid)
	  IF(i_ncstat /= NF_NOERR) &
	    CALL grid_error(c_error='[read_netcdf_currents]: could not identify lon dimension')
      i_ncstat= nf_inq_dimlen(i_fileid, i_dimid, i_lon)
	  IF(i_ncstat /= NF_NOERR) &
	    CALL grid_error(c_error='[read_netcdf_currents]: could not read lon dimension')
      
      i_ncstat= nf_inq_dimid(i_fileid, 'Y_T1245_1745', i_dimid)
	  IF(i_ncstat /= NF_NOERR) &
	    CALL grid_error(c_error='[read_netcdf_currents]: could not identify lat dimension')
      i_ncstat= nf_inq_dimlen(i_fileid, i_dimid, i_lat)
	  IF(i_ncstat /= NF_NOERR) &
	    CALL grid_error(c_error='[read_netcdf_currents]: could not read lat dimension')
	    
	    i_ncstat= nf_inq_dimid(i_fileid, 'AX008', i_dimid)
	  IF(i_ncstat /= NF_NOERR) &
	    CALL grid_error(c_error='[read_netcdf_currents]: could not identify z dimension')
      i_ncstat= nf_inq_dimlen(i_fileid, i_dimid, i_z)
	  IF(i_ncstat /= NF_NOERR) &
		CALL grid_error(c_error='[read_netcdf_currents]: could not read z dimension')
		
!      i_ncstat= nf_inq_dimid(i_fileid, 'time', i_dimid)
!	  IF(i_ncstat /= NF_NOERR) &
!	    CALL grid_error(c_error='[read_netcdf_currents]: could not identify time dimension')
!      i_ncstat= nf_inq_dimlen(i_fileid, i_dimid, i_timesteps)
i_timesteps=1
!	  IF(i_ncstat /= NF_NOERR) &
!	    CALL grid_error(c_error='[read_netcdf_currents]: could not read time dimension')
    
!---------- allocate lat/long/height/time coordinate arrays
	  ALLOCATE( r_latu(i_lat), r_lonv(i_lon), r_lonw(i_lon), &
	    		r_lonu(i_lon), r_latv(i_lat), r_latw(i_lat), &
	    		r_z(i_z), r_zw(i_z), r_time(i_timesteps), stat= i_alct)
	 !write(*,*) i_alct 
     IF(i_alct /= 0) &
	    CALL grid_error(c_error='[read_netcdf_currents]: could not allocate lat/lon/z/time field')
!---------- read lat/long/height/time coordinate values

      i_ncstat= nf_inq_varid(i_fileid, 'LON_U', i_varid)
	  IF(i_ncstat /= NF_NOERR) &
	    CALL grid_error(c_error='[read_netcdf_currents]: could not determine lon varid')
	  i_ncstat= nf_get_vara_real(i_fileid, i_varid, (/ 1, 1 /), (/ i_lon, 1 /), r_lonu)
	  IF(i_ncstat /= NF_NOERR) &
	    CALL grid_error(c_error='[read_netcdf_currents]: could not read lon data')

      i_ncstat= nf_inq_varid(i_fileid, 'LAT_U', i_varid)
	  IF(i_ncstat /= NF_NOERR) &
	    CALL grid_error(c_error='[read_netcdf_currents]: could not determine lat varid')
	  i_ncstat= nf_get_vara_real(i_fileid, i_varid, (/ 100, 1 /), (/ 1, i_lat /), r_latu)
	  IF(i_ncstat /= NF_NOERR) &
	    CALL grid_error(c_error='[read_netcdf_currents]: could not read lat data')

      i_ncstat= nf_inq_varid(i_fileid, 'LON_V', i_varid)
	  IF(i_ncstat /= NF_NOERR) &
	    CALL grid_error(c_error='[read_netcdf_currents]: could not determine lon varid')
      i_ncstat= nf_get_vara_real(i_fileid, i_varid, (/ 1, 1 /), (/ i_lon, 1 /), r_lonv)
	  IF(i_ncstat /= NF_NOERR) &
	    CALL grid_error(c_error='[read_netcdf_currents]: could not read lon data')

      i_ncstat= nf_inq_varid(i_fileid, 'LAT_V', i_varid)
	  IF(i_ncstat /= NF_NOERR) &
	    CALL grid_error(c_error='[read_netcdf_currents]: could not determine lat varid')
      i_ncstat= nf_get_vara_real(i_fileid, i_varid, (/ 100, 1 /), (/ 1, i_lat /), r_latv)
	  IF(i_ncstat /= NF_NOERR) &
	    CALL grid_error(c_error='[read_netcdf_currents]: could not read lat data')

      i_ncstat= nf_inq_varid(i_fileid, 'LON_TS', i_varid)
	  IF(i_ncstat /= NF_NOERR) &
	    CALL grid_error(c_error='[read_netcdf_currents]: could not determine lon varid')
      i_ncstat= nf_get_vara_real(i_fileid, i_varid, (/ 1, 1 /), (/ i_lon, 1 /), r_lonw)
	  IF(i_ncstat /= NF_NOERR) &
	    CALL grid_error(c_error='[read_netcdf_currents]: could not read lon data')

      i_ncstat= nf_inq_varid(i_fileid, 'LAT_TS', i_varid)
	  IF(i_ncstat /= NF_NOERR) &
	    CALL grid_error(c_error='[read_netcdf_currents]: could not determine lat varid')
      i_ncstat= nf_get_vara_real(i_fileid, i_varid, (/ 100, 1 /), (/ 1, i_lat /), r_latw)
	  IF(i_ncstat /= NF_NOERR) &
	    CALL grid_error(c_error='[read_netcdf_currents]: could not read lat data')

	  i_ncstat= nf_inq_varid(i_fileid, 'AX008', i_varid)
	  IF(i_ncstat /= NF_NOERR) &
	    CALL grid_error(c_error='[read_netcdf_currents]: could not determine height varid')
	  i_ncstat= nf_get_var_real(i_fileid, i_varid, r_z)
	  r_z=r_z/1000
	!   write(*,*) r_z
	  IF(i_ncstat /= NF_NOERR) &
	    CALL grid_error(c_error='[read_netcdf_currents]: could not read height data')

	  i_ncstat= nf_inq_varid(i_fileid, 'AX009', i_varid)
	  IF(i_ncstat /= NF_NOERR) &
	    CALL grid_error(c_error='[read_netcdf_currents]: could not determine height varid')
	  i_ncstat= nf_get_var_real(i_fileid, i_varid, r_zw)
	  r_zw=r_zw/1000
	  IF(i_ncstat /= NF_NOERR) &
		CALL grid_error(c_error='[read_netcdf_currents]: could not read height data')

	! Close the grid file
	  i_ncstat= nf_close(i_fileid)
	  IF(i_ncstat /= NF_NOERR) &
	    CALL grid_error(c_error='[read_netcdf_currents]: could not close currents data file')

!---------- initialize some values

	  r_readlast= 0.0
	  i_timeinterval= 0
!	  r_intervallen= 3600.

!---------- Interpolate between timesteps
!	  CALL time_interpol(p_control)

	  RETURN
 2000	  FORMAT(a80)
 2010	  FORMAT(a32)
	  END SUBROUTINE slm_windinit

!*****************************************************************
	  SUBROUTINE slm_windquit

	  IMPLICIT NONE
	INTEGER 		:: i_ncstat,i_fileid
	  
!---------- deallocate wind data arrays

!IF(ALLOCATED(r_flowx))	  DEALLOCATE(r_flowx)
!IF(ALLOCATED(r_flowy))	  DEALLOCATE(r_flowy)
!IF(ALLOCATED(r_flowz))	  DEALLOCATE(r_flowz)

!	  DEALLOCATE(r_lonu, r_latu, r_lonv, r_latv, r_lonw, r_latw, r_z, r_zw)
		
	  RETURN
	  END SUBROUTINE slm_windquit

!*****************************************************************
	  FUNCTION data_interpol(r_coord,i_time) RESULT (r_inter)

!---------- local declarations

	  IMPLICIT NONE
	  REAL, DIMENSION(GRID_dimension) :: r_coord
	  INTEGER         :: i_time
	  REAL, DIMENSION(GRID_dimension) :: r_inter
	  INTEGER                         :: i_loux, i_hiux, i_louy, i_hiuy, &
		i_lovx, i_hivx, i_lovy, i_hivy, &
		i_lowx, i_hiwx, i_lowy, i_hiwy, &
	    i_loz, i_hiz, i_lozz, i_hizz, i_cnt
	  REAL                            :: r_dux, r_duy, r_duz,  r_dvx, r_dvy, r_dvz,&
					r_dyi,r_dz,r_dzi, r_dwx, r_dwy, r_dwz, r_dxui, r_dyui, r_dzui,&
							 r_dxvi, r_dyvi, r_dzvi,r_dxwi, r_dywi, r_dzwi, &
		r_lux, r_hux,r_luy, r_huy, r_lvx, r_hvx,r_lvy, r_hvy, r_lwx, &
		r_hwx,r_lwy, r_hwy, r_lz, r_hz, r_lzz, r_hzz
	  REAL                            :: r_xu1, r_xu2, r_xu3, r_xu4, &
	    r_xv1, r_xv2, r_xv3, r_xv4, r_xw1, r_xw2, r_xw3, r_xw4, &
	    r_yu1, r_yu2, r_yv1, r_yv2, r_yw1, r_yw2
	  REAL                            :: r_scalx, r_scaly, r_scalz
	  REAL                            :: r_deg
	  REAL, PARAMETER                 :: r_earth=40075000. ! earth circumference
	  REAL, PARAMETER                 :: PI=3.1415927
 	
!---------- initialize radians

	  r_deg= 360./r_earth

!---------- find wind box corresponding to coordinate

	  i_loux=1
	  i_hiux=2
	  i_lovx=1
	  i_hivx=2
	  i_lowx=1
	  i_hiwx=2
	  i_louy=1
	  i_hiuy=2
	  i_lovy=1
	  i_hivy=2
	  i_lowy=1
	  i_hiwy=2
	  i_loz=1
	  i_hiz=2
	  i_lozz=1
	  i_hizz=2
	  
	  determine_latu: DO i_cnt=1, i_lat-1
	    IF(r_latu(i_cnt) <= r_coord(2) .AND. r_latu(i_cnt +1) >= r_coord(2)) THEN
	        i_louy= i_cnt
	        i_hiuy= i_cnt+1
	        exit determine_latu
	    END IF
	  END DO determine_latu
	  IF(r_latu(i_lat) <= r_coord(2)) THEN
	    i_louy = i_lat-1
	    i_hiuy = i_lat
	  END IF

	  determine_latv: DO i_cnt=1, i_lat-1
	    IF(r_latv(i_cnt) <= r_coord(2) .AND. r_latv(i_cnt +1) >= r_coord(2)) THEN
	        i_lovy= i_cnt
	        i_hivy= i_cnt+1
	        exit determine_latv
	    END IF
	  END DO determine_latv
	  IF(r_latv(i_lat) <= r_coord(2)) THEN
	    i_lovy = i_lat-1
	    i_hivy = i_lat
	  END IF

	  determine_latw: DO i_cnt=1, i_lat-1
	    IF(r_latw(i_cnt) <= r_coord(2) .AND. r_latw(i_cnt +1) >= r_coord(2)) THEN
	        i_lowy= i_cnt
	        i_hiwy= i_cnt+1
	        exit determine_latw
	    END IF
	  END DO determine_latw
	  IF(r_latw(i_lat) <= r_coord(2)) THEN
	    i_lowy = i_lat-1
	    i_hiwy = i_lat
	  END IF

	  determine_lonu: DO i_cnt=1, i_lon-1
	    IF(r_lonu(i_cnt) <= r_coord(1) .AND. r_lonu(i_cnt +1) >= r_coord(1)) THEN
	        i_loux= i_cnt
	        i_hiux= i_cnt+1
	        exit determine_lonu
	    END IF
	  END DO determine_lonu
	  IF(r_lonu(i_lon) <= r_coord(1)) THEN
	    i_loux = i_lon-1
	    i_hiux = i_lon
	  END IF

	  determine_lonv: DO i_cnt=1, i_lon-1
	    IF(r_lonv(i_cnt) <= r_coord(1) .AND. r_lonv(i_cnt +1) >= r_coord(1)) THEN
	        i_lovx= i_cnt
	        i_hivx= i_cnt+1
	        exit determine_lonv
	    END IF
	  END DO determine_lonv
	  IF(r_lonv(i_lon) <= r_coord(1)) THEN
	    i_lovx = i_lon-1
	    i_hivx = i_lon
	  END IF
	!TODO read lat lon from matlab properly 
	  determine_lonw: DO i_cnt=1, i_lon-1
	    IF(r_lonw(i_cnt) <= r_coord(1) .AND. r_lonw(i_cnt +1) >= r_coord(1)) THEN
	        i_lowx= i_cnt
	        i_hiwx= i_cnt+1
	        exit determine_lonw
	    END IF
	  END DO determine_lonw
	  IF(r_lonw(i_lon) <= r_coord(1)) THEN
	    i_lowx = i_lon-1
	    i_hiwx = i_lon
	  END IF

	  determine_height: DO i_cnt=1, i_z-1
	    IF(r_zw(i_cnt) <= r_coord(3) .AND. r_zw(i_cnt +1) >= r_coord(3)) THEN
	        i_loz= i_cnt
	        i_hiz= i_cnt+1
	        exit determine_height
	    END IF
	  END DO determine_height
	  IF(r_zw(i_z) <= r_coord(3)) THEN
	    i_loz = i_z-1
	    i_hiz = i_z
	  END IF

	  determine_heightw: DO i_cnt=1, i_z-1
	    IF(r_zw(i_cnt) <= r_coord(3) .AND. r_zw(i_cnt +1) >= r_coord(3)) THEN
	        i_lozz= i_cnt
	        i_hizz= i_cnt+1
	        exit determine_heightw
	    END IF
	  END DO determine_heightw
	  IF(r_zw(i_z) <= r_coord(3)) THEN
	    i_lozz = i_z-1
	    i_hizz = i_z
	  END IF
!TODO fix this problem tommorow:fixed
!---------- calculate weights for bilinear interpolation

	  r_dux= r_lonu(i_hiux)- r_lonu(i_loux)
	  r_dvx= r_lonv(i_hivx)- r_lonv(i_lovx)
	  r_dwx= r_lonw(i_hiwx)- r_lonw(i_lowx)

	  r_duy= r_latu(i_hiuy)- r_latu(i_louy)
	  r_dvy= r_latv(i_hivy)- r_latv(i_lovy)
	  r_dwy= r_latw(i_hiwy)- r_latw(i_lowy)

	  r_dz= r_z(i_hiz)- r_z(i_loz)
	  r_dwz= r_zw(i_hizz)- r_zw(i_lozz)

	  r_lux= r_coord(1) - r_lonu(i_loux)
	  r_lvx= r_coord(1) - r_lonv(i_lovx)
	  r_lwx= r_coord(1) - r_lonw(i_lowx)
	  r_luy= r_coord(2) - r_latu(i_louy)
	  r_lvy= r_coord(2) - r_latv(i_lovy)
	  r_lwy= r_coord(2) - r_latw(i_lowy)
	  r_lz= r_coord(3) - r_z(i_loz)
	  r_lzz= r_coord(3) - r_zw(i_lozz)

	  r_hux= r_lonu(i_hiux) - r_coord(1)
	  r_hvx= r_lonv(i_hivx) - r_coord(1)
	  r_hwx= r_lonw(i_hiwx) - r_coord(1)
	  r_huy= r_latu(i_hiuy) - r_coord(2)
	  r_hvy= r_latv(i_hivy) - r_coord(2)
	  r_hwy= r_latw(i_hiwy) - r_coord(2)
	  r_hz= r_z(i_hiz) - r_coord(3)
	  r_hzz= r_zw(i_hizz) - r_coord(3)

	  r_scalx= r_deg * 1./cos(r_coord(2)*PI/180.)  ! in degree/sec
	  r_scaly= r_deg
	  r_scalz= 1.  / 1000.                         ! km to m

!---------- linear interpolation in x-direction
! Interpolation in a Arakawa Staggered C grid
!
! Interpolate V in two XZ planes to final V point
! Interpolate U in two YZ planes to final U point
! Interpolate W in two XY planes to final W point
! The section below is split into three: 
		!r_xun represents the YZ plane first phase (technically
		!should be named r_yun but is left as is for ease of use
		!r_xvn represents the XZ plane first phase (correct
		!representation)
		!r_xwn represents the XY plane first phase (technically
		!should be named r_zwn but " ") 
	  IF(r_duy /= 0.0) THEN
	    r_dyui= 1./r_duy
	    r_xu1= (r_huy* r_flowx(i_loux, i_louy, i_loz, i_time)+ &
	            r_luy* r_flowx(i_loux, i_hiuy, i_loz, i_time))* r_dyui
	    r_xu2= (r_huy* r_flowx(i_hiux, i_louy, i_loz, i_time)+ &
			    r_luy* r_flowx(i_hiux, i_hiuy, i_loz, i_time))* r_dyui
	    r_xu3= (r_huy* r_flowx(i_loux, i_louy, i_hiz, i_time)+ &
	            r_luy* r_flowx(i_loux, i_hiuy, i_hiz, i_time))* r_dyui
        r_xu4= (r_huy* r_flowx(i_hiux, i_louy, i_hiz, i_time)+ &
		        r_luy* r_flowx(i_hiux, i_hiuy, i_hiz, i_time))* r_dyui				
	  ELSE
	    r_xu1= r_flowx(i_loux, i_louy, i_loz, i_time)
	    r_xu2= r_flowx(i_hiux, i_louy, i_loz, i_time)
	    r_xu3= r_flowx(i_loux, i_louy, i_hiz, i_time)
	    r_xu4= r_flowx(i_hiux, i_louy, i_hiz, i_time)
	  END IF
		
	  IF(r_dvx /= 0.0) THEN
	    r_dxvi= 1./r_dvx
	    r_xv1= (r_hvx* r_flowy(i_lovx, i_lovy, i_loz, i_time)+ &
	            r_lvx* r_flowy(i_hivx, i_lovy, i_loz, i_time))* r_dxvi
	    r_xv2= (r_hvx* r_flowy(i_lovx, i_hivy, i_loz, i_time)+ &
	            r_lvx* r_flowy(i_hivx, i_hivy, i_loz, i_time))* r_dxvi
	    r_xv3= (r_hvx* r_flowy(i_lovx, i_lovy, i_hiz, i_time)+ &
	            r_lvx* r_flowy(i_hivx, i_lovy, i_hiz, i_time))* r_dxvi
	    r_xv4= (r_hvx* r_flowy(i_lovx, i_hivy, i_hiz, i_time)+ &
	            r_lvx* r_flowy(i_hivx, i_hivy, i_hiz, i_time))* r_dxvi
	  ELSE
	    r_xv1= r_flowy(i_lovx, i_lovy, i_loz, i_time)
	    r_xv2= r_flowy(i_lovx, i_hivy, i_loz, i_time)
	    r_xv3= r_flowy(i_lovx, i_lovy, i_hiz, i_time)
	    r_xv4= r_flowy(i_lovx, i_hivy, i_hiz, i_time)
	  END IF

	  IF(r_dwz /= 0.0) THEN
	    r_dzwi= 1./r_dwz
	    r_xw1= (r_hzz* r_flowz(i_lowx, i_lowy, i_lozz, i_time)+ &
	            r_lzz* r_flowz(i_lowx, i_lowy, i_hizz, i_time))* r_dzwi
	    r_xw2= (r_hzz* r_flowz(i_hiwx, i_lowy, i_lozz, i_time)+ &
	            r_lzz* r_flowz(i_hiwx, i_lowy, i_hizz, i_time))* r_dzwi
	    r_xw3= (r_hzz* r_flowz(i_lowx, i_hiwy, i_lozz, i_time)+ &
	            r_lzz* r_flowz(i_lowx, i_hiwy, i_hizz, i_time))* r_dzwi
	    r_xw4= (r_hzz* r_flowz(i_hiwx, i_hiwy, i_lozz, i_time)+ &
                r_lzz* r_flowz(i_hiwx, i_hiwy, i_hizz, i_time))* r_dzwi
	  ELSE
	    r_xw1= r_flowz(i_lowx, i_lowy, i_lozz, i_time)
	    r_xw2= r_flowz(i_hiwx, i_lowy, i_lozz, i_time)
	    r_xw3= r_flowz(i_lowx, i_hiwy, i_hizz, i_time)
	    r_xw4= r_flowz(i_hiwx, i_hiwy, i_hizz, i_time)
	  END IF

!---------- linear interpolation in y-direction

	  IF(r_dux /= 0.0) THEN
	    r_dxui= 1./r_dux
	    r_yu1= (r_hux* r_xu1+ r_lux* r_xu2)* r_dxui
	    r_yu2= (r_hux* r_xu3+ r_lux* r_xu4)* r_dxui
      ELSE
	    r_yu1= r_xu1
		r_yu2= r_xu3
	  END IF
	  IF(r_dvy /= 0.0) THEN
	    r_dyvi= 1./r_dvy
	    r_yv1= (r_hvy* r_xv1+ r_lvy* r_xv2)* r_dyvi
	    r_yv2= (r_hvy* r_xv3+ r_lvy* r_xv4)* r_dyvi
	  ELSE
		r_yv1= r_xv1
	    r_yv2= r_xv3
	  END IF
	  IF(r_dvx /= 0.0) THEN
		r_dxvi= 1./r_dvx
		r_yw1= (r_hux* r_xw1+ r_lux* r_xw2)* r_dxvi
	    r_yw2= (r_hux* r_xw3+ r_lux* r_xw4)* r_dxvi
	  ELSE
	    r_yw1= r_xw1
	    r_yw2= r_xw3
	  END IF
!---------- linear interpolation in z-direction

	  IF(r_dz /= 0.0) THEN
	    r_dzi= 1./r_dz
	    r_inter(1)= (r_hz* r_yu1+ r_lz* r_yu2)* r_dzi * r_scalx
		r_inter(2)= (r_hz* r_yv1+ r_lz* r_yv2)* r_dzi * r_scaly
	  ELSE
		r_inter(1)= r_yu1 * r_scalx
		r_inter(2)= r_yv1 * r_scaly
      END IF
	  IF(r_duy /= 0.0) THEN
	    r_dyui= 1./r_duy
	    r_inter(3)= (r_huy* r_yw1+ r_luy* r_yw2)* r_dyui * r_scalz
	  ELSE
	    r_inter(3)= r_yw1 * r_scalz
	  END IF
	  

	  RETURN
	  END FUNCTION data_interpol

!*****************************************************************
	END MODULE ADV_wind
