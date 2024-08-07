!*****************************************************************
!
! MODULE NAME:
!	SLM_advanced
! FUNCTION:
!	provide simple semi-Lagrangian routines
! CONTAINS:
!-----------------------------------------------------------------
!
! NAME:
!	slm_astep
! FUNCTION:
!	one step of the basic SLM algorithm
! SYNTAX:
!	CALL slm_astep(int, real.arr, real.arr)
! ON INPUT:
!	...
! ON OUTPUT:
!	r_tracer: array with tracer values	real
! CALLS:
!
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!	slm_adisplace
! FUNCTION:
!	extrapolate the alpha, values for the displacements of the upstream
!	points from the gridpoints
! SYNTAX:
!	CALL slm_adisplace(int, real.arr, real.arr)
! ON INPUT:
!	i_arlen: array length for the real arrays	integer
!	r_coord: real array of xy-coordinates		real
! ON OUTPUT:
!	r_alpha: displacement vectors to each point	real
! CALLS:
!	wind_field
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!	slm_aupdate
! FUNCTION:
!	calculate the update to the velocity
! SYNTAX:
!	CALL slm_aupdate(int, real.arr, real.arr)
! ON INPUT:
!	i_arlen: array length for the real arrays	integer
!	r_rside: array with right hand side values	real
! ON OUTPUT:
!	r_udate: array with new (updated) gid values	real
! CALLS:
!
! COMMENTS:
!	this routine is trivial for linear advection
!
!-----------------------------------------------------------------
!
! NAME:
!	slm_aupstream
! FUNCTION:
!	calculate right hand side of the equation (upstream values)
! SYNTAX:
!	CALL slm_aupstream(int, real.arr, real.arr)
! ON INPUT:
!	i_arlen: array length for the real arrays	integer
!	r_alpha: displacement vectors to each point	real
! ON OUTPUT:
!	r_rside: array with right hand side values	real
! CALLS:
!
! COMMENTS:
!	this routine is just interpolation for linear advection
!
!-----------------------------------------------------------------
!
! NAME:
!	slm_ainterpolate
! FUNCTION:
!	do the interpolation
! SYNTAX:
!	CALL slm_ainterpolate(grid, int, real, real.arr, real.arr, real.arr)
! ON INPUT:
!	p_ogrid: grid handle to old grid (with data)	TYPE (grid_handle)
!	r_fac:   factor at which point to interpolate	REAL
!	i_arlen: array length for the following arrays	INTEGER
!	r_coord: coordinate array (new grid)		REAL
!	r_alpha: displacement array (corr. to r_coord)	REAL
!	r_value: values on the old grid (array)		REAL
! ON OUTPUT:
!	r_rside: right hand side (interpolated)		REAL
! CALLS:
!
! COMMENTS:
!	this one is plain bi-cubic spline interpolation
!
!-----------------------------------------------------------------
!
! PUBLIC:
!	slm_astep
! COMMENTS:
!
! USES:
!	FLASH_parameters, GRID_api, ADV_wind, ADV_rhs
! LIBRARIES:
!
! REFERENCES:
!
! VERSION(S):
!	1. original version		j. behrens	12/2002
!
!*****************************************************************
	MODULE SLM_advanced
	  USE FLASH_parameters
	  USE MISC_timing
	  USE GRID_api
	  USE ADV_wind
	  USE ADV_rhs
	  PRIVATE
	  PUBLIC  :: slm_astep
	  CONTAINS
!*****************************************************************
	  SUBROUTINE slm_astep(p_ghand, p_param, p_time, r_modtime, i_size, &
	                       r_coord,r_tracer)

!---------- local declarations

	  IMPLICIT NONE

	  TYPE (grid_handle), DIMENSION(GRID_timesteps), INTENT(in) :: p_ghand
	  TYPE (control_struct), INTENT(in)                         :: p_param
	  TYPE (sw_info), INTENT(inout)                             :: p_time
	  REAL, INTENT(in)                                          :: r_modtime
	  INTEGER, INTENT(in)                                       :: i_size
	  REAL, DIMENSION(GRID_dimension,i_size), INTENT(in)        :: r_coord
	  REAL, DIMENSION(i_size), INTENT(out)                      :: r_tracer
	  
	  REAL, DIMENSION(:), ALLOCATABLE                           :: r_newvl
	  REAL, DIMENSION(:,:), ALLOCATABLE                         :: r_alpha
	  INTEGER                                                   :: i_alct

!---------- check size!

	  IF(i_size <= 0) THEN
	    IF(GRID_parameters%iolog > 0) &
	      write(GRID_parameters%iolog,*) 'INFO [slm_astep]: Zero step size, returning to calling routine'
	    RETURN
	  END IF

!---------- allocate auxiliary arrays

	  allocate(r_newvl(i_size), r_alpha(GRID_dimension,i_size), stat=i_alct)
	  not_alloc: IF(i_alct /= 0) THEN
	    CALL grid_error(c_error='[slm_astep]: could not allocate aux. arrays')
	  END IF not_alloc

!-SLM--------- calculate trajectory pieces (displacements)

	  CALL stop_watch('start',3,p_time)
	  CALL slm_adisplace(p_param, i_size, r_coord, r_alpha, r_time=r_modtime)
	  CALL stop_watch('stop ',3,p_time)

!-SLM--------- calculate right hand side

	  CALL stop_watch('start',4,p_time)
	  CALL slm_aupstream(p_ghand, i_size, r_coord, r_alpha, r_newvl)
	  CALL stop_watch('stop ',4,p_time)

!-SLM--------- calculate new grid values

	  CALL stop_watch('start',5,p_time)
	  CALL slm_aupdate(p_param, i_size, r_coord, r_newvl, r_tracer, r_time=r_modtime)
	  CALL stop_watch('stop ',5,p_time)

!-SLM--------- put alpha values to u and v field entries

	  r_alpha= -r_alpha
	  CALL grid_putinfo(p_ghand(i_timeplus), i_size, r_nodevalues=r_alpha(1,:), &
	                    i_valpoint=GRID_ucomp)
	  CALL grid_putinfo(p_ghand(i_timeplus), i_size, r_nodevalues=r_alpha(2,:), &
	                    i_valpoint=GRID_vcomp)
	  CALL grid_putinfo(p_ghand(i_timeplus), i_size, r_nodevalues=r_alpha(3,:), &
	                    i_valpoint=GRID_wcomp)

!-SLM--------- deallocate work arrays

	  deallocate(r_alpha, r_newvl)

	  RETURN
	  END SUBROUTINE slm_astep
!*****************************************************************
	  SUBROUTINE slm_adisplace(p_param, i_arlen, r_coord, r_alpha, r_time)

!---------- local declarations

	  IMPLICIT NONE

	  TYPE (control_struct), INTENT(in)                      :: p_param
	  INTEGER, INTENT(in)                                  :: i_arlen
	  REAL, DIMENSION(GRID_dimension,i_arlen), INTENT(in)  :: r_coord
	  REAL, DIMENSION(GRID_dimension,i_arlen), INTENT(out) :: r_alpha
	  REAL, INTENT(in), OPTIONAL                           :: r_time
	  REAL, DIMENSION(GRID_dimension)                      :: r_fac, r_caf, &
	    r_axy, r_xyc
	  REAL                                                 :: r_dt0, r_dt1, &
	    r_dt2, r_tim
	  INTEGER                                              :: i_cnt1, i_cnt2
          
!---------- set constants

	  r_dt0= p_param%phy%r_deltatime
	  r_dt1= 0.5* p_param%phy%r_deltatime
	  r_dt2= 1.5* p_param%phy%r_deltatime
	  r_fac= 0.5
	  r_caf= 2.0
	  IF(present(r_time)) THEN
	    r_tim= r_time
	  ELSE
	    r_tim= 0.0
	  END IF

!---------- calculate in an iteration process the displacements

	  unknown_loop: DO i_cnt1=1,i_arlen
	    r_axy= 0.0

	    iter_loop: DO i_cnt2=1, p_param%phy%i_adviterations
	      r_xyc= r_coord(:,i_cnt1)- r_fac* r_axy
	      r_axy= r_dt0* slm_windfield(r_xyc, r_time=r_tim)
	    END DO iter_loop

	    r_alpha(:,i_cnt1)= r_axy
	  END DO unknown_loop

	  RETURN
	  END SUBROUTINE slm_adisplace

!*****************************************************************
	  SUBROUTINE slm_aupdate(p_param, i_arlen, r_coord, r_rside, r_udate, r_time)

!---------- local declarations

	  IMPLICIT NONE

	  TYPE (control_struct), INTENT(in)                   :: p_param
	  INTEGER, INTENT(in)                                 :: i_arlen
	  REAL, DIMENSION(GRID_dimension,i_arlen), INTENT(in) :: r_coord
	  REAL, DIMENSION(i_arlen), INTENT(in)                :: r_rside
	  REAL, DIMENSION(i_arlen), INTENT(out)               :: r_udate
	  REAL, INTENT(in), OPTIONAL                          :: r_time
	  INTEGER                                             :: i_cnt
	  REAL                                                :: r_dt, r_tim

!---------- in the linear advection case and with f90 this is just

!	  r_udate= r_rside

!---------- including a non-zero right hand side, we have

	  r_dt= p_param%phy%r_deltatime
	  IF(present(r_time)) THEN
	    r_tim= r_time
	  ELSE
	    r_tim= 0.0
	  END IF

	  main_loop: DO i_cnt=1, i_arlen
	    r_udate(i_cnt)= r_rside(i_cnt)+ r_dt* slm_righthand(p_param,r_coord(:,i_cnt),r_tim)
	  END DO main_loop

	  RETURN
	  END SUBROUTINE slm_aupdate

!*****************************************************************
	  SUBROUTINE slm_aupstream(p_mesh, i_arlen, r_coord, &
	                          r_alpha, r_rside)

!---------- local declarations

	  IMPLICIT NONE

	  TYPE (grid_handle), DIMENSION(GRID_timesteps)       :: p_mesh
	  INTEGER, INTENT(in)                                 :: i_arlen
	  REAL, DIMENSION(GRID_dimension,i_arlen), INTENT(in) :: r_coord
	  REAL, DIMENSION(GRID_dimension,i_arlen), INTENT(in) :: r_alpha
	  REAL, DIMENSION(i_arlen), INTENT(out)               :: r_rside
	  REAL, DIMENSION(GRID_dimension)                     :: r_fac

!---------- set factor (at which point of trajectory shall i interpolate)

	  r_fac= 1.0

!---------- in the linear advection case this is just interpolation

	  CALL slm_ainterpolate(p_mesh, r_fac, i_arlen, r_coord, &
	                       r_alpha, r_rside)

	  RETURN
	  END SUBROUTINE slm_aupstream

!*****************************************************************
	  SUBROUTINE slm_ainterpolate(p_mesh, r_fac, i_arlen, &
	                             r_coord, r_alpha, r_rside)

!---------- local declarations

	  IMPLICIT NONE

	  TYPE (grid_handle), DIMENSION(GRID_timesteps)       :: p_mesh
	  REAL, DIMENSION(GRID_dimension), INTENT(in)         :: r_fac
	  INTEGER, INTENT(in)                                 :: i_arlen
	  REAL, DIMENSION(GRID_dimension,i_arlen), INTENT(in) :: r_coord
	  REAL, DIMENSION(GRID_dimension,i_arlen), INTENT(in) :: r_alpha
	  REAL, DIMENSION(i_arlen), INTENT(out)               :: r_rside
	  REAL, DIMENSION(:,:), ALLOCATABLE                   :: r_upstr
	  REAL                                                :: r_eps
	  INTEGER                                             :: i_cnt, i_alct, &
	    i_val, i_out, i_stat

!---------- initialize constant

	  i_val= GRID_tracer
	  r_eps= GRID_EPS

!---------- allocate work array

	  ALLOCATE(r_upstr(GRID_dimension,i_arlen), stat=i_alct)
	  not_allocated: IF(i_alct /= 0) THEN
	    CALL grid_error(60)
	  END IF not_allocated

!---------- calculate upstream coordinates

	  dim_loop: DO i_cnt=1, GRID_dimension
	    r_upstr(i_cnt,:) = r_coord(i_cnt,:)- r_fac(i_cnt)* r_alpha(i_cnt,:)
	  END DO dim_loop

!---------- loop over nodes: find element containing upstream point

	  node_loop: DO i_cnt=1, i_arlen

!---------- check if upstream value is outside of the domain

	    i_out= grid_domaincheck(p_mesh(i_time), r_upstr(:,i_cnt))

!---------- take the intersection of the trajectory with the boundary as new upstream point

	    out_domain: IF(i_out /= 0) then
	      r_upstr(:,i_cnt)= grid_boundintersect(p_mesh(i_time), &
	                        r_coord(:,i_cnt), r_upstr(:,i_cnt), i_info=i_stat)
	      no_intersect: IF(i_stat /= 0) THEN
	        r_rside(i_cnt)= 0.0
	        CYCLE node_loop
	      END IF no_intersect
	    END IF out_domain

!---------- interpolate

	    r_rside(i_cnt)= grid_coordvalue(p_mesh(i_time), r_upstr(:,i_cnt), &
	                    i_interpolorder=GRID_loworder, i_valpoint=i_val)
	    small_val: IF(abs(r_rside(i_cnt)) < r_eps) THEN
	      r_rside(i_cnt)= 0.0
	    END IF small_val

	  END DO node_loop

!---------- deallocate work array

	  DEALLOCATE(r_upstr)

	  RETURN
	  END SUBROUTINE slm_ainterpolate

	END MODULE SLM_advanced
