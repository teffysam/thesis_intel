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
! NAME:
!	time_dependent_rhs
! FUNCTION:
!	calculate time dependent rhs
! SYNTAX:
!	real= time_dependent_rhs(real)
! ON INPUT:
!	r_time:  time coordinate	real
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
!	3. nonhomogeneous, time dependent rhs	e. gerwing	4/2015
!
!*****************************************************************
	MODULE ADV_rhs
	  USE FLASH_parameters
	  USE GRID_api
	  USE date_time
	  PRIVATE
	  PUBLIC :: slm_righthand
	  CONTAINS
!*****************************************************************
	  FUNCTION slm_righthand(p_param,r_coord, r_time) RESULT (r_rhs)

!---------- local declarations

	  IMPLICIT NONE

	  TYPE (control_struct), INTENT(in)           :: p_param
	  REAL, DIMENSION(GRID_dimension), INTENT(in) :: r_coord
	  REAL, INTENT(in), OPTIONAL                  :: r_time
	  REAL                                        :: r_rhs
	  REAL                                        :: r_tim
	  REAL                                        :: r_radius, r_dist, r_top, r_high
	  REAL, DIMENSION(GRID_dimension)             :: r_centr,r_vec

!---------- set time

	  IF(present(r_time)) THEN
	    r_tim= r_time
	  ELSE
	    r_tim= 0.0
	  END IF

!---------- initilize coordinates of emission source (cylinder)
	  r_centr(1) = 22.9999                      ! x-Position [degree]
	  r_centr(2) = -40.4999                            ! y-Position [degree]
	  r_centr(3) = 0.001                             ! bottom of ash column (above sealevel) [km]
	  r_top = 0.003                                  ! top of ash column (above sealevel) [km]
	  r_radius = 6.003                        ! Radius [degree]

	  r_vec = r_coord -r_centr                      ! Vector between center and coordinate
	  r_dist = sqrt(dot_product(r_vec(1:2),r_vec(1:2)))      ! Distance between center and (x,y) [degree]

	  r_high = r_top - r_centr(3)                   ! ash cloud thickness

!---------- calculate the advection at (x,y) (velocity increasing)

	  IF (r_dist <= r_radius .AND. r_coord(3) >= r_centr(3) .AND. r_coord(3) <= r_top) THEN
	    r_rhs = time_dependent_rhs(p_param,r_tim,r_radius,r_high)
		! r_rhs = 1e7
	  ELSE
	    r_rhs= 0.0
	  END IF

	  RETURN
	  END FUNCTION slm_righthand

!*****************************************************************
	  FUNCTION time_dependent_rhs(p_param,r_time,r_radius,r_high) RESULT (r_rhs)

!---------- local declarations

	  IMPLICIT NONE

	  TYPE (control_struct), INTENT(in)           :: p_param
	  REAL, INTENT(in)                            :: r_time
	  REAL                                        :: r_rhs
	  REAL                                        :: r_radius, r_high 
	  REAL                                        :: r_mass1, r_mass2, r_mass3, r_mass4, r_mass5, r_mass6, r_fac
	  TYPE (date)                                 :: i_start, i_date1, i_date2, i_date3, i_date4, i_date5, i_date6, i_date7, i_date8
	  INTEGER                                     :: i_time1, i_time2, i_time3, i_time4, i_time5, i_time6, i_time7, i_time8

!---------- Enter times of change in eruption phase [year,month,day,hour,minute,second]

	  i_start = date(1991,6,15,6,0,0)                      ! Start of calculation

	  i_date1 = date(1991,6,15,8,10,0)
	  i_date2 = date(1991,6,15,9,10,0)
	  i_date3 = date(1991,6,15,10,27,0)
	  i_date4 = date(1991,6,15,11,27,0)
	  i_date5 = date(1991,6,15,13,41,0)
	  i_date6 = date(1991,6,15,22,41,0)
	  i_date7 = date(1991,6,16,10,41,0)

!---------- Mass discharge rate for the different eruption phases [kg/s]

	  r_fac = 1.0                ! weight percentage of this particle class 

	  r_mass1 = 3.75e+07 * r_fac
	  r_mass2 = 5.85e+07 * r_fac
	  r_mass3 = 2.1e+08 * r_fac
	  r_mass4 = 1.5e+07 * r_fac

!---------- Calculate times in seconds since beginning (first entery: start of calculation!)

	  i_time1 = deltatime_in_seconds(i_start,i_date1)
	  i_time2 = deltatime_in_seconds(i_start,i_date2)
	  i_time3 = deltatime_in_seconds(i_start,i_date3)
	  i_time4 = deltatime_in_seconds(i_start,i_date4)
	  i_time5 = deltatime_in_seconds(i_start,i_date5)
	  i_time6 = deltatime_in_seconds(i_start,i_date6)
	  i_time7 = deltatime_in_seconds(i_start,i_date7)
!---------- Get RHS
	  IF (r_time >= i_time1 .AND. r_time <= i_time3) THEN
	    r_rhs = mass_discharge(p_param,r_mass1,r_radius,r_high)                                   
	  ELSE IF (r_time >= i_time3 .AND. r_time < i_time5) THEN
	    r_rhs = mass_discharge(p_param,r_mass2,r_radius,r_high) 
	  ELSE IF (r_time >= i_time6 .AND. r_time < i_time7) THEN
	    r_rhs = mass_discharge(p_param,r_mass4,r_radius,r_high)
	  ELSE IF (r_time >= i_time5 .AND. r_time <= i_time6) THEN
	    r_rhs = mass_discharge(p_param,r_mass3,r_radius,r_high)
	  ELSE
	    r_rhs = 0.0
	  END IF


	  END FUNCTION time_dependent_rhs
!*****************************************************************
	  FUNCTION mass_discharge(p_param,r_mass, r_radius, r_high) RESULT (r_rhs)

	  IMPLICIT NONE

	  TYPE (control_struct)                       :: p_param
	  REAL, INTENT(in)                            :: r_mass, r_radius, r_high
	  REAL                                        :: r_rhs
	  REAL                                        :: r_volume, r_dt, r_deg
	  REAL, PARAMETER                             :: PI=3.1415927
	  REAL, PARAMETER                             :: r_earth=40075000. ! earth circumference

!---------- factor to convert from degree to metre
	  r_deg= r_earth/360.0

!---------- time step
	  r_dt = p_param%phy%r_deltatime

!---------- Volume of initial ash plume [m^3]
	  r_volume = PI * (r_radius*r_deg)**2 * (r_high*1000)

!---------- Mass discharge per time step [kg/m^3]
	  r_rhs = r_mass / r_volume * r_dt


	  END FUNCTION mass_discharge
!*****************************************************************
	END MODULE ADV_rhs
