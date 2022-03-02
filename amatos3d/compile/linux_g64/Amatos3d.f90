!*****************************************************************
!
! NAME:
!	partest
! FUNCTION:
!	test the grid generation routines used in FLASH
! SYNTAX:
!	gridtest ...
! ON INPUT:
!	
! ON OUTPUT:
!
! CALLS:
!
! COMMENTS:
!
! LIBRARIES:
!
! REFERENCES:
!
! VERSION(S):
!	1. original version			j. behrens	8/97
!	2. now with new grid generator		j. behrens	5/98
!	3. amatos-1.0 compliant			j. behrens	11/2000
!
!*****************************************************************
	PROGRAM gridtest

!---------- uses

	USE MAIN_parameters
	USE MISC_functions
	USE IO_utils
	USE IO_matlabplot
	USE IO_gmvplot
	USE GRID_api
	USE ERROR_est
	USE MISC_poisson

!---------- local declarations

	IMPLICIT NONE

	INTEGER (KIND = GRID_SI)                                   :: i_tmp, i_err, i_cnt, i_alct, i_nn
	INTEGER (KIND = GRID_SI)                                   :: i_elmlist, i_nodlist
	INTEGER (KIND = GRID_SI), PARAMETER                        :: i_vertnum=4
	INTEGER (KIND = GRID_SI), PARAMETER                        :: i_redirout=8
	INTEGER (KIND = GRID_SI), PARAMETER                        :: i_redirlog=7
	REAL (KIND = GRID_SR), DIMENSION(GRID_dimension,i_vertnum) :: r_vertinit
	INTEGER (KIND = GRID_SI), DIMENSION(:), ALLOCATABLE        :: i_mark
	REAL (KIND = GRID_SR), DIMENSION(:,:,:), ALLOCATABLE       :: r_nodes
	REAL (KIND = GRID_SR), DIMENSION(:,:), ALLOCATABLE         :: r_coords
	REAL (KIND = GRID_SR), DIMENSION(:), ALLOCATABLE           :: r_values
	REAL (KIND = GRID_SR), DIMENSION(GRID_dimension)           :: r_xy
	REAL (KIND = GRID_SR)                                      :: r_third=1./3.
	LOGICAL                                   :: l_grd
	CHARACTER (len=32)                        :: c_mfile
	CHARACTER (len=28)                        :: c_tmp
	CHARACTER                                 :: c_dummy
	INTEGER (KIND = GRID_SI)                                   :: i_fst, i_iomatl=15
	REAL (KIND = GRID_SR)                                      :: r_rad=0.15, r_rfc=0.6
	REAL (KIND = GRID_SR)                                      :: r_long, r_short
	INTEGER (KIND = GRID_SI)                                   :: i_ioerr=0
	INTEGER (KIND = GRID_SI), DIMENSION(:,:,:), POINTER        :: i_dfaces
	REAL (KIND = GRID_SR), DIMENSION(:,:), POINTER             :: r_dcoord

!---------- read command line options

	CALL io_getcmdline(p_contr)

!---------- initialize grid generator

	IF(p_contr%cmd%l_output) THEN
	  IF(p_contr%cmd%l_logging) THEN
	    CALL grid_initialize(i_output=i_redirout, i_logging=i_redirlog)
	  END IF
	  CALL grid_initialize(i_output=i_redirout)
	ELSE
	  IF(p_contr%cmd%l_logging) THEN
	    CALL grid_initialize(i_logging=i_redirlog)
	  END IF
	  CALL grid_initialize
	END IF

!---------- read user input

	interactive_input: IF(p_contr%cmd%l_interactive) THEN
	  CALL io_getinterinput(p_contr)
	ELSE interactive_input
	  CALL io_getbatchinput(p_contr)
	END IF interactive_input

!---------- initialize grid parameters

	CALL grid_setparameter(p_grid, i_coarselevel= p_contr%phy%i_crslevel, &
	                               i_finelevel= p_contr%phy%i_reflevel)

!---------- define domain

	r_vertinit= RESHAPE((/ 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 1.0, 0.0 /),(/  3, 4 /))
	CALL grid_definegeometry(i_vertnum, r_vertexarr= r_vertinit)

!---------- create initial triangulation

	CALL grid_createinitial(p_grid, c_filename = p_contr%io%c_triangfile)

!---------- print gridding parameters

	CALL io_putparameters(p_contr)

!---------- set values to the node's r_vals field
	CALL poisson_test(p_grid(i_timeplus), 0)
	CALL plot_gmv(p_grid(i_timeplus), i_time=9999)
	STOP
	i_nn= p_grid(i_timeplus)%i_nnumber
	ALLOCATE(r_coords(GRID_dimension,i_nn), stat=i_alct)
	IF(i_alct /= 0) THEN
	  WRITE(i_ioerr,*) 'ERROR: allocation failed'
	  STOP 1
	END IF
	CALL grid_getinfo(p_grid(i_timeplus), i_nn, r_nodecoordinates=r_coords)
	ALLOCATE(r_values(i_nn), stat=i_alct)
	IF(i_alct /= 0) THEN
	  WRITE(i_ioerr,*) 'ERROR: allocation failed'
	  STOP 1
	END IF
	DO i_cnt=1,i_nn
	  r_values(i_cnt)= f5(r_coords(:,i_cnt))
	END DO
	CALL grid_putinfo(p_grid(i_timeplus), i_nn, i_valpoint= GRID_tracer, r_nodevalues=r_values)
	DO i_cnt=1,i_nn
	  r_values(i_cnt)= f2(r_coords(:,i_cnt))
	END DO
	CALL grid_putinfo(p_grid(i_timeplus), i_nn, i_valpoint= GRID_ucomp, r_nodevalues=r_values)
	DO i_cnt=1,i_nn
	  r_values(i_cnt)= f3(r_coords(:,i_cnt))
	END DO
	CALL grid_putinfo(p_grid(i_timeplus), i_nn, i_valpoint= GRID_vcomp, r_nodevalues=r_values)
	DO i_cnt=1,i_nn
	  r_values(i_cnt)= f4(r_coords(:,i_cnt))
	END DO
	CALL grid_putinfo(p_grid(i_timeplus), i_nn, i_valpoint= GRID_phi, r_nodevalues=r_values)
	DO i_cnt=1,i_nn
	  r_values(i_cnt)= f1(r_coords(:,i_cnt))
	END DO
	CALL grid_putinfo(p_grid(i_timeplus), i_nn, i_valpoint= GRID_zeta, r_nodevalues=r_values)
	DEALLOCATE(r_coords,r_values)

!---------- plot grid (matlab compatible)

	matl_plot: IF(p_contr%io%l_matlab) THEN
	  CALL plot_matlab(p_grid(i_timeplus), i_time=p_grid(i_timeplus)%i_maxlvl, &
	                   c_action='levl')
	END IF matl_plot
	gmv1_plot: IF(p_contr%io%l_gmv) THEN
	  CALL plot_gmv(p_grid(i_timeplus), i_time=p_grid(i_timeplus)%i_maxlvl)
	END IF gmv1_plot

!---------- adaptation loop

 	adapt_loop: DO WHILE(p_grid(i_timeplus)%i_maxlvl < p_grid(i_timeplus)%i_reflvlbnd)
 
!---------- mark some elements for refinement

	CALL error_estimate( p_grid(i_timeplus) )

!---------- adapt grid
 
 	CALL grid_adapt(p_grid(i_timeplus), l_grd)

!---------- set values to the node's r_vals field

	i_nn= p_grid(i_timeplus)%i_nnumber
	ALLOCATE(r_coords(GRID_dimension,i_nn), stat=i_alct)
	IF(i_alct /= 0) THEN
	  WRITE(i_ioerr,*) 'ERROR: allocation failed'
	  STOP 1
	END IF
	CALL grid_getinfo(p_grid(i_timeplus), i_nn, r_nodecoordinates=r_coords)
	ALLOCATE(r_values(i_nn), stat=i_alct)
	IF(i_alct /= 0) THEN
	  WRITE(i_ioerr,*) 'ERROR: allocation failed'
	  STOP 1
	END IF
	DO i_cnt=1,i_nn
	  r_values(i_cnt)= f5(r_coords(:,i_cnt))
	END DO
	CALL grid_putinfo(p_grid(i_timeplus), i_nn, i_valpoint= GRID_tracer, r_nodevalues=r_values)
	DEALLOCATE(r_coords,r_values)

!---------- plot grid (matlab compatible)
 
 	mat_plot: IF(p_contr%io%l_matlab) THEN
 	  CALL plot_matlab(p_grid(i_timeplus), i_time=p_grid(i_timeplus)%i_maxlvl, &
 	                   c_action='levl')
 	END IF mat_plot
 	gmv2_plot: IF(p_contr%io%l_gmv) THEN
	  CALL plot_gmv(p_grid(i_timeplus), i_time=p_grid(i_timeplus)%i_maxlvl)
 	END IF gmv2_plot
 
 	END DO adapt_loop

!---------- get edge lenght

	CALL io_putruntimeinfo(p_grid(i_timeplus))

	CALL grid_edgelength(p_grid(i_timeplus), r_max=r_long, r_min=r_short)
	WRITE(*,2020)
	WRITE(*,2021) r_long
	WRITE(*,2022) r_short
	WRITE(*,2020)

!---------- coarsen again

	crs_loop: DO i_cnt=1,3
 
!---------- mark some elements for refinement

	CALL error_coarse( p_grid(i_timeplus) )

!---------- adapt grid
 
 	CALL grid_adapt(p_grid(i_timeplus), l_grd)

!---------- plot grid (matlab compatible)
 
 	mat3_plot: IF(p_contr%io%l_matlab) THEN
 	  CALL plot_matlab(p_grid(i_timeplus), i_time=p_grid(i_timeplus)%i_reflvlbnd+i_cnt, &
 	                   c_action='levl')
 	END IF mat3_plot
 	gmv3_plot: IF(p_contr%io%l_gmv) THEN
	  CALL plot_gmv(p_grid(i_timeplus),  i_time=p_grid(i_timeplus)%i_reflvlbnd+i_cnt)
 	END IF gmv3_plot

	END DO crs_loop

!---------- get edge lenght

	CALL io_putruntimeinfo(p_grid(i_timeplus))

	CALL grid_edgelength(p_grid(i_timeplus), r_max=r_long, r_min=r_short)
	WRITE(*,2020)
	WRITE(*,2021) r_long
	WRITE(*,2022) r_short
	WRITE(*,2020)

!---------- get dual mesh

	CALL grid_createdual(p_grid(i_timeplus), p_grid(i_timeplus)%i_nnumber, &
	                     i_cnt, i_dfaces, r_dcoord)
	WRITE(*,2030) i_cnt, size(i_dfaces,DIM=2)
	WRITE(*,2020)
	CALL grid_destroydual(i_cnt, i_dfaces, r_dcoord)

!---------- write a save set

	write(c_tmp,*) trim(GRID_parameters%program_name(1:22)), '_save.'
	write(c_mfile,2010) trim(c_tmp), p_grid(i_timeplus)%i_maxlvl
	c_mfile= adjustl(c_mfile)
	CALL grid_writesaveset(c_mfile,p_grid)

!---------- finish grid generator

	CALL grid_terminate

 2000	FORMAT(a)
 2010	FORMAT(a28,i4.4)
 2020	FORMAT(1x,'***** ***** ***** ***** ***** ***** ***** ***** ***** *****')
 2021	FORMAT(1x,'***** Longest edge in triangulation: ',e16.5,' *****')
 2022	FORMAT(1x,'***** Shortest edge in triangulation:',e16.5,' *****')
 2030	FORMAT(1x,'***** Dual nodes:',i8,'     Dual face max.:',i8,' *****')
 2040	FORMAT(1x,'***** Dual nodes:',i8,'     Dual face max.:',i8,' *****')

	END PROGRAM gridtest
