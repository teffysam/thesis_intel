!*****************************************************************
!
! MODULE NAME:
!	SLM_initial
! FUNCTION:
!	initialize slotted cylinder for semi-Lagrangian advection
! CONTAINS:
!-----------------------------------------------------------------
!
! NAME:
!	slm_initialvalues
! FUNCTION:
!	initialize a grid with values
! SYNTAX:
!	CALL slm_initialvalues(grid)
! ON INPUT:
!	p_ghand: grid handle			TYPE (grid_handle)
! ON OUTPUT:
!	p_ghand: grid handle			TYPE (grid_handle)
! CALLS:
!
! COMMENTS:
!	the routine is made for two dimensions only
!
!-----------------------------------------------------------------
!
! NAME:
!	slm_initslot
! FUNCTION:
!	initialize a grid with values of slotted cylinder test case
! SYNTAX:
!	CALL slm_initslot(grid)
! ON INPUT:
!	p_ghand: grid handle			TYPE (grid_handle)
! ON OUTPUT:
!	p_ghand: grid handle			TYPE (grid_handle)
! CALLS:
!
! COMMENTS:
!	the routine is made for two dimensions only
!
!-----------------------------------------------------------------
!
! NAME:
!	slm_analyticsolution
! FUNCTION:
!	calculates the 'analytic solution' to compare with in diagnostics
! SYNTAX:
!	CALL slm_analyticsolution(grid, real, int, real.arr)
! ON INPUT:
!	p_ghand: grid handle			TYPE (grid_handle)
!	r_time:  model time			REAL
!	i_arlen: array length for values array	INTEGER
! ON OUTPUT:
!	r_array: values at gridpoints		REAL
! CALLS:
!
! COMMENTS:
!	the routine is made for two dimensions only
!
!-----------------------------------------------------------------
!
! NAME:
!	slm_initcylndr
! FUNCTION:
!	initialize a grid with values of a cylinder test case
! SYNTAX:
!	CALL slm_initcylndr(grid)
! ON INPUT:
!	p_ghand: grid handle			TYPE (grid_handle)
!	r_centr: coordinates of cyl. centre	REAL
! ON OUTPUT:
!	p_ghand: grid handle			TYPE (grid_handle)
! CALLS:
!
! COMMENTS:
!	the routine is made for two dimensions only
!
!-----------------------------------------------------------------
!
! NAME:
!	in_side
! FUNCTION:
!	checks, if a given point (x,y) lies within a given triangle
! SYNTAX:
!	logical= in_side(real.arr, real.arr, real.arr, real.arr)
! ON INPUT:
!	r_coord: coordinate array		REAL
!	r_node1: node1 of triangle		REAL
!	r_node2: node2 of triangle		REAL
!	r_node3: node3 of triangle		REAL
! ON OUTPUT:
!	l_in:    .true. if r_coord \in p_elem	logical
! CALLS:
!
! COMMENTS:
!	this routine decides whether a point lies in or out of a
!	triangle. this is done with the following approach:
!	calculate the area for the given triangle and for the 
!	three triangles resulting from drawing lines from the given
!	point to all three triangle nodes.
!	if the sum of the areas of those three trianlges exceeds
!	the area of the given trianlge, the the point lies outside
!	of it.
!	for calculation of the areas following formula is used:
!	(Herons formula(?))
!
!	A = sqrt(s* (s- a)* (s- b)* (s- c)),
!
!	where s= 1/2* (a+ b+ c)
!	      a, b, c sides.
!
!	in order to calculate the sidelengths |x-y|= sqrt(x**2-y**2)
!	the complex absolute value intrinsic function is used.
!	hopefully this fuction is faster than using sqrt and
!	power-of-two instead.
!
!	internally double precision is used in this function, because
!	machine precision is crucial when a coordinate pair appears
!	near the edge or corner of an element.
!
!-----------------------------------------------------------------
!
! NAME:
!	dc_area
! FUNCTION:
!	calculate area of a triangle (in a plain) in double precision
! SYNTAX:
!	real= dc_area(real.arr, real.arr, real.arr)
! ON INPUT:
!	r_coord1: node1 of triangle			REAL
!	r_coord2: node2 of triangle			REAL
!	r_coord3: node3 of triangle			REAL
! ON OUTPUT:
!	r_area:   area of triangle			REAL
! CALLS:
!
! COMMENTS:
!
!-----------------------------------------------------------------
!
! PUBLIC:
!	slm_initialvalues, slm_analyticsolution
! COMMENTS:
!
! USES:
!	MISC_globalparam, MISC_error, GRID_api
! LIBRARIES:
!
! REFERENCES:
!
! VERSION(S):
!	1. original version		j. behrens	7/97
!	2. names changed		j. behrens	7/97
!	3. changed to use GRID_api	j. behrens	11/97
!	4. changed interfaces		j. behrens	12/97
!	5. compliant to amatos 1.0	j. behrens	12/2000
!
!*****************************************************************
	MODULE SLM_initial
	  USE FLASH_parameters
	  USE GRID_api
	  PRIVATE
	  PUBLIC slm_initialvalues, slm_analyticsolution
	  REAL, DIMENSION(GRID_dimension) :: r_cntr=(/ 0.25, 0.5, 0.5 /)
	  REAL                            :: r_hgt=1.0
	  REAL                            :: r_srd=0.20
	  CONTAINS
!*****************************************************************
	  SUBROUTINE slm_initialvalues(p_ghand)

!---------- local declarations

	  IMPLICIT NONE

	  TYPE (grid_handle), INTENT(in)             :: p_ghand
	  INTEGER                                    :: i_lev= 6

!---------- initialize some constant for the slotted cylinder

	  CALL slm_initball(p_ghand)

	  RETURN
	  END SUBROUTINE slm_initialvalues
!*****************************************************************
	  SUBROUTINE slm_analyticsolution(p_ghand, r_time, i_arlen, r_array)

!---------- local declarations

	  IMPLICIT NONE

	  TYPE (grid_handle), INTENT(in)              :: p_ghand
	  REAL, INTENT(in)                            :: r_time
	  INTEGER, INTENT(in)                         :: i_arlen
	  REAL, DIMENSION(i_arlen), INTENT(out)       :: r_array

!---------- this is a dummy
!	  r_array= 0.0
	  
!---------- analyticsolution: ball
	  CALL slm_analyticsolution_BALL(p_ghand, r_time, i_arlen, r_array)



	  RETURN
	  END SUBROUTINE slm_analyticsolution
	  
!*****************************************************************
	  SUBROUTINE slm_analyticsolution_BALL(p_ghand, r_time, i_arlen, r_array)

!---------- local declarations

	  IMPLICIT NONE

	  TYPE (grid_handle), INTENT(in)              :: p_ghand
	  REAL, DIMENSION(GRID_dimension)             :: r_centr
	  REAL                                        :: r_rds, r_dpt
	  REAL					      :: r_rds_kreisbahn
	  REAL, DIMENSION(GRID_dimension)             :: r_tmp
	  REAL, INTENT(in)                            :: r_time
	  REAL					      :: r_tim, r_time_one_turn
	  REAL					      :: r_PI, r_phi	
	  INTEGER, INTENT(in)                         :: i_arlen
	  INTEGER                                     :: i_count, i_num, i_alct
	  REAL, DIMENSION(i_arlen), INTENT(out)       :: r_array
	  REAL, DIMENSION(:,:), ALLOCATABLE           :: r_coo
	  


!---------- initialize some constant for the slotted cylinder

	  r_rds = r_srd* r_srd

!---------- here is the crux: where is the center of the ball at time r_time

	  
	  r_tim 		= r_time
	  r_time_one_turn 	= 1800. * 96		! Time which the ball needs for one turn around the center
	  r_PI 			= GRID_PI
	  r_phi			= r_PI			! Phasenwinkel um die Anfangskonstellation zu beruecksichtigen
	  r_centr		= r_cntr
	  r_rds_kreisbahn	= 0.25

	  r_centr(1) 		= r_rds_kreisbahn * COS(2*r_PI * r_tim/r_time_one_turn + r_phi)	!Beschreibung der Kreisbahn um den Ursprung
	  r_centr(2) 		= r_rds_kreisbahn * SIN(2*r_PI * r_tim/r_time_one_turn + r_phi) !Beschreibung der Kreisbahn um den Ursprung
 
	  r_centr(1)		= r_centr(1) + 0.5	! Ins richtige Koordinatensystem
	  r_centr(2)		= r_centr(2) + 0.5	! Ins richtige Koordsys verschieben

	  IF (r_tim == 0.0) THEN
	    r_centr= r_cntr
	  ENDIF


!---------- allocate workspace
	  
	  i_num= p_ghand%i_nnumber
	  ALLOCATE(r_coo(GRID_dimension,i_num), stat=i_alct)
          IF(i_alct /= 0) THEN
		CALL grid_error(55)
	  END IF	

!---------- get information

	  CALL grid_getinfo(p_ghand, i_num, r_nodecoordinates= r_coo)
	

!---------- test if is node is in or out of the ball and set r_array	  		
	  r_array = 0.0
	  node_loop: DO i_count= 1, i_num
	    r_tmp(:) = r_coo(:,i_count) - r_centr(:)
	    r_dpt    = dot_product(r_tmp, r_tmp)
	    inside: IF(r_dpt <= r_rds) THEN
	      r_array(i_count)= r_hgt
	    END IF inside
	  END DO node_loop

	
!---------- deallocate workspace

	  DEALLOCATE(r_coo)


	  RETURN
	  END SUBROUTINE slm_analyticsolution_BALL



!*****************************************************************
	  SUBROUTINE slm_initball(p_ghand)

!---------- local declarations

	  IMPLICIT NONE

	  TYPE (grid_handle), INTENT(in)             :: p_ghand
	  REAL, DIMENSION(GRID_dimension)            :: r_centr
	  REAL                                       :: r_rds, r_dpt
	  REAL, DIMENSION(GRID_dimension)            :: r_tmp
	  INTEGER                                    :: i_count, i_num, i_alct
	  REAL, DIMENSION(:), ALLOCATABLE            :: r_aux
	  REAL, DIMENSION(:,:), ALLOCATABLE          :: r_coo

!---------- initialize some constant for the slotted cylinder

	  r_rds= r_srd* r_srd
	  r_centr= r_cntr

!---------- allocate workspace

	  i_num= p_ghand%i_nnumber
	  ALLOCATE(r_aux(i_num), r_coo(GRID_dimension,i_num), stat= i_alct)
	  IF(i_alct /= 0) THEN
	    CALL grid_error(55)
	  END IF

!---------- get information

	  CALL grid_getinfo(p_ghand, i_num, r_nodecoordinates= r_coo)

!---------- loop over the nodes

	  node_loop: DO i_count= 1, i_num
	    r_aux(i_count)= 0.0
	    r_tmp(:)      = r_coo(:,i_count)- r_centr(:)
	    r_dpt= dot_product(r_tmp, r_tmp)
	    inside: IF(r_dpt <= r_rds) THEN
	      r_aux(i_count)= r_hgt
	    END IF inside
	  END DO node_loop

!---------- update grid information

	  CALL grid_putinfo(p_ghand, i_num, i_valpoint= GRID_tracer, r_nodevalues= r_aux)

!---------- deallocate workspace

	  DEALLOCATE(r_aux, r_coo)

	  RETURN
	  END SUBROUTINE slm_initball

	END MODULE SLM_initial
