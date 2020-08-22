!*****************************************************************
!
! MODULE NAME:
!	FEM_gridmanag
! FUNCTION:
!	handle several grid items
! CONTAINS:
!-----------------------------------------------------------------
!
! NAME:
!	grid_extract
! FUNCTION:
!	extract an array from elements/nodes in linked list
! SYNTAX:
!	CALL grid_extract(grid, char, int, int, real)
! ON INPUT:
!	p_ghand: grid handle                  (required)	type (grid_handle)
!	c_action: specify the requested action (required)	character
!	i_arlen:  array length                 (required)	integer
! ON OUTPUT:
!	r_array1:  extracted array (1D)         (optional)	real
!	r_array2:  extracted array (2D)         (optional)	real
!	i_array1:  extracted array (1D)         (optional)	real
!	i_array2:  extracted array (2D)         (optional)	real
! CALLS:
!
! COMMENTS:
!	c_action reacts according to the following keywords
!	  xcoo: extract x-coordinate from nodes
!	  ycoo: extract y-coordinate from nodes
!	  uval: extract values of x-component of velocity (U)
!	  vval: extract values of y-component of velocity (V)
!	  pval: extract values of geop. height          (Phi)
!	  trac: extract values of tracer                  (T)
!	  vort: extract values of vorticity            (Zeta)
!	for these a node pointer has to be supplied
!	  nodi: extract 2D-array of node indices
!	  nodx: extract 2D-array of polygons (elements) x-coordinates
!	  nody: extract 2D-array of polygons (elements) y-coordinates
!	  nodu: extract 2D-array of U values corresp. to polygons
!	  nodv: extract 2D-array of V values corresp. to polygons
!	  nodp: extract 2D-array of Phi values corresp. to polygons
!	  nodt: extract 2D-array of T values corresp. to polygons
!	  nodz: extract 2D-array of Zeta values corresp. to polygons
!	for these an element pointer has to be supplied
!	  elfl: extract flag of element
!	  effl: extract flag of element (finest level only)
!	  ellv: extract level of element
!	  eflv: extract level of finest grid elements
!	  elme: extract marked edge of element
!	for these an element pointer has to be supplied
!
!-----------------------------------------------------------------
!
! NAME:
!	grid_update
! FUNCTION:
!	update values in a linked list from arrayvalues
! SYNTAX:
!	call grid_update(grid, char, int, int, real)
! ON INPUT:
!	p_ghand: grid handle                  (required)	type (grid_handle)
!	c_action: specify the requested action (required)	character
!	i_arlen:  array length                 (required)	integer
!	i_array1: array for updating (1D)      (optional)	integer
!	i_array2: array for updating (2D)      (optional)	integer
!	r_array1: array for updating (1D)      (optional)	real
!	r_array2: array for updating (2D)      (optional)	real
! ON OUTPUT:
!
! CALLS:
!
! COMMENTS:
!	c_action reacts according to the following keywords
!	  xcoo: update x-coordinate from nodes
!	  ycoo: update y-coordinate from nodes
!	  uval: update values of x-component of velocity (U)
!	  vval: update values of y-component of velocity (V)
!	  pval: update values of geop. height          (Phi)
!	  trac: update values of tracer                  (T)
!	  vort: update values of vorticity            (Zeta)
!	for these a node pointer has to be supplied
!	  elfl: update flag of element
!	  effl: update flag of element (finest level only)
!	  elin: update indices (after coarsening)
!	  egin: update indices (after coarsening)
!	  ndin: update indices (after coarsening)
!	for these an element pointer has to be supplied
!	the element pointer is possibly for future use...
!
!-----------------------------------------------------------------
!
! NAME:
!	grid_findelement
! FUNCTION:
!	find an element corresponding to given coordinates
! SYNTAX:
!	elmt.ptr= grid_findelement(real.arr, grid)
! ON INPUT:
!	r_coord:  coordinate array             (required)	real
!	p_ghand: grid handle                  (required)	type (grid_handle)
! ON OUTPUT:
!	p_found:  element containing r_coord			elmt.ptr
! CALLS:
!
! COMMENTS:
!	a NULL pointer is returned by grid_findelement if r_coord
!	has not been found in any of the triangles of the whole
!	triangulation
!
!-----------------------------------------------------------------
!
! NAME:
!	in_out
! FUNCTION:
!	checks, if a given point (x,y) lies within a given triangle
! SYNTAX:
!	logical= in_out(real.arr, elmt.ptr)
! ON INPUT:
!	r_coord:  coordinate array             (required)	real
!	p_elem:   pointer to element to test   (required)	elmt.ptr
! ON OUTPUT:
!	l_in:     .true. if r_coord \in p_elem			logical
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
!	grid_boundpline
! FUNCTION:
!	create a polygon line from boundary edges
! SYNTAX:
!	call grid_boundpline(grid, int, real.arr, real.arr)
! ON INPUT:
!	p_handle: first edge of the linked list     (required)	TYPE (grid_handle)
! ON OUTPUT:
!	i_vert:   number of vertices				integer
!	r_xcoord: x-coordinated of polygon vertices		real
!	r_ycoord: y-coordinated of polygon vertices		real
! CALLS:
!
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!	grid_nodear
! FUNCTION:
!	calculate area pieces for each node (for integration over the domain in R2)
! SYNTAX:
!	call grid_nodear(grid, int, real.arr)
! ON INPUT:
!	p_handle: first edge of the linked list     (required)	TYPE (grid_handle)
! ON OUTPUT:
!	i_siz:    number of nodes (array size)			integer
!	r_area:   array of area pieces for each node		real
! CALLS:
!
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!	grid_nodevol
! FUNCTION:
!	calculate volume pieces for each node (for integration over the domain in R3)
! SYNTAX:
!	call grid_nodevol(grid, int, real.arr)
! ON INPUT:
!	p_handle: first edge of the linked list     (required)	TYPE (grid_handle)
! ON OUTPUT:
!	i_siz:    number of nodes (array size)			integer
!	r_volume:   array of volume pieces for each node	real
! CALLS:
!
! COMMENTS: not working so far, p_nhash has no entries for tetrahedra
!
!-----------------------------------------------------------------
!
! PUBLIC:
!	grid_extract, grid_update, grid_findelement, grid_boundpline,
!	grid_nodear, grid_nodevol
! COMMENTS:
!	!!! ATENTION !!!
!	between grid_extract and update_array no grid manipilations are
!	allowed in order not to corrupt data integrity
! USES:
!	MISC_globalparam, FEM_define, FEM_handle
! LIBRARIES:
!
! REFERENCES:
!
! VERSION(S):
!	1. original version		j. behrens	9/96
!	2. added find_element, et al.	j. behrens	10/96
!	3. added handles, time, renamed	j. behrens	10/96
!	4. nodal values time depend.	j. behrens	1/97
!	5. added grid_boundpline and
!	   grid_nodear		j. behrens	7/97
!	6. hashing tables		j. behrens	7/97
!	7. USE FEM_utils added		j. behrens	1/98
!       8. grid_nodevol added 	        l. mentrup      6/02
!	9. grid_tetvol &
!	   grid_tetbaryc added		l. mentrup	9/02
!
!*****************************************************************
	MODULE FEM_gridmanag
	  USE MISC_globalparam
	  USE MISC_error
	  USE FEM_define
	  USE FEM_handle
	  USE FEM_utils
	  PRIVATE
	  PUBLIC :: elmt, node, edge
	  PUBLIC :: grid_extract, grid_update
	  PUBLIC :: grid_findelement, grid_boundpline, grid_nodear, &
	            grid_findtetra, grid_nodevol, grid_tetvol, &
		    grid_tetbaryc, grid_upstr_nodevol
	CONTAINS
!*****************************************************************
	SUBROUTINE grid_extract(p_ghand, c_action, &
	                        i_array1, i_array2, r_array1, r_array2)

!---------- local declarations

	IMPLICIT NONE 
    INTEGER (KIND = GRID_SI), PARAMETER                                 :: nn = 4
	TYPE (grid_handle), INTENT(in)                     :: p_ghand
	CHARACTER (len=nn), INTENT(in)                     :: c_action
	INTEGER (KIND = GRID_SI), DIMENSION(:), INTENT(out), OPTIONAL       :: i_array1
	INTEGER (KIND = GRID_SI), DIMENSION(:,:), INTENT(out), OPTIONAL     :: i_array2
	REAL (KIND = GRID_SR), DIMENSION(:), INTENT(out), OPTIONAL          :: r_array1
	REAL (KIND = GRID_SR), DIMENSION(:,:), INTENT(out), OPTIONAL        :: r_array2
	INTEGER (KIND = GRID_SI)                                            :: i_count, i_nc, i_tim, i_ec, i_ec1
        TYPE (tetra), POINTER                              :: p_ttmp
	TYPE (elmt), POINTER                               :: p_etmp
        

! not yet used	TYPE (edge), POINTER                       :: p_gtmp
	TYPE (node), POINTER                               :: p_ntmp

!---------- set time tag

	i_tim= p_ghand%i_timetag

!---------- check for the requested action

	action_type: SELECT CASE (c_action(1:4))

!---------- extract x-coordinates (nodewise)

	  CASE('xcoo') action_type
	    IF(.NOT. present(r_array1)) THEN
	      CALL print_error(64)
	    ELSE
	      DO i_count= 1, p_ghand%i_nnumber
	        p_ntmp => p_nhash(i_ngrid(i_count,i_tim))%np
	        r_array1(i_count)= p_ntmp%def%r_coor(1)
	      END DO
	    END IF

!---------- extract y-coordinates (nodewise)

	  CASE('ycoo') action_type
	    IF(.NOT. present(r_array1)) THEN
	      CALL print_error(64)
	    ELSE
	      DO i_count= 1, p_ghand%i_nnumber
	        p_ntmp => p_nhash(i_ngrid(i_count,i_tim))%np
	        r_array1(i_count)= p_ntmp%def%r_coor(2)
	      END DO
	    END IF

!---------- extract z-coordinates (nodewise)

         CASE('zcoo') action_type
          IF(.NOT. present(r_array1)) THEN
	      CALL print_error(64)
	    ELSE
	      DO i_count= 1, p_ghand%i_nnumber
	        p_ntmp => p_nhash(i_ngrid(i_count,i_tim))%np
	        r_array1(i_count)= p_ntmp%def%r_coor(3)
	      END DO
	    END IF


!---------- extract u-values (nodewise)

	  CASE('uval') action_type
	    IF(.NOT. present(r_array1)) THEN
	      CALL print_error(64)
	    ELSE
	      DO i_count= 1, p_ghand%i_nnumber
	        p_ntmp => p_nhash(i_ngrid(i_count,i_tim))%np
	        r_array1(i_count)= p_ntmp%att%r_vals(DEF_ucomp,i_tim)
	      END DO
	    END IF

!---------- extract v-values (nodewise)

	  CASE('vval') action_type
	    IF(.NOT. present(r_array1)) THEN
	      CALL print_error(64)
	    ELSE
	      DO i_count= 1, p_ghand%i_nnumber
	        p_ntmp => p_nhash(i_ngrid(i_count,i_tim))%np
	        r_array1(i_count)= p_ntmp%att%r_vals(DEF_vcomp,i_tim)
	      END DO
	    END IF

!---------- extract v-values (nodewise)

	  CASE('wval') action_type
	    IF(.NOT. present(r_array1)) THEN
	      CALL print_error(64)
	    ELSE
	      DO i_count= 1, p_ghand%i_nnumber
	        p_ntmp => p_nhash(i_ngrid(i_count,i_tim))%np
	        r_array1(i_count)= p_ntmp%att%r_vals(DEF_wcomp,i_tim)
	      END DO
	    END IF

!---------- extract Phi-values

	  CASE('pval') action_type
	    IF(.NOT. present(r_array1)) THEN
	      CALL print_error(64)
	    ELSE
	      DO i_count= 1, p_ghand%i_nnumber
	        p_ntmp => p_nhash(i_ngrid(i_count,i_tim))%np
	        r_array1(i_count)= p_ntmp%att%r_vals(DEF_phi,i_tim)
	      END DO
	    END IF

!---------- extract vorticity-values

	  CASE('vort') action_type
	    IF(.NOT. present(r_array1)) THEN
	      CALL print_error(64)
	    ELSE
	      DO i_count= 1, p_ghand%i_nnumber
	        p_ntmp => p_nhash(i_ngrid(i_count,i_tim))%np
	        r_array1(i_count)= p_ntmp%att%r_vals(DEF_zeta,i_tim)
	      END DO
	    END IF

!---------- extract tracer values

	  CASE('trac') action_type
	    IF(.NOT. present(r_array1)) THEN
	      CALL print_error(64)
	    ELSE
	      DO i_count= 1, p_ghand%i_nnumber
	        p_ntmp => p_nhash(i_ngrid(i_count,i_tim))%np
	        r_array1(i_count)= p_ntmp%att%r_vals(DEF_tracer,i_tim)
	      END DO
	    END IF

!---------- extract node indices

	  CASE('nodi') action_type
	    IF(.NOT. present(i_array2)) THEN
	      CALL print_error(64)
	    ELSE
	      DO i_count= 1, p_ghand%i_enumfine
	        p_etmp => p_ehash(i_efine(i_count,i_tim))%ep
	        DO i_nc=1,DEF_elnodes
	          p_ntmp=> p_nhash(p_etmp%def%p_node(i_nc))%np
	          i_array2(i_nc,i_count)= p_ntmp%def%i_indx
	        END DO
	      END DO
	    END IF

!---------- extract x-values (elementwise) (only finest grid)

	  CASE('nodx') action_type
	    IF(.NOT. present(r_array2)) THEN
	      CALL print_error(64)
	    ELSE
	      DO i_count= 1, p_ghand%i_enumfine
	        p_etmp => p_ehash(i_efine(i_count,i_tim))%ep
	        DO i_nc=1,DEF_elnodes
	          p_ntmp=> p_nhash(p_etmp%def%p_node(i_nc))%np
	          r_array2(i_nc,i_count)= p_ntmp%def%r_coor(1)
	        END DO
	      END DO
	    END IF

!---------- extract y-values (elementwise) (only finest grid)

	  CASE('nody') action_type
	    IF(.NOT. present(r_array2)) THEN
	      CALL print_error(64)
	    ELSE
	      DO i_count= 1, p_ghand%i_enumfine
	        p_etmp => p_ehash(i_efine(i_count,i_tim))%ep
	        DO i_nc=1,DEF_elnodes
	          p_ntmp=> p_nhash(p_etmp%def%p_node(i_nc))%np
	          r_array2(i_nc,i_count)= p_ntmp%def%r_coor(2)
	        END DO
	      END DO
	    END IF


!---------- extract z-values (elementwise) (only finest grid)

	  CASE('nodez') action_type
	    IF(.NOT. present(r_array2)) THEN
	      CALL print_error(64)
	    ELSE
	      DO i_count= 1, p_ghand%i_enumfine
	        p_etmp => p_ehash(i_efine(i_count,i_tim))%ep
	        DO i_nc=1,DEF_elnodes
	          p_ntmp=> p_nhash(p_etmp%def%p_node(i_nc))%np
	          r_array2(i_nc,i_count)= p_ntmp%def%r_coor(3)
	        END DO
	      END DO
	    END IF

!---------- extract U-values (elementwise) (only finest grid)

	  CASE('nodu') action_type
	    IF(.NOT. present(r_array2)) THEN
	      CALL print_error(64)
	    ELSE
	      DO i_count= 1, p_ghand%i_enumfine
	        p_etmp => p_ehash(i_efine(i_count,i_tim))%ep
	        DO i_nc=1,DEF_elnodes
	          p_ntmp=> p_nhash(p_etmp%def%p_node(i_nc))%np
	          r_array2(i_nc,i_count)= p_ntmp%att%r_vals(DEF_ucomp,i_tim)
	        END DO
	      END DO
	    END IF

!---------- extract V-values (elementwise) (only finest grid)

	  CASE('nodv') action_type
	    IF(.NOT. present(r_array2)) THEN
	      CALL print_error(64)
	    ELSE
	      DO i_count= 1, p_ghand%i_enumfine
	        p_etmp => p_ehash(i_efine(i_count,i_tim))%ep
	        DO i_nc=1,DEF_elnodes
	          p_ntmp=> p_nhash(p_etmp%def%p_node(i_nc))%np
	          r_array2(i_nc,i_count)= p_ntmp%att%r_vals(DEF_vcomp,i_tim)
	        END DO
	      END DO
	    END IF

!---------- extract V-values (elementwise) (only finest grid)

	  CASE('nodw') action_type
	    IF(.NOT. present(r_array2)) THEN
	      CALL print_error(64)
	    ELSE
	      DO i_count= 1, p_ghand%i_enumfine
	        p_etmp => p_ehash(i_efine(i_count,i_tim))%ep
	        DO i_nc=1,DEF_elnodes
	          p_ntmp=> p_nhash(p_etmp%def%p_node(i_nc))%np
	          r_array2(i_nc,i_count)= p_ntmp%att%r_vals(DEF_wcomp,i_tim)
	        END DO
	      END DO
	    END IF

!---------- extract Phi-values (elementwise) (only finest grid)

	  CASE('nodp') action_type
	    IF(.NOT. present(r_array2)) THEN
	      CALL print_error(64)
	    ELSE
	      DO i_count= 1, p_ghand%i_enumfine
	        p_etmp => p_ehash(i_efine(i_count,i_tim))%ep
	        DO i_nc=1,DEF_elnodes
	          p_ntmp=> p_nhash(p_etmp%def%p_node(i_nc))%np
	          r_array2(i_nc,i_count)= p_ntmp%att%r_vals(DEF_phi,i_tim)
	        END DO
	      END DO
	    END IF

!---------- extract Tracer-values (elementwise) (only finest grid)

	  CASE('nodt') action_type
	    IF(.NOT. present(r_array2)) THEN
	      CALL print_error(64)
	    ELSE
	      DO i_count= 1, p_ghand%i_enumfine
	        p_etmp => p_ehash(i_efine(i_count,i_tim))%ep
	        DO i_nc=1,DEF_elnodes
	          p_ntmp=> p_nhash(p_etmp%def%p_node(i_nc))%np
	          r_array2(i_nc,i_count)= p_ntmp%att%r_vals(DEF_tracer,i_tim)
	        END DO
	      END DO
	    END IF

!---------- extract Vorticity-values (elementwise) (only finest grid)

	  CASE('nodz') action_type
	    IF(.NOT. present(r_array2)) THEN
	      CALL print_error(64)
	    ELSE
	      DO i_count= 1, p_ghand%i_enumfine
	        p_etmp => p_ehash(i_efine(i_count,i_tim))%ep
	        DO i_nc=1,DEF_elnodes
	          p_ntmp=> p_nhash(p_etmp%def%p_node(i_nc))%np
	          r_array2(i_nc,i_count)= p_ntmp%att%r_vals(DEF_zeta,i_tim)
	        END DO
	      END DO
	    END IF

!---------- extract flags (elementwise)

	  CASE('elfl') action_type
	    IF(.NOT. present(i_array1)) THEN
	      CALL print_error(64)
	    ELSE
	      DO i_count= 1, p_ghand%i_enumber
	        p_etmp => p_ehash(i_egrid(i_count, i_tim))%ep
	        i_array1(i_count)= p_etmp%att%i_stat(i_tim)
	      END DO
	    END IF

!---------- extract flags (elementwise, finest level)

	  CASE('effl') action_type
	    IF(.NOT. present(i_array1)) THEN
	      CALL print_error(64)
	    ELSE
	      DO i_count= 1, p_ghand%i_enumfine
	        p_etmp => p_ehash(i_efine(i_count,i_tim))%ep
	        i_array1(i_count)= p_etmp%att%i_stat(i_tim)
	      END DO
	    END IF


!---------- extract tetrahedon flags

	  CASE('tefl') action_type
	    IF(.NOT. present(i_array1)) THEN
	      CALL print_error(64)
	    ELSE
	      DO i_count= 1, p_ghand%i_tnumber
	        p_ttmp => p_thash(i_tgrid(i_count, i_tim))%tp
	        i_array1(i_count)= p_ttmp%att%i_stat(i_tim)
	      END DO
	    END IF

!---------- extract tetrahedon flags (finest level)

	  CASE('tffl') action_type
	    IF(.NOT. present(i_array1)) THEN
	      CALL print_error(64)
	    ELSE
	      DO i_count= 1, p_ghand%i_tnumfine
	        p_ttmp => p_thash(i_tfine(i_count, i_tim))%tp
	        i_array1(i_count)= p_ttmp%att%i_stat(i_tim)
	      END DO
	    END IF


!---------- extract attributes (elementwise)

	  CASE('elme') action_type
	    IF(.NOT. present(i_array1)) THEN
	      CALL print_error(64)
	    ELSE
	      DO i_count= 1, p_ghand%i_enumber
	        p_etmp => p_ehash(i_egrid(i_count, i_tim))%ep
	        i_array1(i_count)= p_etmp%att%i_mark
	      END DO
	    END IF

!---------- extract levels (elementwise)

	  CASE('ellv') action_type
	    IF(.NOT. present(i_array1)) THEN
	      CALL print_error(64)
	    ELSE
	      DO i_count= 1, p_ghand%i_enumber
	        p_etmp => p_ehash(i_egrid(i_count, i_tim))%ep
	        i_array1(i_count)= p_etmp%att%i_levl
	      END DO
	    END IF

!---------- extract levels (elementwise, finest level)

	  CASE('eflv') action_type
	    IF(.NOT. present(i_array1)) THEN
	      CALL print_error(64)
	    ELSE
	      DO i_count= 1, p_ghand%i_enumfine
	        p_etmp => p_ehash(i_efine(i_count,i_tim))%ep
	        i_array1(i_count)= p_etmp%att%i_levl
	      END DO
	    END IF


!---------- extract tetra levels 

	  CASE('tllv') action_type
	    IF(.NOT. present(i_array1)) THEN
	      CALL print_error(64)
	    ELSE
	      DO i_count= 1, p_ghand%i_tnumber
	        p_ttmp => p_thash(i_tgrid(i_count, i_tim))%tp
	        i_array1(i_count)= p_ttmp%att%i_levl
	      END DO
	    END IF

!---------- extract tetra levels (finest level)

	  CASE('tflv') action_type
	    IF(.NOT. present(i_array1)) THEN
	      CALL print_error(64)
	    ELSE
	      DO i_count= 1, p_ghand%i_tnumfine
	        p_ttmp => p_thash(i_tfine(i_count, i_tim))%tp
	        i_array1(i_count)= p_ttmp%att%i_levl
	      END DO
	    END IF
!---------- get tetrahedron's element indices (array of (4,m))

	  CASE('telf') action_type
	    IF(.NOT. present(i_array2)) THEN
	      CALL print_error(64)
	    ELSE
	      DO i_count= 1, p_ghand%i_tnumfine
	        p_ttmp => p_thash(i_tfine(i_count,i_tim))%tp
	        i_array2(:,i_count)= p_ttmp%def%p_node
	        DO i_ec=1,DEF_tetnodes
		      loopt3: DO i_ec1= 1,p_ghand%i_nnumber
		        IF(i_ngrid(i_ec1,i_tim) == i_array2(i_ec,i_count)) THEN
		          i_array2(i_ec,i_count)= i_ec1
		          EXIT loopt3
		        END IF
		      END DO loopt3
	        END DO
	      END DO
	    END IF

	  CASE DEFAULT action_type
	    CALL print_error(61)
	END SELECT action_type

	RETURN
	END SUBROUTINE grid_extract
!*****************************************************************
	SUBROUTINE grid_update(p_ghand, c_action, &
	                       i_array1, i_array2, r_array1, r_array2)

!---------- local declarations

	IMPLICIT NONE

	TYPE (grid_handle), INTENT(in)                     :: p_ghand
	CHARACTER (len=4), INTENT(in)                      :: c_action
	INTEGER (KIND = GRID_SI), DIMENSION(:), INTENT(in), OPTIONAL        :: i_array1
	INTEGER (KIND = GRID_SI), DIMENSION(:,:), INTENT(in), OPTIONAL      :: i_array2
	REAL (KIND = GRID_SR), DIMENSION(:), INTENT(in), OPTIONAL           :: r_array1
	REAL (KIND = GRID_SR), DIMENSION(:,:), INTENT(in), OPTIONAL         :: r_array2
	INTEGER (KIND = GRID_SI)                                            :: i_count, i_tim
	TYPE (elmt), POINTER                               :: p_etmp
	TYPE (edge), POINTER                               :: p_gtmp
	TYPE (node), POINTER                               :: p_ntmp
    TYPE (tetra), POINTER                              :: p_ttmp


!---------- set time tag

	i_tim= p_ghand%i_timetag

!---------- check for the requested action

	action_type: SELECT CASE (c_action)

!---------- update x-coordinate

	  CASE('xcoo') action_type
	    IF(.NOT. present(r_array1)) THEN
	      CALL print_error(65)
	    ELSE
	      DO i_count= 1, p_ghand%i_nnumber
	        p_ntmp => p_nhash(i_ngrid(i_count,i_tim))%np
	        p_ntmp%def%r_coor(1)= r_array1(i_count)
	      END DO
	    END IF

!---------- update y-coordinate

	  CASE('ycoo') action_type
	    IF(.NOT. present(r_array1)) THEN
	      CALL print_error(65)
	    ELSE
	      DO i_count= 1, p_ghand%i_nnumber
	        p_ntmp => p_nhash(i_ngrid(i_count,i_tim))%np
	        p_ntmp%def%r_coor(2)= r_array1(i_count)
	      END DO
	    END IF

!---------- update z-coordinate

	  CASE('zcoo') action_type
	    IF(.NOT. present(r_array1)) THEN
	      CALL print_error(65)
	    ELSE
	      DO i_count= 1, p_ghand%i_nnumber
	        p_ntmp => p_nhash(i_ngrid(i_count,i_tim))%np
	        p_ntmp%def%r_coor(3)= r_array1(i_count)
	      END DO
	    END IF


!---------- update u-values

	  CASE('uval') action_type
	    IF(.NOT. present(r_array1)) THEN
	      CALL print_error(65)
	    ELSE
	      DO i_count= 1, p_ghand%i_nnumber
	        p_ntmp => p_nhash(i_ngrid(i_count,i_tim))%np
	        p_ntmp%att%r_vals(DEF_ucomp,i_tim)= r_array1(i_count)
	      END DO
	    END IF

!---------- update v-values

	  CASE('vval') action_type
	    IF(.NOT. present(r_array1)) THEN
	      CALL print_error(65)
	    ELSE
	      DO i_count= 1, p_ghand%i_nnumber
	        p_ntmp => p_nhash(i_ngrid(i_count,i_tim))%np
	        p_ntmp%att%r_vals(DEF_vcomp,i_tim)= r_array1(i_count)
	      END DO
	    END IF

!---------- update w-values

	  CASE('wval') action_type
	    IF(.NOT. present(r_array1)) THEN
	      CALL print_error(65)
	    ELSE
	      DO i_count= 1, p_ghand%i_nnumber
	        p_ntmp => p_nhash(i_ngrid(i_count,i_tim))%np
	        p_ntmp%att%r_vals(DEF_wcomp,i_tim)= r_array1(i_count)
	      END DO
	    END IF

!---------- update Phi-values

	  CASE('pval') action_type
	    IF(.NOT. present(r_array1)) THEN
	      CALL print_error(65)
	    ELSE
	      DO i_count= 1, p_ghand%i_nnumber
	        p_ntmp => p_nhash(i_ngrid(i_count,i_tim))%np
	        p_ntmp%att%r_vals(DEF_phi,i_tim)= r_array1(i_count)
	      END DO
	    END IF

!---------- update Vorticity-values

	  CASE('vort') action_type
	    IF(.NOT. present(r_array1)) THEN
	      CALL print_error(65)
	    ELSE
	      DO i_count= 1, p_ghand%i_nnumber
	        p_ntmp => p_nhash(i_ngrid(i_count,i_tim))%np
	        p_ntmp%att%r_vals(DEF_zeta,i_tim)= r_array1(i_count)
	      END DO
	    END IF

!---------- update tracer

	  CASE('trac') action_type
	    IF(.NOT. present(r_array1)) THEN
	      CALL print_error(65)
	    ELSE
	      DO i_count= 1, p_ghand%i_nnumber
	        p_ntmp => p_nhash(i_ngrid(i_count,i_tim))%np
	        p_ntmp%att%r_vals(DEF_tracer,i_tim)= r_array1(i_count)
	      END DO
	    END IF

!---------- update flags

	  CASE('elfl') action_type
	    IF(.NOT. present(i_array1)) THEN
	      CALL print_error(65)
	    ELSE
	      DO i_count= 1, p_ghand%i_enumber
	        p_etmp => p_ehash(i_egrid(i_count, i_tim))%ep
	        p_etmp%att%i_stat(i_tim)= i_array1(i_count)
	      END DO
	    END IF

!---------- update flags (fine level only)

	  CASE('effl') action_type !--- update flag only on finest level
	    IF(.NOT. present(i_array1)) THEN
	      CALL print_error(65)
	    ELSE
	      DO i_count= 1, p_ghand%i_enumfine
	        p_etmp => p_ehash(i_efine(i_count,i_tim))%ep
	        p_etmp%att%i_stat(i_tim)= i_array1(i_count)
	      END DO
	    END IF

!---------- update tetra flags

	  CASE('tefl') action_type
	    IF(.NOT. present(i_array1)) THEN
	      CALL print_error(65)
	    ELSE
	      DO i_count= 1, p_ghand%i_tnumber
	        p_ttmp => p_thash(i_tgrid(i_count, i_tim))%tp
	        p_ttmp%att%i_stat(i_tim)= i_array1(i_count)
	      END DO
	    END IF

!---------- update tetra flags (fine level only)

	  CASE('tffl') action_type !--- update flag only on finest level
	    IF(.NOT. present(i_array1)) THEN
	      CALL print_error(65)
	    ELSE
	      DO i_count= 1, p_ghand%i_tnumfine
	        p_ttmp => p_thash(i_tfine(i_count, i_tim))%tp
	        p_ttmp%att%i_stat(i_tim)= i_array1(i_count)
	      END DO
	    END IF

!---------- update indices of tetrahedra

	  CASE('tein') action_type !--- update indices
	    IF(.NOT. present(i_array1)) THEN
	      CALL print_error(65)
	    ELSE
	      DO i_count= 1, p_ghand%i_tnumber
	        p_ttmp => p_thash(i_tgrid(i_count, i_tim))%tp
	        p_ttmp%def%i_indx= i_array1(i_count)
	      END DO
	    END IF




!---------- update indices of elements

	  CASE('elin') action_type !--- update indices
	    IF(.NOT. present(i_array1)) THEN
	      CALL print_error(65)
	    ELSE
	      DO i_count= 1, p_ghand%i_enumber
	        p_etmp => p_ehash(i_egrid(i_count, i_tim))%ep
	        p_etmp%def%i_indx= i_array1(i_count)
	      END DO
	    END IF


!---------- update indices of edges

	  CASE('egin') action_type !--- update indices
	    IF(.NOT. present(i_array1)) THEN
	      CALL print_error(65)
	    ELSE
	      DO i_count= 1, p_ghand%i_gnumber
	        p_gtmp => p_ghash(i_ggrid(i_count, i_tim))%gp
	        p_gtmp%def%i_indx= i_array1(i_count)
	      END DO
	    END IF

!---------- update indices of nodes

	  CASE('ndin') action_type !--- update indices
	    IF(.NOT. present(i_array1)) THEN
	      CALL print_error(65)
	    ELSE
	      DO i_count= 1, p_ghand%i_nnumber
	        p_ntmp => p_nhash(i_ngrid(i_count,i_tim))%np
	        p_ntmp%def%i_indx= i_array1(i_count)
	      END DO
	    END IF

	  CASE DEFAULT action_type
	    CALL print_error(61)
	END SELECT action_type

	RETURN
	END SUBROUTINE grid_update

!*****************************************************************
	FUNCTION grid_findelement(r_coord, p_ghand) RESULT (p_found)

!---------- local declarations

	IMPLICIT NONE

	REAL (KIND = GRID_SR), DIMENSION(DEF_dimension), INTENT(in) :: r_coord
	TYPE (grid_handle), INTENT(in)             :: p_ghand
	TYPE (elmt), POINTER                       :: p_found
	TYPE (elmt), POINTER                       :: p_etmp, p_esis
	LOGICAL                                    :: l_consec
	INTEGER (KIND = GRID_SI)                                    :: i_cnt, i_tim, i_ind, i_dni

!---------- set time tag

	i_tim= p_ghand%i_timetag

!---------- initialize p_found

	nullify(p_found)

!---------- start with a consecutive search

	l_consec=  .TRUE.
	i_dni   =  1
	p_etmp  => p_ehash(i_dni)%ep
	search_loop: DO WHILE(associated(p_etmp))

	  consec_search: IF(l_consec) THEN

!---------- check if element contains given coordinates

	    contained: IF(in_out(r_coord, p_etmp)) THEN
	      finest: IF(p_etmp%att%i_stat(i_tim) == DEF_unrefined) THEN
	        p_found => p_etmp
	        nullify(p_etmp)
	        EXIT search_loop
	      ELSE finest
	        l_consec= .FALSE.
	      END IF finest
	    ELSE contained
	      i_dni  =  i_dni+ 1
	      all_searched: IF(i_dni > p_ghand%i_enumber) THEN
	        i_ind= 0
	        IF(GRID_parameters%iolog > 0) THEN
	          write(GRID_parameters%iolog,*) 'WARNING      : [grid_findelement] Element search not successful'
	          write(GRID_parameters%iolog,*) '             : Coordinates           : ', r_coord
	          write(GRID_parameters%iolog,*) '             : Element set to (index): ', i_ind
	        END IF
	        nullify(p_etmp)
	        EXIT search_loop
	      END IF all_searched
	      p_etmp => p_ehash(i_dni)%ep
	    END IF contained

!---------- this is the hierarchical (bin tree) search

	  ELSE consec_search
	    sisters_loop: DO i_cnt=1, DEF_elchild
	      p_esis=> p_ehash(p_etmp%lnk%p_chil(i_cnt,i_tim))%ep
	      inside: IF(in_out(r_coord,p_esis)) THEN
	        fine: IF(p_esis%att%i_stat(i_tim) == DEF_unrefined) THEN
	          p_found => p_esis
	          nullify(p_etmp)
	          EXIT search_loop
	        ELSE IF(p_esis%att%i_stat(i_tim) == DEF_refined) THEN fine
	          p_etmp => p_esis
	          EXIT sisters_loop
	        END IF fine
	      ELSE inside
!---------- for future research: this process might be restarted recursively?!
	        no_success: IF(i_cnt >= DEF_elchild) THEN
	          IF(GRID_parameters%iolog > 0) THEN
	            write(GRID_parameters%iolog,*) 'WARNING      : Element search aborted before reaching finest level'
	            write(GRID_parameters%iolog,*) '             : Coordinates: ', r_coord
	          END IF
	          p_found=> p_etmp
	          i_ind= p_found%def%i_indx
	          IF(GRID_parameters%iolog > 0) &
	            write(GRID_parameters%iolog,*) '             : Index of found element: ', i_ind
	          nullify(p_etmp)
	          EXIT search_loop
	        END IF no_success
	      END IF inside
	    END DO sisters_loop
	  END IF consec_search

	END DO search_loop

	RETURN
	END FUNCTION grid_findelement


!*****************************************************************
	FUNCTION grid_findtetra(r_coord, p_ghand) RESULT (p_found)

!---------- local declarations

	IMPLICIT NONE

	REAL (KIND = GRID_SR), DIMENSION(DEF_dimension), INTENT(in) :: r_coord
	TYPE (grid_handle), INTENT(in)             :: p_ghand
	TYPE (tetra), POINTER                       :: p_found
	TYPE (tetra), POINTER                       :: p_ttmp, p_tsis
	LOGICAL                                    :: l_consec
	INTEGER (KIND = GRID_SI)                                    :: i_cnt, i_tim, i_ind, i_dni

!---------- set time tag

	i_tim= p_ghand%i_timetag

!---------- initialize p_found

	nullify(p_found)

!---------- start with a consecutive search

	l_consec=  .TRUE.
	i_dni   =  1
	p_ttmp  => p_thash(i_dni)%tp
	search_loop: DO WHILE(associated(p_ttmp))

	  consec_search: IF(l_consec) THEN

!---------- check if tetrahedron contains given coordinates

	    contained: IF(THREEDIM_in_out(r_coord, p_ttmp)) THEN
	      finest: IF(p_ttmp%att%i_stat(i_tim) == DEF_unrefined) THEN
	        p_found => p_ttmp
	        nullify(p_ttmp)
	        EXIT search_loop
	      ELSE finest
	        l_consec= .FALSE.
	      END IF finest
	    ELSE contained
	      i_dni  =  i_dni+ 1
	      all_searched: IF(i_dni > p_ghand%i_tnumber) THEN
	        i_ind= 0
	        IF(GRID_parameters%ioout > 0) THEN
	          write(GRID_parameters%ioout,*) 'WARNING      : [grid_findtetra] Tetrahedron search not successful'
	          write(GRID_parameters%ioout,*) '             : Coordinates           : ', r_coord
	          write(GRID_parameters%ioout,*) '             : Tetrahedron set to (index): ', i_ind
	        END IF
	        nullify(p_ttmp)
	        EXIT search_loop
	      END IF all_searched
	      p_ttmp => p_thash(i_dni)%tp
	    END IF contained

!---------- this is the hierarchical (bin tree) search

	  ELSE consec_search
	    sisters_loop: DO i_cnt=1, DEF_elchild
	      p_tsis=> p_thash(p_ttmp%lnk%p_chil(i_cnt,i_tim))%tp
	      inside: IF(THREEDIM_in_out(r_coord,p_tsis)) THEN
	        fine: IF(p_tsis%att%i_stat(i_tim) == DEF_unrefined) THEN
	          p_found => p_tsis
	          nullify(p_ttmp)
	          EXIT search_loop
	        ELSE IF(p_tsis%att%i_stat(i_tim) == DEF_refined) THEN fine
	          p_ttmp => p_tsis
	          EXIT sisters_loop
	        END IF fine
	      ELSE inside
!---------- for future research: this process might be restarted recursively?!
	        no_success: IF(i_cnt >= DEF_elchild) THEN
	          IF(GRID_parameters%iolog > 0) THEN
	            write(GRID_parameters%iolog,*) 'WARNING      : Tetrahedron search aborted before reaching finest level'
	            write(GRID_parameters%iolog,*) '             : Coordinates: ', r_coord
	          END IF
	          p_found=> p_ttmp
	          i_ind= p_found%def%i_indx
	          IF(GRID_parameters%iolog > 0) &
	            write(GRID_parameters%iolog,*) '             : Index of found tetrahedron: ', i_ind
	          nullify(p_ttmp)
	          EXIT search_loop
	        END IF no_success
	      END IF inside
	    END DO sisters_loop
	  END IF consec_search

	END DO search_loop

	RETURN
	END FUNCTION grid_findtetra


!*****************************************************************

	SUBROUTINE grid_boundpline(p_handle, i_vert, r_xcoord, r_ycoord)

!---------- local declarations

	IMPLICIT NONE

	TYPE (grid_handle), INTENT(in) :: p_handle
	INTEGER (KIND = GRID_SI), INTENT(out)           :: i_vert
	REAL (KIND = GRID_SR), DIMENSION(:)             :: r_xcoord, r_ycoord
	INTEGER (KIND = GRID_SI)                        :: i_cnt, i_ind, j_cnt, i_tim
	TYPE (node), POINTER           :: p_ntmp, p_ndif
	TYPE (edge), POINTER           :: p_gtmp

!---------- follow the boundary edge list

	i_tim= p_handle%i_timetag
	i_cnt= 1
	i_ind= 1
	DO j_cnt= 1, p_handle%i_gnumboun
	p_gtmp => p_ghash(i_gboun(j_cnt,i_tim))%gp
	  IF(i_cnt > 1) THEN
	    p_ndif => p_nhash(p_gtmp%def%p_node(i_ind))%np
	    IF(p_ntmp%def%i_indx == p_ndif%def%i_indx) THEN
	      i_ind= 3- i_ind
	    END IF
	  END IF
	  p_ntmp         => p_nhash(p_gtmp%def%p_node(i_ind))%np
	  r_xcoord(i_cnt)=  p_ntmp%def%r_coor(1)
	  r_ycoord(i_cnt)=  p_ntmp%def%r_coor(2)
	  i_cnt= i_cnt+ 1
	END DO

!---------- set vertex counter

	i_vert= i_cnt- 1
	RETURN
	END SUBROUTINE grid_boundpline

!*****************************************************************

	SUBROUTINE grid_nodear(p_ghand, i_siz, r_area)

!---------- local declarations

	IMPLICIT NONE

	TYPE (grid_handle), INTENT(in) :: p_ghand
	INTEGER (KIND = GRID_SI), INTENT(inout)         :: i_siz
	REAL (KIND = GRID_SR), DIMENSION(:)             :: r_area
	INTEGER (KIND = GRID_SI)                        :: i_count, i_cnt, j_cnt, i_tim
	REAL (KIND = GRID_SR), PARAMETER                :: r_1o3= (1./DEF_elnodes)
	REAL (KIND = GRID_SR), DIMENSION(DEF_dimension,DEF_elnodes) :: r_c
	TYPE (node), POINTER           :: p_ntmp, p_nnod
	TYPE (elmt), POINTER           :: p_etmp

!---------- calculate area pieces for each node

	i_count= 0
	i_tim =  p_ghand%i_timetag
	integrat: DO i_count= 1, p_ghand%i_nnumber
	  p_ntmp=> p_nhash(i_ngrid(i_count,i_tim))%np
	  r_area(i_count)= 0.0
	  patch_loop: DO i_cnt= 1, p_ntmp%att%i_ptch(i_tim)
	    p_etmp=> p_ehash(p_ntmp%att%p_ptch(i_cnt,i_tim))%ep

	    node_loop: DO j_cnt=1,DEF_elnodes
	      p_nnod    => p_nhash(p_etmp%def%p_node(j_cnt))%np
	      r_c(:,j_cnt)=  p_nnod%def%r_coor
	    END DO node_loop
	    r_area(i_count)= r_area(i_count)+ r_1o3* calc_area(r_c(:,1), &
	                                                       r_c(:,2), r_c(:,3))
	  END DO patch_loop
	END DO integrat

	i_siz= i_count- 1

	RETURN
	END SUBROUTINE grid_nodear
!*****************************************************************

	SUBROUTINE grid_nodevol(p_ghand, i_siz, r_volum)



!---------- local declarations

	IMPLICIT NONE

	TYPE (grid_handle), INTENT(in) 			:: p_ghand
	INTEGER (KIND = GRID_SI), INTENT(inout)         			:: i_siz
	REAL (KIND = GRID_SR), DIMENSION(:)             			:: r_volum
	REAL (KIND = GRID_SR) 			       			:: r_vol
	INTEGER (KIND = GRID_SI)                        			:: i_count, i_cnt, j_cnt, i_tim
	INTEGER (KIND = GRID_SI)						:: i_alct
        INTEGER (KIND = GRID_SI), DIMENSION(DEF_tetnodes)		:: i_nindx
	INTEGER (KIND = GRID_SI), DIMENSION(:), ALLOCATABLE		:: i_invlist
	
	REAL (KIND = GRID_SR), PARAMETER                			:: r_1o4= (1./DEF_tetnodes)
	REAL (KIND = GRID_SR), DIMENSION(DEF_dimension, DEF_tetnodes) 	:: r_c
	TYPE (node), POINTER           			:: p_ntmp, p_nnod
	TYPE (tetra), POINTER           		:: p_ttmp



	IF (size(r_volum) < p_ghand%i_nnumber) THEN
  	    CALL print_error(a_err='ERROR [grid_nodevol]: Size of volume array too small')								
	END IF


!---------- calculate volume pieces for each node
!---------- new idea: calculate volume of each tetra and add 1/4 to each node

	i_count = 0
	i_tim   = p_ghand%i_timetag
	r_volum = 0.0
	i_nindx = 0
	
	ALLOCATE(i_invlist(p_ghand%i_ntotal),stat=i_alct)
	IF(i_alct /= 0) THEN 
	  CALL print_error(a_err='ERROR [grid_nodevol]: Could not allocate memory')
	END IF
	i_invlist(:) = 0
	DO i_cnt=1,p_ghand%i_nnumber
	  i_invlist(i_ngrid(i_cnt,i_tim)) = i_cnt
	END DO
	
	integrat: DO i_count= 1, p_ghand%i_tnumfine
	    p_ttmp=> p_thash(i_tfine(i_count,i_tim))%tp 
	  
	    node_loop: DO i_cnt=1,DEF_tetnodes
	      p_nnod    => p_nhash(p_ttmp%def%p_node(i_cnt))%np
	      r_c(:,i_cnt)=  p_nnod%def%r_coor

!--------- FOR FINE GRID ONLY! p_nnod%def%i_indx gives the nodeindex in the normal, NOT the fine, grid
!--------- here the node indices (of the fine grid) are saved for assigning 1/4 V to each node later
	      i_nindx(i_cnt) = i_invlist(p_nnod%def%i_indx)
	      
!	      DO j_cnt=1,p_ghand%i_nnumber
!	        IF (i_ngrid(j_cnt,i_tim) == p_nnod%def%i_indx) THEN
!	          i_nindx(i_cnt) = j_cnt
!		  EXIT
!	        END IF
!	      END DO
	      	
	    END DO node_loop
	    
	    r_vol= calc_volume(r_c(:,1), r_c(:,2), r_c(:,3), r_c(:,4))
	    
	    
	    node_loop_again: DO j_cnt=1,DEF_tetnodes
	      r_volum(i_nindx(j_cnt)) = r_volum(i_nindx(j_cnt)) + (r_1o4 * r_vol)
	    END DO node_loop_again
	  
	END DO integrat

	DEALLOCATE(i_invlist)

	i_siz= p_ghand%i_nnumber

	RETURN
	END SUBROUTINE grid_nodevol
!*****************************************************************
	SUBROUTINE grid_upstr_nodevol(p_ghand, i_siz, r_upstr, r_volum)



!---------- local declarations

	IMPLICIT NONE

	TYPE (grid_handle), INTENT(in) 			:: p_ghand
	INTEGER (KIND = GRID_SI), INTENT(inout)         			:: i_siz
	REAL (KIND = GRID_SR), DIMENSION(:)             			:: r_volum
	REAL (KIND = GRID_SR), DIMENSION(DEF_dimension, i_siz)		:: r_upstr
	REAL (KIND = GRID_SR) 			       			:: r_vol
	INTEGER (KIND = GRID_SI)                        			:: i_count, i_cnt, j_cnt, i_tim
	INTEGER (KIND = GRID_SI)						:: i_alct
        INTEGER (KIND = GRID_SI), DIMENSION(DEF_tetnodes)		:: i_nindx
	INTEGER (KIND = GRID_SI), DIMENSION(:), ALLOCATABLE		:: i_invlist
	
	REAL (KIND = GRID_SR), PARAMETER                			:: r_1o4= (1./DEF_tetnodes)
	REAL (KIND = GRID_SR), DIMENSION(DEF_dimension, DEF_tetnodes) 	:: r_c
	TYPE (node), POINTER           			:: p_ntmp, p_nnod
	TYPE (tetra), POINTER           		:: p_ttmp



	IF (size(r_volum) < p_ghand%i_nnumber) THEN
  	    CALL print_error(a_err='ERROR [grid_nodevol]: Size of volume array too small')								
	END IF


!---------- calculate volume pieces for each node
!---------- new idea: calculate volume of each tetra and add 1/4 to each node

	i_count = 0
	i_tim   = p_ghand%i_timetag
	r_volum = 0.0
	i_nindx = 0
	
	ALLOCATE(i_invlist(p_ghand%i_ntotal),stat=i_alct)
	IF(i_alct /= 0) THEN 
	  CALL print_error(a_err='ERROR [grid_nodevol]: Could not allocate memory')
	END IF
	i_invlist(:) = 0
	DO i_cnt=1,p_ghand%i_nnumber
	  i_invlist(i_ngrid(i_cnt,i_tim)) = i_cnt
	END DO
	
	integrat: DO i_count= 1, p_ghand%i_tnumfine
	    p_ttmp=> p_thash(i_tfine(i_count,i_tim))%tp 
	  
	    node_loop: DO i_cnt=1,DEF_tetnodes
	      r_c(:,i_cnt)=  r_upstr(:,p_ttmp%def%p_node(i_cnt))

!--------- FOR FINE GRID ONLY! p_nnod%def%i_indx gives the nodeindex in the normal, NOT the fine, grid
!--------- here the node indices (of the fine grid) are saved for assigning 1/4 V to each node later
	      i_nindx(i_cnt) = i_invlist(p_ttmp%def%p_node(i_cnt))
	      	
	    END DO node_loop
	    
	    r_vol= calc_volume(r_c(:,1), r_c(:,2), r_c(:,3), r_c(:,4))
	    
	    
	    node_loop_again: DO j_cnt=1,DEF_tetnodes
	      r_volum(i_nindx(j_cnt)) = r_volum(i_nindx(j_cnt)) + (r_1o4 * r_vol)
	    END DO node_loop_again
	  
	END DO integrat

	DEALLOCATE(i_invlist)

	i_siz= p_ghand%i_nnumber

	RETURN
	END SUBROUTINE grid_upstr_nodevol
!*****************************************************************
	SUBROUTINE grid_tetvol(p_ghand, i_siz, r_volum)



!---------- local declarations

	IMPLICIT NONE

	TYPE (grid_handle), INTENT(in) 			:: p_ghand
	INTEGER (KIND = GRID_SI), INTENT(inout)         			:: i_siz
	REAL (KIND = GRID_SR), DIMENSION(:)       	      		:: r_volum
	INTEGER (KIND = GRID_SI)                        			:: i_count, i_cnt, j_cnt, i_tim
	REAL (KIND = GRID_SR), DIMENSION(DEF_dimension, DEF_tetnodes) 	:: r_c
	TYPE (node), POINTER           			:: p_ntmp, p_nnod
	TYPE (tetra), POINTER           		:: p_ttmp



	IF (size(r_volum) < p_ghand%i_tnumfine) THEN
  	    CALL print_error(a_err='ERROR [grid_tetvol]: Size of volume array too small')								
	END IF


!---------- calculate volume of each tetrahedron in the mesh

	i_count = 0
	i_tim   = p_ghand%i_timetag
	r_volum = 0.0
	
	integrat: DO i_count= 1, p_ghand%i_tnumfine
	    p_ttmp=> p_thash(i_tfine(i_count,i_tim))%tp 
	  
	    node_loop: DO i_cnt=1,DEF_tetnodes
	      p_nnod    => p_nhash(p_ttmp%def%p_node(i_cnt))%np
	      r_c(:,i_cnt)=  p_nnod%def%r_coor
	    END DO node_loop
	    
	    r_volum(i_count)= calc_volume(r_c(:,1), r_c(:,2), r_c(:,3), r_c(:,4))
	  
	END DO integrat

	i_siz= p_ghand%i_tnumfine

	RETURN
	END SUBROUTINE grid_tetvol
!*****************************************************************
	SUBROUTINE grid_tetbaryc(p_ghand, i_siz, r_bary)



!---------- local declarations

	IMPLICIT NONE

	TYPE (grid_handle), INTENT(in) 			:: p_ghand
	INTEGER (KIND = GRID_SI), INTENT(inout)         			:: i_siz
	REAL (KIND = GRID_SR), DIMENSION(:,:), INTENT(inout)		:: r_bary
	INTEGER (KIND = GRID_SI)                        			:: i_count, i_cnt, i_tim
	REAL (KIND = GRID_SR), DIMENSION(DEF_dimension, DEF_tetnodes) 	:: r_c
	TYPE (node), POINTER           			:: p_ntmp, p_nnod
	TYPE (tetra), POINTER           		:: p_ttmp



	IF (size(r_bary(1,:)) < p_ghand%i_tnumfine) THEN
  	    CALL print_error(a_err='ERROR [grid_tetbarycenter]: Size of barycenter array too small')								
	END IF
	
	IF (size(r_bary(:,1)) /= DEF_dimension) THEN
  	    CALL print_error(a_err='ERROR [grid_tetbarycenter]: Dimension mismatch. This is 3D version')								
	END IF


!---------- calculate barycenter of each tetrahedron in the mesh

	i_count = 0
	i_tim   = p_ghand%i_timetag
	r_bary  = 0.0
	
	integrat: DO i_count= 1, p_ghand%i_tnumfine
	    p_ttmp=> p_thash(i_tfine(i_count,i_tim))%tp 
	  
	    node_loop: DO i_cnt=1,DEF_tetnodes
	      p_nnod    => p_nhash(p_ttmp%def%p_node(i_cnt))%np
	      r_c(:,i_cnt)=  p_nnod%def%r_coor
	    END DO node_loop
	    
	    r_bary(:,i_count)= calc_barycenter(r_c(:,1), r_c(:,2), r_c(:,3), r_c(:,4))
	  
	END DO integrat

	i_siz= p_ghand%i_tnumfine

	RETURN
	END SUBROUTINE grid_tetbaryc
!*****************************************************************
	END MODULE FEM_gridmanag




