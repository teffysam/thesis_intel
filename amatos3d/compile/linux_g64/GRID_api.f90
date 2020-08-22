!*****************************************************************
!
! MODULE NAME:
!	GRID_api
! FUNCTION:
!	provide a programming interface for the grid generator
! CONTAINS:
!-----------------------------------------------------------------
!
! NAME:
!	grid_definegeometry
! FUNCTION:
!	define outline of the computational domain (to be triangulated)
! SYNTAX:
!	CALL grid_definegeometry()
! ON INPUT:
!
! ON OUTPUT:
!
! CALLS:
!
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!	grid_createinitial
! FUNCTION:
!	create an initial (coarse) grid
! SYNTAX:
!	CALL grid_createinitial(grid.type)
! ON INPUT:
!	p_mesh: grid handle with initialized bounds	TYPE(grid_handle)
! ON OUTPUT:
!	p_mesh: modified grid handle			TYPE(grid_handle)
! CALLS:
!	grid_create, grid_globalrefine, print_error
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!	grid_getinfo
! FUNCTION:
!	get information out of all grid items
! SYNTAX:
!	CALL grid_getinfo(grid, int, logical, real.arr, real.arr, int, int.arr,
!	                  real.arr, real.arr, int.arr, int.arr, int.arr)
! ON INPUT:
!	p_mesh:      mesh handle				TYPE(grid_handle)
!	i_arrlen:    array length				INTEGER (KIND = GRID_SI)
!	l_finelevel: finest level only indicator		LOGICAL
!	i_valpoint:  pointer to real values (optional)		INTEGER (KIND = GRID_SI)
! ON OUTPUT:
!	r_nodecoordinates: coordinates of nodes         (opt.)	REAL (KIND = GRID_SR)
!	r_nodevalues:      values at nodes              (opt.)	REAL (KIND = GRID_SR)
!	i_elementnodes:    node indices of elements     (opt.)	INTEGER (KIND = GRID_SI)
!	r_elementcoordinates: coords. of elements nodes (opt.)	REAL (KIND = GRID_SR)
!	r_elementvalues:   values at elem. nodes        (opt.)	REAL (KIND = GRID_SR)
!	i_elementstatus:   element's status             (opt.)	INTEGER (KIND = GRID_SI)
!	i_elementlevel:    element's refinement level   (opt.)	INTEGER (KIND = GRID_SI)
!	i_elementmark:     element's marked edge index  (opt.)	INTEGER (KIND = GRID_SI)
! CALLS:
!
! COMMENTS:
!	this routine's behaviour is controlled via the given dummy
!	arguments.
!
!-----------------------------------------------------------------
!
! NAME:
!	grid_putinfo
! FUNCTION:
!	put information to all grid items
! SYNTAX:
!	CALL grid_putinfo(grid, int, logical, real.arr, int, real.arr, int.arr)
! ON INPUT:
!	p_mesh:      mesh handle				TYPE(grid_handle)
!	i_arrlen:    array length				INTEGER (KIND = GRID_SI)
!	l_finelevel: finest level only indicator		LOGICAL
!	i_valpoint:  pointer to real values (optional)		INTEGER (KIND = GRID_SI)
! ON OUTPUT:
!	r_nodevalues:      values at nodes              (opt.)	REAL (KIND = GRID_SR)
!	r_elementvalues:   values at elem. nodes        (opt.)	REAL (KIND = GRID_SR)
!	i_elementstatus:   element's status             (opt.)	INTEGER (KIND = GRID_SI)
! CALLS:
!
! COMMENTS:
!	this routine's behaviour is controlled via the given dummy
!	arguments.
!
!-----------------------------------------------------------------
!
! NAME:
!	grid_getiteminfo
! FUNCTION:
!	get information out of one grid item
! SYNTAX:
!	CALL grid_getiteminfo()
! ON INPUT:
!
! ON OUTPUT:
!
! CALLS:
!
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!	grid_coordvalue
! FUNCTION:
!	get (interpolated) value at given (arbitrary) coordinates
! SYNTAX:
!	real= grid_coordvalue(grid, real.arr, int, int)
! ON INPUT:
!	p_mesh:          mesh handle				TYPE(grid_handle)
!	r_coordinates:   coordinates				REAL (KIND = GRID_SR)
!	i_interpolorder: order of interpolation (optional)	INTEGER (KIND = GRID_SI)
!	i_valpoint:      pointer to value type (optional)	INTEGER (KIND = GRID_SI)
! ON OUTPUT:
!	r_value:         interpolated value			REAL (KIND = GRID_SR)
! CALLS:
!
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!	grid_terminate
! FUNCTION:
!	destroy grid data structures and terminate the grid generator
! SYNTAX:
!	CALL grid_terminate
! ON INPUT:
!
! ON OUTPUT:
!
! CALLS:
!
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!	grid_sweep
! FUNCTION:
!	clear grid from obsolecent data items
! SYNTAX:
!	CALL grid_sweep
! ON INPUT:
!
! ON OUTPUT:
!
! CALLS:
!
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!	grid_adapt
! FUNCTION:
!	adapt the grid according to certain marks inserted to grid items
! SYNTAX:
!	CALL grid_adapt(grid, logical)
! ON INPUT:
!	p_mesh: mesh handle data struct.	TYPE(grid_handle)
! ON OUTPUT:
!	p_mesh: modified mesh			TYPE(grid_handle)
!	l_changed: changed indicator		LOGICAL
! CALLS:
!
! COMMENTS:
!	assume that in a prior step, the elements are marked for refinement
!	resp. coarsening
!
!	l_changed= .TRUE.     if grid has changed
!	l_changed= .FALSE.    if there was nothing to do
!-----------------------------------------------------------------
!
! NAME:
!	grid_getpolyline
! FUNCTION:
!	get edge's node coordinated building a certain polyline
! SYNTAX:
!	CALL grid_getpolyline(grid, int, int, int, real.arr)
! ON INPUT:
!	p_mesh:     grid handle			TYPE(grid_handle)
!	i_linetype: type of polyline		INTEGER (KIND = GRID_SI)
!	i_arrlen:   length of array		INTEGER (KIND = GRID_SI)
! ON OUTPUT:
!	i_efflen:   filled array items		INTEGER (KIND = GRID_SI)
!	r_vertices: coordinates of polyline	REAL (KIND = GRID_SR)
! CALLS:
!
! COMMENTS:
!	two types of polylines are supported:
!	GRID_boundary:  polyline of boundary edges
!	GRID_partition: polyline of partition edges (only  in parallel)
!
!-----------------------------------------------------------------
!
! NAME:
!	grid_readsaveset (contained in module FEM_saveset, exported)
! FUNCTION:
!	read a saveset from disc
! SYNTAX:
!	CALL grid_readsaveset(char, grid.arr)
! ON INPUT:
!	c_file:   filename of output file	CHARACTER
! ON OUTPUT:
!	p_handle: an array of grid handles	TYPE (grid_handle)
! CALLS:
!
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!	grid_writesaveset (contained in module FEM_saveset, exported)
! FUNCTION:
!	write a saveset to disc
! SYNTAX:
!	CALL grid_writesaveset(char, grid.arr)
! ON INPUT:
!	c_file:   filename of input file	CHARACTER
!	p_handle: an array of grid handles	TYPE (grid_handle)
! ON OUTPUT:
!
! CALLS:
!
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!	grid_readdomain (contained in module FEM_saveset, exported)
! FUNCTION:
!	read the polygonal domain outlines from disc
! SYNTAX:
!	CALL grid_readdomain(int, real.arr, char)
! ON INPUT:
!	c_file:   filename of input file (opt.)	CHARACTER
! ON OUTPUT:
!	i_arrlen: length of array 		INTEGER (KIND = GRID_SI)
!	r_polyar: array with polygon		REAL (KIND = GRID_SR)
! CALLS:
!
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!	grid_timeduplicate (contained in module FEM_gridgen, exported)
! FUNCTION:
!	duplicate a grid by duplicating the linked lists
! SYNTAX:
!	CALL grid_timeduplicate(grid, grid)
! ON INPUT:
!	p_grid1: handle to original grid		TYPE (grid_handle)
! ON OUTPUT:
!	p_grid2: handle to duplicate grid		TYPE (grid_handle)
! CALLS:
!
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!	grid_timetoggle
! FUNCTION:
!	toggle time tags
! SYNTAX:
!	CALL grid_timetoggle
! ON INPUT:
!
! ON OUTPUT:
!
! CALLS:
!
! COMMENTS:
!	this is for readability only
!-----------------------------------------------------------------
!
! NAME:
!	grid_integral
! FUNCTION:
!	calculate the (global) integral over one grid value
! SYNTAX:
!	real= grid_integral(grid, int, real)
! ON INPUT:
!	p_mesh:      mesh handle			TYPE(grid_handle)
!	i_valpoint:  pointer to value type (optional)	INTEGER (KIND = GRID_SI)
!	r_watermark: watermark value       (optional)	REAL (KIND = GRID_SR)
!	l_lowbound:  bound for watermark   (optional)	LOGICAL
! ON OUTPUT:
!	r_value:     integrated value			REAL (KIND = GRID_SR)
! CALLS:
!
! COMMENTS:
!	a conditional integration is possible: calculate the integral
!	only for those elements of the grid, for which the "collumn
!	height" of the integrated scalar values is larger/smaller than
!	the watermark. l_lowbound has the meaning:
!	  l_lowbound == .TRUE.  : collumn larger than r_watermark
!	  l_lowbound == .FALSE. : collumn smaller than r_watermark
!
!-----------------------------------------------------------------
!
! NAME:
!	grid_setparameter
! FUNCTION:
!	set grid parameters (like refinement level, etc.)
! SYNTAX:
!	CALL grid_setparameter(grid, int, int)
! ON INPUT:
!	p_mesh:        mesh handle			TYPE(grid_handle)
!	i_coarselevel: max. coarsening level (optional)	INTEGER (KIND = GRID_SI)
!	i_finelevel:   max. refinement level (optional)	INTEGER (KIND = GRID_SI)
! ON OUTPUT:
!
! CALLS:
!
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!	grid_nodearea (contained in module FEM_gridmanag, exported)
! FUNCTION:
!	calculate area pieces for each node (for integration over the domain in R2)
! SYNTAX:
!	call grid_nodearea(grid, int, real.arr)
! ON INPUT:
!	p_handle: grid handle     (required)		TYPE (grid_handle)
! ON OUTPUT:
!	i_siz:    number of nodes (array size)		INTEGER (KIND = GRID_SI)
!	r_area:   array of area pieces for each node	REAL (KIND = GRID_SR)
! CALLS:
!
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!	grid_nodevolume (contained in module FEM_gridmanag, exported)
! FUNCTION:
!	calculate volume pieces for each node (for integration over the domain in R3)
! SYNTAX:
!	call grid_nodevolume(grid, int, real.arr)
! ON INPUT:
!	p_handle: grid handle     (required)			TYPE (grid_handle)
! ON OUTPUT:
!	i_siz:    number of nodes (array size)			INTEGER (KIND = GRID_SI)
!	r_volume:   array of volume pieces for each node	REAL (KIND = GRID_SR)
! CALLS:
!
! COMMENTS:
!
!-----------------------------------------------------------------
! NAME:
!	grid_tetbarycenter (contained in module FEM_gridmanag, exported)
! FUNCTION:
!	calculate the barycenter of tetras
! SYNTAX:
!	call grid_tetbarycenter(grid, int, real.arr)
! ON INPUT:
!	p_handle: grid handle     (required)			TYPE (grid_handle)
!       r_bary:	  preallocated array of barycenter coords	REAL (KIND = GRID_SR) arrXarr
!		  REAL (KIND = GRID_SR)(3) coordinates, No of Tetras in mesh
! ON OUTPUT:
!	i_siz:    number of nodes (array size)			INTEGER (KIND = GRID_SI)
!	r_bary:   array of tetbarycenter coords, tetra no.	REAL (KIND = GRID_SR) arrXarr
! CALLS:
!	grid_tetbaryc
! COMMENTS:
!
!-----------------------------------------------------------------
! NAME:
!	grid_tetvolume (contained in module FEM_gridmanag, exported)
! FUNCTION:
!	calculate the volume of all tetras
! SYNTAX:
!	call grid_tetvolume(grid, int, real.arr)
! ON INPUT:
!	p_handle: grid handle     (required)			TYPE (grid_handle)
!       r_volume: preallocated array for volume			REAL (KIND = GRID_SR) array
!		  Length: No of Tetras in mesh
! ON OUTPUT:
!	i_siz:    number of nodes (array size)			INTEGER (KIND = GRID_SI)
!	r_volume: array of volume, length: no. of tetra 	REAL (KIND = GRID_SR) array
! CALLS:
!	grid_tetvol
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!	grid_boundintersect
! FUNCTION:
!	calculate the intersection point of a line with the boundary
! SYNTAX:
!	real.arr= grid_boundintersect(grid, real.arr, real.arr, int)
! ON INPUT:
!	p_mesh:  grid handle				TYPE (grid_handle)
!	r_start: starting point coordinates of line	REAL (KIND = GRID_SR)
!	r_end:   ending point coordinates of line	REAL (KIND = GRID_SR)
! ON OUTPUT:
!	grid_boundintersect:  intersection coordinates	REAL (KIND = GRID_SR)
!	i_info:  information on success status		INTEGER (KIND = GRID_SI)
! CALLS:
!
! COMMENTS:
!	the i_info variable informs on success:
!	i_info=  0: successfully found an intersection point
!	i_info= -1: there is possibly no intersection!
!
!-----------------------------------------------------------------
!
! NAME:
!	grid_domaincheck
! FUNCTION:
!	check if a point is inside the domain (just using boundary edges)
! SYNTAX:
!	int= grid_domaincheck(grid, real.arr)
! ON INPUT:
!	p_mesh:       grid handle			TYPE (grid_handle)
!	r_coordinate: coordinate of point to check for	REAL (KIND = GRID_SR)
! ON OUTPUT:
!	grid_domaincheck: return value			INTEGER (KIND = GRID_SI)
! CALLS:
!
! COMMENTS:
!	the result of grid_domaincheck indicates:
!	 0: inside of the domain
!	-1: outside of the domain
!
!-----------------------------------------------------------------
!
! NAME:
!	grid_createdual
! FUNCTION:
!	create (and return) data structure for a dual mesh
! SYNTAX:
!	CALL grid_createdual(mesh, int, int, int.arr, real.arr)
! ON INPUT:
!	p_mesh:     grid handle				TYPE (grid_handle)
!	i_origlen:  number of nodes in mesh		INTEGER (KIND = GRID_SI)
! ON OUTPUT:
!	i_duallen:  number of nodes in dual mesh	INTEGER (KIND = GRID_SI)
!	i_dualface: array with face pointers		INTEGER (KIND = GRID_SI)
!	r_dualcoor: array with dual coordinates		REAL (KIND = GRID_SR)
! CALLS:
!
! COMMENTS:
!	In this 3D version of create_dual, the dual mesh is not truely what
!	one would normally consider a dual mesh. This dual mesh is extended
!	for ease of use in calculations.
!	r_dualcoor returns the dual coordinates, as usual. I.e. each primary
!	cell's barycenter is a dual coordinate, additionally, each primary
!	boundary face's barycenter is one dual coordinate. We added for each
!	inner edge one additional dual coordinate such that we can describe
!	the dual cell's faces by triangles.
!	So, i_dualface returns a collection or triangular indices to dual
!	coordinates (index list). For each primary node (last array dimension)
!	one gets a number of face tiles (middle array dimension) with three
!	vertex indices each (first array dimension).
!	Obtain the number of face tiles by size(i_dualface,DIM=2)
!
!-----------------------------------------------------------------
!
! NAME:
!	grid_destroydual
! FUNCTION:
!	destroy data structure for a dual mesh
! SYNTAX:
!	CALL grid_destroydual(int, int.arr, real.arr)
! ON INPUT:
!	i_duallen:  number of nodes in dual mesh	INTEGER (KIND = GRID_SI)
!	i_dualedge: array with edge pointers		INTEGER (KIND = GRID_SI)
!	r_dualcoor: array with dual coordinates		REAL (KIND = GRID_SR)
! ON OUTPUT:
!
! CALLS:
!
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!	grid_error
! FUNCTION:
!	simple error handling
! SYNTAX:
!	CALL grid_error(int, char)
! ON INPUT:
!	i_error: error code for error message	INTEGER (KIND = GRID_SI)
!	c_error: error message (string)		CHARACTER
! ON OUTPUT:
!
! CALLS:
!
! COMMENTS:
!	if i_error > 20, grid_error stops the execution and
!	prints an error message, otherwise a warning message
!	is printed and execution is resumed.
!
!-----------------------------------------------------------------
!
! NAME:
!
! FUNCTION:
!
! SYNTAX:
!
! ON INPUT:
!
! ON OUTPUT:
!
! CALLS:
!
! COMMENTS:
!
!-----------------------------------------------------------------
!
! PUBLIC:
!
! COMMENTS:
!	there are several routines to control the grid (documented above).
!	but there are also several state variables defined in this module
!	which can be accessed as part of the application programming
!	interface (api). these state variables are listed below
!
!	i_time:        current timestep toggle (should not be altered)
!	i_timeplus:    future timestep toggle  (should not be altered)
!	i_timeminus:   past timestep toggle    (should not be altered)
!	p_mesh:        grid handling data structure
! USES:
!
! LIBRARIES:
!
! REFERENCES:
!
! VERSION(S):
!	1. original version				j. behrens	9/97
!	2. FEM_utils, and intersection detection added	j. behrens	1/98
!	3. grid_nodevolume added			l. mentrup	6/02
!	4. grid_tetvolume, grid_tetbarycenter added     l. mentrup      9/02
!	   adjacency option added			l. mentrup
!	5. speed up in grid_getinfo		j.behrens/l.mentrup	3/03
!	6. grid_createdual adjusted for 3D		j. behrens	12/03
!
!*****************************************************************
	MODULE GRID_api
	USE MISC_globalparam
	USE MISC_error
	USE MISC_utils
	USE FEM_define
	USE FEM_handle
	USE FEM_utils
	USE FEM_create
	USE FEM_gridgen
	USE FEM_gridmanag
	USE FEM_interpolation
	USE FEM_inittriang
	USE FEM_saveset
        USE FEM_dataretrieve
	PRIVATE

!---------- global parameters

	INTEGER (KIND = GRID_SI), PARAMETER :: GRID_loworder=1  ! interpolation order
	INTEGER (KIND = GRID_SI), PARAMETER :: GRID_highorder=2 ! interpolation order
	INTEGER (KIND = GRID_SI), PARAMETER :: GRID_boundary=1  ! get boundary from grid_getpolyline
	INTEGER (KIND = GRID_SI), PARAMETER :: GRID_partition=2 ! get partition boundary
	INTEGER (KIND = GRID_SI), PARAMETER :: GRID_boundedges=3 ! get polyline along  boundary edges

!---------- export some grid generator parameters

	INTEGER (KIND = GRID_SI), PARAMETER :: GRID_ucomp= DEF_ucomp
	INTEGER (KIND = GRID_SI), PARAMETER :: GRID_vcomp= DEF_vcomp
	INTEGER (KIND = GRID_SI), PARAMETER :: GRID_wcomp= DEF_wcomp
	INTEGER (KIND = GRID_SI), PARAMETER :: GRID_phi= DEF_phi
	INTEGER (KIND = GRID_SI), PARAMETER :: GRID_zeta= DEF_zeta
	INTEGER (KIND = GRID_SI), PARAMETER :: GRID_tracer= DEF_tracer
	INTEGER (KIND = GRID_SI), PARAMETER :: GRID_elementorder= DEF_elorder
	INTEGER (KIND = GRID_SI), PARAMETER :: GRID_elementnodes= DEF_elnodes
	INTEGER (KIND = GRID_SI), PARAMETER :: GRID_elementedges= DEF_eledges
	INTEGER (KIND = GRID_SI), PARAMETER :: GRID_tetranodes= DEF_tetnodes
	INTEGER (KIND = GRID_SI), PARAMETER :: GRID_tetraedges= DEF_tetedges
	INTEGER (KIND = GRID_SI), PARAMETER :: GRID_tetraelements= DEF_tetelmts
	INTEGER (KIND = GRID_SI), PARAMETER :: GRID_elementchildren= DEF_elchild
	INTEGER (KIND = GRID_SI), PARAMETER :: GRID_edgenodes= DEF_egnodes
	INTEGER (KIND = GRID_SI), PARAMETER :: GRID_edgeelements= DEF_egelems
	INTEGER (KIND = GRID_SI), PARAMETER :: GRID_edgechildren= DEF_egchild
	INTEGER (KIND = GRID_SI), PARAMETER :: GRID_dimension= DEF_dimension
	INTEGER (KIND = GRID_SI), PARAMETER :: GRID_timesteps= DEF_timesteps
	INTEGER (KIND = GRID_SI), PARAMETER :: GRID_patchelements= DEF_ndpatch
	INTEGER (KIND = GRID_SI), PARAMETER :: GRID_elementvalues= DEF_evalsize
	INTEGER (KIND = GRID_SI), PARAMETER :: GRID_nodevalues=DEF_nvalsize

	INTEGER (KIND = GRID_SI), PARAMETER :: GRID_pleaserefine= DEF_pleasrefine
	INTEGER (KIND = GRID_SI), PARAMETER :: GRID_pleasecoarse= DEF_pleascoarse

	REAL (KIND = GRID_SR)               :: GRID_SIDDAY=DEF_SIDDAY
	REAL (KIND = GRID_SR)               :: GRID_GRAV  =DEF_GRAV
	REAL (KIND = GRID_SR)               :: GRID_EPS
	REAL (KIND = GRID_SR)               :: GRID_PI
	REAL (KIND = GRID_SR)               :: GRID_OMEGA
	REAL (KIND = GRID_SR)               :: GRID_RADIUS

!---------- global variables

	INTEGER (KIND = GRID_SI)            :: i_time      ! current time toggle
	INTEGER (KIND = GRID_SI)            :: i_timeplus  ! future time toggle
	INTEGER (KIND = GRID_SI)            :: i_timeminus ! past time toggle

!---------- we need a point which is definitely out of the domain

	REAL (KIND = GRID_SR), DIMENSION(DEF_dimension) :: r_outofdomain

!---------- public variables (and types!)

	PUBLIC :: grid_handle                 ! exported from FEM_handle
	PUBLIC :: GRID_parameters, grid_param ! exported from MISC_globalparam
    PUBLIC :: GRID_SR, GRID_DR, GRID_SI, GRID_DI ! exported from FEM_param

!---------- constants and variables

	PUBLIC :: GRID_elementorder,    GRID_elementnodes,  GRID_elementedges, &
	          GRID_elementchildren, GRID_edgenodes,     GRID_edgeelements, &
	          GRID_edgechildren,    GRID_dimension,     GRID_patchelements, &
	          GRID_elementvalues,   GRID_nodevalues,    GRID_pleaserefine, &
	          GRID_pleasecoarse,    GRID_timesteps,     GRID_tetranodes, &
	          GRID_tetraedges,      GRID_tetraelements
	PUBLIC :: GRID_RADIUS,          GRID_PI,            GRID_SIDDAY, &
	          GRID_GRAV,            GRID_OMEGA,         GRID_EPS
	PUBLIC :: GRID_loworder,        GRID_highorder,     GRID_ucomp, &
	          GRID_vcomp,           GRID_phi,           GRID_zeta, &
	          GRID_tracer,          GRID_boundary,      GRID_partition, &
		  GRID_boundedges,      GRID_wcomp
	PUBLIC :: i_time,               i_timeplus,         i_timeminus
	PUBLIC :: p_grid

!---------- routines and functions

	PUBLIC :: grid_definegeometry,  grid_createinitial, grid_getinfo, &
	          grid_putinfo,         grid_getiteminfo,   grid_coordvalue, &
		  grid_terminate,       grid_adapt,         grid_getpolyline, &
	          grid_setparameter,    grid_readinitial,   grid_boundintersect, &
	          grid_domaincheck,     grid_createdual,    grid_destroydual, &
		  grid_timeduplicate,   grid_timetoggle,    grid_sweep, &
		  grid_initialize,      grid_writesaveset,  grid_readsaveset, &
		  grid_readdomain,      grid_nodearea,      grid_edgelength, &
		  grid_nodevolume, 	grid_tetbarycenter, grid_tetvolume, &
		  grid_error,           grid_upstr_nodevolume
!	          grid_nodegradient,    grid_findelmt, &
		

	CONTAINS
!*****************************************************************
	SUBROUTINE grid_definegeometry(i_vertices, i_dimensions, i_polylines, &
	                               i_polymask, r_vertexarr)

!---------- local declarations

	IMPLICIT NONE

	INTEGER (KIND = GRID_SI), INTENT(in)                         :: i_vertices
	INTEGER (KIND = GRID_SI), INTENT(in), OPTIONAL               :: i_dimensions
	INTEGER (KIND = GRID_SI), INTENT(in), OPTIONAL               :: i_polylines
	INTEGER (KIND = GRID_SI), INTENT(in), DIMENSION(:), OPTIONAL :: i_polymask
	REAL (KIND = GRID_SR), INTENT(in), DIMENSION(:,:), OPTIONAL  :: r_vertexarr
	INTEGER (KIND = GRID_SI)                                     :: i_dim, i_pline
	LOGICAL                                     :: l_nopoly=.FALSE.
	INTEGER (KIND = GRID_SI)                                     :: i_alct

!---------- check for given dummy arguments

	dim_check: IF(.NOT. present(i_dimensions)) THEN
	  IF(GRID_parameters%iolog > 0) THEN
	    write(GRID_parameters%iolog,*) 'GRID_api [grid_definegeometry]: assuming dimension = 2'
	  END IF
	  i_dim= 2
	END IF dim_check

	poly_check: IF(present(i_polylines)) THEN
	  pmask_check: IF(.NOT. present(i_polymask)) THEN
	    IF(GRID_parameters%iolog > 0) THEN
	      write(GRID_parameters%iolog,*) 'GRID_api [grid_definegeometry]: no polyline mask given, assuming only one polyline'
	    END IF
	    i_pline= 1
	  END IF pmask_check
	ELSE poly_check
	  IF(GRID_parameters%iolog > 0) THEN
	    write(GRID_parameters%iolog,*) 'GRID_api [grid_definegeometry]: assuming only one polyline'
	  END IF
	  i_pline= 1
	END IF poly_check

	arr_check: IF(.NOT. present(r_vertexarr)) THEN
	  IF(GRID_parameters%iolog > 0) THEN
	    write(GRID_parameters%iolog,*) 'GRID_api [grid_definegeometry]: assuming unit sqare domain'
	  END IF
	  l_nopoly= .TRUE.

!---------- intermediate version defines a (unit) square

	  i_boundvertices= 4
	  ALLOCATE(r_boundpoly(DEF_dimension,i_boundvertices), stat= i_alct)
	  IF(i_alct /= 0) THEN
	    CALL print_error(199)
	  END IF
	  r_boundpoly(:,1)= (/ 0.0, 0.0 /) ! TWO-DIMENSIONAL!!!!!!!
	  r_boundpoly(:,2)= (/ 1.0, 0.0 /) ! TWO-DIMENSIONAL!!!!!!!
	  r_boundpoly(:,3)= (/ 1.0, 1.0 /) ! TWO-DIMENSIONAL!!!!!!!
	  r_boundpoly(:,4)= (/ 0.0, 1.0 /) ! TWO-DIMENSIONAL!!!!!!!

	END IF arr_check

!---------- intermediate version ignores polygon and defines a (unit) square

	given_poly: IF(.NOT. l_nopoly) THEN
	  i_boundvertices= i_vertices
	  ALLOCATE(r_boundpoly(DEF_dimension,i_boundvertices), stat= i_alct)
	  IF(i_alct /= 0) THEN
	    CALL print_error(199)
	  END IF
	  r_boundpoly= r_vertexarr
	END IF given_poly

!---------- determine a point lying out of the domain

	r_outofdomain= 2.* MAXVAL(r_boundpoly,DIM=2)
	r_outofdomain(1)= r_outofdomain(1)+ 0.25* ABS(r_outofdomain(1))

!---------- initialize and synchronize timestep toggles

	IF(DEF_timesteps == 2) THEN
	i_pasttime=    1
	i_currenttime= 1
	i_futuretime=  2
	ELSE IF(DEF_timesteps == 3) THEN
	i_pasttime=    1
	i_currenttime= 2
	i_futuretime=  3
	END IF
	i_time=        i_currenttime
	i_timeplus=    i_futuretime
	i_timeminus=   i_pasttime

!---------- initialize grid handle

	p_grid(i_timeminus)%i_timetag= i_timeminus
	p_grid(i_time)%i_timetag=      i_time
	p_grid(i_timeplus)%i_timetag=  i_timeplus

	RETURN
	END SUBROUTINE grid_definegeometry
!*****************************************************************
	SUBROUTINE grid_createinitial(p_mesh, c_filename)

!---------- local declarations

	IMPLICIT NONE

	TYPE (grid_handle), DIMENSION(DEF_timesteps)     :: p_mesh
	CHARACTER (LEN=GRID_parameters%i_stringlength), OPTIONAL  :: c_filename
	CHARACTER (LEN=GRID_parameters%i_stringlength)            :: c_nam

!---------- check parameters in grid data structure

	bound_check1: IF((p_mesh(i_time)%i_crslvlbnd <= 0) .OR. (p_mesh(i_time)%i_reflvlbnd <= 0)) THEN
	  call print_error(50)
	END IF bound_check1

	bound_check2: IF(p_mesh(i_time)%i_crslvlbnd > p_mesh(i_time)%i_reflvlbnd) THEN
	  call print_error(51)
	END IF bound_check2

!---------- initialize file name
	  file_present: IF(present(c_filename)) THEN
	    c_nam= c_filename
	  ELSE file_present
	    c_nam= 'Triang.dat'
	  END IF file_present

!---------- create a very coarse mesh and initialize hash data strutures

	CALL grid_create(p_mesh(i_time), c_name= c_nam)

!---------- create a very coarse mesh and initialize hash data strutures

	CALL grid_timeduplicate(p_mesh(i_time), p_mesh(i_timeplus))

!---------- now refine it to the desired level

	CALL grid_globalrefine(p_mesh(i_timeplus))

	RETURN
	END SUBROUTINE grid_createinitial
!*****************************************************************
	SUBROUTINE grid_readinitial(p_mesh, c_filename)

!---------- local declarations

	IMPLICIT NONE

	TYPE (grid_handle), DIMENSION(DEF_timesteps) :: p_mesh
	CHARACTER (LEN=32), OPTIONAL                 :: c_filename
	CHARACTER (LEN=32)                           :: c_filnam

!---------- initialize filename

	fil_present: IF(present(c_filename)) THEN
	  c_filnam= c_filename
	ELSE fil_present
	  write(c_filnam,*) trim(GRID_parameters%program_name), '_save.save'
	  c_filnam= adjustl(c_filnam)
	  IF(GRID_parameters%iolog > 0) THEN
	    write(GRID_parameters%iolog,*) 'GRID_api [grid_readinitial]: default file is: ',c_filnam
	  END IF
	END IF fil_present

!---------- create a very coarse mesh and initialize hash data strutures

	CALL grid_readsaveset(c_filnam, p_mesh)

!---------- create a very coarse mesh and initialize hash data strutures

	CALL grid_initfromsave(p_mesh)

	RETURN
	END SUBROUTINE grid_readinitial
!*****************************************************************
	FUNCTION grid_coordvalue(p_mesh, r_coordinate, i_interpolorder, i_valpoint, i_index) &
	RESULT (r_value)

!---------- local declarations

	IMPLICIT NONE

	TYPE (grid_handle)                         :: p_mesh
	REAL (KIND = GRID_SR), DIMENSION(DEF_dimension), INTENT(in) :: r_coordinate
	REAL (KIND = GRID_SR)                                       :: r_value
	INTEGER (KIND = GRID_SI), INTENT(in), OPTIONAL              :: i_interpolorder
	INTEGER (KIND = GRID_SI), INTENT(in), OPTIONAL              :: i_valpoint
	INTEGER (KIND = GRID_SI), INTENT(out), OPTIONAL             :: i_index
	REAL (KIND = GRID_SR)                                       :: r_eps, r_spe
	TYPE (tetra), POINTER                       :: p_tmp,p_tetra 
	INTEGER (KIND = GRID_SI)                                    :: i_iord, i_valp, i_itim, &
	  i_check, i_cnt
        LOGICAL                                    :: l_in

!--------- - first check, if coordinatevalue is inside the domain

	i_check= grid_domaincheck(p_mesh, r_coordinate)

!---------- if coordinatevalue is outside the domain just set interpolant to zero

	out_dom: IF(i_check /= 0) THEN
	  r_value= 0.0
	  IF(present(i_index)) i_index= -1
	  IF(GRID_parameters%iolog > 0) THEN
	    WRITE(GRID_parameters%iolog,*) 'GRID_api [grid3D_coordvalue]: coordinate outside the domain'
	    WRITE(GRID_parameters%iolog,*) '                            (x,y,z)= ',r_coordinate
	  END IF
	  RETURN
	END IF out_dom

!---------- initialize constant

	r_spe= 1.
	r_eps= epsilon(r_spe)
	i_itim= p_mesh%i_timetag

!---------- set default interpolation parameters

	order_present: IF(.NOT. present(i_interpolorder)) THEN
	  IF(GRID_parameters%iolog > 0) THEN
	    WRITE(GRID_parameters%iolog,*) 'GRID_api [grid3D_coordvalue]: assuming default (high) interpolation order'
	  END IF
	  i_iord= GRID_highorder
	ELSE order_present
	  i_iord= i_interpolorder
	END IF order_present

	value_present: IF(.NOT. present(i_valpoint)) THEN
	  IF(GRID_parameters%iolog > 0) THEN
	    WRITE(GRID_parameters%iolog,*) 'GRID_api [grid3D_coordvalue]: assuming default interpolation value (tracer)'
	  END IF
	  i_valp= GRID_tracer
	ELSE value_present
	  i_valp= i_valpoint
	END IF value_present

!---------- find tetrahedron containing coordinates

	nullify(p_tetra)
	p_tetra => grid_findtetra(r_coordinate, p_mesh)

!---------- watch out for the boundary...

	outside: IF(.NOT. associated(p_tetra)) THEN ! not yet implemented correctly
	  r_value= 0.0
	  IF(present(i_index)) i_index= 0
	  RETURN
	END IF outside
	IF(present(i_index)) i_index= p_tetra%def%i_indx

!---------- recognize interpolation order

	int_order: IF(i_interpolorder == GRID_loworder) THEN
	  r_value= grid_loorderinterpol(r_coordinate, p_tetra, i_valp, i_itim)
	ELSE IF(i_interpolorder == GRID_highorder) THEN int_order
	  r_value= grid_hiorderinterpol(r_coordinate, p_tetra, i_valp, i_itim)
	ELSE int_order
	  IF(GRID_parameters%iolog > 0) THEN
	    WRITE(GRID_parameters%iolog,*) 'GRID_api [grid3D_coordvalue]: this interpolation order is not available'
	  END IF
	END IF int_order

	RETURN
	END FUNCTION grid_coordvalue

!*****************************************************************
	SUBROUTINE grid_initialize(i_output, i_logging)

!---------- local declarations

	IMPLICIT NONE
	INTEGER (KIND = GRID_SI), OPTIONAL, INTENT(in) :: i_output
	INTEGER (KIND = GRID_SI), OPTIONAL, INTENT(in) :: i_logging
	REAL (KIND = GRID_SR)                          :: r_eins=1.
! 	REAL (KIND = GRID_SR)                          :: r_eumf=40000000. ! circumference of earth
	REAL (KIND = GRID_SR)                          :: r_eumf=1.        ! circumference of unit sphere
	LOGICAL                       :: l_log, l_red
	CHARACTER (LEN=32)            :: c_file
	INTEGER (KIND = GRID_SI)                       :: i_fst

!---------- check input

	out_present: IF(present(i_output)) THEN
	  l_red= .TRUE.
	ELSE out_present
	  l_red= .FALSE.
	END IF out_present

	log_present: IF(present(i_logging)) THEN
	  l_log= .TRUE.
	ELSE log_present
	  l_log= .FALSE.
	END IF log_present

!---------- initialize logging

	log_on: IF(l_log) THEN
	  GRID_parameters%iolog= i_logging

!---------- open log file

	  write(c_file,*) trim(GRID_parameters%program_name), '.log'
	  c_file= adjustl(c_file)
	  open(GRID_parameters%iolog, file=c_file, form='formatted', iostat= i_fst) 
	  log_opened: IF(i_fst /= 0) THEN
	    CALL print_error(111)
	  END IF log_opened
	  write(GRID_parameters%iolog,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++'
          write(GRID_parameters%iolog,*) ' LOGFILE: ', c_file
	  write(GRID_parameters%iolog,*) ' UNIT:    ', GRID_parameters%iolog
	  write(GRID_parameters%iolog,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++'
	ELSE log_on
	  GRID_parameters%iolog= -1
	END IF log_on

!---------- redirect output

	out_redir: IF(l_red) THEN
	  GRID_parameters%ioout= 8

!---------- open output file

	  write(c_file,*) trim(GRID_parameters%program_name), '.out'
	  c_file= adjustl(c_file)
	  open(GRID_parameters%ioout, file=c_file, form='formatted', iostat= i_fst) 
	  out_opened: IF(i_fst /= 0) THEN
	    CALL print_error(112)
	  END IF out_opened
	  IF(GRID_parameters%iolog > 0) THEN
	    write(GRID_parameters%iolog,*) 'INFO: Output file ', c_file, ' opened on unit: ', GRID_parameters%ioout
	  END IF
	ELSE out_redir
	  GRID_parameters%ioout= 6
	END IF out_redir

!---------- initialize constant PI, and machine precision EPS

	GRID_PI= 4.* atan(r_eins)
	GRID_EPS= epsilon(r_eins)
	GRID_OMEGA= 2.* DEF_PI/ DEF_SIDDAY
	GRID_RADIUS= r_eumf

!---------- set internal constants

	DEF_PI=     GRID_PI
	DEF_EPS=    GRID_EPS
	DEF_OMEGA=  GRID_OMEGA
	DEF_RADIUS= GRID_RADIUS

	RETURN
	END SUBROUTINE grid_initialize
!*****************************************************************
	SUBROUTINE grid_terminate

!---------- local declarations

	IMPLICIT NONE

!---------- free grid data structure

	CALL grid_remove(p_grid)

!---------- deallocate boundary polygon

	IF(ALLOCATED(r_boundpoly)) THEN
	  DEALLOCATE(r_boundpoly)
	END IF

	RETURN
	END SUBROUTINE grid_terminate
!*****************************************************************
	SUBROUTINE grid_adapt(p_mesh, l_changed)

!---------- local declarations

	IMPLICIT NONE

	TYPE (grid_handle), INTENT(inout)      :: p_mesh
	LOGICAL, INTENT(out)                   :: l_changed
	INTEGER (KIND = GRID_SI), DIMENSION(:), ALLOCATABLE     :: i_aux
	INTEGER (KIND = GRID_SI)                                :: i_alct, i_size, i_crs, i_ref
	INTEGER (KIND = GRID_SI)                                :: i_save1, i_save2

!---------- initialize refinement flag

	l_changed= .FALSE.
	i_save1= p_mesh%i_tnumber

!---------- allocate work arrays

	i_size= p_mesh%i_tnumfine
	allocate(i_aux(i_size), stat=i_alct)
	not_alloc: IF(i_alct /= 0) THEN
	  CALL print_error(83)
	END IF not_alloc

!---------- extract info on marked elements

	CALL grid_getinfo(p_mesh, i_size, l_finelevel=.TRUE., i_tetrastatus= i_aux)
	i_ref= count(i_aux == DEF_pleasrefine)
	i_crs= count(i_aux == DEF_pleascoarse)

!---------- refine the grid

	IF(i_ref > 0) THEN
	  CALL grid_refine(p_mesh)
	END IF
	i_save2= p_mesh%i_tnumber

!---------- coarsen the grid

	IF(i_crs > 0) THEN
	  CALL grid_coarse(p_mesh)
	END IF

!---------- deallocate arrays

	deallocate(i_aux)

!---------- set grid changed flag

	grid_change: IF((i_save1 /= p_mesh%i_tnumber) .OR. &
	                (i_save2 /= p_mesh%i_tnumber)) THEN
	  l_changed= .TRUE.
	END IF grid_change

	RETURN
	END SUBROUTINE grid_adapt
!*****************************************************************
	SUBROUTINE grid_getpolyline(p_mesh, i_linetype, i_arrlen, i_efflen, r_vertices)

!---------- local declarations

	IMPLICIT NONE

	TYPE(grid_handle)                       :: p_mesh
	INTEGER (KIND = GRID_SI), INTENT(in)                     :: i_linetype
	INTEGER (KIND = GRID_SI), INTENT(in)                     :: i_arrlen
	INTEGER (KIND = GRID_SI), INTENT(out)                    :: i_efflen
	REAL (KIND = GRID_SR), DIMENSION(DEF_dimension,i_arrlen) :: r_vertices
	INTEGER (KIND = GRID_SI)                        :: i_cnt, i_ind, j_cnt, i_tim
	TYPE (node), POINTER           :: p_ntmp, p_ndif
	TYPE (edge), POINTER           :: p_gtmp

!---------- decide what to do

	type_choice: IF(i_linetype == GRID_boundedges) THEN

!---------- supply the boundary polygon line

	i_tim= p_mesh%i_timetag
	i_cnt= 1
	i_ind= 1
	DO j_cnt= 1, p_mesh%i_gnumboun
	p_gtmp => p_ghash(i_gboun(j_cnt, i_tim))%gp
	  IF(i_cnt > 1) THEN
	    p_ndif => p_nhash(p_gtmp%def%p_node(i_ind))%np
	    IF(p_ntmp%def%i_indx == p_ndif%def%i_indx) THEN
	      i_ind= 3- i_ind
	    END IF
	  END IF
	  p_ntmp             => p_nhash(p_gtmp%def%p_node(i_ind))%np
	  r_vertices(:,i_cnt)=  p_ntmp%def%r_coor
	  i_cnt              =  i_cnt+ 1
	END DO

	i_efflen= i_cnt- 1

	ELSE IF (i_linetype == GRID_boundary) THEN type_choice
	  IF(i_boundvertices > i_arrlen) THEN
	    CALL print_error(199)
	  END IF
	  i_efflen= i_boundvertices
	  r_vertices(:,1:i_efflen)= r_boundpoly(:,1:i_efflen)
	ELSE IF (i_linetype == GRID_partition) THEN type_choice
	  CONTINUE ! not supported in serial implementation...
	ELSE type_choice
	  IF(GRID_parameters%iolog > 0) THEN
	    WRITE(GRID_parameters%iolog,*) 'GRID_api [grid_getpolyline]: this line type is not available'
	  END IF
	END IF type_choice

	RETURN
	END SUBROUTINE  grid_getpolyline
!*****************************************************************
	SUBROUTINE grid_timetoggle

!---------- local declarations

	IMPLICIT NONE

	INTEGER (KIND = GRID_SI)               :: i_tmp

!---------- shift times

	IF(DEF_timesteps == 2) THEN
	  i_tmp         = i_currenttime ! <--- only two times
	  i_currenttime = i_futuretime  ! <--- only two times
	  i_futuretime  = i_tmp         ! <--- only two times
	  i_pasttime    = i_currenttime ! <--- for compatibility only
	ELSE IF (DEF_timesteps == 3) THEN
	  i_tmp         = i_pasttime
	  i_pasttime    = i_currenttime
	  i_currenttime = i_futuretime
	  i_futuretime  = i_tmp
	END IF

!---------- synchronize times

	i_time=        i_currenttime
	i_timeplus=    i_futuretime
	i_timeminus=   i_pasttime

	RETURN
	END SUBROUTINE grid_timetoggle

!*****************************************************************
	FUNCTION grid_integral(p_mesh, i_valpoint, r_watermark, l_lowbound) &
	RESULT (r_value)

!---------- local declarations

	IMPLICIT NONE
	TYPE (grid_handle)                         :: p_mesh
	REAL (KIND = GRID_SR)                                       :: r_value
	INTEGER (KIND = GRID_SI), INTENT(in), OPTIONAL              :: i_valpoint
	REAL (KIND = GRID_SR), INTENT(in), OPTIONAL                 :: r_watermark
	LOGICAL, INTENT(in), OPTIONAL              :: l_lowbound
	INTEGER (KIND = GRID_SI)                                    :: i_valp, i_time
	INTEGER (KIND = GRID_SI)                                    :: i_cnt, j_cnt
	REAL (KIND = GRID_SR), DIMENSION(DEF_dimension,DEF_elnodes) :: r_coo
	REAL (KIND = GRID_SR), DIMENSION(DEF_elnodes)               :: r_val
	REAL (KIND = GRID_SR)                                       :: r_are, r_c
	REAL (KIND = GRID_SR), PARAMETER                            :: r_div=(1./DEF_elnodes)
	TYPE (elmt), POINTER                       :: p_etmp
	TYPE (node), POINTER                       :: p_ntmp
	LOGICAL                                    :: l_wat
	REAL (KIND = GRID_SR)                                       :: r_inv

!---------- set timestamp

	i_time= p_mesh%i_timetag

!---------- initialize result

	r_value= 0.0

!---------- set default integration parameters

	value_present: IF(.NOT. present(i_valpoint)) THEN
	  IF(GRID_parameters%iolog > 0) THEN
	    WRITE(GRID_parameters%iolog,*) 'GRID_api [grid_integral]: assuming default integration value (tracer)'
	  END IF
	  i_valp= GRID_tracer
	ELSE value_present
	  i_valp= i_valpoint
	END IF value_present

	water_present: IF(present(r_watermark)) THEN
	  l_wat= .TRUE.
	  sign_present: IF(.NOT. present(l_lowbound)) THEN
	    IF(GRID_parameters%iolog > 0) THEN
	      WRITE(GRID_parameters%iolog,*) 'GRID_api [grid_integral]: assuming default sign for watermark'
	    END IF
	    r_inv= 1.0
	  ELSE sign_present
	    IF(l_lowbound) THEN
	      r_inv= 1.0
	    ELSE
	      r_inv= -1.0
	    END IF
	  END IF sign_present
	ELSE water_present
	  l_wat= .FALSE.
	END IF water_present

!---------- loop over all elements

	elmt_loop: DO i_cnt= 1,p_mesh%i_enumfine
	  p_etmp=> p_ehash(i_efine(i_cnt,i_time))%ep

!---------- calculate area of element, first collect information

	  node_loop: DO j_cnt= 1,DEF_elnodes
	    p_ntmp=> p_nhash(p_etmp%def%p_node(j_cnt))%np
	    r_coo(:,j_cnt)= p_ntmp%def%r_coor(:)
	    r_val(j_cnt)  = p_ntmp%att%r_vals(i_valp,i_time)
	  END DO node_loop

!---------- area is calculated by the vector crossproduct (2-dim.)

	  r_are= calc_area(r_coo(:,1), r_coo(:,2), r_coo(:,3))

!---------- now this is the "collumn height" and the global integral

	  r_c    = SUM(r_val)* r_div
	  water_mark: IF(l_wat) THEN
	    IF((r_inv*r_c) > (r_inv*r_watermark)) r_value= r_value+ (r_are* r_c)
	  ELSE water_mark
	    r_value= r_value+ (r_are* r_c)
	  END IF water_mark

	END DO elmt_loop

	RETURN
	END FUNCTION grid_integral

!*****************************************************************
	SUBROUTINE grid_setparameter(p_mesh, i_coarselevel, i_finelevel)

!---------- local declarations

	IMPLICIT NONE
	TYPE (grid_handle), DIMENSION(DEF_timesteps) :: p_mesh
	INTEGER (KIND = GRID_SI), OPTIONAL                            :: i_coarselevel
	INTEGER (KIND = GRID_SI), OPTIONAL                            :: i_finelevel

!---------- check for coarse parameter

	crs_present: IF(present(i_coarselevel)) THEN
	  p_mesh(:)%i_crslvlbnd= i_coarselevel
	END IF crs_present

!---------- check for fine parameter

	fin_present: IF(present(i_finelevel)) THEN
	  p_mesh(:)%i_reflvlbnd= i_finelevel
	END IF fin_present

	RETURN
	END SUBROUTINE grid_setparameter

!*****************************************************************
	SUBROUTINE grid_nodearea(p_mesh, i_siz, r_area)

!---------- local declarations

	IMPLICIT NONE
	TYPE (grid_handle), INTENT(in) :: p_mesh
	INTEGER (KIND = GRID_SI), INTENT(inout)         :: i_siz
	REAL (KIND = GRID_SR), DIMENSION(:)             :: r_area

	CALL grid_nodear(p_mesh, i_siz, r_area)

	RETURN
	END SUBROUTINE grid_nodearea
!*****************************************************************
	SUBROUTINE grid_nodevolume(p_mesh, i_siz, r_volume)

!---------- local declarations

	IMPLICIT NONE
	TYPE (grid_handle), INTENT(in) :: p_mesh
	INTEGER (KIND = GRID_SI), INTENT(inout)         :: i_siz
	REAL (KIND = GRID_SR), DIMENSION(:)             :: r_volume

	CALL grid_nodevol(p_mesh, i_siz, r_volume)

	RETURN
	END SUBROUTINE grid_nodevolume
!*****************************************************************
	SUBROUTINE grid_upstr_nodevolume(p_mesh, i_siz, r_upstr, r_volume)

!---------- local declarations

	IMPLICIT NONE
	TYPE (grid_handle), INTENT(in) 		:: p_mesh
	REAL (KIND = GRID_SR), DIMENSION(:,:)			:: r_upstr		
	INTEGER (KIND = GRID_SI), INTENT(inout)         		:: i_siz
	REAL (KIND = GRID_SR), DIMENSION(:)             		:: r_volume

	CALL grid_upstr_nodevol(p_mesh, i_siz, r_upstr, r_volume)

	RETURN
	END SUBROUTINE grid_upstr_nodevolume
!*****************************************************************
	SUBROUTINE grid_tetbarycenter(p_mesh, i_siz, r_bary)

!---------- local declarations

	IMPLICIT NONE
	TYPE (grid_handle), INTENT(in) 		:: p_mesh
	INTEGER (KIND = GRID_SI), INTENT(inout)         		:: i_siz
	REAL (KIND = GRID_SR), DIMENSION(:,:), INTENT(inout) 	:: r_bary

	CALL grid_tetbaryc(p_mesh, i_siz, r_bary)

	RETURN
	END SUBROUTINE grid_tetbarycenter
!*****************************************************************
	SUBROUTINE grid_tetvolume(p_mesh, i_siz, r_volume)

!---------- local declarations

	IMPLICIT NONE
	TYPE (grid_handle), INTENT(in) 		:: p_mesh
	INTEGER (KIND = GRID_SI), INTENT(inout)         		:: i_siz
	REAL (KIND = GRID_SR), DIMENSION(:), INTENT(inout)	:: r_volume

	CALL grid_tetvol(p_mesh, i_siz, r_volume)

	RETURN
	END SUBROUTINE grid_tetvolume
!*****************************************************************
	SUBROUTINE grid_sweep

!---------- local declarations

	IMPLICIT NONE

	CALL grid_clean

	RETURN
	END SUBROUTINE grid_sweep
!*****************************************************************
	FUNCTION grid_domaincheck(p_mesh, r_coordinate) RESULT (i_inout)

!---------- local declarations

	IMPLICIT NONE
	TYPE (grid_handle), INTENT(in)             :: p_mesh
	REAL (KIND = GRID_SR), DIMENSION(DEF_dimension), INTENT(in) :: r_coordinate
	INTEGER (KIND = GRID_SI)                                    :: i_inout
	INTEGER (KIND = GRID_SI)                                    :: i_intersects, i_cnt, &
	  j_cnt, i_stat, i_tim
	LOGICAL                                    :: l_dupl
	TYPE (edge), POINTER                       :: p_gtmp
	TYPE (node), POINTER                       :: p_nod
	REAL (KIND = GRID_SR), DIMENSION(DEF_dimension)             :: r_coo1, r_coo2, r_intersect
	INTEGER (KIND = GRID_SI), PARAMETER                         :: i_maxsect=16
	REAL (KIND = GRID_SR), DIMENSION(DEF_dimension,i_maxsect)   :: r_oldsect
	REAL (KIND = GRID_SR)                                       :: r_al1, r_al2, r_tmp, r_eps

	TYPE (tetra), POINTER                      :: p_tmp
	INTEGER (KIND = GRID_SI)                                    :: i_iord, i_valp, i_itim, &
	  i_check
        LOGICAL                                    :: l_in

!--------- - first check, if coordinatevalue is inside the domain
!            THIS IS A VERY DIRTY HACK!!!

	i_inout= -1
	tetra_loop: DO i_cnt=1,6
	  p_tmp => p_thash(i_cnt)%tp
	  l_in = THREEDIM_in_out(r_coordinate,p_tmp)
	  IF (l_in) THEN
	    i_inout=0
	    EXIT tetra_loop
	  END IF
	END DO tetra_loop

! !---------- initialize result and other things
! 
! 	i_tim       = p_mesh%i_timetag
! 	i_inout     = 0
! 	i_intersects= 0
! 	r_oldsect   = 0.0
! 	r_tmp       = 1.0
! 	r_eps       = epsilon(r_tmp)
! 
! !---------- loop through all boundary edges
! 
! 	bound_loop: DO i_cnt= 1, p_mesh%i_gnumboun
! 
! !---------- get edges nodes
! 
! 	  p_gtmp => p_ghash(i_gboun(i_cnt, i_tim))%gp
! 	  p_nod  => p_nhash(p_gtmp%def%p_node(1))%np
! 	  r_coo1 =  p_nod%def%r_coor
! 	  p_nod  => p_nhash(p_gtmp%def%p_node(2))%np
! 	  r_coo2 =  p_nod%def%r_coor
! 
! !---------- check if coordinate lies on edge, then it is inside (2D ONLY)
! 
! 	  IF(r_coo1(1) == r_coo2(1) .AND. r_coo1(1) == r_coordinate(1)) THEN
! 	    IF(r_coordinate(2) <= MAX(r_coo1(2),r_coo2(2)) .AND. &
! 	       r_coordinate(2) >= MIN(r_coo1(2),r_coo2(2))) THEN
! 	      RETURN
! 	    END IF
!           END IF
! 	  IF(r_coo1(2) == r_coo2(2) .AND. r_coo1(2) == r_coordinate(2)) THEN
! 	    IF(r_coordinate(1) <= MAX(r_coo1(1),r_coo2(1)) .AND. &
! 	       r_coordinate(1) >= MIN(r_coo1(1),r_coo2(1))) THEN
! 	      RETURN
! 	    END IF
!           END IF
! !	  r_tmp= abs(r_coo1(1))
! !	  r_al1= ((r_coo2(1)+ r_tmp)+ (r_coordinate(1)+ r_tmp))/ &
! !	         ((r_coo1(1)+ r_tmp)+ (r_coo2(1)+ r_tmp))
! !	  r_tmp= abs(r_coo1(2))
! !	  r_al2= ((r_coo2(2)+ r_tmp)+ (r_coordinate(2)+ r_tmp))/ &
! !	         ((r_coo1(2)+ r_tmp)+ (r_coo2(2)+ r_tmp))
! !	  IF(r_al1 == r_al2) RETURN
! 
! !---------- determine if there is an intersection
! 
! 	  r_intersect= edge_intersect(r_coo1, r_coo2, r_coordinate, &
! 	                              r_outofdomain, i_info=i_stat)
! 
! !---------- if this is an intersection, then count it
! 
! 	  inter_count: IF(i_stat == 0) THEN
! 	    IF(i_intersects >= i_maxsect) THEN
! 	      CALL print_error(199)
! 	    END IF
! 
! !---------- check for duplicate intersections
! 
! 	    l_dupl= .TRUE.
! 	    DO j_cnt=1,i_intersects
! 	      r_coo1= r_intersect- r_oldsect(:,j_cnt)
! 	      IF(abs(r_coo1(1)) < r_eps .AND. abs(r_coo1(2)) < r_eps) THEN
! 	        l_dupl= .FALSE.
! 	      END IF
! 	    END DO
! 	    IF(l_dupl) THEN
! 	      i_intersects= i_intersects+ 1
! 	      r_oldsect(:,i_intersects)= r_intersect(:)
! 	    END IF
! 	  END IF inter_count
! 	END DO bound_loop
! 
! !---------- in case of i_intersects even, we are outside, otherwise inside!
! 
! 	even_odd: IF(MOD(i_intersects,2) == 0) THEN
! 	  i_inout= -1
! 	END IF even_odd

	RETURN
	END FUNCTION grid_domaincheck
!*****************************************************************
	FUNCTION grid_boundintersect(p_mesh, r_start, r_end, i_info) RESULT (r_intersect)

!---------- local declarations

	IMPLICIT NONE
	TYPE (grid_handle), INTENT(in)             :: p_mesh
	REAL (KIND = GRID_SR), DIMENSION(DEF_dimension), INTENT(in) :: r_start, r_end
	INTEGER (KIND = GRID_SI), OPTIONAL                          :: i_info
	REAL (KIND = GRID_SR), DIMENSION(DEF_dimension)             :: r_intersect
	INTEGER (KIND = GRID_SI)                                    :: i_cnt, i_stat, i_tim
	TYPE (elmt), POINTER                       :: p_etmp
	TYPE (node), POINTER                       :: p_nod
	REAL (KIND = GRID_SR), DIMENSION(DEF_dimension)             :: r_coo1, r_coo2, r_coo3

!---------- initialize result and info

	i_tim= p_mesh%i_timetag
	r_intersect= 0.0
	i_info= -1

!---------- loop through the boundary elements

	bound_loop: DO i_cnt= 1, p_mesh%i_enumboun

!---------- get edges nodes

	  p_etmp => p_ehash(i_eboun(i_cnt, i_tim))%ep
	  p_nod  => p_nhash(p_etmp%def%p_node(1))%np
	  r_coo1 =  p_nod%def%r_coor
	  p_nod  => p_nhash(p_etmp%def%p_node(2))%np
	  r_coo2 =  p_nod%def%r_coor
	  p_nod  => p_nhash(p_etmp%def%p_node(3))%np
	  r_coo3 =  p_nod%def%r_coor

!---------- determine if there is an intersection

	  r_intersect= element_intersect(r_coo1, r_coo2, r_coo3, r_start, r_end, i_info=i_stat)

!---------- check if this is really an intersection

	  check_inter: IF(i_stat == 0) THEN
	    i_info= 0
	    EXIT bound_loop
	  END IF check_inter
	END DO bound_loop

	DO i_cnt=1,DEF_dimension
	  IF(abs(r_intersect(i_cnt)) < DEF_EPS) r_intersect(i_cnt)= 0.0
	END DO

	RETURN
	END FUNCTION grid_boundintersect
!*****************************************************************
	SUBROUTINE grid_createdual(p_mesh, i_origlen, i_duallen, i_dualface, r_dualcoor)

!---------- local declarations

	IMPLICIT NONE
	TYPE (grid_handle), INTENT(in)             :: p_mesh
	INTEGER (KIND = GRID_SI), INTENT(in)                        :: i_origlen
	INTEGER (KIND = GRID_SI), INTENT(out)                       :: i_duallen
	INTEGER (KIND = GRID_SI), DIMENSION(:,:,:), POINTER         :: i_dualface
	REAL (KIND = GRID_SR), DIMENSION(:,:), POINTER              :: r_dualcoor
	INTEGER (KIND = GRID_SI), DIMENSION(:), ALLOCATABLE         :: i_eaux, i_taux, i_gaux, &
	  i_ncnt, i_ninv
	INTEGER (KIND = GRID_SI)                                    :: i_size1, i_size2, i_tim, &
	  i_cnt, i_tnc, j_cnt, i_alct, i_nme, i_gme, i_eme, i_gcnt, i_fcnt, &
	  i_ccnt, i_tmpduallen, i_size3, i_size4, i_dualfacecnt, i_cn1, i_cn2, &
	  i_nod1, i_nod2, i_dual, i_dn, i_tet1, i_tet2
	TYPE (tetra), POINTER                      :: p_ttmp, p_ttet
	TYPE (elmt), POINTER                       :: p_etmp
	TYPE (edge), POINTER                       :: p_gtmp
	TYPE (node), POINTER                       :: p_ntmp1, p_ntmp2, p_ntmp
	INTEGER (KIND = GRID_SI), DIMENSION(2,2*DEF_ndpatch)        :: i_tmpedges
	REAL (KIND = GRID_SR), DIMENSION(DEF_dimension,2*DEF_ndpatch) :: r_tmpcoords
	REAL (KIND = GRID_SR), DIMENSION(DEF_dimension,DEF_tetnodes)  :: r_points
	REAL (KIND = GRID_SR), DIMENSION(DEF_dimension)             :: r_centr
	REAL (KIND = GRID_SR), PARAMETER                            :: r_third=1./3.
	INTEGER (KIND = GRID_SI), DIMENSION(2*DEF_ndpatch)          :: i_done, i_facedone
	INTEGER (KIND = GRID_SI), DIMENSION(:,:,:), POINTER         :: i_tmpdf
	REAL (KIND = GRID_SR), DIMENSION(:,:), POINTER              :: r_tmpdc

!---------- check dimensions

	IF(p_mesh%i_nnumber /= i_origlen) THEN
	  CALL print_error(a_err='[grid_createdual]: Inconsistent array length!')
	END IF

!---------- set time

	i_tim= p_mesh%i_timetag

!---------- initialize

	i_duallen= 0

!---------- allocate index arrays

	i_size1= p_mesh%i_gtotal
	i_size2= p_mesh%i_etotal
	i_size3= p_mesh%i_ttotal
	i_size4= p_mesh%i_ntotal
	allocate(i_gaux(i_size1), i_eaux(i_size2), i_taux(i_size3), &
	         i_ncnt(i_size4), i_ninv(i_size4), stat=i_alct)
	IF(i_alct /= 0) CALL print_error(a_err='[grid_createdual]: Auxiliary arrays could not be allocated')
	i_gaux= 0; i_eaux= 0; i_taux= 0; i_ncnt= 0; i_ninv= 0

!---------- allocate dual coordinate array

	i_size1= p_mesh%i_enumfine+ p_mesh%i_enumboun+ p_mesh%i_gnumber
	allocate(r_tmpdc(DEF_dimension,i_size1), stat=i_alct)
	IF(i_alct /= 0) CALL print_error(a_err='[grid_createdual]: Temporary dual coordinate array could not be allocated')

!---------- loop through the elements and create dual grid points

	tetra_loop: DO i_cnt=1, p_mesh%i_tnumfine
	  p_ttmp=> p_thash(i_tfine(i_cnt,i_tim))%tp

!---------- first mid point

	  point_loop: DO j_cnt=1, DEF_tetnodes
	    p_ntmp=> p_nhash(p_ttmp%def%p_node(j_cnt))%np
	    r_points(:,j_cnt)= p_ntmp%def%r_coor(:)
	  END DO point_loop

!---------- new dual point

	  i_duallen                = i_duallen+ 1
	  r_tmpdc(:,i_duallen)  = calc_barycenter(r_points(:,1), r_points(:,2), &
	                                             r_points(:,3), r_points(:,4))
	  i_taux(p_ttmp%def%i_indx)= i_duallen
	END DO tetra_loop

!---------- dual points at boundary edges

! 	boun_loop: DO i_cnt= 1, p_mesh%i_enumboun
! 	  p_etmp=> p_ehash(i_eboun(i_cnt,i_tim))%ep
	boun_loop: DO i_cnt= 1, p_mesh%i_enumfine
	  p_etmp=> p_ehash(i_efine(i_cnt,i_tim))%ep
	  IF((p_etmp%att%p_tets(1) == 0) .OR. (p_etmp%att%p_tets(2) == 0)) THEN

!---------- find nodes

	  DO j_cnt=1,DEF_elnodes
	    p_ntmp=> p_nhash(p_etmp%def%p_node(j_cnt))%np
	    r_points(:,j_cnt)= p_ntmp%def%r_coor(:)
	  END DO

!---------- new dual point

	  i_duallen                = i_duallen+ 1
	  r_tmpdc(:,i_duallen)  = (r_points(:,1)+ r_points(:,2)+ r_points(:,3))* r_third
	  i_eaux(p_etmp%def%i_indx)= i_duallen
	  END IF
	END DO boun_loop

!---------- create inverse index list

	DO i_cnt=1,p_mesh%i_nnumber
	  i_ninv(i_ngrid(i_cnt,i_tim))= i_cnt
	END DO

!---------- allocate edges array

	i_size1= p_mesh%i_nnumber
	i_size2= 2* DEF_egelems* DEF_ndpatch ! this is a heuristic fudge value
	i_size3= 0
	allocate(i_tmpdf(3,i_size2,i_size1), stat=i_alct)
	IF(i_alct /= 0) CALL print_error(a_err='[grid_createdual]: Temporary dual index array could not be allocated')
	i_tmpdf= 0

!---------- loop over all edges of fine grid, and contribute faces to corr. nodes

	edge_loop: DO i_cnt=1,p_mesh%i_gnumfine
	  p_gtmp => p_ghash(i_gfine(i_cnt,i_tim))%gp

!---------- the two nodes this edge contributes to

	  P_ntmp1=> p_nhash(p_gtmp%def%p_node(1))%np
	  i_nod1 =  i_ninv(p_ntmp1%def%i_indx)
	  P_ntmp2=> p_nhash(p_gtmp%def%p_node(2))%np
	  i_nod2 =  i_ninv(p_ntmp2%def%i_indx)
	  i_cn1  =  i_ncnt(i_nod1)
	  i_cn2  =  i_ncnt(i_nod2)

!---------- create additional dual point at edge's midpoint

	  r_centr= (P_ntmp1%def%r_coor+ P_ntmp2%def%r_coor)* 0.5
	  i_duallen= i_duallen+ 1
	  r_tmpdc(:,i_duallen)= r_centr(:)

	  i_done= 0; i_dn= 0

!---------- loop over all faces surrounding this edge

	  face_loop: DO j_cnt=1,DEF_egelems
	    IF(p_gtmp%att%p_elem(j_cnt) == 0) exit face_loop
	    p_etmp=> p_ehash(p_gtmp%att%p_elem(j_cnt))%ep

!---------- go through adjacent tetras/ boundary face

	    i_cn1= i_cn1+1; i_cn2= i_cn2+1
	    i_tet1= p_etmp%att%p_tets(1)
	    IF(i_tet1 /= 0) THEN
! 	      IF(.NOT. ANY(i_done==i_tet1)) THEN
! 	        i_dn= i_dn+1
! 		i_done(i_dn)= i_tet1
		i_tmpdf(1,i_cn1,i_nod1)=i_taux(i_tet1) 
		i_tmpdf(1,i_cn2,i_nod2)=i_taux(i_tet1) 
! 	      END IF
	    ELSE
	      i_tmpdf(1,i_cn1,i_nod1)=i_eaux(p_etmp%def%i_indx) 
	      i_tmpdf(1,i_cn2,i_nod2)=i_eaux(p_etmp%def%i_indx) 
	    END IF

	    i_tet2= p_etmp%att%p_tets(2)
	    IF(i_tet2 /= 0) THEN
! 	      IF(.NOT. ANY(i_done==i_tet2)) THEN
! 	        i_dn= i_dn+1
! 		i_done(i_dn)= i_tet2
		i_tmpdf(2,i_cn1,i_nod1)=i_taux(i_tet2) 
		i_tmpdf(2,i_cn2,i_nod2)=i_taux(i_tet2) 
! 	      END IF
	    ELSE
	      i_tmpdf(2,i_cn1,i_nod1)=i_eaux(p_etmp%def%i_indx) 
	      i_tmpdf(2,i_cn2,i_nod2)=i_eaux(p_etmp%def%i_indx) 
	    END IF
	    i_tmpdf(3,i_cn1,i_nod1)= i_duallen
	    i_tmpdf(3,i_cn2,i_nod2)= i_duallen
	    
	  END DO face_loop
	  i_ncnt(i_nod1)= i_cn1
	  i_ncnt(i_nod2)= i_cn2
	END DO edge_loop

! !---------- loop over nodes to create dual elements
! 
! 	node_loop: DO i_cnt=1,p_mesh%i_nnumber
! 	  p_ntmp=> p_nhash(i_ngrid(i_cnt,i_tim))%np
! 	  i_nme  =  p_ntmp%def%i_indx
! 	  i_done=  0
! 	  i_gcnt=  0
! 	  i_dualfacecnt= 0
! 
! !---------- loop over elements in node's patch
! 
! 	  ptch_loop: DO j_cnt=1,p_ntmp%att%i_ptch(i_tim)
! 	    p_ttmp=> p_thash(p_ntmp%att%p_ptch(j_cnt,i_tim))%tp
! 
! !---------- find adjacent edges
! 
! 	    adj_loop: DO i_tnc=1, DEF_tetedges
! 	      p_gtmp=> p_ghash(p_ttmp%def%p_edge(i_tnc))%gp
! 	      i_gme =  p_gtmp%def%i_indx
! 	      i_tmpedges= 0
! 	      r_tmpcoords= 0.0
! 	      i_fcnt    = 0
! 	      i_ccnt    = 0
! 	      adj_chek: IF((p_gtmp%def%p_node(1) == i_nme) .OR. &
! 	                   (p_gtmp%def%p_node(2) == i_nme)) THEN
! 
! !---------- loop through elements of one edge, the corresponding tetra's dual nodes form a face
! 
! 	        egelem_loop: DO i_cn1=1, DEF_egelems
! 		  IF(p_gtmp%att%p_elem(i_cn1) == 0) EXIT egelem_loop
! 		  p_etmp=> p_ehash(p_gtmp%att%p_elem(i_cn1))%ep
! 		  i_fcnt= i_fcnt+1
! 
! !---------- the edges create dual edges, we will use them later for triangle face construction
! 
! 		  IF(p_etmp%att%p_tets(1) /= 0) THEN
! 		    i_tmpedges(1,i_fcnt)= i_taux(p_etmp%att%p_tets(1))
! 		  ELSE
! 		    i_tmpedges(1,i_fcnt)= i_eaux(p_etmp%def%i_indx)
! 		  END IF
! 		  i_ccnt= i_ccnt+1
! 		  r_tmpcoords(:,i_ccnt)= r_tmpdc(:,i_tmpedges(1,i_fcnt))
! 		  IF(p_etmp%att%p_tets(2) /= 0) THEN
! 		    i_tmpedges(2,i_fcnt)= i_taux(p_etmp%att%p_tets(2))
! 		  ELSE
! 		    i_tmpedges(2,i_fcnt)= i_eaux(p_etmp%def%i_indx)
! 		  END IF
! 		  i_ccnt= i_ccnt+1
! 		  r_tmpcoords(:,i_ccnt)= r_tmpdc(:,i_tmpedges(2,i_fcnt))
! 
! !---------- create an additional dual node, the midpoint of such a face, or use the one already created
! 
! 	          IF(i_gaux(p_gtmp%def%i_indx) /= 0) THEN
! 		    i_tmpduallen= i_gaux(p_gtmp%def%i_indx)
! 		  ELSE
! 		    r_centr= 0.0
! 		    DO i_cn2= 1,i_ccnt
! 		      r_centr= r_centr+ r_tmpcoords(:,i_cn2)
! 		    END DO
! 		    r_centr= r_centr/real(i_ccnt)
! 		    i_duallen= i_duallen+1
! 		    r_tmpdc(:,i_duallen)= r_centr
! 		    i_gaux(p_gtmp%def%i_indx)= i_duallen
! 		    i_tmpduallen= i_duallen
! 		  END IF
! 
! !---------- now collect the information into the triangular face data struct.
! 
! 		  DO i_cn2=1,i_fcnt
! 		    i_dualfacecnt= i_dualfacecnt+1
! 		    IF(i_dualfacecnt > i_size2) THEN
! 		      CALL print_error(a_err='[grid_createdual]: too many dual faces... resize array!')
! 		    END IF
! 		    i_tmpdf(1,i_dualfacecnt,i_cnt)= i_tmpedges(1,i_cn2)
! 		    i_tmpdf(2,i_dualfacecnt,i_cnt)= i_tmpedges(2,i_cn2)
! 		    i_tmpdf(3,i_dualfacecnt,i_cnt)= i_tmpduallen
! 		  END DO
! 		END DO egelem_loop
! 	      END IF adj_chek
! 	    END DO adj_loop
! 	  END DO ptch_loop
! 	  i_tmpdf(:,i_dualfacecnt+1:i_size2,i_cnt)= 0
! 	  i_size3= max(i_size3,i_dualfacecnt)
! 	END DO node_loop

!---------- allocate the true output arrays and copy results

	i_size3= maxval(i_ncnt)
! 	write(*,*) ' i_size3= ',i_size3
	allocate(r_dualcoor(DEF_dimension, i_duallen), &
	         i_dualface(3, i_size3, i_size1), stat=i_alct)
	IF(i_alct /= 0) CALL print_error(a_err='[grid_createdual]: Dual arrays could not be allocated')
	r_dualcoor(:,1:i_duallen)= r_tmpdc(:,1:i_duallen)
	i_dualface(:,1:i_size3,1:i_size1)= i_tmpdf(:,1:i_size3,1:i_size1)

!---------- deallocate temporary arrays

	deallocate(i_gaux, i_eaux, i_taux, i_ncnt, i_ninv, r_tmpdc, i_tmpdf)

	RETURN
	END SUBROUTINE grid_createdual

!*****************************************************************
	SUBROUTINE grid_destroydual(i_duallen, i_dualface, r_dualcoor)

!---------- local declarations

	IMPLICIT NONE
	INTEGER (KIND = GRID_SI), INTENT(in)                        :: i_duallen
	INTEGER (KIND = GRID_SI), DIMENSION(:,:,:), POINTER         :: i_dualface
	REAL (KIND = GRID_SR), DIMENSION(:,:), POINTER              :: r_dualcoor

!---------- deallocate arrays

!	IF(associated(i_dualface)) THEN
	  deallocate(i_dualface)
!	END IF
	nullify(i_dualface)
!	IF(associated(r_dualcoor)) THEN
	  deallocate(r_dualcoor)
!	END IF
	nullify(r_dualcoor)

	RETURN
	END SUBROUTINE grid_destroydual
!*****************************************************************
	SUBROUTINE grid_edgelength(p_mesh, r_max, r_min, r_edgelength)

!---------- local declarations

	IMPLICIT NONE
	TYPE (grid_handle), INTENT(in)             :: p_mesh
	REAL (KIND = GRID_SR), OPTIONAL                             :: r_min, r_max
	REAL (KIND = GRID_SR), DIMENSION(:), OPTIONAL               :: r_edgelength
	REAL (KIND = GRID_SR), DIMENSION(:,:), ALLOCATABLE          :: r_coo
	REAL (KIND = GRID_SR), DIMENSION(:), ALLOCATABLE            :: r_aux
	INTEGER (KIND = GRID_SI), DIMENSION(:,:), ALLOCATABLE       :: i_aux
	INTEGER (KIND = GRID_SI)                                    :: i_nlen, i_glen, i_alct, &
	  i_cnt
	REAL (KIND = GRID_SR), DIMENSION(DEF_dimension)             :: r_dif

!---------- gather information

	i_glen= p_mesh%i_gnumfine
	i_nlen= p_mesh%i_nnumber
	allocate(r_coo(DEF_dimension,i_nlen), stat=i_alct)
	not_alloc1: IF(i_alct /= 0) THEN
	  CALL print_error(119)
	END IF not_alloc1
	allocate(r_aux(i_glen), stat=i_alct)
	not_alloc2: IF(i_alct /= 0) THEN
	  CALL print_error(119)
	END IF not_alloc2
	allocate(i_aux(DEF_egnodes,i_glen), stat=i_alct)
	not_alloc3: IF(i_alct /= 0) THEN
	  CALL print_error(119)
	END IF not_alloc3
	CALL grid_getinfo(p_mesh, i_glen, l_finelevel=.TRUE., &
	                  i_edgenodes=i_aux, l_relative=.TRUE.)
	CALL grid_getinfo(p_mesh, i_nlen, r_nodecoordinates=r_coo)

!---------- calculate edge lenghts !!! FOR ONLY 2 EDGE NODES !!!

	main_loop: DO i_cnt=1,i_glen
	  r_dif(:)= r_coo(:,i_aux(1,i_cnt))- r_coo(:,i_aux(2,i_cnt))
	  r_aux(i_cnt)= euklid_norm(r_dif)
	END DO main_loop

!---------- now configure output...

	max_reqst: IF(present(r_max)) THEN
	  r_max= maxval(r_aux)
	END IF max_reqst
	min_reqst: IF(present(r_min)) THEN
	  r_min= minval(r_aux)
	END IF min_reqst
	vec_reqst: IF(present(r_edgelength)) THEN
	  IF(size(r_edgelength) < i_glen) THEN
	    CALL print_error(120)
	  END IF
	  r_edgelength(1:i_glen)= r_aux(1:i_glen)
	END IF vec_reqst

!---------- deallocate temporary arrays

	deallocate(i_aux, r_aux, r_coo)

	RETURN
	END SUBROUTINE grid_edgelength
!*****************************************************************
	SUBROUTINE grid_readsaveset(c_file, p_handle)

!---------- local declarations

	IMPLICIT NONE
	CHARACTER (len=32)                             :: c_file
	TYPE (grid_handle), DIMENSION(DEF_timesteps)   :: p_handle

!---------- call routine

	CALL grid_rsaveset(c_file, p_handle)

!---------- update time variables

	i_time=        i_currenttime
	i_timeplus=    i_futuretime
	i_timeminus=   i_pasttime

	RETURN
	END SUBROUTINE grid_readsaveset
!*****************************************************************
	SUBROUTINE grid_writesaveset(c_file, p_handle)

!---------- local declarations

	IMPLICIT NONE
	CHARACTER (len=32)                             :: c_file
	TYPE (grid_handle), DIMENSION(DEF_timesteps)   :: p_handle

!---------- call routine

	CALL grid_wsaveset(c_file, p_handle)

	RETURN
	END SUBROUTINE grid_writesaveset

!*****************************************************************
	SUBROUTINE grid_error(i_error,c_error)

!---------- local declarations

	IMPLICIT NONE

	INTEGER (KIND = GRID_SI), INTENT(in), optional     :: i_error
	CHARACTER(*),intent(in), optional :: c_error

	IF(present(i_error)) THEN
	  IF(present(c_error)) THEN
	    CALL print_error(i_err=i_error, a_err=c_error)
	  ELSE
	    CALL print_error(i_err=i_error)
	  ENDIF
	ELSE IF(present(c_error)) THEN
	  CALL print_error(a_err=c_error)
	ELSE
	  CALL print_error(i_err=99)
	END IF

	END SUBROUTINE grid_error

!*****************************************************************
!	SUBROUTINE 
!
!---------- local declarations
!
!	IMPLICIT NONE
!
!	RETURN
!	END SUBROUTINE 

	END MODULE GRID_api


