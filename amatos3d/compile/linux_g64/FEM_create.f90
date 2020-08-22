!*****************************************************************
!
! MODULE NAME:
!	FEM_create
! FUNCTION:
!	FEM creation routines
! CONTAINS:
!-----------------------------------------------------------------
!
! NAME:
!	create_elmt
! FUNCTION:
!	create a new element (includes allocation, insertion,...)
! SYNTAX:
!	elmt.ptr= create_elmt(int.arr, int.arr, int.arr, int, int, int, &
!	                      int, int.arr, int)
! ON INPUT:
!	p_nodes:	node indices			INTEGER (KIND = GRID_SI)
!	p_edges:	edges				INTEGER (KIND = GRID_SI)
!	p_tetras:	neighboring tetras   (optional)	INTEGER (KIND = GRID_SI)
!	i_mark:		marked edge index    (optional)	INTEGER (KIND = GRID_SI)
!	i_boundary	type of bound. cond. (optional)	INTEGER (KIND = GRID_SI)
!	i_level:	gridlevel of element (optional)	INTEGER (KIND = GRID_SI)
!	i_stat: 	flag of element      (optional)	INTEGER (KIND = GRID_SI)
!	p_children:	children             (optional)	INTEGER (KIND = GRID_SI)
!	p_mother:	pointer to mother    (optional)	INTEGER (KIND = GRID_SI)
! ON OUTPUT:
!	pe_new:		pointer to this new element	type(elmt) pointer
! CALLS:
!	print_error, hash_realloc
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!	create_tetra
! FUNCTION:
!	create a new tetrahedron (includes allocation, insertion,...)
! SYNTAX:
!	tetra.ptr= create_elmt(int.arr, int.arr, int.arr, int, int, &
!	                      int.arr, int)
! ON INPUT:
!	p_nodes:	node indices			INTEGER (KIND = GRID_SI)
!	p_edges:	edges				INTEGER (KIND = GRID_SI)
!	p_elmts:	element indices			INTEGER (KIND = GRID_SI)
!	i_level:	gridlevel of element (optional)	INTEGER (KIND = GRID_SI)
!	i_stat: 	flag of tetrahedron    (optional)	INTEGER (KIND = GRID_SI)
!	p_children:	children             (optional)	INTEGER (KIND = GRID_SI)
!	p_mother:	pointer to mother    (optional)	INTEGER (KIND = GRID_SI)
! ON OUTPUT:
!	pe_new:		pointer to this new tetrahedron	type(tetra) pointer
! CALLS:
!	print_error, hash_realloc
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!	create_edge
! FUNCTION:
!	create a new edge (includes allocation, insertion,...)
! SYNTAX:
!	edge.ptr= create_edge(int.arr, int.arr, &
!                             int, int, int.arr, int)
! ON INPUT:
!	p_nodes:	node indices			INTEGER (KIND = GRID_SI)
!	p_elements:	neighboring elements (optional)	INTEGER (KIND = GRID_SI)
!	i_boundary:	type of bound. cond. (optional)	INTEGER (KIND = GRID_SI)
!	i_stat:		status (refined,...) (optional)	INTEGER (KIND = GRID_SI)
!	p_children:	children             (optional)	INTEGER (KIND = GRID_SI)
!	p_periodic:	periodic partner     (optional)	INTEGER (KIND = GRID_SI)
! ON OUTPUT:
!	pg_new:		pointer to this new node	type(node) pointer
! CALLS:
!	print_error, hash_realloc
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!	create_node
! FUNCTION:
!	create a new node (includes allocation, insertion,...)
! SYNTAX:
!	node.ptr= create_node(real_arr, int, int, int)
! ON INPUT:
!	r_coordinates:	coordinates for node		REAL (KIND = GRID_SR)
!	i_stat:		refinement status    (optional)	INTEGER (KIND = GRID_SI)
!	p_edge:		pointer edge         (optional)	INTEGER (KIND = GRID_SI)
!	p_periodic:	periodic counterpart (optional)	INTEGER (KIND = GRID_SI)
! ON OUTPUT:
!	pn_new:		pointer to this new node	type(node) pointer
! CALLS:
!	print_error, hash_realloc
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!	destroy_elmt
! FUNCTION:
!	destroy the data structure of one element and free memory
! SYNTAX:
!	call destroy_elmt(elmt.ptr, grid)
! ON INPUT:
!	pe_old:	pointer to element to destroy	TYPE (elmt) POINTER
!	p_ghand: grid handle 			TYPE (grid_handle)
! ON OUTPUT:
!
! CALLS:
!
! COMMENTS:
!	this deallocates the element and frees memory
!
!-----------------------------------------------------------------
!
! NAME:
!	destroy_tetra
! FUNCTION:
!	destroy the data structure of one tetrahedron and free memory
! SYNTAX:
!	call destroy_tetra(tetra.ptr, grid)
! ON INPUT:
!	pt_old:	pointer to element to destroy	TYPE (tetra) POINTER
!	p_ghand: grid handle 			TYPE (grid_handle)
! ON OUTPUT:
!
! CALLS:
!
! COMMENTS:
!	this deallocates the tetrahedron and frees memory
!
!-----------------------------------------------------------------
!
! NAME:
!	destroy_edge
! FUNCTION:
!	destroy the data structure of one edge and free memory
! SYNTAX:
!	call destroy_edge(edge.ptr, grid)
! ON INPUT:
!	pg_old:	pointer to edge to destroy	TYPE (edge) POINTER
!	p_ghand: grid handle			TYPE (grid_handle)
! ON OUTPUT:
!
! CALLS:
!
! COMMENTS:
!	this deallocates the edge
!
!-----------------------------------------------------------------
!
! NAME:
!	destroy_node
! FUNCTION:
!	destroy the data structure of one node and free memory
! SYNTAX:
!	call destroy_node(node.ptr, grid)
! ON INPUT:
!	pn_old:	pointer to node to destroy	TYPE (node) POINTER
!	p_ghand: grid handle			TYPE (grid_handle)
! ON OUTPUT:
!
! CALLS:
!
! COMMENTS:
!	this deallocates node
!
!-----------------------------------------------------------------
!
! NAME:
!	erase_elmt
! FUNCTION:
!	erase an element from the corresponding grid
! SYNTAX:
!	call erase_elmt(elmt.ptr, grid)
! ON INPUT:
!	pe_old:	pointer to element to erase	TYPE (elmt) POINTER
!	p_ghand: grid handle			TYPE (grid_handle)
! ON OUTPUT:
!	pg_old: modified element		TYPE (elmt) POINTER
! CALLS:
!
! COMMENTS:
!	this just sets the status to "erased"
!
!-----------------------------------------------------------------
!
! NAME:
!	erase_tetra
! FUNCTION:
!	erase a tetrahedron from the corresponding grid
! SYNTAX:
!	call erase_tetra(tetra.ptr, grid)
! ON INPUT:
!	pt_old:	pointer to element to erase	TYPE (tetra) POINTER
!	p_ghand: grid handle			TYPE (grid_handle)
! ON OUTPUT:
!	pg_old: modified element		TYPE (tetra) POINTER
! CALLS:
!
! COMMENTS:
!	this just sets the status to "erased"
!
!-----------------------------------------------------------------
!
! NAME:
!	erase_edge
! FUNCTION:
!	erase an edge from the corresponding grid
! SYNTAX:
!	CALL erase_edge(edge.ptr, grid)
! ON INPUT:
!	pg_old:	pointer to edge to erase	TYPE (edge) POINTER
!	p_ghand: grid handle			TYPE (grid_handle)
! ON OUTPUT:
!	pg_old: modified element		TYPE (edge) POINTER
! CALLS:
!
! COMMENTS:
!	this just sets the status to "erased"
!
!-----------------------------------------------------------------
!
! NAME:
!	erase_node
! FUNCTION:
!	erase a node from the corresponding grid
! SYNTAX:
!	call erase_node(node.ptr, grid)
! ON INPUT:
!	pn_old:	pointer to node to erase	TYPE (node) POINTER
!	p_ghand: grid handle			TYPE (grid_handle)
! ON OUTPUT:
!	pn_old: modified node			TYPE (node) POINTER
! CALLS:
!
! COMMENTS:
!	this just sets the status to "erased"
!
!-----------------------------------------------------------------
!
! NAME:
!	hash_initalloc
! FUNCTION:
!	allocate (initial) hash table
! SYNTAX:
!	CALL hash_initalloc(int, int, char)
! ON INPUT:
!	i_init: assumed array size (optional)	INTEGER (KIND = GRID_SI)
!	c_action: kind of array	   (optional)	CHARACTER
! ON OUTPUT:
!	i_size:	array size			integer
! CALLS:
!	print_error
! COMMENTS:
!	there is a default initial array size of 1024 items, when
!	i_init is not given. the allocated array reside in PUBLIC
!	memory
!
!-----------------------------------------------------------------
!
! NAME:
!	hash_realloc
! FUNCTION:
!	reallocate hash table (enlarge or lessen)
! SYNTAX:
!	CALL hash_realloc(int, int, char)
! ON INPUT:
!	i_old: current size of array		INTEGER (KIND = GRID_SI)
!	i_new: new size of array		INTEGER (KIND = GRID_SI)
!	c_action: kind of array	   (optional)	CHARACTER
! ON OUTPUT:
!	i_old: set to new array size		INTEGER (KIND = GRID_SI)
! CALLS:
!	print_error
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!	hash_pack
! FUNCTION:
!	pack the hashing tables if grid objects have been removed
!	lessen the size if possible
! SYNTAX:
!	CALL hash_pack(grid)
! ON INPUT:
!	p_ghand: grid handle data struct.	TYPE (grid_handle)
! ON OUTPUT:
!	p_ghand: modified grid handle		TYPE (grid_handle)
! CALLS:
!	print_error, hash_realloc, hash_fineupdate
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!	hash_gridupdate
! FUNCTION:
!	update grid index array structures
! SYNTAX:
!	CALL hash_gridupdate(grid)
! ON INPUT:
!	p_ghand: grid handle data struct.	TYPE (grid_handle)
! ON OUTPUT:
!	p_ghand: modified grid handle		TYPE (grid_handle)
! CALLS:
!	hash_realloc
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!	hash_fineupdate
! FUNCTION:
!	update finest level (and boundary) index array structures
! SYNTAX:
!	CALL hash_fineupdate(grid)
! ON INPUT:
!	p_ghand: grid handle data struct.	TYPE (grid_handle)
! ON OUTPUT:
!	p_ghand: modified grid handle		TYPE (grid_handle)
! CALLS:
!	hash_realloc
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!	clearup_grid
! FUNCTION:
!	clear (remove) obsolete grid items (elements, edges, nodes) from grid
! SYNTAX:
!	CALL clearup_grid(grid)
! ON INPUT:
!	p_cleardrid: handle to grid		TYPE (grid_handle)
! ON OUTPUT:
!	p_cleardrid: reset handle		TYPE (grid_handle)
! CALLS:
!	erase_node, erase_edge, erase_elmt, hash_pack
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!	grid_clean
! FUNCTION:
!	clear (remove) obsolete grid items (elements, edges, nodes) from
!	old grid and free memory (to be called after ALL calculations
!	on old grid have been done)
! SYNTAX:
!	CALL grid_clean
! ON INPUT:
!
! ON OUTPUT:
!
! CALLS:
!	destroy_node, destroy_edge, destroy_elmt, hash_pack, renew_patches
! COMMENTS:
!	deletes items from the oldest grid
!
!-----------------------------------------------------------------
!
! NAME:
!	renew_patches
! FUNCTION:
!	update the links in p_ptch data structure of nodes
! SYNTAX:
!	CALL renew_patches(grid)
! ON INPUT:
!	p_ghand: handle to grid		TYPE (grid_handle)
! ON OUTPUT:
!
! CALLS:
!	print_error
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!	grid_initdatastruct
! FUNCTION:
!	initialize hashing tables (grid data structures)
! SYNTAX:
!	CALL grid_initdatastruct(char)
! ON INPUT:
!	c_action: action to be taken (optional)		CHAR
! ON OUTPUT:
!
! CALLS:
!	hash_initalloc
! COMMENTS:
!	if c_action == 'size' then read appropriate array sizes
!	   from constants "i_ehashmax", "i_ghashmax", ...
!	else initialize arrays in default sizes
!
!-----------------------------------------------------------------
!	
! PUBLIC:
!	all
! COMMENTS:
!
! USES:
!	MISC_globalparam, MISC_error, FEM_define, FEM_handle
! LIBRARIES:
!
! REFERENCES:
!
! VERSION(S):
!	1. original version			j. behrens	10/97
!	[2. parallel version			j. behrens	1/98]
!	3. bug fixes, hash_gridupdate		j. behrens	2/98
!
!*****************************************************************
	MODULE FEM_create
	  USE MISC_globalparam
	  USE MISC_error
	  USE FEM_define
	  USE FEM_handle
	  PUBLIC
	  CONTAINS
!*****************************************************************
	FUNCTION create_elmt(p_nodes, &
	           p_edges, p_tetras, i_mark, i_boundary, i_level, i_stat, &
	           p_children, p_mother, p_devideedge) RESULT (pe_new)

!---------- local declarations

	IMPLICIT NONE

	TYPE (elmt), POINTER                        :: pe_new
	INTEGER (KIND = GRID_SI), INTENT(in), DIMENSION(DEF_elnodes) :: p_nodes
	INTEGER (KIND = GRID_SI), INTENT(in), DIMENSION(DEF_eledges) :: p_edges
	INTEGER (KIND = GRID_SI), INTENT(in), OPTIONAL               :: i_mark
	INTEGER (KIND = GRID_SI), INTENT(in), OPTIONAL               :: i_boundary
	INTEGER (KIND = GRID_SI), INTENT(in), OPTIONAL               :: i_level
	INTEGER (KIND = GRID_SI), INTENT(in), OPTIONAL               :: i_stat
	INTEGER (KIND = GRID_SI), INTENT(in), &
	     DIMENSION(DEF_elchild), OPTIONAL       :: p_children
	INTEGER (KIND = GRID_SI), INTENT(in), &
	     DIMENSION(DEF_elmtets), OPTIONAL       :: p_tetras
	INTEGER (KIND = GRID_SI), INTENT(in), OPTIONAL               :: p_mother
	INTEGER (KIND = GRID_SI), INTENT(in), OPTIONAL               :: p_devideedge
	TYPE (elmt), POINTER                        :: p_etmp
	INTEGER (KIND = GRID_SI)                                     :: i_cnt, i_tnc, &
	  i_ind, i_emx, i_alct

!---------- allocate new element

	i_ind                        = p_grid(i_futuretime)%i_etotal+ 1
	allocate(p_etmp, stat=i_alct)
	not_allocated: IF(i_alct /= 0) THEN
	  CALL print_error(21)
	END IF not_allocated
	p_etmp%def%i_indx            = i_ind
	p_grid(:)%i_etotal= i_ind

!---------- set up defining values

	p_etmp%def%p_node= p_nodes
	p_etmp%def%p_edge= p_edges

!---------- set optional arguments if present

	tetras_ok: IF(present(p_tetras)) THEN
	  p_etmp%att%p_tets = p_tetras
	ELSE tetras_ok
	  p_etmp%att%p_tets = 0
	END IF tetras_ok

	mark_ok: IF(present(i_mark)) THEN
	  p_etmp%att%i_mark= i_mark
	ELSE mark_ok
	  p_etmp%att%i_mark= 0
	END IF mark_ok

	bound_ok: IF(present(i_boundary)) THEN
	  p_etmp%att%i_boun = i_boundary
	ELSE bound_ok
	  p_etmp%att%i_boun = 0
	END IF bound_ok

	level_ok: IF(present(i_level)) THEN
	  p_etmp%att%i_levl= i_level
	ELSE level_ok
	  p_etmp%att%i_levl= 0
	END IF level_ok

	flag_ok: IF(present(i_stat)) THEN
	  p_etmp%att%i_stat= i_stat
	ELSE flag_ok
	  p_etmp%att%i_stat= 0
	END IF flag_ok

	devideedge_ok: IF(present(p_devideedge)) THEN
	  p_etmp%att%i_edge = p_devideedge
	ELSE devideedge_ok
	  p_etmp%att%i_edge = 0
	END IF devideedge_ok

	child_ok: IF(present(p_children)) THEN
	  child_tim: DO i_tnc=1,DEF_timesteps
	    p_etmp%lnk%p_chil(:,i_tnc)= p_children
	  END DO child_tim
	ELSE child_ok
	  p_etmp%lnk%p_chil(:,:)= 0
	END IF child_ok

	mother_ok: IF(present(p_mother)) THEN
	  p_etmp%lnk%p_prnt= p_mother
	ELSE mother_ok
	  p_etmp%lnk%p_prnt= 0
	END IF mother_ok

!---------- set timestamp

	p_etmp%att%i_time = i_futuretime

!---------- initialize values array

	p_etmp%att%r_vals = 0.0

!---------- put it into the hash table

	bound_check: IF(i_ind > i_ehashmax) THEN
	  i_emx= i_ehashmax+ DEF_hashblock
	  CALL hash_realloc(i_ehashmax, i_emx, c_action='elmthash')
	END IF bound_check
	p_ehash(i_ind)%ep=> p_etmp

!---------- put it into the current grid index table

	i_cnt= p_grid(i_futuretime)%i_enumber+ 1
	grid_check: IF(i_cnt > i_egridmax) THEN
	  i_emx= i_egridmax+ DEF_hashblock
	  CALL hash_realloc(i_egridmax, i_emx, c_action='elmtgrid')
	END IF grid_check
	i_egrid(i_cnt,i_futuretime)= i_ind
	p_grid(i_futuretime)%i_enumber= i_cnt

!---------- put it into the finest level index table,
!           since each new objekt is in the finest level

	i_cnt= p_grid(i_futuretime)%i_enumfine+ 1
	fine_check: IF(i_cnt > i_efinemax) THEN
	  i_emx= i_efinemax+ DEF_hashblock
	  CALL hash_realloc(i_efinemax, i_emx, c_action='elmtfine')
	END IF fine_check
	i_efine(i_cnt,i_futuretime)= p_etmp%def%i_indx
	p_grid(i_futuretime)%i_enumfine= i_cnt

!---------- put it into the boundary index table

	bound_edge: IF(p_etmp%att%i_boun /= DEF_inner) THEN
	  i_cnt= p_grid(i_futuretime)%i_enumboun+ 1
	  boun_check: IF(i_cnt > i_ebounmax) THEN
	  i_emx= i_ebounmax+ DEF_hashblock
	    CALL hash_realloc(i_ebounmax, i_emx, c_action='elemboun')
	  END IF boun_check
	  i_eboun(i_cnt,i_futuretime)= i_ind
	  p_grid(i_futuretime)%i_enumboun= i_cnt
	END IF bound_edge

!---------- set return value (point at temporary pointer)

	pe_new               => p_etmp

	RETURN
	END FUNCTION create_elmt

!*****************************************************************
	FUNCTION create_tetra(p_nodes, p_edges, p_elmts, i_level, i_stat, &
	           p_children, p_mother, p_devideelmt) RESULT (pt_new)

!---------- local declarations

	IMPLICIT NONE

	TYPE (tetra), POINTER                        :: pt_new
	INTEGER (KIND = GRID_SI), INTENT(in), DIMENSION(DEF_tetelmts) :: p_elmts
	INTEGER (KIND = GRID_SI), INTENT(in), DIMENSION(DEF_tetnodes) :: p_nodes
	INTEGER (KIND = GRID_SI), INTENT(in), DIMENSION(DEF_tetedges) :: p_edges
	INTEGER (KIND = GRID_SI), INTENT(in), OPTIONAL                :: i_level
	INTEGER (KIND = GRID_SI), INTENT(in), OPTIONAL                :: i_stat
	INTEGER (KIND = GRID_SI), INTENT(in), &
	     DIMENSION(DEF_tetchild), OPTIONAL       :: p_children
	INTEGER (KIND = GRID_SI), INTENT(in), OPTIONAL                :: p_mother
	INTEGER (KIND = GRID_SI), INTENT(in), OPTIONAL                :: p_devideelmt
	TYPE (tetra), POINTER                        :: p_ttmp
	INTEGER (KIND = GRID_SI)                                      :: i_cnt, i_tnc, &
	  i_ind, i_emx, i_alct

!---------- allocate new element

	i_ind                        = p_grid(i_futuretime)%i_ttotal+ 1
	allocate(p_ttmp, stat=i_alct)
	not_allocated: IF(i_alct /= 0) THEN
	  CALL print_error(21)
	END IF not_allocated
	p_ttmp%def%i_indx            = i_ind
	p_grid(:)%i_ttotal= i_ind

!---------- set up defining values

	p_ttmp%def%p_node= p_nodes
	p_ttmp%def%p_edge= p_edges
	p_ttmp%def%p_elem= p_elmts

!---------- set optional arguments if present

	level_ok: IF(present(i_level)) THEN
	  p_ttmp%att%i_levl= i_level
	ELSE level_ok
	  p_ttmp%att%i_levl= 0
	END IF level_ok

	flag_ok: IF(present(i_stat)) THEN
	  p_ttmp%att%i_stat= i_stat
	ELSE flag_ok
	  p_ttmp%att%i_stat= 0
	END IF flag_ok

	devideelmt_ok: IF(present(p_devideelmt)) THEN
	  p_ttmp%att%i_elmt = p_devideelmt
	ELSE devideelmt_ok
	  p_ttmp%att%i_elmt = 0
	END IF devideelmt_ok

	child_ok: IF(present(p_children)) THEN
	  child_tim: DO i_tnc=1,DEF_timesteps
	    p_ttmp%lnk%p_chil(:,i_tnc)= p_children
	  END DO child_tim
	ELSE child_ok
	  p_ttmp%lnk%p_chil(:,:)= 0
	END IF child_ok

	mother_ok: IF(present(p_mother)) THEN
	  p_ttmp%lnk%p_prnt= p_mother
	ELSE mother_ok
	  p_ttmp%lnk%p_prnt= 0
	END IF mother_ok

!---------- set timestamp

	p_ttmp%att%i_time = i_futuretime

!---------- initialize values array

	p_ttmp%att%r_vals = 0.0

!---------- put it into the hash table

	bound_check: IF(i_ind > i_thashmax) THEN
	  i_emx= i_thashmax+ DEF_hashblock
	  CALL hash_realloc(i_thashmax, i_emx, c_action='tetrhash')
	END IF bound_check
	p_thash(i_ind)%tp=> p_ttmp

!---------- put it into the current grid index table

	i_cnt= p_grid(i_futuretime)%i_tnumber+ 1
	grid_check: IF(i_cnt > i_tgridmax) THEN
	  i_emx= i_tgridmax+ DEF_hashblock
	  CALL hash_realloc(i_tgridmax, i_emx, c_action='tetrgrid')
	END IF grid_check
	i_tgrid(i_cnt,i_futuretime)= i_ind
	p_grid(i_futuretime)%i_tnumber= i_cnt

!---------- put it into the finest level index table,
!           since each new objekt is in the finest level

	i_cnt= p_grid(i_futuretime)%i_tnumfine+ 1
	fine_check: IF(i_cnt > i_tfinemax) THEN
	  i_emx= i_tfinemax+ DEF_hashblock
	  CALL hash_realloc(i_tfinemax, i_emx, c_action='tetrfine')
	END IF fine_check
	i_tfine(i_cnt, i_futuretime)= p_ttmp%def%i_indx
	p_grid(i_futuretime)%i_tnumfine= i_cnt

!---------- set return value (point at temporary pointer)

	pt_new               => p_ttmp

	RETURN
	END FUNCTION create_tetra

!*****************************************************************
	FUNCTION create_edge(p_nodes, &
	           p_elements, i_numelements, i_boundary, i_stat, &
	           p_children, p_periodic, p_devidenode) RESULT (pg_new)

!---------- local declarations

	IMPLICIT NONE

	TYPE (edge), POINTER                        :: pg_new
	INTEGER (KIND = GRID_SI), INTENT(in), DIMENSION(DEF_egnodes) :: p_nodes
	INTEGER (KIND = GRID_SI), INTENT(in), &
	     DIMENSION(DEF_egelems), OPTIONAL       :: p_elements
	INTEGER (KIND = GRID_SI), INTENT(in), OPTIONAL               :: i_numelements
	INTEGER (KIND = GRID_SI), INTENT(in), OPTIONAL               :: i_boundary
	INTEGER (KIND = GRID_SI), INTENT(in), OPTIONAL               :: i_stat
	INTEGER (KIND = GRID_SI), INTENT(in), &
	     DIMENSION(DEF_egchild), OPTIONAL       :: p_children
	INTEGER (KIND = GRID_SI), INTENT(in), OPTIONAL               :: p_periodic
	INTEGER (KIND = GRID_SI), INTENT(in), OPTIONAL               :: p_devidenode
	TYPE (edge), POINTER                        :: p_gtmp
	INTEGER (KIND = GRID_SI)                                     :: i_alct, i_cnt, i_ind, i_gmx

!---------- allocate new edge

	i_ind                        = p_grid(i_futuretime)%i_gtotal+ 1
	allocate(p_gtmp, stat=i_alct)
	not_allocated: IF(i_alct /= 0) THEN
	  CALL print_error(21)
	END IF not_allocated
	p_gtmp%def%i_indx            = i_ind
	p_grid(:)%i_gtotal= i_ind

!---------- set up defining values

	p_gtmp%def%p_node= p_nodes

!---------- set optional arguments if present

	elem_ok: IF(present(p_elements)) THEN
	  IF(.NOT. present(i_numelements)) THEN
	    CALL print_error(a_err='[create_edge]: No. of shared elements not present')
	  ELSE
	    p_gtmp%att%i_elem = i_numelements
	    p_gtmp%att%p_elem(1:i_numelements)= p_elements(1:i_numelements)
	    p_gtmp%att%p_elem(i_numelements+1:DEF_egelems)= 0
	  ENDIF
	ELSE elem_ok
	  p_gtmp%att%i_elem = 0
	  p_gtmp%att%p_elem = 0
	END IF elem_ok

	bound_ok: IF(present(i_boundary)) THEN
	  p_gtmp%att%i_boun = i_boundary
	ELSE bound_ok
	  p_gtmp%att%i_boun = 0
	END IF bound_ok

	status_ok: IF(present(i_stat)) THEN
	  p_gtmp%att%i_stat = i_stat
	ELSE status_ok
	  p_gtmp%att%i_stat = 0
	END IF status_ok

	child_ok: IF(present(p_children)) THEN
	  DO i_cnt= 1, DEF_timesteps
	    p_gtmp%lnk%p_chil(:,i_cnt) = p_children(:)
	  END DO
	ELSE child_ok
	  p_gtmp%lnk%p_chil(:,:) = 0
	END IF child_ok

	periodic_ok: IF(present(p_periodic)) THEN
	  p_gtmp%lnk%p_peri = p_periodic
	ELSE periodic_ok
	  p_gtmp%lnk%p_peri = 0
	END IF periodic_ok

	devidenode_ok: IF(present(p_devidenode)) THEN
	  p_gtmp%att%i_node = p_devidenode
	ELSE devidenode_ok
	  p_gtmp%att%i_node = 0
	END IF devidenode_ok

!---------- set timestamp

	p_gtmp%att%i_time = i_futuretime

!---------- put it into the hash table

	bound_check: IF(i_ind > i_ghashmax) THEN
	  i_gmx= i_ghashmax+ DEF_hashblock
	  CALL hash_realloc(i_ghashmax, i_gmx, c_action='edgehash')
	END IF bound_check
	p_ghash(i_ind)%gp=> p_gtmp

!---------- put it into the current grid index table

	i_cnt= p_grid(i_futuretime)%i_gnumber+ 1
	grid_check: IF(i_cnt > i_ggridmax) THEN
	  i_gmx= i_ggridmax+ DEF_hashblock
	  CALL hash_realloc(i_ggridmax, i_gmx, c_action='edgegrid')
	END IF grid_check
	i_ggrid(i_cnt,i_futuretime)= i_ind
	p_grid(i_futuretime)%i_gnumber= i_cnt

!---------- put it into the finest level index table,
!           since each new objekt is in the finest level

	i_cnt= p_grid(i_futuretime)%i_gnumfine+ 1
	fine_check: IF(i_cnt > i_gfinemax) THEN
	  i_gmx= i_gfinemax+ DEF_hashblock
	  CALL hash_realloc(i_gfinemax, i_gmx, c_action='edgefine')
	END IF fine_check
	i_gfine(i_cnt,i_futuretime)= i_ind
	p_grid(i_futuretime)%i_gnumfine= i_cnt

!---------- put it into the boundary index table

	bound_edge: IF(p_gtmp%att%i_boun /= DEF_inner) THEN
	  i_cnt= p_grid(i_futuretime)%i_gnumboun+ 1
	  boun_check: IF(i_cnt > i_gbounmax) THEN
	  i_gmx= i_gbounmax+ DEF_hashblock
	    CALL hash_realloc(i_gbounmax, i_gmx, c_action='edgeboun')
	  END IF boun_check
	  i_gboun(i_cnt,i_futuretime)= i_ind
	  p_grid(i_futuretime)%i_gnumboun= i_cnt
	END IF bound_edge

!---------- set return value (point at temporary pointer)

	pg_new               => p_gtmp

	RETURN
	END FUNCTION create_edge

!*****************************************************************
	FUNCTION create_node(r_coordinates, i_stat, &
	           p_edge, p_periodic) RESULT (pn_new)

!---------- local declarations

	IMPLICIT NONE

	TYPE (node), POINTER                        :: pn_new
	REAL (KIND = GRID_SR), DIMENSION(DEF_dimension), INTENT(in)  :: r_coordinates
	INTEGER (KIND = GRID_SI), INTENT(in), OPTIONAL               :: i_stat
	INTEGER (KIND = GRID_SI), INTENT(in), OPTIONAL               :: p_edge
	INTEGER (KIND = GRID_SI), INTENT(in), OPTIONAL               :: p_periodic
	TYPE (node), POINTER                        :: p_ntmp
	INTEGER (KIND = GRID_SI)                                     :: i_cnt, i_ind, &
	  i_nmx, i_alct

!---------- allocate new element

	i_ind                        = p_grid(i_futuretime)%i_ntotal+ 1
	allocate(p_ntmp, stat=i_alct)
	not_allocated: IF(i_alct /= 0) THEN
	  CALL print_error(21)
	END IF not_allocated
	p_ntmp%def%i_indx            = i_ind
	p_grid(:)%i_ntotal= i_ind
	
!---------- set up defining values

	p_ntmp%def%r_coor= r_coordinates

!---------- set optional arguments if present

	stat_ok: IF(present(i_stat)) THEN
	  p_ntmp%att%i_stat = i_stat
	ELSE stat_ok
	  p_ntmp%att%i_stat = 0
	END IF stat_ok

	edge_ok: IF(present(p_edge)) THEN
	  p_ntmp%att%p_edge = p_edge
	ELSE edge_ok
	  p_ntmp%att%p_edge = 0
	END IF edge_ok

	periodic_ok: IF(present(p_periodic)) THEN
	  p_ntmp%lnk%p_peri = p_periodic
	ELSE periodic_ok
	  p_ntmp%lnk%p_peri = 0
	END IF periodic_ok

!---------- set timestamp

	p_ntmp%att%i_time = i_futuretime

!---------- initialize patch

	p_ntmp%att%i_ptch = 0.0
	p_ntmp%att%p_ptch = 0.0

!---------- initialize values array

	p_ntmp%att%r_vals = 0.0

!---------- put it into the hash table

	bound_check: IF(i_ind > i_nhashmax) THEN
	  i_nmx= i_nhashmax+ DEF_hashblock
	  CALL hash_realloc(i_nhashmax, i_nmx, c_action='nodehash')
	END IF bound_check
	p_nhash(i_ind)%np => p_ntmp

!---------- put it into the current grid index table

	i_cnt= p_grid(i_futuretime)%i_nnumber+ 1
	grid_check: IF(i_cnt > i_ngridmax) THEN
	  i_nmx= i_ngridmax+ DEF_hashblock
	  CALL hash_realloc(i_ngridmax, i_nmx, c_action='nodegrid')
	END IF grid_check
	i_ngrid(i_cnt,i_futuretime)= i_ind
	p_grid(i_futuretime)%i_nnumber= i_cnt

!---------- set return value (point at temporary pointer)

	pn_new => p_ntmp

	RETURN
	END FUNCTION create_node

!*****************************************************************
	SUBROUTINE destroy_elmt(pe_old, p_ghand)

!---------- local declarations

	IMPLICIT NONE

	TYPE (elmt), POINTER :: pe_old
	TYPE (grid_handle)   :: p_ghand
	TYPE (elmt), POINTER :: p_etmp
	INTEGER (KIND = GRID_SI)              :: i_tim, i_ind

!---------- set time tag

	i_tim= p_ghand%i_timetag
	i_ind= pe_old%def%i_indx

!---------- set temporary pointer

	p_etmp => pe_old

!---------- take out of the hashing table

	nullify(p_ehash(i_ind)%ep)
	nullify(pe_old)

!---------- now the real deallocation

	deallocate(p_etmp)

	RETURN
	END SUBROUTINE destroy_elmt

!*****************************************************************
	SUBROUTINE destroy_tetra(pt_old, p_ghand)

!---------- local declarations

	IMPLICIT NONE

	TYPE (tetra), POINTER :: pt_old
	TYPE (grid_handle)    :: p_ghand
	TYPE (tetra), POINTER :: p_ttmp
	INTEGER (KIND = GRID_SI)               :: i_tim, i_ind

!---------- set time tag

	i_tim= p_ghand%i_timetag
	i_ind= pt_old%def%i_indx

!---------- set temporary pointer

	p_ttmp => pt_old

!---------- take out of the hashing table

	nullify(p_thash(i_ind)%tp)
	nullify(pt_old)

!---------- now the real deallocation

	deallocate(p_ttmp)

	RETURN
	END SUBROUTINE destroy_tetra

!*****************************************************************
	SUBROUTINE destroy_edge(pg_old, p_ghand)

!---------- local declarations

	IMPLICIT NONE

	TYPE (edge), POINTER :: pg_old
	TYPE (grid_handle)   :: p_ghand
	TYPE (edge), POINTER :: p_gtmp
	INTEGER (KIND = GRID_SI)              :: i_tim, i_ind

!---------- set time tag

	i_tim= p_ghand%i_timetag
	i_ind= pg_old%def%i_indx

!---------- set temporary pointer

	p_gtmp => pg_old

!---------- take out of the hashing table

	nullify(p_ghash(i_ind)%gp)
	nullify(pg_old)

!---------- true deallocation

	deallocate(p_gtmp)

	RETURN
	END SUBROUTINE destroy_edge

!*****************************************************************
	SUBROUTINE destroy_node(pn_old, p_ghand)

!---------- local declarations

	IMPLICIT NONE

	TYPE (node), POINTER :: pn_old
	TYPE (grid_handle)   :: p_ghand
	TYPE (node), POINTER :: p_ntmp
	INTEGER (KIND = GRID_SI)              :: i_tim, i_ind

!---------- set time tag

	i_tim= p_ghand%i_timetag
	i_ind= pn_old%def%i_indx

!---------- set pointer

	p_ntmp => pn_old

!---------- take out of the hashing table

	nullify(p_nhash(i_ind)%np)
	nullify(pn_old)

!---------- true deallocation

	deallocate(p_ntmp)

	RETURN
	END SUBROUTINE destroy_node

!*****************************************************************
	SUBROUTINE erase_elmt(pe_old, p_ghand)

!---------- local declarations

	IMPLICIT NONE

	TYPE (elmt), POINTER :: pe_old
	TYPE (grid_handle)   :: p_ghand
	TYPE (elmt), POINTER :: p_parent
	INTEGER (KIND = GRID_SI)              :: i_tim, i_ind, i_cnt

!---------- set time tag

	i_tim= p_ghand%i_timetag
	i_ind= pe_old%def%i_indx

!---------- set time dependent values to zero

	parent_notify: IF(pe_old%lnk%p_prnt(i_tim) /= 0) THEN
	  p_parent=> p_ehash(pe_old%lnk%p_prnt(i_tim))%ep
	  DO i_cnt=1,DEF_elchild
	    IF(p_parent%lnk%p_chil(i_cnt,i_tim) == i_ind) THEN
	      p_parent%lnk%p_chil(i_cnt,i_tim)= 0
	    END IF
	  END DO
	END IF parent_notify

	pe_old%lnk%p_chil(:,i_tim)= 0

!---------- set erased status flag

	pe_old%att%i_stat(i_tim)  = DEF_erased

!---------- reset time tag

	pe_old%att%i_time= i_currenttime

	RETURN
	END SUBROUTINE erase_elmt

!*****************************************************************
	SUBROUTINE erase_tetra(pt_old, p_ghand)

!---------- local declarations

	IMPLICIT NONE

	TYPE (tetra), POINTER :: pt_old
	TYPE (grid_handle)    :: p_ghand
	TYPE (tetra), POINTER :: p_parent
	INTEGER (KIND = GRID_SI)      	      :: i_tim, i_ind, i_cnt

!---------- set time tag

	i_tim= p_ghand%i_timetag
	i_ind= pt_old%def%i_indx

!---------- set time dependent values to zero

	parent_notify: IF(pt_old%lnk%p_prnt(i_tim) /= 0) THEN
	  p_parent=> p_thash(pt_old%lnk%p_prnt(i_tim))%tp
	  DO i_cnt=1,DEF_tetchild
	    IF(p_parent%lnk%p_chil(i_cnt,i_tim) == i_ind) THEN
	      p_parent%lnk%p_chil(i_cnt,i_tim)= 0
	    END IF
	  END DO
	END IF parent_notify

	pt_old%lnk%p_chil(:,i_tim)= 0

!---------- set erased status flag

	pt_old%att%i_stat(i_tim)  = DEF_erased

!---------- reset time tag

	pt_old%att%i_time= i_currenttime

	RETURN
	END SUBROUTINE erase_tetra

!*****************************************************************
	SUBROUTINE erase_edge(pg_old, p_ghand)

!---------- local declarations

	IMPLICIT NONE

	TYPE (edge), POINTER :: pg_old
	TYPE (grid_handle)   :: p_ghand
	INTEGER (KIND = GRID_SI)              :: i_tim

!---------- set time tag

	i_tim= p_ghand%i_timetag

!---------- set time dependent values to zero

	pg_old%lnk%p_chil(:,i_tim)= 0
	pg_old%att%i_node(i_tim)  = 0

!---------- set erased status flag

	pg_old%att%i_stat(i_tim)  = DEF_erased

!---------- set time tag

	pg_old%att%i_time= i_currenttime

	RETURN
	END SUBROUTINE erase_edge

!*****************************************************************
	SUBROUTINE erase_node(pn_old, p_ghand)

!---------- local declarations

	IMPLICIT NONE

	TYPE (node), POINTER :: pn_old
	TYPE (grid_handle)   :: p_ghand
	INTEGER (KIND = GRID_SI)              :: i_tim

!---------- set time tag

	i_tim= p_ghand%i_timetag

!---------- set time dependent values to zero

	pn_old%att%r_vals(:,i_tim)= 0

!---------- set erased status flag

	pn_old%att%i_stat(i_tim)  = DEF_erased

!---------- set time tag

	pn_old%att%i_time= i_currenttime

	RETURN
	END SUBROUTINE erase_node
!*****************************************************************
	SUBROUTINE hash_initalloc(i_size, i_init, c_action)

!---------- local declarations

	IMPLICIT NONE
	INTEGER (KIND = GRID_SI)                                              :: i_size
	INTEGER (KIND = GRID_SI), OPTIONAL                                    :: i_init
	CHARACTER (len=8), OPTIONAL                          :: c_action
	INTEGER (KIND = GRID_SI)                                              :: i_alct, &
	  i_esiz, i_gsiz, i_nsiz, i_isiz, i_tsiz
	INTEGER (KIND = GRID_SI), PARAMETER                                   :: i_einit= 1024
	INTEGER (KIND = GRID_SI), PARAMETER                                   :: i_ginit= 1024
	INTEGER (KIND = GRID_SI), PARAMETER                                   :: i_ninit= 1024
	INTEGER (KIND = GRID_SI), PARAMETER                                   :: i_iinit= 1024
	INTEGER (KIND = GRID_SI), PARAMETER                                   :: i_tinit= 1024
	CHARACTER (len=8)                                    :: c_act

!---------- set action type

	action_present: IF(present(c_action)) THEN
	  c_act= c_action
	ELSE action_present
	  c_act= 'default '
	END IF action_present

!---------- set initial sizes

	size_present: IF(present(i_init)) THEN
	  i_esiz= i_init
	  i_tsiz= i_init
	  i_gsiz= i_init
	  i_nsiz= i_init
	  i_isiz= i_init
	ELSE size_present
	  i_esiz= i_einit
	  i_tsiz= i_tinit
	  i_gsiz= i_ginit
	  i_nsiz= i_ninit
	  i_isiz= i_iinit
	END IF size_present

	array_type: SELECT CASE (c_act)
	  
!---------- array type selection (element)

	CASE('elmthash') array_type

!---------- allocate array

	  allocate(p_ehash(i_esiz), stat=i_alct)
	  IF(i_alct /= 0) THEN
	    CALL print_error(30)
	  END IF
	  i_size=  i_esiz

!---------- print logging information, if required

	  IF(GRID_parameters%iolog > 0) THEN
	    WRITE(GRID_parameters%iolog,*) ' LOGINFO: hashing table for elements allocated,'
	    WRITE(GRID_parameters%iolog,*) ' initial size: ', i_size
	  END IF

	  
!---------- array type selection (tetrahedron)

	CASE('tetrhash') array_type

!---------- allocate array

	  allocate(p_thash(i_tsiz), stat=i_alct)
	  IF(i_alct /= 0) THEN
	    CALL print_error(30)
	  END IF
	  i_size=  i_tsiz

!---------- print logging information, if required

	  IF(GRID_parameters%iolog > 0) THEN
	    WRITE(GRID_parameters%iolog,*) ' LOGINFO: hashing table for tetrahedrons allocated,'
	    WRITE(GRID_parameters%iolog,*) ' initial size: ', i_size
	  END IF


!---------- array type selection (edge)

	CASE('edgehash') array_type

!---------- allocate array

	  allocate(p_ghash(i_gsiz), stat=i_alct)
	  IF(i_alct /= 0) THEN
	    CALL print_error(30)
	  END IF
	  i_size=  i_gsiz

!---------- print logging information, if required

	  IF(GRID_parameters%iolog > 0) THEN
	    WRITE(GRID_parameters%iolog,*) ' LOGINFO: hashing table for edges allocated,'
	    WRITE(GRID_parameters%iolog,*) ' initial size: ', i_size
	  END IF

!---------- array type selection (node)

	CASE('nodehash') array_type

!---------- allocate array

	  allocate(p_nhash(i_nsiz), stat=i_alct)
	  IF(i_alct /= 0) THEN
	    CALL print_error(30)
	  END IF
	  i_size=  i_nsiz

!---------- print logging information, if required

	  IF(GRID_parameters%iolog > 0) THEN
	    WRITE(GRID_parameters%iolog,*) ' LOGINFO: hashing table for nodes allocated,'
	    WRITE(GRID_parameters%iolog,*) ' initial size: ', i_size
	  END IF

!---------- array type selection (integer)

	CASE('elmtgrid') array_type

!---------- allocate array

	  allocate(i_egrid(i_isiz,DEF_timesteps), stat=i_alct)
	  IF(i_alct /= 0) THEN
	    CALL print_error(30)
	  END IF
	  i_size =  i_isiz
	  i_egrid= 0

!---------- print logging information, if required

	  IF(GRID_parameters%iolog > 0) THEN
	    WRITE(GRID_parameters%iolog,*) ' LOGINFO: integer auxiliary index table allocated,'
	    WRITE(GRID_parameters%iolog,*) ' initial size: ', i_size
	  END IF

!---------- array type selection (integer)

	CASE('elmtfine') array_type

!---------- allocate array

	  allocate(i_efine(i_isiz,DEF_timesteps), stat=i_alct)
	  IF(i_alct /= 0) THEN
	    CALL print_error(30)
	  END IF
	  i_size =  i_isiz
	  i_efine= 0

!---------- print logging information, if required

	  IF(GRID_parameters%iolog > 0) THEN
	    WRITE(GRID_parameters%iolog,*) ' LOGINFO: integer auxiliary index table allocated,'
	    WRITE(GRID_parameters%iolog,*) ' initial size: ', i_size
	  END IF

!---------- array type selection (integer)

	CASE('elmtboun') array_type

!---------- allocate array

	  allocate(i_eboun(i_isiz,DEF_timesteps), stat=i_alct)
	  IF(i_alct /= 0) THEN
	    CALL print_error(30)
	  END IF
	  i_size =  i_isiz
	  i_eboun= 0

!---------- print logging information, if required

	  IF(GRID_parameters%iolog > 0) THEN
	    WRITE(GRID_parameters%iolog,*) ' LOGINFO: integer auxiliary index table allocated,'
	    WRITE(GRID_parameters%iolog,*) ' initial size: ', i_size
	  END IF


!---------- array type selection (integer)

	CASE('tetrgrid') array_type

!---------- allocate array

	  allocate(i_tgrid(i_isiz,DEF_timesteps), stat=i_alct)
	  IF(i_alct /= 0) THEN
	    CALL print_error(30)
	  END IF
	  i_size =  i_isiz
	  i_tgrid= 0

!---------- print logging information, if required

	  IF(GRID_parameters%iolog > 0) THEN
	    WRITE(GRID_parameters%iolog,*) ' LOGINFO: integer auxiliary index table allocated,'
	    WRITE(GRID_parameters%iolog,*) ' initial size: ', i_size
	  END IF

!---------- array type selection (integer)

	CASE('tetrfine') array_type

!---------- allocate array

	  allocate(i_tfine(i_isiz,DEF_timesteps), stat=i_alct)
	  IF(i_alct /= 0) THEN
	    CALL print_error(30)
	  END IF
	  i_size =  i_isiz
	  i_tfine= 0

!---------- print logging information, if required

	  IF(GRID_parameters%iolog > 0) THEN
	    WRITE(GRID_parameters%iolog,*) ' LOGINFO: integer auxiliary index table allocated,'
	    WRITE(GRID_parameters%iolog,*) ' initial size: ', i_size
	  END IF


!---------- array type selection (integer)

	CASE('edgegrid') array_type

!---------- allocate array

	  allocate(i_ggrid(i_isiz,DEF_timesteps), stat=i_alct)
	  IF(i_alct /= 0) THEN
	    CALL print_error(30)
	  END IF
	  i_size =  i_isiz
	  i_ggrid= 0

!---------- print logging information, if required

	  IF(GRID_parameters%iolog > 0) THEN
	    WRITE(GRID_parameters%iolog,*) ' LOGINFO: integer auxiliary index table allocated,'
	    WRITE(GRID_parameters%iolog,*) ' initial size: ', i_size
	  END IF

!---------- array type selection (integer)

	CASE('edgefine') array_type

!---------- allocate array

	  allocate(i_gfine(i_isiz, DEF_timesteps), stat=i_alct)
	  IF(i_alct /= 0) THEN
	    CALL print_error(30)
	  END IF
	  i_size =  i_isiz
	  i_gfine= 0

!---------- print logging information, if required

	  IF(GRID_parameters%iolog > 0) THEN
	    WRITE(GRID_parameters%iolog,*) ' LOGINFO: integer auxiliary index table allocated,'
	    WRITE(GRID_parameters%iolog,*) ' initial size: ', i_size
	  END IF

!---------- array type selection (integer)

	CASE('edgeboun') array_type

!---------- allocate array

	  allocate(i_gboun(i_isiz,DEF_timesteps), stat=i_alct)
	  IF(i_alct /= 0) THEN
	    CALL print_error(30)
	  END IF
	  i_size =  i_isiz
	  i_gboun= 0

!---------- print logging information, if required

	  IF(GRID_parameters%iolog > 0) THEN
	    WRITE(GRID_parameters%iolog,*) ' LOGINFO: integer auxiliary index table allocated,'
	    WRITE(GRID_parameters%iolog,*) ' initial size: ', i_size
	  END IF

!---------- array type selection (integer)

	CASE('nodegrid') array_type

!---------- allocate array

	  allocate(i_ngrid(i_isiz,DEF_timesteps), stat=i_alct)
	  IF(i_alct /= 0) THEN
	    CALL print_error(30)
	  END IF
	  i_size =  i_isiz
	  i_ngrid= 0

!---------- print logging information, if required

	  IF(GRID_parameters%iolog > 0) THEN
	    WRITE(GRID_parameters%iolog,*) ' LOGINFO: integer auxiliary index table allocated,'
	    WRITE(GRID_parameters%iolog,*) ' initial size: ', i_size
	  END IF

!---------- array type selection (integer)

	CASE DEFAULT array_type
	  IF(GRID_parameters%iolog > 0) THEN
	    WRITE(GRID_parameters%iolog,*) ' LOGINFO: DEFAULT actiontype in hash_initalloc'
	  END IF
	  RETURN
	END SELECT array_type

	RETURN

	END SUBROUTINE hash_initalloc

!*****************************************************************
	SUBROUTINE hash_realloc(i_old, i_new, c_action)

!---------- local declarations

	IMPLICIT NONE
	INTEGER (KIND = GRID_SI), INTENT(inout)                               :: i_old
	INTEGER (KIND = GRID_SI), INTENT(inout)                               :: i_new
	CHARACTER (len=8), OPTIONAL                          :: c_action
	TYPE (elmt_ptr_arr), DIMENSION(:), ALLOCATABLE       :: e_tmparr
	TYPE (edge_ptr_arr), DIMENSION(:), ALLOCATABLE       :: g_tmparr
	TYPE (tetra_ptr_arr), DIMENSION(:), ALLOCATABLE      :: t_tmparr
	TYPE (node_ptr_arr), DIMENSION(:), ALLOCATABLE       :: n_tmparr
	INTEGER (KIND = GRID_SI), DIMENSION(:), ALLOCATABLE                   :: i_tmparr
	INTEGER (KIND = GRID_SI), DIMENSION(:,:), ALLOCATABLE                 :: j_tmparr
	INTEGER (KIND = GRID_SI)                                              :: i_alct, i_cnt, j_cnt
	CHARACTER (len=8)                                    :: c_act

!---------- set action type

	action_present: IF(present(c_action)) THEN
	  c_act= c_action
	ELSE action_present
	  c_act= 'default '
	END IF action_present

	array_type: SELECT CASE (c_act)

!---------- array type selection (element)

	CASE('elmthash') array_type

!---------- allocate temporary array and let e_arr point to it

	  allocate(e_tmparr(i_new), stat=i_alct)
	  IF(i_alct /= 0) THEN
	    CALL print_error(31)
	  END IF

!---------- copy array

	  i_cnt            =  i_old
	  IF(i_old > i_new) i_cnt= i_new
	  e_tmparr(1:i_cnt)=  p_ehash(1:i_cnt)

!---------- deallocate old array and allocate with new size

	  IF (allocated(p_ehash)) THEN
	    deallocate(p_ehash)
	  END IF
	  allocate(p_ehash(i_new), stat=i_alct)
	  IF(i_alct /= 0) THEN
	    CALL print_error(31)
	  END IF

!---------- copy array

	  p_ehash(1:i_cnt)= e_tmparr(1:i_cnt)

!---------- deallocate old array and allocate with new size

	  deallocate(e_tmparr)

!---------- print logging information, if required

	  IF(GRID_parameters%iolog > 0) THEN
	    WRITE(GRID_parameters%iolog,*) ' LOGINFO: hashing table for elements reallocated,'
	    WRITE(GRID_parameters%iolog,*) ' initial size: ', i_old
	    WRITE(GRID_parameters%iolog,*) ' new size:     ', i_new
	  END IF


!---------- array type selection (tetrahedron)

	CASE('tetrhash') array_type

!---------- allocate temporary array and let e_arr point to it

	  allocate(t_tmparr(i_new), stat=i_alct)
	  IF(i_alct /= 0) THEN
	    CALL print_error(31)
	  END IF

!---------- copy array

	  i_cnt            =  i_old
	  IF(i_old > i_new) i_cnt= i_new
	  t_tmparr(1:i_cnt)=  p_thash(1:i_cnt)

!---------- deallocate old array and allocate with new size

	  IF (allocated(p_thash)) THEN
	    deallocate(p_thash)
	  END IF
	  allocate(p_thash(i_new), stat=i_alct)
	  IF(i_alct /= 0) THEN
	    CALL print_error(31)
	  END IF

!---------- copy array

	  p_thash(1:i_cnt)= t_tmparr(1:i_cnt)

!---------- deallocate temporary array and allocate with new size

	  deallocate(t_tmparr)

!---------- print logging information, if required

	  IF(GRID_parameters%iolog > 0) THEN
	    WRITE(GRID_parameters%iolog,*) ' LOGINFO: hashing table for tetrahedrons reallocated,'
	    WRITE(GRID_parameters%iolog,*) ' initial size: ', i_old
	    WRITE(GRID_parameters%iolog,*) ' new size:     ', i_new
	  END IF


!---------- array type selection (edge)

	CASE('edgehash') array_type

!---------- allocate temporary array and let e_arr point to it

	  allocate(g_tmparr(i_new), stat=i_alct)
	  IF(i_alct /= 0) THEN
	    CALL print_error(31)
	  END IF

!---------- copy array

	  i_cnt            =  i_old
	  IF(i_old > i_new) i_cnt= i_new
	  g_tmparr(1:i_cnt)=  p_ghash(1:i_cnt)

!---------- deallocate old array and allocate with new size

	  IF (allocated(p_ghash)) THEN
	    deallocate(p_ghash)
	  END IF
	  allocate(p_ghash(i_new), stat=i_alct)
	  IF(i_alct /= 0) THEN
	    CALL print_error(31)
	  END IF

!---------- copy array

	  p_ghash(1:i_cnt)= g_tmparr(1:i_cnt)

!---------- deallocate old array and allocate with new size

	  deallocate(g_tmparr)

!---------- print logging information, if required

	  IF(GRID_parameters%iolog > 0) THEN
	    WRITE(GRID_parameters%iolog,*) ' LOGINFO: hashing table for edges reallocated,'
	    WRITE(GRID_parameters%iolog,*) ' initial size: ', i_old
	    WRITE(GRID_parameters%iolog,*) ' new size:     ', i_new
	  END IF

!---------- array type selection (node)

	CASE('nodehash') array_type

!---------- allocate temporary array and let e_arr point to it

	  allocate(n_tmparr(i_new), stat=i_alct)
	  IF(i_alct /= 0) THEN
	    CALL print_error(31)
	  END IF

!---------- copy array

	  i_cnt            =  i_old
	  IF(i_old > i_new) i_cnt= i_new
	  n_tmparr(1:i_cnt)=  p_nhash(1:i_cnt)

!---------- deallocate old array and allocate with new size

	  IF (allocated(p_nhash)) THEN
	    deallocate(p_nhash)
	  END IF
	  allocate(p_nhash(i_new), stat=i_alct)
	  IF(i_alct /= 0) THEN
	    CALL print_error(31)
	  END IF

!---------- copy array

	  p_nhash(1:i_cnt)= n_tmparr(1:i_cnt)

!---------- deallocate old array and allocate with new size

	  deallocate(n_tmparr)

!---------- print logging information, if required

	  IF(GRID_parameters%iolog > 0) THEN
	    WRITE(GRID_parameters%iolog,*) ' LOGINFO: hashing table for nodes reallocated,'
	    WRITE(GRID_parameters%iolog,*) ' initial size: ', i_old
	    WRITE(GRID_parameters%iolog,*) ' new size:     ', i_new
	  END IF

!---------- array type selection (integer)

	CASE('elmtgrid') array_type

!---------- allocate temporary array and let e_arr point to it

	  allocate(j_tmparr(i_new, DEF_timesteps), stat=i_alct)
	  IF(i_alct /= 0) THEN
	    CALL print_error(31)
	  END IF

!---------- copy array

	  i_cnt                  = i_old
	  IF(i_old > i_new) i_cnt= i_new
	  DO j_cnt=1, DEF_timesteps
	    j_tmparr(1:i_cnt,j_cnt)      = i_egrid(1:i_cnt,j_cnt)
	    j_tmparr(i_cnt+1:i_new,j_cnt)= 0
	  END DO

!---------- deallocate old array and allocate with new size

	  IF (allocated(i_egrid)) THEN
	    deallocate(i_egrid)
	  END IF
	  allocate(i_egrid(i_new, DEF_timesteps), stat=i_alct)
	  IF(i_alct /= 0) THEN
	    CALL print_error(31)
	  END IF

!---------- copy array

	  DO j_cnt=1, DEF_timesteps
	    i_egrid(1:i_new,j_cnt)= j_tmparr(1:i_new,j_cnt)
	  END DO

!---------- deallocate old array and allocate with new size

	  deallocate(j_tmparr)

!---------- print logging information, if required

	  IF(GRID_parameters%iolog > 0) THEN
	    WRITE(GRID_parameters%iolog,*) ' LOGINFO: grid index table for elements reallocated,'
	    WRITE(GRID_parameters%iolog,*) ' initial size: ', i_old
	    WRITE(GRID_parameters%iolog,*) ' new size:     ', i_new
	  END IF

!---------- array type selection (integer)

	CASE('elmtfine') array_type

!---------- allocate temporary array and let e_arr point to it

	  allocate(j_tmparr(i_new,DEF_timesteps), stat=i_alct)
	  IF(i_alct /= 0) THEN
	    CALL print_error(31)
	  END IF

!---------- copy array

	  i_cnt                  = i_old
	  IF(i_old > i_new) i_cnt= i_new
	  DO j_cnt=1, DEF_timesteps
	    j_tmparr(1:i_cnt,j_cnt)      = i_efine(1:i_cnt,j_cnt)
	    j_tmparr(i_cnt+1:i_new,j_cnt)= 0
	  END DO

!---------- deallocate old array and allocate with new size

	  IF (allocated(i_efine)) THEN
	    deallocate(i_efine)
	  END IF
	  allocate(i_efine(i_new, DEF_timesteps), stat=i_alct)
	  IF(i_alct /= 0) THEN
	    CALL print_error(31)
	  END IF

!---------- copy array

	  DO j_cnt=1, DEF_timesteps
	    i_efine(1:i_new,j_cnt)= j_tmparr(1:i_new,j_cnt)
	  END DO

!---------- deallocate old array and allocate with new size

	  deallocate(j_tmparr)

!---------- print logging information, if required

	  IF(GRID_parameters%iolog > 0) THEN
	    WRITE(GRID_parameters%iolog,*) ' LOGINFO: fine index table for elements reallocated,'
	    WRITE(GRID_parameters%iolog,*) ' initial size: ', i_old
	    WRITE(GRID_parameters%iolog,*) ' new size:     ', i_new
	  END IF


!---------- array type selection (integer)

	CASE('elmtboun') array_type

!---------- allocate temporary array and let e_arr point to it

	  allocate(j_tmparr(i_new, DEF_timesteps), stat=i_alct)
	  IF(i_alct /= 0) THEN
	    CALL print_error(31)
	  END IF

!---------- copy array

	  i_cnt                  = i_old
	  IF(i_old > i_new) i_cnt= i_new
	  DO j_cnt=1, DEF_timesteps
	    j_tmparr(1:i_cnt,j_cnt)      = i_eboun(1:i_cnt,j_cnt)
	    j_tmparr(i_cnt+1:i_new,j_cnt)= 0
	  END DO

!---------- deallocate old array and allocate with new size

	  IF (allocated(i_eboun)) THEN
	    deallocate(i_eboun)
	  END IF
	  allocate(i_eboun(i_new, DEF_timesteps), stat=i_alct)
	  IF(i_alct /= 0) THEN
	    CALL print_error(31)
	  END IF

!---------- copy array

	  DO j_cnt=1, DEF_timesteps
	    i_eboun(1:i_new,j_cnt)= j_tmparr(1:i_new,j_cnt)
	  END DO

!---------- deallocate temporary array and allocate with new size

	  deallocate(j_tmparr)

!---------- print logging information, if required

	  IF(GRID_parameters%iolog > 0) THEN
	    WRITE(GRID_parameters%iolog,*) ' LOGINFO: boundary index table for elements reallocated,'
	    WRITE(GRID_parameters%iolog,*) ' initial size: ', i_old
	    WRITE(GRID_parameters%iolog,*) ' new size:     ', i_new
	  END IF


!---------- array type selection (integer)

	CASE('tetrgrid') array_type

!---------- allocate temporary array and let e_arr point to it

	  allocate(j_tmparr(i_new, DEF_timesteps), stat=i_alct)
	  IF(i_alct /= 0) THEN
	    CALL print_error(31)
	  END IF

!---------- copy array

	  i_cnt                  = i_old
	  IF(i_old > i_new) i_cnt= i_new
	  DO j_cnt=1, DEF_timesteps
	    j_tmparr(1:i_cnt,j_cnt)      = i_tgrid(1:i_cnt,j_cnt)
	    j_tmparr(i_cnt+1:i_new,j_cnt)= 0
	  END DO

!---------- deallocate old array and allocate with new size

	  IF (allocated(i_tgrid)) THEN
	    deallocate(i_tgrid)
	  END IF
	  allocate(i_tgrid(i_new, DEF_timesteps), stat=i_alct)
	  IF(i_alct /= 0) THEN
	    CALL print_error(31)
	  END IF

!---------- copy array

	  DO j_cnt=1, DEF_timesteps
	    i_tgrid(1:i_new,j_cnt)= j_tmparr(1:i_new,j_cnt)
	  END DO

!---------- deallocate old array and allocate with new size

	  deallocate(j_tmparr)

!---------- print logging information, if required

	  IF(GRID_parameters%iolog > 0) THEN
	    WRITE(GRID_parameters%iolog,*) ' LOGINFO: grid index table for tetrahedrons reallocated,'
	    WRITE(GRID_parameters%iolog,*) ' initial size: ', i_old
	    WRITE(GRID_parameters%iolog,*) ' new size:     ', i_new
	  END IF

!---------- array type selection (integer)

	CASE('tetrfine') array_type

!---------- allocate temporary array and let e_arr point to it

	  allocate(j_tmparr(i_new,DEF_timesteps), stat=i_alct)
	  IF(i_alct /= 0) THEN
	    CALL print_error(31)
	  END IF

!---------- copy array

	  i_cnt                  = i_old
	  IF(i_old > i_new) i_cnt= i_new
	  DO j_cnt= 1, DEF_timesteps
	    j_tmparr(1:i_cnt,j_cnt)      = i_tfine(1:i_cnt,j_cnt)
	    j_tmparr(i_cnt+1:i_new,j_cnt)= 0
	  END DO

!---------- deallocate old array and allocate with new size

	  IF (allocated(i_tfine)) THEN
	    deallocate(i_tfine)
	  END IF
	  allocate(i_tfine(i_new, DEF_timesteps), stat=i_alct)
	  IF(i_alct /= 0) THEN
	    CALL print_error(31)
	  END IF

!---------- copy array
	  
	  DO j_cnt=1, DEF_timesteps
       	    i_tfine(1:i_new,j_cnt)= j_tmparr(1:i_new,j_cnt)
	  END DO
	  
!---------- deallocate temporary array and allocate with new size

	  deallocate(j_tmparr)

!---------- print logging information, if required

	  IF(GRID_parameters%iolog > 0) THEN
	    WRITE(GRID_parameters%iolog,*) ' LOGINFO: fine index table for tetrahedrons reallocated,'
	    WRITE(GRID_parameters%iolog,*) ' initial size: ', i_old
	    WRITE(GRID_parameters%iolog,*) ' new size:     ', i_new
	  END IF


!---------- array type selection (integer)

	CASE('edgegrid') array_type

!---------- allocate temporary array and let e_arr point to it

	  allocate(j_tmparr(i_new, DEF_timesteps), stat=i_alct)
	  IF(i_alct /= 0) THEN
	    CALL print_error(31)
	  END IF

!---------- copy array

	  i_cnt                  = i_old
	  IF(i_old > i_new) i_cnt= i_new
	  DO j_cnt=1, DEF_timesteps
	    j_tmparr(1:i_cnt,j_cnt)      = i_ggrid(1:i_cnt,j_cnt)
	    j_tmparr(i_cnt+1:i_new,j_cnt)= 0
	  END DO

!---------- deallocate old array and allocate with new size

	  IF (allocated(i_ggrid)) THEN
	    deallocate(i_ggrid)
	  END IF
	  allocate(i_ggrid(i_new, DEF_timesteps), stat=i_alct)
	  IF(i_alct /= 0) THEN
	    CALL print_error(31)
	  END IF

!---------- copy array

	  DO j_cnt=1, DEF_timesteps
	    i_ggrid(1:i_new,j_cnt)= j_tmparr(1:i_new,j_cnt)
	  END DO

!---------- deallocate old array and allocate with new size

	  deallocate(j_tmparr)

!---------- print logging information, if required

	  IF(GRID_parameters%iolog > 0) THEN
	    WRITE(GRID_parameters%iolog,*) ' LOGINFO: grid index table for edges reallocated,'
	    WRITE(GRID_parameters%iolog,*) ' initial size: ', i_old
	    WRITE(GRID_parameters%iolog,*) ' new size:     ', i_new
	  END IF

!---------- array type selection (integer)

	CASE('edgefine') array_type

!---------- allocate temporary array and let e_arr point to it

	  allocate(j_tmparr(i_new,DEF_timesteps), stat=i_alct)
	  IF(i_alct /= 0) THEN
	    CALL print_error(31)
	  END IF

!---------- copy array

	  i_cnt                  = i_old
	  IF(i_old > i_new) i_cnt= i_new
	  DO j_cnt=1,DEF_timesteps
	    j_tmparr(1:i_cnt,j_cnt)      = i_gfine(1:i_cnt,j_cnt)
	    j_tmparr(i_cnt+1:i_new,j_cnt)= 0
	  END DO

!---------- deallocate old array and allocate with new size

	  IF (allocated(i_gfine)) THEN
	    deallocate(i_gfine)
	  END IF
	  allocate(i_gfine(i_new, DEF_timesteps), stat=i_alct)
	  IF(i_alct /= 0) THEN
	    CALL print_error(31)
	  END IF

!---------- copy array
	  DO j_cnt= 1,DEF_timesteps
	    i_gfine(1:i_new,j_cnt)= j_tmparr(1:i_new,j_cnt)
	  END DO

!---------- deallocate old array and allocate with new size

	  deallocate(j_tmparr)

!---------- print logging information, if required

	  IF(GRID_parameters%iolog > 0) THEN
	    WRITE(GRID_parameters%iolog,*) ' LOGINFO: fine index table for edges reallocated,'
	    WRITE(GRID_parameters%iolog,*) ' initial size: ', i_old
	    WRITE(GRID_parameters%iolog,*) ' new size:     ', i_new
	  END IF

!---------- array type selection (integer)

	CASE('edgeboun') array_type

!---------- allocate temporary array and let e_arr point to it

	  allocate(j_tmparr(i_new, DEF_timesteps), stat=i_alct)
	  IF(i_alct /= 0) THEN
	    CALL print_error(31)
	  END IF

!---------- copy array

	  i_cnt                  = i_old
	  IF(i_old > i_new) i_cnt= i_new
	  DO j_cnt=1, DEF_timesteps
	    j_tmparr(1:i_cnt,j_cnt)      = i_gboun(1:i_cnt,j_cnt)
	    j_tmparr(i_cnt+1:i_new,j_cnt)= 0
	  END DO

!---------- deallocate old array and allocate with new size

	  IF (allocated(i_gboun)) THEN
	    deallocate(i_gboun)
	  END IF
	  allocate(i_gboun(i_new, DEF_timesteps), stat=i_alct)
	  IF(i_alct /= 0) THEN
	    CALL print_error(31)
	  END IF

!---------- copy array

	  DO j_cnt=1, DEF_timesteps
	    i_gboun(1:i_new,j_cnt)= j_tmparr(1:i_new,j_cnt)
	  END DO

!---------- deallocate old array and allocate with new size

	  deallocate(j_tmparr)

!---------- print logging information, if required

	  IF(GRID_parameters%iolog > 0) THEN
	    WRITE(GRID_parameters%iolog,*) ' LOGINFO: boundary index table for edges reallocated,'
	    WRITE(GRID_parameters%iolog,*) ' initial size: ', i_old
	    WRITE(GRID_parameters%iolog,*) ' new size:     ', i_new
	  END IF

!---------- array type selection (integer)

	CASE('nodegrid') array_type

!---------- allocate temporary array and let e_arr point to it

	  allocate(j_tmparr(i_new, DEF_timesteps), stat=i_alct)
	  IF(i_alct /= 0) THEN
	    CALL print_error(31)
	  END IF

!---------- copy array

	  i_cnt                  = i_old
	  IF(i_old > i_new) i_cnt= i_new
	  DO j_cnt=1, DEF_timesteps
	    j_tmparr(1:i_cnt,j_cnt)      = i_ngrid(1:i_cnt,j_cnt)
	    j_tmparr(i_cnt+1:i_new,j_cnt)= 0
	  END DO

!---------- deallocate old array and allocate with new size

	  IF (allocated(i_ngrid)) THEN
	    deallocate(i_ngrid)
	  END IF
	  allocate(i_ngrid(i_new, DEF_timesteps), stat=i_alct)
	  IF(i_alct /= 0) THEN
	    CALL print_error(31)
	  END IF

!---------- copy array

	  DO j_cnt=1, DEF_timesteps
	    i_ngrid(1:i_new,j_cnt)= j_tmparr(1:i_new,j_cnt)
	  END DO

!---------- deallocate old array and allocate with new size

	  deallocate(j_tmparr)

!---------- print logging information, if required

	  IF(GRID_parameters%iolog > 0) THEN
	    WRITE(GRID_parameters%iolog,*) ' LOGINFO: grid index table for nodes reallocated,'
	    WRITE(GRID_parameters%iolog,*) ' initial size: ', i_old
	    WRITE(GRID_parameters%iolog,*) ' new size:     ', i_new
	  END IF

	CASE DEFAULT array_type
	  IF(GRID_parameters%iolog > 0) THEN
	    WRITE(GRID_parameters%iolog,*) ' LOGINFO: DEFAULT actiontype in hash_realloc'
	  END IF
	  RETURN
	END SELECT array_type
	i_old= i_new

	RETURN

	END SUBROUTINE hash_realloc

!*****************************************************************
	SUBROUTINE hash_pack(p_ghand)

!---------- local declarations

	IMPLICIT NONE
	TYPE (grid_handle)                           :: p_ghand
	INTEGER (KIND = GRID_SI), DIMENSION(:), ALLOCATABLE :: i_auxe, i_auxg, i_auxn, i_auxt
	INTEGER (KIND = GRID_SI)                                      :: i_olde, i_oldg, i_oldn, &
	  i_newe, i_newg, i_newn, i_cnt, j_cnt, i_count, i_tim, i_newind, &
	  i_newefin, i_newgfin, i_newgboun, i_alct, i_oldt, i_newt, &
          i_newtfin, i_neweboun, i_tmp
	TYPE (elmt), POINTER                         :: p_etmp
	TYPE (edge), POINTER                         :: p_gtmp
	TYPE (node), POINTER                         :: p_ntmp
	TYPE (tetra), POINTER                        :: p_ttmp
	REAL (KIND = GRID_SR)                                         :: r_tmp

!---------- initialize integer counter variables

	i_tim= p_ghand%i_timetag
	i_newe= 1    ; i_newg= 1    ; i_newn= 1    ; i_newt= 1
	i_newefin= 1 ; i_newgfin= 1 ; i_newtfin= 1
	i_newgboun= 1 ; i_neweboun= 1

!---------- allocate auxiliary arrays

	i_olde= p_ghand%i_etotal
	i_oldt= p_ghand%i_ttotal
	i_oldg= p_ghand%i_gtotal
	i_oldn= p_ghand%i_ntotal
	allocate(i_auxe(0:i_olde), i_auxg(0:i_oldg), i_auxn(0:i_oldn), &
		i_auxt(0:i_oldt), stat=i_alct)
	IF(i_alct /= 0) THEN
	  CALL print_error(32)
	END IF
	i_auxe(0)= 0; i_auxg(0)= 0; i_auxn(0)= 0; i_auxt(0)= 0

!---------- search hashing tables for gaps and collect these in index arrays

	i_count= 0
	elem_search: DO i_cnt=1,i_olde
	  assoc_elem: IF(associated(p_ehash(i_cnt)%ep)) THEN
	    i_count= i_count+ 1
	    i_auxe(i_cnt)= i_count
	  ELSE assoc_elem
	    i_auxe(i_cnt)= 0
	  END IF assoc_elem
	END DO elem_search
	p_grid(:)%i_etotal= i_count
	r_tmp = REAL(i_count, GRID_SR)/ REAL(DEF_hashblock, GRID_SR)
	i_newe= CEILING(r_tmp)* DEF_hashblock

	i_count= 0
	tetra_search: DO i_cnt=1,i_oldt
	  assoc_tetra: IF(associated(p_thash(i_cnt)%tp)) THEN
	    i_count= i_count+ 1
	    i_auxt(i_cnt)= i_count
	  ELSE assoc_tetra
	    i_auxt(i_cnt)= 0
	  END IF assoc_tetra
	END DO tetra_search
	p_grid(:)%i_ttotal= i_count
	r_tmp = REAL(i_count, GRID_SR)/ REAL(DEF_hashblock, GRID_SR)
	i_newt= CEILING(r_tmp)* DEF_hashblock
	    
	i_count= 0
	edge_search: DO i_cnt=1,i_oldg
	  assoc_edge: IF(associated(p_ghash(i_cnt)%gp)) THEN
	    i_count= i_count+ 1
	    i_auxg(i_cnt)= i_count
	  ELSE assoc_edge
	    i_auxg(i_cnt)= 0
	  END IF assoc_edge
	END DO edge_search
	p_grid(:)%i_gtotal= i_count
	r_tmp = REAL(i_count, GRID_SR)/ REAL(DEF_hashblock, GRID_SR)
	i_newg= CEILING(r_tmp)* DEF_hashblock
	    
	i_count= 0
	node_search: DO i_cnt=1,i_oldn
	  assoc_node: IF(associated(p_nhash(i_cnt)%np)) THEN
	    i_count= i_count+ 1
	    i_auxn(i_cnt)= i_count
	  ELSE assoc_node
	    i_auxn(i_cnt)= 0
	  END IF assoc_node
	END DO node_search
	p_grid(:)%i_ntotal= i_count
	r_tmp = REAL(i_count, GRID_SR)/ REAL(DEF_hashblock, GRID_SR)
	i_newn= CEILING(r_tmp)* DEF_hashblock

!---------- pack hashing tables

	elem_pack: DO i_cnt=1,i_olde
	  i_newind= i_auxe(i_cnt)
	  elem_nogap: IF(i_newind /= 0) THEN
	    p_ehash(i_newind)              = p_ehash(i_cnt)
	    p_ehash(i_newind)%ep%def%i_indx= i_newind
	  END IF elem_nogap
	END DO elem_pack

	tetra_pack: DO i_cnt=1,i_oldt
	  i_newind= i_auxt(i_cnt)
	  tetra_nogap: IF(i_newind /= 0) THEN
	    p_thash(i_newind)              = p_thash(i_cnt)
	    p_thash(i_newind)%tp%def%i_indx= i_newind
	  END IF tetra_nogap
	END DO tetra_pack

	edge_pack: DO i_cnt=1,i_oldg
	  i_newind= i_auxg(i_cnt)
	  edge_nogap: IF(i_newind /= 0) THEN
	    p_ghash(i_newind)              = p_ghash(i_cnt)
	    p_ghash(i_newind)%gp%def%i_indx= i_newind
	  END IF edge_nogap
	END DO edge_pack

	node_pack: DO i_cnt=1,i_oldn
	  i_newind= i_auxn(i_cnt)
	  node_nogap: IF(i_newind /= 0) THEN
	    p_nhash(i_newind)              = p_nhash(i_cnt)
	    p_nhash(i_newind)%np%def%i_indx= i_newind
	  END IF node_nogap
	END DO node_pack

!---------- update index tables

	elem_gridind: DO j_cnt=1, DEF_timesteps
	  i_count= 0
	  elem_number: DO i_cnt= 1, i_egridmax
	    i_newind= i_auxe(i_egrid(i_cnt,j_cnt))
	    zero_elem: IF(i_newind /= 0) THEN
	      i_count= i_count+ 1
	      i_egrid(i_count, j_cnt)= i_newind
	    END IF zero_elem
	  END DO elem_number
	  i_egrid(i_count+1:i_egridmax,j_cnt)= 0
	  p_grid(j_cnt)%i_enumber= i_count
	END DO elem_gridind

	tetra_gridind: DO j_cnt=1, DEF_timesteps
	  i_count= 0
	  tetra_number: DO i_cnt= 1, i_tgridmax
	    i_newind= i_auxt(i_tgrid(i_cnt,j_cnt))
	    zero_tetra: IF(i_newind /= 0) THEN
	      i_count= i_count+ 1
	      i_tgrid(i_count, j_cnt)= i_newind
	    END IF zero_tetra
	  END DO tetra_number
	  i_tgrid(i_count+1:i_tgridmax,j_cnt)= 0
	  p_grid(j_cnt)%i_tnumber= i_count
	END DO tetra_gridind
	    
	edge_gridind: DO j_cnt=1, DEF_timesteps
	  i_count= 0
	  edge_number: DO i_cnt= 1, i_ggridmax
	    i_newind= i_auxg(i_ggrid(i_cnt,j_cnt))
	    zero_edge: IF(i_newind /= 0) THEN
	      i_count= i_count+ 1
	      i_ggrid(i_count, j_cnt)= i_newind
	    END IF zero_edge
	  END DO edge_number
	  i_ggrid(i_count+1:i_ggridmax,j_cnt)= 0
	  p_grid(j_cnt)%i_gnumber = i_count
	END DO edge_gridind

	node_gridind: DO j_cnt=1, DEF_timesteps
	  i_count= 0
	  node_number: DO i_cnt= 1, i_ngridmax
	    i_newind= i_auxn(i_ngrid(i_cnt,j_cnt))
	    zero_node: IF(i_newind /= 0) THEN
	      i_count= i_count+ 1
	      i_ngrid(i_count, j_cnt)= i_newind
	    END IF zero_node
	  END DO node_number
	  i_ngrid(i_count+1:i_ngridmax,j_cnt)= 0
	  p_grid(j_cnt)%i_nnumber= i_count
	END DO node_gridind

	CALL hash_gridupdate(p_ghand)

!---------- now update numbering of all the grid objects and renew index arrays

	i_count  =  p_ghand%i_etotal
	elem_update: DO i_cnt=1, i_count
	  p_etmp           => p_ehash(i_cnt)%ep
	  p_etmp%def%p_node=  i_auxn(p_etmp%def%p_node)
	  p_etmp%def%p_edge=  i_auxg(p_etmp%def%p_edge)
	  p_etmp%att%i_edge=  i_auxg(p_etmp%att%i_edge)
	  p_etmp%att%p_tets=  i_auxt(p_etmp%att%p_tets)
	  DO j_cnt=1, DEF_timesteps
	    p_etmp%lnk%p_prnt(j_cnt)  = i_auxe(p_etmp%lnk%p_prnt(j_cnt))
	    p_etmp%lnk%p_chil(:,j_cnt)= i_auxe(p_etmp%lnk%p_chil(:,j_cnt))
	  END DO
	END DO elem_update

	i_count  =  p_ghand%i_ttotal
	tetra_update: DO i_cnt=1, i_count
	  p_ttmp           => p_thash(i_cnt)%tp
	  p_ttmp%def%p_node=  i_auxn(p_ttmp%def%p_node)
	  p_ttmp%def%p_edge=  i_auxg(p_ttmp%def%p_edge)
	  p_ttmp%def%p_elem=  i_auxe(p_ttmp%def%p_elem)
	  DO j_cnt=1, DEF_timesteps
	    p_ttmp%att%i_elmt(j_cnt)  = i_auxe(p_ttmp%att%i_elmt(j_cnt))
	    p_ttmp%lnk%p_prnt(j_cnt)  = i_auxt(p_ttmp%lnk%p_prnt(j_cnt))
	    p_ttmp%lnk%p_chil(:,j_cnt)= i_auxt(p_ttmp%lnk%p_chil(:,j_cnt))
	  END DO
	END DO tetra_update

	i_count   =  p_ghand%i_gtotal
	edge_update: DO i_cnt=1, i_count
	  p_gtmp           => p_ghash(i_cnt)%gp
	  i_tmp            =  p_gtmp%att%i_elem
	  p_gtmp%def%p_node=  i_auxn(p_gtmp%def%p_node)
	  p_gtmp%att%p_elem(1:i_tmp)=  i_auxe(p_gtmp%att%p_elem(1:i_tmp))
	  p_gtmp%lnk%p_peri=  i_auxg(p_gtmp%lnk%p_peri)
	  p_gtmp%att%i_node=  i_auxn(p_gtmp%att%i_node)
	  DO j_cnt=1, DEF_timesteps
	    p_gtmp%lnk%p_chil(:,j_cnt)= i_auxg(p_gtmp%lnk%p_chil(:,j_cnt))
	  END DO
	END DO edge_update

	i_count   =  p_ghand%i_ntotal
	node_update: DO i_cnt=1, i_count
	  p_ntmp           => p_nhash(i_cnt)%np
	  DO j_cnt=1, DEF_timesteps
	    i_tmp                     =  p_ntmp%att%i_ptch(j_cnt)
	    p_ntmp%att%p_ptch(1:i_tmp,j_cnt)= i_auxt(p_ntmp%att%p_ptch(1:i_tmp,j_cnt))
	  END DO
	  p_ntmp%att%p_edge= i_auxg(p_ntmp%att%p_edge)
	  p_ntmp%lnk%p_peri= i_auxn(p_ntmp%lnk%p_peri)
	END DO node_update

!---------- deallocate auxiliary arrays

	deallocate(i_auxe, i_auxg, i_auxn, i_auxt)

!---------- reallocate hashing tables and index arrays (if possible)

	IF(i_ehashmax > i_newe) &
	  CALL hash_realloc(i_ehashmax, i_newe, c_action='elmthash')
	IF(i_thashmax > i_newt) &
	  CALL hash_realloc(i_thashmax, i_newt, c_action='tetrhash')
	IF(i_ghashmax > i_newg) &
	  CALL hash_realloc(i_ghashmax, i_newg, c_action='edgehash')
	IF(i_nhashmax > i_newn) &
	  CALL hash_realloc(i_nhashmax, i_newn, c_action='nodehash')

!---------- update fine and boundary index tables

	CALL hash_fineupdate(p_grid(i_futuretime))

	RETURN
	END SUBROUTINE hash_pack

!*****************************************************************
	SUBROUTINE hash_gridupdate(p_ghand)

!---------- local declarations

	IMPLICIT NONE
	TYPE (grid_handle)                 :: p_ghand
	INTEGER (KIND = GRID_SI)                            :: i_cnt, i_tim, i_alct
	INTEGER (KIND = GRID_SI)                            :: i_olde, i_oldg, i_oldn, i_oldt
	INTEGER (KIND = GRID_SI)                            :: i_newe, i_newg, i_newn, i_newt
	INTEGER (KIND = GRID_SI)                            :: i_cnte, i_cntg, i_cntn, i_cntt
	INTEGER (KIND = GRID_SI)                            :: i_maxe, i_maxg, i_maxn, i_maxt
	REAL (KIND = GRID_SR)                               :: r_tmp
	INTEGER (KIND = GRID_SI), DIMENSION(:), ALLOCATABLE :: i_auxe, i_auxg, i_auxn, i_auxt

!---------- initialize integer counter variables

	i_tim= p_ghand%i_timetag

!---------- allocate auxiliary arrays

	i_olde= i_egridmax; i_oldg= i_ggridmax; i_oldn= i_ngridmax
	i_oldt= i_tgridmax
	allocate(i_auxe(0:i_olde), i_auxg(0:i_oldg), i_auxn(0:i_oldn), &
		 i_auxt(0:i_oldt), stat=i_alct)
	IF(i_alct /= 0) THEN
	  CALL print_error(32)
	END IF
	i_auxe(0)= 0; i_auxg(0)= 0; i_auxn(0)= 0; i_auxt(0)= 0

!---------- search hashing tables for gaps and collect these in index arrays

	i_cnte= 0
	elem_loop: DO i_cnt=1,p_ghand%i_enumber
	  exist_elem: IF(i_egrid(i_cnt,i_tim) /= 0) THEN
	    i_cnte= i_cnte+ 1
	    i_auxe(i_cnte)= i_egrid(i_cnt,i_tim)
	  END IF exist_elem
	END DO elem_loop
	r_tmp = REAL(i_cnte, GRID_SR)/ REAL(DEF_hashblock, GRID_SR)
	i_newe= CEILING(r_tmp)* DEF_hashblock

	i_cntt= 0
	tetra_loop: DO i_cnt=1,p_ghand%i_tnumber
	  exist_tetra: IF(i_tgrid(i_cnt,i_tim) /= 0) THEN
	    i_cntt= i_cntt+ 1
	    i_auxt(i_cntt)= i_tgrid(i_cnt,i_tim)
	  END IF exist_tetra
	END DO tetra_loop
	r_tmp = REAL(i_cntt, GRID_SR)/ REAL(DEF_hashblock, GRID_SR)
	i_newt= CEILING(r_tmp)* DEF_hashblock
	    
	i_cntg= 0
	edge_loop: DO i_cnt=1,p_ghand%i_gnumber
	  exist_edge: IF(i_ggrid(i_cnt,i_tim) /= 0) THEN
	    i_cntg= i_cntg+ 1
	    i_auxg(i_cntg)= i_ggrid(i_cnt,i_tim)
	  END IF exist_edge
	END DO edge_loop
	r_tmp = REAL(i_cntg, GRID_SR)/ REAL(DEF_hashblock, GRID_SR)
	i_newg= CEILING(r_tmp)* DEF_hashblock
	    
	i_cntn= 0
	node_loop: DO i_cnt=1,p_ghand%i_nnumber
	  exist_node: IF(i_ngrid(i_cnt,i_tim) /= 0) THEN
	    i_cntn= i_cntn+ 1
	    i_auxn(i_cntn)= i_ngrid(i_cnt,i_tim)
	  END IF exist_node
	END DO node_loop
	r_tmp = REAL(i_cntn, GRID_SR)/ REAL(DEF_hashblock, GRID_SR)
	i_newn= CEILING(r_tmp)* DEF_hashblock
	    
!---------- update index tables

	i_egrid(1:i_cnte,i_tim)       = i_auxe(1:i_cnte)
	i_egrid(i_cnte+1:i_olde,i_tim)= 0
	p_ghand%i_enumber= i_cnte

	i_tgrid(1:i_cntt,i_tim)       = i_auxt(1:i_cntt)
	i_tgrid(i_cntt+1:i_oldt,i_tim)= 0
	p_ghand%i_tnumber= i_cntt

	i_ggrid(1:i_cntg,i_tim)       = i_auxg(1:i_cntg)
	i_ggrid(i_cntg+1:i_oldg,i_tim)= 0
	p_ghand%i_gnumber= i_cntg

	i_ngrid(1:i_cntn,i_tim)       = i_auxn(1:i_cntn)
	i_ngrid(i_cntn+1:i_oldn,i_tim)= 0
	p_ghand%i_nnumber= i_cntn

!---------- deallocate auxiliary arrays

	deallocate(i_auxe, i_auxg, i_auxn, i_auxt)

!---------- determine required arraysize

	i_maxe= MAXVAL(p_grid(:)%i_enumber)
	r_tmp = REAL(i_maxe, GRID_SR)/ REAL(DEF_hashblock, GRID_SR)
	i_newe= MAX(i_newe, (CEILING(r_tmp)* DEF_hashblock))

	i_maxt= MAXVAL(p_grid(:)%i_tnumber)
	r_tmp = REAL(i_maxt, GRID_SR)/ REAL(DEF_hashblock, GRID_SR)
	i_newt= MAX(i_newt, (CEILING(r_tmp)* DEF_hashblock))

	i_maxg= MAXVAL(p_grid(:)%i_gnumber)
	r_tmp = REAL(i_maxg, GRID_SR)/ REAL(DEF_hashblock, GRID_SR)
	i_newg= MAX(i_newg, (CEILING(r_tmp)* DEF_hashblock))

	i_maxn= MAXVAL(p_grid(:)%i_nnumber)
	r_tmp = REAL(i_maxn, GRID_SR)/ REAL(DEF_hashblock, GRID_SR)
	i_newn= MAX(i_newn, (CEILING(r_tmp)* DEF_hashblock))


!---------- reallocate hashing tables and index arrays (if possible)

	IF(i_egridmax > i_newe) &
	  CALL hash_realloc(i_egridmax, i_newe, c_action='elmtgrid')
	IF(i_tgridmax > i_newt) &
	  CALL hash_realloc(i_tgridmax, i_newt, c_action='tetrgrid')
	IF(i_ggridmax > i_newg) &
	  CALL hash_realloc(i_ggridmax, i_newg, c_action='edgegrid')
	IF(i_ngridmax > i_newn) &
	  CALL hash_realloc(i_ngridmax, i_newn, c_action='nodegrid')


	RETURN
	END SUBROUTINE hash_gridupdate
!*****************************************************************
	SUBROUTINE hash_fineupdate(p_ghand)

!---------- local declarations

	IMPLICIT NONE
	TYPE (grid_handle)            :: p_ghand
	INTEGER (KIND = GRID_SI)                       :: i_cnt, i_ef, i_gf, i_gb, i_tim, i_tmp
	INTEGER (KIND = GRID_SI)                       :: i_eb, i_tf, i_nsiz
	REAL (KIND = GRID_SR)                          :: r_tmp
	TYPE (elmt), POINTER          :: p_etmp
	TYPE (tetra), POINTER         :: p_ttmp
	TYPE (edge), POINTER          :: p_gtmp

!---------- initialize counters

	i_tim= p_ghand%i_timetag
	i_ef= 0; i_gf= 0; i_gb= 0; i_tf= 0; i_eb= 0

!---------- loop through elements
!           NOTE: take indices of children as indicator for finest grid, because
!                 in SLM_adapt some elements on finest grid have flag 
!                 DEF_pleascoarse instead of DEF_unrefined

	elem_loop: DO i_cnt=1, p_ghand%i_enumber
	  p_etmp => p_ehash(i_egrid(i_cnt, i_tim))%ep
	  fine_levl: IF(p_etmp%lnk%p_chil(1, i_tim) == 0) THEN
	    i_ef= i_ef+ 1
	    IF(i_ef > i_efinemax) THEN
	      i_nsiz= i_efinemax+ DEF_hashblock
	      CALL hash_realloc(i_efinemax, i_nsiz, c_action='elmtfine')
	    END IF
	    i_efine(i_ef, i_tim)= i_egrid(i_cnt, i_tim)
	    boun_elmt: IF(p_etmp%att%i_boun /= DEF_inner) THEN
	      i_eb= i_eb+ 1
	      IF(i_eb > i_ebounmax) THEN
	        i_nsiz= i_ebounmax+ DEF_hashblock
	        CALL hash_realloc(i_ebounmax, i_nsiz, c_action='elmtboun')
	      END IF
	      i_eboun(i_eb, i_tim)= i_egrid(i_cnt, i_tim)
	    END IF boun_elmt
	  END IF fine_levl
	END DO elem_loop

!---------- loop through tetrahedrons
!           NOTE: take indices of children as indicator for finest grid, because
!                 in SLM_adapt some elements on finest grid have flag 
!                 DEF_pleascoarse instead of DEF_unrefined

	tetra_loop: DO i_cnt=1, p_ghand%i_tnumber
	  p_ttmp => p_thash(i_tgrid(i_cnt, i_tim))%tp
	  fine_tetra: IF(p_ttmp%lnk%p_chil(1, i_tim) == 0) THEN
	    i_tf= i_tf+ 1
	    IF(i_tf > i_tfinemax) THEN
	      i_nsiz= i_tfinemax+ DEF_hashblock
	      CALL hash_realloc(i_tfinemax, i_nsiz, c_action='tetrfine')
	    END IF
	    i_tfine(i_tf, i_tim)= i_tgrid(i_cnt, i_tim)
	  END IF fine_tetra
	END DO tetra_loop

!---------- loop through edges

	edge_loop: DO i_cnt=1, p_ghand%i_gnumber
	  p_gtmp => p_ghash(i_ggrid(i_cnt, i_tim))%gp
	  fine_edge: IF(p_gtmp%att%i_stat(i_tim) == DEF_unrefined) THEN
	    i_gf= i_gf+ 1
	    IF(i_gf > i_gfinemax) THEN
	      i_nsiz= i_gfinemax+ DEF_hashblock
	      CALL hash_realloc(i_gfinemax, i_nsiz, c_action='edgefine')
	    END IF
	    i_gfine(i_gf, i_tim)= i_ggrid(i_cnt, i_tim)
	    boun_edge: IF(p_gtmp%att%i_boun /= DEF_inner) THEN
	      i_gb= i_gb+ 1
	      IF(i_gb > i_gbounmax) THEN
	        i_nsiz= i_gbounmax+ DEF_hashblock
	        CALL hash_realloc(i_gbounmax, i_nsiz, c_action='edgeboun')
	      END IF
	      i_gboun(i_gb, i_tim)= i_ggrid(i_cnt, i_tim)
	    END IF boun_edge
	  END IF fine_edge
	END DO edge_loop

!---------- update handles

	p_ghand%i_enumfine= i_ef
	p_ghand%i_gnumfine= i_gf
	p_ghand%i_tnumfine= i_tf
	p_ghand%i_gnumboun= i_gb
	p_ghand%i_enumboun= i_eb

!---------- reallocate index tables if necessary

	i_tmp = MAXVAL(p_grid(:)%i_enumfine)
	r_tmp = REAL(i_tmp, GRID_SR)/ REAL(DEF_hashblock, GRID_SR)
	i_tmp = MAX(i_ef, (CEILING(r_tmp)* DEF_hashblock))
	IF(i_efinemax > i_tmp) &
	  CALL hash_realloc(i_efinemax, i_tmp, c_action='elmtfine')
	
	i_tmp = MAXVAL(p_grid(:)%i_tnumfine)
	r_tmp = REAL(i_tmp, GRID_SR)/ REAL(DEF_hashblock, GRID_SR)
	i_tmp = MAX(i_tf,(CEILING(r_tmp)* DEF_hashblock))
	IF(i_tfinemax > i_tmp) &
	  CALL hash_realloc(i_tfinemax, i_tmp, c_action='tetrfine')
	  
	i_tmp = MAXVAL(p_grid(:)%i_gnumfine)
	r_tmp = REAL(i_tmp, GRID_SR)/ REAL(DEF_hashblock, GRID_SR)
	i_tmp = MAX(i_gf, (CEILING(r_tmp)* DEF_hashblock))
	IF(i_gfinemax > i_tmp) &
	  CALL hash_realloc(i_gfinemax, i_tmp, c_action='edgefine')
	  
	i_tmp = MAXVAL(p_grid(:)%i_gnumboun)  
	r_tmp = REAL(i_tmp, GRID_SR)/ REAL(DEF_hashblock, GRID_SR)
	i_tmp = MAX(i_gb, (CEILING(r_tmp)* DEF_hashblock))
	IF(i_gbounmax > i_tmp) &
	  CALL hash_realloc(i_gbounmax, i_tmp, c_action='edgeboun')
	  
	i_tmp = MAXVAL(p_grid(:)%i_enumboun)  
	r_tmp = REAL(i_tmp, GRID_SR)/ REAL(DEF_hashblock, GRID_SR)
	i_tmp = MAX(i_eb, (CEILING(r_tmp)* DEF_hashblock))
	IF(i_ebounmax > i_tmp) &
	  CALL hash_realloc(i_ebounmax, i_tmp, c_action='elmtboun')

!---------- nullify empty tails

	i_efine(i_ef+1:i_efinemax, i_tim)= 0
	i_tfine(i_tf+1:i_tfinemax, i_tim)= 0
	i_gfine(i_gf+1:i_gfinemax, i_tim)= 0
	i_gboun(i_gb+1:i_gbounmax, i_tim)= 0
	i_eboun(i_eb+1:i_ebounmax, i_tim)= 0

	RETURN
	END SUBROUTINE hash_fineupdate
!*****************************************************************
	SUBROUTINE clearup_grid(p_cleargrid)

!---------- local declarations

	IMPLICIT NONE

	TYPE (grid_handle), INTENT(inout)    :: p_cleargrid

	INTEGER (KIND = GRID_SI)                              :: i_tim, i_cnt, i_end, i_lvl, i_tmp

!---------- set time tag

	i_tim= p_cleargrid%i_timetag

!---------- loop through the linked list of elements

	i_end = p_cleargrid%i_enumber
	element_loop: DO i_cnt= 1, i_end
	  i_tmp= p_ehash(i_egrid(i_cnt,i_tim))%ep%att%i_time

!---------- destroy element only if it is too old

	  too_olde: IF(i_tmp /= i_tim) THEN
	    CALL erase_elmt(p_ehash(i_egrid(i_cnt,i_tim))%ep, p_cleargrid)
	    i_egrid(i_cnt,i_tim)= 0
	  END IF too_olde
	END DO element_loop
	i_egrid(i_end+1:i_egridmax, i_tim)= 0


!---------- loop through the linked list of tetrahedrons

	i_end = p_cleargrid%i_tnumber
	tetrahedron_loop: DO i_cnt= 1, i_end
	  i_tmp= p_thash(i_tgrid(i_cnt,i_tim))%tp%att%i_time

!---------- destroy element only if it is too old

	  too_oldt: IF(i_tmp /= i_tim) THEN
	    CALL erase_tetra(p_thash(i_tgrid(i_cnt,i_tim))%tp, p_cleargrid)
	    i_tgrid(i_cnt,i_tim)= 0
	  END IF too_oldt
	END DO tetrahedron_loop
	i_tgrid(i_end+1:i_tgridmax, i_tim)= 0


!---------- loop through the linked list of edges

	i_end = p_cleargrid%i_gnumber
	edge_loop: DO i_cnt= 1, i_end
	  i_tmp= p_ghash(i_ggrid(i_cnt,i_tim))%gp%att%i_time

!---------- destroy edge only if it is too old

	  too_oldg: IF(i_tmp /= i_tim) THEN
	    CALL erase_edge(p_ghash(i_ggrid(i_cnt,i_tim))%gp, p_cleargrid)
	    i_ggrid(i_cnt,i_tim)= 0
	  END IF too_oldg
	END DO edge_loop
	i_ggrid(i_end+1:i_ggridmax, i_tim)= 0

!---------- loop through the linked list of nodes

	i_end = p_cleargrid%i_nnumber
	node_loop: DO i_cnt= 1, i_end
	  i_tmp= p_nhash(i_ngrid(i_cnt,i_tim))%np%att%i_time

!---------- destroy element only if it is too old

	  too_oldn: IF(i_tmp /= i_tim) THEN
	    CALL erase_node(p_nhash(i_ngrid(i_cnt,i_tim))%np, p_cleargrid)
	    i_ngrid(i_cnt,i_tim)= 0
	  END IF too_oldn
	END DO node_loop
	i_ngrid(i_end+1:i_ngridmax, i_tim)= 0

!---------- pack hashing and index tables

!	CALL hash_pack(p_cleargrid)
	CALL hash_gridupdate(p_cleargrid)
	CALL hash_fineupdate(p_cleargrid)

!---------- update global level information

	i_lvl =  p_ehash(i_egrid(1,i_tim))%ep%att%i_levl
	p_cleargrid%i_minlvl=  i_lvl
	p_cleargrid%i_maxlvl=  i_lvl
	lvl_loop: DO i_cnt=2,p_cleargrid%i_enumber
	  i_lvl               =  p_ehash(i_egrid(i_cnt,i_tim))%ep%att%i_levl
	  p_cleargrid%i_minlvl=  MIN(p_cleargrid%i_minlvl, i_lvl)
	  p_cleargrid%i_maxlvl=  MAX(p_cleargrid%i_maxlvl, i_lvl)
	END DO lvl_loop

	RETURN
	END SUBROUTINE clearup_grid

!*****************************************************************
	SUBROUTINE grid_clean

!---------- local declarations

	IMPLICIT NONE

	TYPE (elmt), POINTER                 :: p_eclr
	TYPE (tetra), POINTER                :: p_tclr
	TYPE (edge), POINTER                 :: p_gclr
	TYPE (node), POINTER                 :: p_nclr
	INTEGER (KIND = GRID_SI)                              :: i_tim, i_cnt, i_end, i_tmp

!---------- set time tag (globally available)

	i_tim= i_pasttime
	i_tmp= i_futuretime

!---------- loop through the elements and destroy erased ones

	i_end = p_grid(i_tim)%i_etotal
	element_loop: DO i_cnt= 1, i_end
	  p_eclr => p_ehash(i_cnt)%ep
	  erased_e: IF(ANY(p_eclr%att%i_stat == DEF_erased)) THEN
	    CALL destroy_elmt(p_eclr, p_grid(i_tim))
	  END IF erased_e
	END DO element_loop

!---------- loop through the tetrahedrons and destroy erased ones

	i_end = p_grid(i_tim)%i_ttotal
	tetrahedron_loop: DO i_cnt= 1, i_end
	  p_tclr => p_thash(i_cnt)%tp
	  erased_t: IF(ANY(p_tclr%att%i_stat == DEF_erased)) THEN
	    CALL destroy_tetra(p_tclr, p_grid(i_tim))
	  END IF erased_t
	END DO tetrahedron_loop

!---------- loop through the edges and destroy erased ones

	i_end = p_grid(i_tim)%i_gtotal
	edge_loop: DO i_cnt= 1, i_end
	  p_gclr => p_ghash(i_cnt)%gp
	  erased_g: IF(ANY(p_gclr%att%i_stat == DEF_erased)) THEN
	    CALL destroy_edge(p_gclr, p_grid(i_tim))
	  END IF erased_g
	END DO edge_loop

!---------- loop through the nodes and destroy erased ones

	i_end = p_grid(i_tim)%i_ntotal
	node_loop: DO i_cnt= 1, i_end
	  p_nclr => p_nhash(i_cnt)%np
	  erased_n: IF(ANY(p_nclr%att%i_stat == DEF_erased)) THEN
	    CALL destroy_node(p_nclr, p_grid(i_tim))
	  END IF erased_n
	END DO node_loop

!---------- pack hashing and index tables

	CALL hash_pack(p_grid(i_tim))

!---------- update indices and patches

	CALL renew_patches(p_grid(i_futuretime))

	RETURN
	END SUBROUTINE grid_clean

!*****************************************************************
	SUBROUTINE renew_patches(p_ghand)

!---------- local declarations

	IMPLICIT NONE

	TYPE (grid_handle)      :: p_ghand
	TYPE (tetra), POINTER   :: p_ttmp
	TYPE (elmt), POINTER    :: p_etmp
	TYPE (edge), POINTER    :: p_gtmp
	TYPE (node), POINTER    :: p_ntmp
	INTEGER (KIND = GRID_SI)	                :: i_tim, i_cnt, j_cnt, i_tmp, k_cnt, i_mot, i_mpo

!---------- initialize time tag

	i_tim= p_ghand%i_timetag
	IF(i_tim /= i_futuretime) THEN
	  CALL print_error(a_err='[renew_patches]: Please modify only the future grid!')
	END IF

!---------- initialize counters for this grid

	init_loop: DO j_cnt= 1, p_ghand%i_nnumber
	  p_ntmp            => p_nhash(i_ngrid(j_cnt,i_tim))%np
	  p_ntmp%att%i_ptch(i_tim) =  0
	  p_ntmp%att%p_ptch(:,i_tim) =  0
	END DO init_loop

!---------- loop through the tetrahedrons and update node patches

	tetra_loop: DO k_cnt= 1, p_ghand%i_tnumfine
	  p_ttmp => p_thash(i_tfine(k_cnt,i_tim))%tp
	  node_loop: DO i_cnt=1, DEF_tetnodes
	    p_ntmp => p_nhash(p_ttmp%def%p_node(i_cnt))%np
	    i_tmp =  p_ntmp%att%i_ptch(i_tim)+ 1
	    over_flow: IF(i_tmp > DEF_ndpatch) THEN
	      CALL print_error(a_err='[renew_patches]: Node`s patch update produced overflow')
	    ELSE over_flow
	      p_ntmp%att%p_ptch(i_tmp,i_tim) = p_ttmp%def%i_indx
	      p_ntmp%att%i_ptch(i_tim) = i_tmp
	    END IF over_flow
	  END DO node_loop
	END DO tetra_loop

!---------- initialize edge counters for this grid

	initg_loop: DO j_cnt= 1, p_ghand%i_gnumber
	  p_gtmp            => p_ghash(i_ggrid(j_cnt,i_tim))%gp
	  p_gtmp%att%i_elem=  0
	  p_gtmp%att%p_elem=  0
	END DO initg_loop

!---------- loop through the elements and update edge element info

	elem_loop: DO k_cnt= 1, p_ghand%i_enumber
	  p_etmp => p_ehash(i_egrid(k_cnt,i_tim))%ep
	  edge_loop: DO i_cnt=1, DEF_eledges
	    p_gtmp => p_ghash(p_etmp%def%p_edge(i_cnt))%gp
	    i_tmp =  p_gtmp%att%i_elem

!---------- check, if elements mother has been registered, if so, replace mother

	    i_mpo=0
	    i_mot=p_etmp%lnk%p_prnt(i_tim)
	    check_loop: DO j_cnt=1,i_tmp
	      IF(p_gtmp%att%p_elem(j_cnt) == i_mot) THEN
	        i_mpo= j_cnt
	        EXIT check_loop
	      END IF
	    END DO check_loop
	    IF(i_mpo > 0) THEN
	      p_gtmp%att%p_elem(j_cnt)= p_etmp%def%i_indx
	    ELSE
	      i_tmp= i_tmp+ 1
	      overg_flow: IF(i_tmp > DEF_egelems) THEN
	        CALL print_error(a_err='[renew_patches]: Edge update produced overflow')
	      ELSE overg_flow
	        p_gtmp%att%p_elem(i_tmp)= p_etmp%def%i_indx
	        p_gtmp%att%i_elem       = i_tmp
	      END IF overg_flow
	    END IF
	  END DO edge_loop
	END DO elem_loop

	RETURN
	END SUBROUTINE renew_patches

!*****************************************************************
	SUBROUTINE grid_initdatastruct(c_action)

!---------- local declarations

	IMPLICIT NONE

	CHARACTER (len=4), OPTIONAL :: c_action
	INTEGER (KIND = GRID_SI)                     :: i_tmp
	CHARACTER (len=4)           :: c_act

!---------- initialize action type

	act_present: IF(present(c_action)) THEN
	  c_act= c_action
	ELSE act_present
	  c_act= 'init'
	END IF act_present

	act_type: IF(c_act == 'size') THEN

!---------- initialize hashing tables

	  i_tmp= i_ehashmax
	  CALL hash_initalloc(i_ehashmax, i_init= i_tmp, c_action='elmthash')
	  i_tmp= i_thashmax
	  CALL hash_initalloc(i_thashmax, i_init= i_tmp, c_action='tetrhash')
	  i_tmp= i_ghashmax
	  CALL hash_initalloc(i_ghashmax, i_init= i_tmp, c_action='edgehash')
	  i_tmp= i_nhashmax
	  CALL hash_initalloc(i_nhashmax, i_init= i_tmp, c_action='nodehash')

!---------- initialize grid index arrays

	  i_tmp= i_egridmax
	  CALL hash_initalloc(i_egridmax, i_init= i_tmp, c_action='elmtgrid')
	  i_tmp= i_tgridmax
	  CALL hash_initalloc(i_tgridmax, i_init= i_tmp, c_action='tetrgrid')
	  i_tmp= i_ggridmax
	  CALL hash_initalloc(i_ggridmax, i_init= i_tmp, c_action='edgegrid')
	  i_tmp= i_ngridmax
	  CALL hash_initalloc(i_ngridmax, i_init= i_tmp, c_action='nodegrid')

!---------- initialize finest grid and boundary index arrays

	  i_tmp= i_efinemax
	  CALL hash_initalloc(i_efinemax, i_init= i_tmp, c_action='elmtfine')
	  i_tmp= i_tfinemax
	  CALL hash_initalloc(i_tfinemax, i_init= i_tmp, c_action='tetrfine')
	  i_tmp= i_gfinemax
	  CALL hash_initalloc(i_gfinemax, i_init= i_tmp, c_action='edgefine')
	  i_tmp= i_gbounmax
	  CALL hash_initalloc(i_gbounmax, i_init= i_tmp, c_action='edgeboun')
	  i_tmp= i_ebounmax
	  CALL hash_initalloc(i_ebounmax, i_init= i_tmp, c_action='elmtboun')
	ELSE act_type

!---------- initialize hashing tables

	  CALL hash_initalloc(i_ehashmax, c_action='elmthash')
	  CALL hash_initalloc(i_ghashmax, c_action='edgehash')
	  CALL hash_initalloc(i_nhashmax, c_action='nodehash')
	  CALL hash_initalloc(i_thashmax, c_action='tetrhash')

!---------- initialize grid index arrays

	  CALL hash_initalloc(i_egridmax, c_action='elmtgrid')
	  CALL hash_initalloc(i_ggridmax, c_action='edgegrid')
	  CALL hash_initalloc(i_ngridmax, c_action='nodegrid')
	  CALL hash_initalloc(i_tgridmax, c_action='tetrgrid')

!---------- initialize finest grid and boundary index arrays

	  CALL hash_initalloc(i_efinemax, c_action='elmtfine')
	  CALL hash_initalloc(i_tfinemax, c_action='tetrfine')
	  CALL hash_initalloc(i_gfinemax, c_action='edgefine')
	  CALL hash_initalloc(i_gbounmax, c_action='edgeboun')
	  CALL hash_initalloc(i_ebounmax, c_action='elmtboun')
	END IF act_type

	RETURN
	END SUBROUTINE grid_initdatastruct

	END MODULE FEM_create







