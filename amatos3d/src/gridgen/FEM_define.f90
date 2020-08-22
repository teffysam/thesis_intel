!*****************************************************************
!
! MODULE NAME:
!	FEM_define
! FUNCTION:
!	define data structures for elements
! CONTAINS:
!
! PUBLIC:
!
! COMMENTS:
!	use different affixes for different data types:
!	  n*_:	integer parameters
!	  i*_:	integer variables
!	  r*_:	real variables
!	  p*_:	pointer variables
!	  l*_:	logical variables
!
!	  *e_:	element associated variables
!	  *g_:	edge associated variables
!	  *n_:	node associated variables
!
! USES:
!	FEM_param
! LIBRARIES:
!
! REFERENCES:
!
! VERSION(S):
!	1. original version		j. behrens	8/96
!	2. now with edges		j. behrens	10/96
!	3. nodal values time depend.	j. behrens	1/97
!	4. use FEM_param added		j. behrens	7/97
!	5. new version with hash tables	j. behrens	7/97
!	6. bug in patch addressed	j. behrens	11/97
!	7. domain boundary added	j. behrens	12/97
!	8. tetraeder struct. added	t. landes	12/99
!
!*****************************************************************
	MODULE FEM_define

	USE FEM_param
	PUBLIC

!---------- sub types

!---------- this defines a tetraeder
	TYPE tetra_def
	  INTEGER (KIND = GRID_SI)                                       :: i_indx ! index
	  INTEGER (KIND = GRID_SI), DIMENSION(DEF_tetnodes)              :: p_node ! nodes
	  INTEGER (KIND = GRID_SI), DIMENSION(DEF_tetedges)              :: p_edge ! edges
	  INTEGER (KIND = GRID_SI), DIMENSION(DEF_tetelmts)              :: p_elem ! elements
	END TYPE tetra_def

!---------- these are the attributes of a tetraeder
	TYPE tetra_att
	  INTEGER (KIND = GRID_SI)                                       :: i_time ! timestamp
	  INTEGER (KIND = GRID_SI)                                       :: i_levl ! grid level
	  INTEGER (KIND = GRID_SI), DIMENSION(DEF_timesteps)             :: i_elmt ! element dividing the tetraeder
	  INTEGER (KIND = GRID_SI), DIMENSION(DEF_timesteps)             :: i_stat ! status
	  REAL (KIND=GRID_SR), DIMENSION(DEF_evalsize)                 :: r_vals ! real values
	END TYPE tetra_att

!---------- these define a tetraeder tree (links)
	TYPE tetra_lnk
	  INTEGER (KIND = GRID_SI), DIMENSION(DEF_timesteps)              :: p_prnt ! pointer to parent
	  INTEGER (KIND = GRID_SI), DIMENSION(DEF_tetchild,DEF_timesteps) :: p_chil ! pointer to children
	END TYPE tetra_lnk

!---------- type tetraeder
	TYPE tetra
	  TYPE(tetra_def)                                :: def ! defining a tetraeder
	  TYPE(tetra_att)                                :: att ! attributes of a tetraeder
	  TYPE(tetra_lnk)                                :: lnk ! the linked list
	END TYPE tetra

!---------- this defines an element
	TYPE elmt_def
	  INTEGER (KIND = GRID_SI)                                       :: i_indx ! index
	  INTEGER (KIND = GRID_SI), DIMENSION(DEF_elnodes)               :: p_node ! nodes
	  INTEGER (KIND = GRID_SI), DIMENSION(DEF_eledges)               :: p_edge ! edges
	END TYPE elmt_def

!---------- these are attributes of an element
	TYPE elmt_att
	  INTEGER (KIND = GRID_SI)                                       :: i_time ! timestamp
	  INTEGER (KIND = GRID_SI)                                       :: i_mark ! marked side of element
	  INTEGER (KIND = GRID_SI)                                       :: i_levl ! grid level
	  INTEGER (KIND = GRID_SI)                                       :: i_boun ! boundary condition
!	  INTEGER (KIND = GRID_SI)                                       :: i_tets ! no. of tetras sharing element
	  INTEGER (KIND = GRID_SI), DIMENSION(DEF_elmtets)               :: p_tets ! tetras sharing element
	  INTEGER (KIND = GRID_SI), DIMENSION(DEF_timesteps)             :: i_edge ! edge dividing the element
	  INTEGER (KIND = GRID_SI), DIMENSION(DEF_timesteps)             :: i_stat ! status (flag/property)
	  REAL (KIND=GRID_SR), DIMENSION(DEF_evalsize)                 :: r_vals ! real values (e.g. element matrix)
	END TYPE elmt_att

!---------- these define an element-tree (links)
	TYPE elmt_lnk
	  INTEGER (KIND = GRID_SI), DIMENSION(DEF_timesteps)             :: p_prnt ! pointer to parent
	  INTEGER (KIND = GRID_SI), DIMENSION(DEF_elchild,DEF_timesteps) :: p_chil ! pointer to children
	END TYPE elmt_lnk

!---------- this defines an edge
	TYPE edge_def
	  INTEGER (KIND = GRID_SI)                                       :: i_indx ! index
	  INTEGER (KIND = GRID_SI), DIMENSION(DEF_egnodes)               :: p_node ! nodes bounding edge
	END TYPE edge_def

!---------- these are attributes of an edge
	TYPE edge_att
	  INTEGER (KIND = GRID_SI)                                       :: i_time ! timestamp
	  INTEGER (KIND = GRID_SI)                                       :: i_elem ! no. of elements sharing edge
	  INTEGER (KIND = GRID_SI), DIMENSION(DEF_egelems)               :: p_elem ! elements sharing edge
	  INTEGER (KIND = GRID_SI), DIMENSION(DEF_timesteps)             :: i_node ! node dividing the edge
	  INTEGER (KIND = GRID_SI)                                       :: i_boun ! boundary condition
	  INTEGER (KIND = GRID_SI), DIMENSION(DEF_timesteps)             :: i_stat ! status (refined...)
	END TYPE edge_att

!---------- this defines a linked list (resp. a tree) for edges
	TYPE edge_lnk
	  INTEGER (KIND = GRID_SI), DIMENSION(DEF_egchild,DEF_timesteps) :: p_chil ! pointer to child
	  INTEGER (KIND = GRID_SI)                                       :: p_peri ! periodic boundary
	END TYPE edge_lnk

!---------- this defines a node
	TYPE node_def
	  INTEGER (KIND = GRID_SI)                                       :: i_indx ! index
	  REAL (KIND=GRID_SR), DIMENSION(DEF_dimension)                :: r_coor ! coordinates
	END TYPE node_def

!---------- these are attributes of a node
	TYPE node_att
	  INTEGER (KIND = GRID_SI)                                       :: i_time ! timestamp
	  INTEGER (KIND = GRID_SI)                                       :: p_edge ! edge devided by node
	  INTEGER (KIND = GRID_SI), DIMENSION(DEF_timesteps)             :: i_ptch ! no. of elements in patch
	  INTEGER (KIND = GRID_SI), DIMENSION(DEF_timesteps)             :: i_stat ! status (refined...)
	  INTEGER (KIND = GRID_SI), DIMENSION(DEF_ndpatch,DEF_timesteps) :: p_ptch ! element patch around node
	  REAL (KIND=GRID_SR), DIMENSION(DEF_nvalsize,DEF_timesteps)   :: r_vals ! real values (U,V,...)
	END TYPE node_att

!---------- this defines a linked list (resp. a tree)
	TYPE node_lnk
	  INTEGER (KIND = GRID_SI)                                       :: p_peri ! periodic boundary
	END TYPE node_lnk

!---------- type element

	TYPE elmt
	  TYPE (elmt_def)                             :: def    ! defining an element
	  TYPE (elmt_att)                             :: att    ! attributes of an element
	  TYPE (elmt_lnk)                             :: lnk    ! the linked list
	END TYPE elmt

!---------- type edge

	TYPE edge
	  TYPE (edge_def)                             :: def    ! defining an element
	  TYPE (edge_att)                             :: att    ! attributes of an element
	  TYPE (edge_lnk)                             :: lnk    ! the linked list
	END TYPE edge

!---------- type node

	TYPE node
	  TYPE (node_def)                             :: def    ! defining an element
	  TYPE (node_att)                             :: att    ! attributes of an element
	  TYPE (node_lnk)                             :: lnk    ! the linked list
	END TYPE node

!---------- auxiliary types

	TYPE tetra_ptr_arr
	  TYPE (tetra), POINTER                       :: tp
	END TYPE tetra_ptr_arr

	TYPE elmt_ptr_arr
	  TYPE (elmt), POINTER                        :: ep
	END TYPE elmt_ptr_arr

	TYPE edge_ptr_arr
	  TYPE (edge), POINTER                        :: gp
	END TYPE edge_ptr_arr

	TYPE node_ptr_arr
	  TYPE (node), POINTER                        :: np
	END TYPE node_ptr_arr

!---------- hashing table data structures (allocatable)

	TYPE (tetra_ptr_arr), DIMENSION(:), ALLOCATABLE :: p_thash   ! hashing table for tetraeders
	INTEGER (KIND = GRID_SI)                                        :: i_thashmax ! array bound

	TYPE (elmt_ptr_arr), DIMENSION(:), ALLOCATABLE :: p_ehash    ! hashing table for elements
	INTEGER (KIND = GRID_SI)                                        :: i_ehashmax ! array bound
	TYPE (edge_ptr_arr), DIMENSION(:), ALLOCATABLE :: p_ghash    ! hashing table for edges
	INTEGER (KIND = GRID_SI)                                        :: i_ghashmax ! array bound
	TYPE (node_ptr_arr), DIMENSION(:), ALLOCATABLE :: p_nhash    ! hashing table for nodes
	INTEGER (KIND = GRID_SI)                                        :: i_nhashmax ! array bound

	INTEGER (KIND = GRID_SI), DIMENSION(:,:), ALLOCATABLE          :: i_tgrid    ! index table for whole grids
	INTEGER (KIND = GRID_SI)                                       :: i_tgridmax ! array bound
	INTEGER (KIND = GRID_SI), DIMENSION(:,:), ALLOCATABLE          :: i_tfine    ! index table for fine grid
	INTEGER (KIND = GRID_SI)                                       :: i_tfinemax ! array bound

	INTEGER (KIND = GRID_SI), DIMENSION(:,:), ALLOCATABLE           :: i_egrid                ! index table for whole grids
	INTEGER (KIND = GRID_SI)                                        :: i_egridmax             ! array bounds (dynamically changed)
	INTEGER (KIND = GRID_SI), DIMENSION(:,:), ALLOCATABLE           :: i_efine                ! index table for fine grid
	INTEGER (KIND = GRID_SI), DIMENSION(:,:), ALLOCATABLE           :: i_eboun                ! index table for boundary elements
	INTEGER (KIND = GRID_SI)                                        :: i_efinemax, i_ebounmax ! array bound

	INTEGER (KIND = GRID_SI), DIMENSION(:,:), ALLOCATABLE           :: i_ggrid    ! index table for whole grids
	INTEGER (KIND = GRID_SI)                                        :: i_ggridmax ! array bounds (dynamically changed)
	INTEGER (KIND = GRID_SI), DIMENSION(:,:), ALLOCATABLE           :: i_gfine    ! index table for fine grid elements
	INTEGER (KIND = GRID_SI), DIMENSION(:,:), ALLOCATABLE           :: i_gboun    ! index table for boundary edges
	INTEGER (KIND = GRID_SI)                                        :: i_gfinemax, i_gbounmax

	INTEGER (KIND = GRID_SI), DIMENSION(:,:), ALLOCATABLE           :: i_ngrid    ! index table for whole grids
	INTEGER (KIND = GRID_SI)                                        :: i_ngridmax ! array bounds (dynamically changed)

!---------- polygonal line defining the domain boundary

	REAL (KIND=GRID_SR), DIMENSION(:,:), ALLOCATABLE              :: r_boundpoly ! polygon defining domain boundary
	INTEGER (KIND = GRID_SI)                                        :: i_boundvertices ! number of boundary vertices

!---------- Radius and other constants of the spherical domain

	REAL (KIND=GRID_SR), PARAMETER                                :: DEF_GRAV=9.80616   ! initialize gravity
	REAL (KIND=GRID_SR), PARAMETER                                :: DEF_SIDDAY=86164.1 ! initialize siderical day
	REAL (KIND=GRID_SR)                                           :: DEF_EPS
	REAL (KIND=GRID_SR)                                           :: DEF_PI
	REAL (KIND=GRID_SR)                                           :: DEF_RADIUS
	REAL (KIND=GRID_SR)                                           :: DEF_OMEGA

        INTEGER (KIND = GRID_SI), DIMENSION(:,:), ALLOCATABLE           :: i_dsfc_start, i_dsfc_end ! start/end for dof arrays DIM(femtype, timestep)
 
	END MODULE FEM_define
