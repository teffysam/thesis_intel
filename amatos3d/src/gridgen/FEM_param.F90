!*****************************************************************
!
! MODULE NAME:
!	FEM_param
! FUNCTION:
!	define parameters for the grid data structures
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
!
! LIBRARIES:
!
! REFERENCES:
!
! VERSION(S):
!	1. original version			j. behrens	7/97
!	2. block size f. hash table alloc added	j. behrens	7/97
!
!*****************************************************************
#if !defined(SGL_DBL) && !defined(DBL_DBL) && !defined(DBL_QUAD)
#define SGL_DBL
#endif

	MODULE FEM_param

	PUBLIC

!---------- real kind parameters: calculations in single, 
!                                 some critical things in double precision
#ifdef SGL_DBL
	INTEGER, PARAMETER :: GRID_SR = selected_real_kind(6,20)
	INTEGER, PARAMETER :: GRID_DR = selected_real_kind(14,40)
#endif
!---------- real kind parameters: calculations in double, 
!                                 some critical things in double as well
#ifdef DBL_DBL
 	INTEGER, PARAMETER :: GRID_SR = selected_real_kind(14,40)
 	INTEGER, PARAMETER :: GRID_DR = GRID_SR
#endif
!---------- real kind parameters: calculations in double, 
!                                 some critical things in quad precision
#ifdef DBL_QUAD
 	INTEGER, PARAMETER :: GRID_SR = selected_real_kind(14,40)
 	INTEGER, PARAMETER :: GRID_DR = selected_real_kind(28,80)
#endif
!---------- integer kind parameters

	INTEGER, PARAMETER :: GRID_SI = selected_int_kind(8)
	INTEGER, PARAMETER :: GRID_DI = selected_int_kind(16)

!---------- size parameters

	INTEGER (KIND = GRID_SI), PARAMETER                            :: DEF_elorder= 3     ! order of element (3: linear)
	INTEGER (KIND = GRID_SI), PARAMETER                            :: DEF_elnodes= 3     ! no. of nodes
	INTEGER (KIND = GRID_SI), PARAMETER                            :: DEF_eledges= 3     ! no. of edges
	INTEGER (KIND = GRID_SI), PARAMETER                            :: DEF_elchild= 2     ! no. of children
	INTEGER (KIND = GRID_SI), PARAMETER                            :: DEF_elmtets= 2     ! no. of tetraeders
	INTEGER (KIND = GRID_SI), PARAMETER                            :: DEF_egnodes= 2     ! no. of nodes
	INTEGER (KIND = GRID_SI), PARAMETER                            :: DEF_egelems= 16    ! no. of elements
	INTEGER (KIND = GRID_SI), PARAMETER                            :: DEF_egchild= 2     ! no. of elements
	INTEGER (KIND = GRID_SI), PARAMETER                            :: DEF_dimension= 3   ! no. of dimensions
	INTEGER (KIND = GRID_SI), PARAMETER                            :: DEF_ndpatch= 48    ! elements in patch
!	INTEGER (KIND = GRID_SI), PARAMETER                            :: DEF_timesteps= 3   ! timesteps saved
	INTEGER (KIND = GRID_SI), PARAMETER                            :: DEF_timesteps= 2   ! timesteps saved
!- DEF_nvalsize is defined later on. From def follows DEF_constno = 3
!	INTEGER (KIND = GRID_SI), PARAMETER                            :: DEF_nvalsize= 6    ! size of real array (node)
	INTEGER (KIND = GRID_SI), PARAMETER			      :: DEF_tetnodes= 4    ! no. of nodes
	INTEGER (KIND = GRID_SI), PARAMETER			      :: DEF_tetedges= 6    ! no. of edges
	INTEGER (KIND = GRID_SI), PARAMETER			      :: DEF_tetelmts= 4    ! no. of elements
	INTEGER (KIND = GRID_SI), PARAMETER			      :: DEF_tetchild= 2    ! no. of children

#ifdef SAMATOS
        INTEGER (KIND = GRID_SI), PARAMETER          :: DEF_constno= 3    ! number of constituents
#elif AMATOS
        INTEGER (KIND = GRID_SI), PARAMETER          :: DEF_constno= 5    ! number of constituents
#elif AMATOS3D
        INTEGER (KIND = GRID_SI), PARAMETER          :: DEF_constno= 5    ! number of constituents
#endif

!---------- size of value arrays parameters. Modify these, when modifiying FEM_siginit...

        INTEGER (KIND = GRID_SI), PARAMETER          :: DEF_evalsize= 9              ! size of real array (elmt)
        INTEGER (KIND = GRID_SI), PARAMETER          :: DEF_gvalsize= DEF_constno    ! size of real array (edge)
        INTEGER (KIND = GRID_SI), PARAMETER          :: DEF_nvalsize= 2*DEF_constno  ! size of real array (node)
        INTEGER (KIND = GRID_SI), PARAMETER          :: DEF_maxfemtypes=32           ! max no. of FEM types supported


!---------- definition parameters

	INTEGER (KIND = GRID_SI), PARAMETER                            :: DEF_inner=      0  ! inner node/ edge
	INTEGER (KIND = GRID_SI), PARAMETER                            :: DEF_dirichlet= -1  ! Dirichlet boundary
	INTEGER (KIND = GRID_SI), PARAMETER                            :: DEF_neumann=   -2  ! Neumann boundary
	INTEGER (KIND = GRID_SI), PARAMETER                            :: DEF_unrefined=  0  ! Flag for unrefined
	INTEGER (KIND = GRID_SI), PARAMETER                            :: DEF_refined=    1  ! Flag for refined
	INTEGER (KIND = GRID_SI), PARAMETER                            :: DEF_pleasrefine=2  ! Flag for item to be refined
	INTEGER (KIND = GRID_SI), PARAMETER                            :: DEF_pleascoarse=3  ! Flag for item to be coarsened
	INTEGER (KIND = GRID_SI), PARAMETER                            :: DEF_erased=99      ! Flag for erased item

!---------- block size for hash table allocation

	INTEGER (KIND = GRID_SI), PARAMETER                            :: DEF_hashblock= 4096 ! Size of alloc blocks

!---------- pointer definitions for real arrays

	INTEGER (KIND = GRID_SI), PARAMETER                            :: DEF_ucomp= 1       ! Entry for u-component
	INTEGER (KIND = GRID_SI), PARAMETER                            :: DEF_vcomp= 2       ! Entry for v-component
	INTEGER (KIND = GRID_SI), PARAMETER                            :: DEF_wcomp= 3       ! Entry for w-component
	INTEGER (KIND = GRID_SI), PARAMETER                            :: DEF_phi= 4         ! Entry for geopot. height
	INTEGER (KIND = GRID_SI), PARAMETER                            :: DEF_zeta= 5        ! Entry for vorticity
	INTEGER (KIND = GRID_SI), PARAMETER                            :: DEF_tracer= 6      ! Entry for tracer

	INTEGER (KIND = GRID_SI), PARAMETER, DIMENSION(3,3)            :: DEF_elmatrix= &
	  RESHAPE((/ 1, 2, 3, 4, 5, 6, 7, 8, 9 /), (/ 3, 3 /))              ! Translate 2D to 1D array

	END MODULE FEM_param
