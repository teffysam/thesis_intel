!*****************************************************************
!
! MODULE NAME:
!	FEM_handle
! FUNCTION:
!	define data structures for element handling and parameters
! CONTAINS:
!
! PUBLIC:
!	p_grid, i_futuretime, i_currenttime, i_pasttime, grid_handle
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
!	  *t_:  tetraeder associated variables
!
! USES:
!	FEM_param, FEM_define
! LIBRARIES:
!
! REFERENCES:
!
! VERSION(S):
!	1. original version		j. behrens	7/97
!	2. slimmer version for hashing	j. behrens	7/97
!
!*****************************************************************
	MODULE FEM_handle

	USE FEM_define

	PUBLIC

!---------- grid handles

	TYPE grid_handle
	  INTEGER (KIND = GRID_SI)                                     :: i_timetag    ! the time toggle
	  INTEGER (KIND = GRID_SI)				      :: i_ttotal     ! total no. of tetraeders
	  INTEGER (KIND = GRID_SI)				      :: i_tnumber    ! no. of tetraeders on this grid
	  INTEGER (KIND = GRID_SI)				      :: i_tnumfine   ! no. of tetraeders on this grid
	  INTEGER (KIND = GRID_SI)                                     :: i_etotal     ! total no. of elements
	  INTEGER (KIND = GRID_SI)                                     :: i_enumber    ! no. of elements on this grid
	  INTEGER (KIND = GRID_SI)                                     :: i_enumfine   ! no. of elements on this grid
	  INTEGER (KIND = GRID_SI)				      :: i_enumboun   ! no. of elements on boundary
	  INTEGER (KIND = GRID_SI)                                     :: i_gtotal     ! total no. of edges
	  INTEGER (KIND = GRID_SI)                                     :: i_gnumber    ! no. of edges on this grid
	  INTEGER (KIND = GRID_SI)                                     :: i_gnumfine   ! no. of edges on this grid
	  INTEGER (KIND = GRID_SI)                                     :: i_gnumboun   ! no. of edges on boundary
	  INTEGER (KIND = GRID_SI)                                     :: i_ntotal     ! total no. of nodes
	  INTEGER (KIND = GRID_SI)                                     :: i_nnumber    ! no. of nodes on this grid
	  INTEGER (KIND = GRID_SI)                                     :: i_minlvl     ! the minimal current level
	  INTEGER (KIND = GRID_SI)                                     :: i_maxlvl     ! the maximal current level
	  INTEGER (KIND = GRID_SI)                                     :: i_crslvlbnd  ! the minimal level
	  INTEGER (KIND = GRID_SI)                                     :: i_reflvlbnd  ! the maximal level
          INTEGER(KIND=GRID_SI), DIMENSION(DEF_maxfemtypes)            :: i_unknowns
	END TYPE grid_handle

	TYPE (grid_handle), DIMENSION(DEF_timesteps)  :: p_grid

!---------- some common variables

	INTEGER (KIND = GRID_SI)                                       :: i_futuretime  ! timestep toggle
	INTEGER (KIND = GRID_SI)                                       :: i_currenttime ! timestep toggle
	INTEGER (KIND = GRID_SI)                                       :: i_pasttime    ! timestep toggle

	END MODULE FEM_handle
