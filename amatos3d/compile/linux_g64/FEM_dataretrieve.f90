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
!*****************************************************************
MODULE FEM_dataretrieve
        USE MISC_globalparam
        USE MISC_error
        USE FEM_define
        USE FEM_handle

        PUBLIC :: grid_getinfo, grid_putinfo, grid_getiteminfo

        CONTAINS

!*****************************************************************

	SUBROUTINE grid_getinfo(p_mesh, i_arrlen, l_finelevel, r_nodecoordinates, &
	  r_nodevalues, i_valpoint, i_elementnodes, i_tetranodes, i_tetraelmts, i_tetraedges, r_elementcoordinates, r_tetracoordinates, &
	  r_elementvalues, i_elementstatus, i_elementlevel, i_elementmark, &
	  i_edgenodes, l_relative, r_tetravalues, i_tetralevel, i_tetrastatus, i_adjacencies)

!---------- local declarations

	IMPLICIT NONE

	TYPE (grid_handle)                                 :: p_mesh
	INTEGER (KIND = GRID_SI), INTENT(in)                                :: i_arrlen
	LOGICAL, OPTIONAL                                  :: l_finelevel
	REAL (KIND = GRID_SR), DIMENSION(DEF_dimension,i_arrlen), OPTIONAL  :: r_nodecoordinates
	REAL (KIND = GRID_SR), DIMENSION(i_arrlen), OPTIONAL                :: r_nodevalues
	INTEGER (KIND = GRID_SI), OPTIONAL                                  :: i_valpoint
	INTEGER (KIND = GRID_SI), DIMENSION(DEF_elnodes,i_arrlen), OPTIONAL :: i_elementnodes
	INTEGER (KIND = GRID_SI), DIMENSION(DEF_tetnodes,i_arrlen), OPTIONAL :: i_tetranodes
	INTEGER (KIND = GRID_SI), DIMENSION(DEF_tetelmts,i_arrlen), OPTIONAL :: i_tetraelmts
	INTEGER (KIND = GRID_SI), DIMENSION(DEF_tetedges,i_arrlen), OPTIONAL :: i_tetraedges
	INTEGER (KIND = GRID_SI), DIMENSION(2, i_arrlen), OPTIONAL	    :: i_adjacencies
	REAL (KIND = GRID_SR), DIMENSION(DEF_dimension,DEF_elnodes,i_arrlen), OPTIONAL &
	                                                   :: r_elementcoordinates
	REAL (KIND = GRID_SR), DIMENSION(DEF_dimension,DEF_tetnodes,i_arrlen), OPTIONAL &
	                                                   :: r_tetracoordinates
	REAL (KIND = GRID_SR), DIMENSION(DEF_elnodes,i_arrlen), OPTIONAL    :: r_elementvalues
	REAL (KIND = GRID_SR), DIMENSION(DEF_tetnodes,i_arrlen), OPTIONAL   :: r_tetravalues
	INTEGER (KIND = GRID_SI), DIMENSION(i_arrlen), OPTIONAL             :: i_elementstatus
	INTEGER (KIND = GRID_SI), DIMENSION(i_arrlen), OPTIONAL             :: i_tetrastatus
	INTEGER (KIND = GRID_SI), DIMENSION(i_arrlen), OPTIONAL             :: i_elementlevel
	INTEGER (KIND = GRID_SI), DIMENSION(i_arrlen), OPTIONAL             :: i_elementmark
	INTEGER (KIND = GRID_SI), DIMENSION(DEF_egnodes,i_arrlen), OPTIONAL :: i_edgenodes
	LOGICAL, OPTIONAL                                  :: l_relative
	INTEGER (KIND = GRID_SI), DIMENSION(i_arrlen), OPTIONAL             :: i_tetralevel
	INTEGER (KIND = GRID_SI)                                            :: i_count, j_count, i_pnt, i_tim, &
	  i_nc, i_nc1, i_ec, i_ec1, i_gc, i_gc1, i_tc
	  
	INTEGER (KIND = GRID_SI), DIMENSION(:), ALLOCATABLE                 :: i_invlist
	INTEGER (KIND = GRID_SI)                                            :: i_alct
	
	LOGICAL                                            :: l_fine, l_relt
	TYPE (tetra), POINTER                              :: p_ttmp
	TYPE (elmt), POINTER                               :: p_etmp
	TYPE (edge), POINTER                               :: p_gtmp
	TYPE (node), POINTER                               :: p_ntmp

!---------- set time tag

	i_tim= p_mesh%i_timetag

!---------- set fine grid toggle (default is 'true')

	fine_set: IF(present(l_finelevel)) THEN
	  l_fine= l_finelevel
	ELSE fine_set
	  l_fine= .TRUE.
	END IF fine_set

!---------- set relative grid toggle (default is 'false')

	relt_set: IF(present(l_relative)) THEN
	  l_relt= l_relative
	ELSE relt_set
	  l_relt= .FALSE.
	END IF relt_set

!---------- action depends on dummy argument given; get coordinates

	action_type: IF(present(r_nodecoordinates)) THEN
	  DO i_count= 1, p_mesh%i_nnumber
	    p_ntmp => p_nhash(i_ngrid(i_count,i_tim))%np
	    r_nodecoordinates(:,i_count)= p_ntmp%def%r_coor(:)
	  END DO

!---------- get nodal values, default is tracer value, if not given in i_valpoint

	ELSE IF(present(r_nodevalues)) THEN action_type
	  IF(present(i_valpoint)) THEN
	    i_pnt= i_valpoint
	  ELSE
	    i_pnt= DEF_tracer
	  END IF
	  DO i_count= 1, p_mesh%i_nnumber
	    p_ntmp => p_nhash(i_ngrid(i_count,i_tim))%np
	    r_nodevalues(i_count)= p_ntmp%att%r_vals(i_pnt,i_tim)
	  END DO

!---------- get element's node indices (array of (3,m))

	ELSE IF(present(i_elementnodes)) THEN action_type
	  IF(l_relt) THEN
	    ALLOCATE(i_invlist(p_mesh%i_ntotal), stat=i_alct)
	    IF(i_alct /= 0) THEN
	      CALL print_error(a_err='[grid_getinfo]: could not allocate auxiliary array')
	    END IF
	    i_invlist(:) = 0
	    DO i_nc=1,p_mesh%i_nnumber
	      i_invlist(i_ngrid(i_nc,i_tim)) = i_nc
	    END DO
	  ENDIF
	  IF(l_fine) THEN
	    DO i_count= 1, p_mesh%i_enumfine
	      p_etmp => p_ehash(i_efine(i_count,i_tim))%ep
	      IF(l_relt) THEN
	        DO i_nc=1,DEF_elnodes
		  i_elementnodes(i_nc,i_count)= i_invlist(p_etmp%def%p_node(i_nc))
	        END DO
	      ELSE
		i_elementnodes(:,i_count)= p_etmp%def%p_node(:)
	      END IF
	    END DO
	  ELSE
	    DO i_count= 1, p_mesh%i_enumber
	      p_etmp => p_ehash(i_egrid(i_count,i_tim))%ep
	      IF(l_relt) THEN
	        DO i_nc=1,DEF_elnodes
		  i_elementnodes(i_nc,i_count)= i_invlist(p_etmp%def%p_node(i_nc))
		END DO
	      ELSE
		i_elementnodes(:,i_count)= p_etmp%def%p_node(:)
	      END IF
	    END DO
	  END IF
	  IF(l_relt) THEN
	    DEALLOCATE(i_invlist)
	  END IF

!---------- get edge's node indices (array of (2,m))

	ELSE IF(present(i_edgenodes)) THEN action_type
	  IF(l_relt) THEN
	    ALLOCATE(i_invlist(p_mesh%i_ntotal), stat=i_alct)
	    IF(i_alct /= 0) THEN
	      CALL print_error(a_err='[grid_getinfo]: could not allocate auxiliary array')
	    END IF
	    i_invlist(:) = 0
	    DO i_nc=1,p_mesh%i_nnumber
	      i_invlist(i_ngrid(i_nc,i_tim)) = i_nc
	    END DO
	  ENDIF
	  IF(l_fine) THEN
	    DO i_count= 1, p_mesh%i_gnumfine
	      p_gtmp => p_ghash(i_gfine(i_count,i_tim))%gp	      
	      IF(l_relt) THEN
	        DO i_nc=1,DEF_egnodes
		  i_edgenodes(i_nc,i_count)= i_invlist(p_gtmp%def%p_node(i_nc))     
	        END DO
	      ELSE
	        i_edgenodes(:,i_count)= p_gtmp%def%p_node(:)
	      END IF
	    END DO
	  ELSE
	    DO i_count= 1, p_mesh%i_gnumber
	      p_gtmp => p_ghash(i_ggrid(i_count,i_tim))%gp      
	      IF(l_relt) THEN
	        DO i_nc=1,DEF_egnodes
		  i_edgenodes(i_nc,i_count)= i_invlist(p_gtmp%def%p_node(i_nc))
	        END DO
	      ELSE
	        i_edgenodes(:,i_count)= p_gtmp%def%p_node(:)
	      END IF
	    END DO
	  END IF
	  IF(l_relt) THEN
	    DEALLOCATE(i_invlist)
	  END IF

!---------- get tetrahedron's node indices (array of (4,m))

	ELSE IF(present(i_tetranodes)) THEN action_type
	  IF(l_relt) THEN
	    ALLOCATE(i_invlist(p_mesh%i_ntotal), stat=i_alct)
	    IF(i_alct /= 0) THEN
	      CALL print_error(a_err='[grid_getinfo]: could not allocate auxiliary array')
	    END IF
	    i_invlist(:) = 0
	    DO i_nc=1,p_mesh%i_nnumber
	      i_invlist(i_ngrid(i_nc,i_tim)) = i_nc
	    END DO
	  ENDIF
	  IF(l_fine) THEN
	    DO i_count= 1, p_mesh%i_tnumfine
	      p_ttmp => p_thash(i_tfine(i_count,i_tim))%tp
	      IF(l_relt) THEN
	        DO i_nc=1,DEF_tetnodes
		  i_tetranodes(i_nc,i_count)= i_invlist(p_ttmp%def%p_node(i_nc))
	        END DO
	      ELSE
		i_tetranodes(:,i_count)= p_ttmp%def%p_node(:)
	      END IF
	    END DO
	  ELSE
	    DO i_count= 1, p_mesh%i_tnumber
	      p_ttmp => p_thash(i_tgrid(i_count,i_tim))%tp
	      IF(l_relt) THEN
	        DO i_nc=1,DEF_tetnodes
		  i_tetranodes(i_nc,i_count)= i_invlist(p_ttmp%def%p_node(i_nc))
	        END DO
	      ELSE
		i_tetranodes(:,i_count)= p_ttmp%def%p_node(:)
	      END IF
	    END DO
	  END IF
	  IF(l_relt) THEN
	    DEALLOCATE(i_invlist)
	  END IF
	  
!---------- get tetrahedron's element indices (array of (4,m))

	ELSE IF(present(i_tetraelmts)) THEN action_type
	  IF(l_relt) THEN
	    ALLOCATE(i_invlist(p_mesh%i_etotal), stat=i_alct)
	    IF(i_alct /= 0) THEN
	      CALL print_error(a_err='[grid_getinfo]: could not allocate auxiliary array')
	    END IF
	    i_invlist(:) = 0
	    DO i_ec=1,p_mesh%i_enumber
	      i_invlist(i_egrid(i_ec,i_tim)) = i_ec
	    END DO
	  ENDIF
	  IF(l_fine) THEN
	    DO i_count= 1, p_mesh%i_tnumfine
	      p_ttmp => p_thash(i_tfine(i_count,i_tim))%tp
	      IF(l_relt) THEN
	        DO i_ec=1, DEF_tetelmts
		  i_tetraelmts(i_ec,i_count)= i_invlist(p_ttmp%def%p_elem(i_ec))
		END DO
	      ELSE
	        i_tetraelmts(:,i_count)= p_ttmp%def%p_elem(:)
	      END IF
	    END DO
	  ELSE
	    DO i_count= 1, p_mesh%i_tnumber
	      p_ttmp => p_thash(i_tgrid(i_count,i_tim))%tp
	      IF(l_relt) THEN
	        DO i_ec=1,DEF_tetelmts
		  i_tetraelmts(i_ec,i_count)= i_invlist(p_ttmp%def%p_elem(i_ec))
	  	END DO
	      ELSE
	        i_tetraelmts(:,i_count)= p_ttmp%def%p_elem(:)
	      END IF
	    END DO
	  END IF
	  IF(l_relt) THEN
	    DEALLOCATE(i_invlist)
	  END IF

!---------- get tetrahedron's edge indices (array of (6,m))

	ELSE IF(present(i_tetraedges)) THEN action_type
	  IF(l_relt) THEN
	    ALLOCATE(i_invlist(p_mesh%i_gtotal), stat=i_alct)
	    IF(i_alct /= 0) THEN
	      CALL print_error(a_err='[grid_getinfo]: could not allocate auxiliary array')
	    END IF
	    i_invlist(:) = 0
	    DO i_gc=1,p_mesh%i_gnumber
	      i_invlist(i_ggrid(i_gc,i_tim)) = i_gc
	    END DO
	  END IF
	  IF(l_fine) THEN
	    DO i_count= 1, p_mesh%i_tnumfine
	      p_ttmp => p_thash(i_tfine(i_count,i_tim))%tp
	      IF(l_relt) THEN
	        DO i_gc=1,DEF_tetedges
		  i_tetraedges(i_gc,i_count) = i_invlist(p_ttmp%def%p_edge(i_gc))
	        END DO
	      ELSE
		i_tetraedges(:,i_count) = p_ttmp%def%p_edge(:)
	      END IF
	    END DO
	  ELSE
	    DO i_count= 1, p_mesh%i_tnumber
	      p_ttmp => p_thash(i_tgrid(i_count,i_tim))%tp	      
	      IF(l_relt) THEN
	        DO i_gc=1,DEF_tetedges
		  i_tetraedges(i_gc,i_count)= i_invlist(p_ttmp%def%p_edge(i_gc))
	        END DO
	      ELSE
	        i_tetraedges(:,i_count)= p_ttmp%def%p_edge(:)
	      END IF
	    END DO
	  END IF
	  IF(l_relt) THEN
	    DEALLOCATE(i_invlist)
	  END IF

!---------- get element's node coordinates (array of (2,3,m))

	ELSE IF(present(r_elementcoordinates)) THEN action_type
	  IF(l_fine) THEN
	    DO i_count= 1, p_mesh%i_enumfine
	      p_etmp => p_ehash(i_efine(i_count,i_tim))%ep
	      DO i_nc=1,DEF_elnodes
	        p_ntmp=> p_nhash(p_etmp%def%p_node(i_nc))%np
	        r_elementcoordinates(:,i_nc,i_count)= p_ntmp%def%r_coor(:)
	      END DO
	    END DO
	  ELSE
	    DO i_count= 1, p_mesh%i_enumber
	      p_etmp => p_ehash(i_egrid(i_count,i_tim))%ep
	      DO i_nc=1,DEF_elnodes
	        p_ntmp=> p_nhash(p_etmp%def%p_node(i_nc))%np
	        r_elementcoordinates(:,i_nc,i_count)= p_ntmp%def%r_coor(:)
	      END DO
	    END DO
	  END IF

!---------- get element's node coordinates (array of (2,3,m))

        ELSE IF(present(r_tetracoordinates)) THEN action_type
          IF(l_fine) THEN
            DO i_count= 1, p_mesh%i_tnumfine
              p_ttmp => p_thash(i_tfine(i_count,i_tim))%tp
              DO i_nc=1,DEF_tetnodes
                p_ntmp=> p_nhash(p_ttmp%def%p_node(i_nc))%np
                r_tetracoordinates(:,i_nc,i_count)= p_ntmp%def%r_coor(:)
              END DO
            END DO
          ELSE
            DO i_count= 1, p_mesh%i_tnumber
              p_ttmp => p_thash(i_tgrid(i_count,i_tim))%tp
              DO i_nc=1,DEF_tetnodes
                p_ntmp=> p_nhash(p_etmp%def%p_node(i_nc))%np
                r_tetracoordinates(:,i_nc,i_count)= p_ntmp%def%r_coor(:)
              END DO
            END DO
          END IF

!---------- get element's node values (array of (3,m)), default: tracer value

	ELSE IF(present(r_elementvalues)) THEN action_type
	  IF(present(i_valpoint)) THEN
	    i_pnt= i_valpoint
	  ELSE
	    i_pnt= DEF_tracer
	  END IF
	  IF(l_fine) THEN
	    DO i_count= 1, p_mesh%i_enumfine
	      p_etmp => p_ehash(i_efine(i_count,i_tim))%ep
	      DO i_nc=1,DEF_elnodes
	        p_ntmp=> p_nhash(p_etmp%def%p_node(i_nc))%np
	        r_elementvalues(i_nc,i_count)= p_ntmp%att%r_vals(i_pnt,i_tim)
	      END DO
	    END DO
	  ELSE
	    DO i_count= 1, p_mesh%i_enumber
	      p_etmp => p_ehash(i_egrid(i_count,i_tim))%ep
	      DO i_nc=1,DEF_elnodes
	        p_ntmp=> p_nhash(p_etmp%def%p_node(i_nc))%np
	        r_elementvalues(i_nc,i_count)= p_ntmp%att%r_vals(i_pnt,i_tim)
	      END DO
	    END DO
	  END IF

!---------- get element's node values (array of (3,m)), default: tracer value

	ELSE IF(present(r_tetravalues)) THEN action_type
	  IF(present(i_valpoint)) THEN
	    i_pnt= i_valpoint
	  ELSE
	    i_pnt= DEF_tracer
	  END IF
	  IF(l_fine) THEN
	    DO i_count= 1, p_mesh%i_tnumfine
	      p_ttmp => p_thash(i_tfine(i_count,i_tim))%tp
	      DO i_nc=1,DEF_tetnodes
	        p_ntmp=> p_nhash(p_ttmp%def%p_node(i_nc))%np
	        r_tetravalues(i_nc,i_count)= p_ntmp%att%r_vals(i_pnt,i_tim)
	      END DO
	    END DO
	  ELSE
	    DO i_count= 1, p_mesh%i_tnumber
	      p_ttmp => p_thash(i_tgrid(i_count,i_tim))%tp
	      DO i_nc=1,DEF_tetnodes
	        p_ntmp=> p_nhash(p_ttmp%def%p_node(i_nc))%np
	        r_tetravalues(i_nc,i_count)= p_ntmp%att%r_vals(i_pnt,i_tim)
	      END DO
	    END DO
	  END IF
	  

!---------- added by L. Mentrup, 09/02, begin

!---------- get element's neighbouring tetra indices (array of (2,m))

	ELSE IF(present(i_adjacencies)) THEN action_type
	  IF(l_fine) THEN
	    ALLOCATE(i_invlist(p_mesh%i_ttotal), stat=i_alct)
	    IF(i_alct /= 0) THEN
	      CALL print_error(a_err='[grid_getinfo]: could not allocate auxiliary array')
	    END IF
	    i_invlist(:) = 0
	    DO i_tc=1,p_mesh%i_tnumfine
	      i_invlist(i_tfine(i_tc,i_tim)) = i_tc
	    END DO
	  ENDIF
	  i_adjacencies(:,:) = 0
	  IF(l_fine) THEN
	    DO i_count= 1, p_mesh%i_enumber
	      p_etmp => p_ehash(i_egrid(i_count,i_tim))%ep
	      IF(p_etmp%att%p_tets(1) /= 0)  i_adjacencies(1,i_count) = i_invlist(p_etmp%att%p_tets(1))
	      IF(p_etmp%att%p_tets(2) /= 0)  i_adjacencies(2,i_count) = i_invlist(p_etmp%att%p_tets(2))
	    END DO  
	  ELSE
	    DO i_count= 1, p_mesh%i_enumber
	      p_etmp => p_ehash(i_egrid(i_count,i_tim))%ep
	      i_adjacencies(:,i_count) = p_etmp%att%p_tets(:)
	    END DO  
	  END IF
	  IF(l_fine) THEN
	    DEALLOCATE(i_invlist)
	  END IF
	    
!---------- added by L. Mentrup, 09/02, end

!---------- get element's status (array of (m))

	ELSE IF(present(i_elementstatus)) THEN action_type
	  IF(l_fine) THEN
	    DO i_count= 1, p_mesh%i_enumfine
	      p_etmp => p_ehash(i_efine(i_count,i_tim))%ep
	      i_elementstatus(i_count)= p_etmp%att%i_stat(i_tim)
	    END DO
	  ELSE
	    DO i_count= 1, p_mesh%i_enumber
	      p_etmp => p_ehash(i_egrid(i_count,i_tim))%ep
	      i_elementstatus(i_count)= p_etmp%att%i_stat(i_tim)
	    END DO
	  END IF

!---------- get tetrahedron's status (array of (m))

	ELSE IF(present(i_tetrastatus)) THEN action_type
	  IF(l_fine) THEN
	    DO i_count= 1, p_mesh%i_tnumfine
	      p_ttmp => p_thash(i_tfine(i_count,i_tim))%tp
	      i_tetrastatus(i_count)= p_ttmp%att%i_stat(i_tim)
	    END DO
	  ELSE
	    DO i_count= 1, p_mesh%i_tnumber
	      p_ttmp => p_thash(i_tgrid(i_count,i_tim))%tp
	      i_tetrastatus(i_count)= p_ttmp%att%i_stat(i_tim)
	    END DO
	  END IF
	
!---------- get element's level (array of (m))

	ELSE IF(present(i_elementlevel)) THEN action_type
	  IF(l_fine) THEN
	    DO i_count= 1, p_mesh%i_enumfine
	      p_etmp => p_ehash(i_efine(i_count,i_tim))%ep
	      i_elementlevel(i_count)= p_etmp%att%i_levl
	    END DO
	  ELSE
	    DO i_count= 1, p_mesh%i_enumber
	      p_etmp => p_ehash(i_egrid(i_count,i_tim))%ep
	      i_elementlevel(i_count)= p_etmp%att%i_levl
	    END DO
	  END IF
	
!---------- get tetrahedron's level (array of (m))

	ELSE IF(present(i_tetralevel)) THEN action_type
	  IF(l_fine) THEN
	    DO i_count= 1, p_mesh%i_tnumfine
	      p_ttmp => p_thash(i_tfine(i_count,i_tim))%tp
	      i_tetralevel(i_count)= p_ttmp%att%i_levl
	    END DO
	  ELSE
	    DO i_count= 1, p_mesh%i_tnumber
	      p_ttmp => p_thash(i_tgrid(i_count,i_tim))%tp
	      i_tetralevel(i_count)= p_ttmp%att%i_levl
	    END DO
	  END IF

!---------- get element's marked edge (array of (m))

	ELSE IF(present(i_elementmark)) THEN action_type
	  IF(l_fine) THEN
	    DO i_count= 1, p_mesh%i_enumfine
	      p_etmp => p_ehash(i_efine(i_count,i_tim))%ep
	      i_elementmark(i_count)= p_etmp%att%i_mark
	    END DO
	  ELSE
	    DO i_count= 1, p_mesh%i_enumber
	      p_etmp => p_ehash(i_egrid(i_count,i_tim))%ep
	      i_elementmark(i_count)= p_etmp%att%i_mark
	    END DO
	  END IF

!---------- default... 

	ELSE action_type
	  IF(GRID_parameters%iolog > 0) THEN
	    WRITE(GRID_parameters%iolog,*) 'GRID_api [grid_getinfo]: no action requested'
	  END IF
	END IF action_type

	RETURN
	END SUBROUTINE grid_getinfo
!*****************************************************************
	SUBROUTINE grid_putinfo(p_mesh, i_arrlen, l_finelevel, &
	  r_nodevalues, i_valpoint, r_elementvalues, i_elementstatus, &
	  r_tetravalues, i_tetrastatus)

!---------- local declarations

	IMPLICIT NONE

	TYPE (grid_handle)                                 :: p_mesh
	INTEGER (KIND = GRID_SI), INTENT(in)                                :: i_arrlen
	LOGICAL, OPTIONAL                                  :: l_finelevel
	REAL (KIND = GRID_SR), DIMENSION(i_arrlen), OPTIONAL                :: r_nodevalues
	INTEGER (KIND = GRID_SI), OPTIONAL                                  :: i_valpoint
	REAL (KIND = GRID_SR), DIMENSION(DEF_elnodes,i_arrlen), OPTIONAL    :: r_elementvalues
	INTEGER (KIND = GRID_SI), DIMENSION(i_arrlen), OPTIONAL             :: i_elementstatus
	INTEGER (KIND = GRID_SI), DIMENSION(i_arrlen), OPTIONAL             :: i_tetrastatus
	REAL (KIND = GRID_SR), DIMENSION(DEF_tetnodes,i_arrlen), OPTIONAL   :: r_tetravalues
	INTEGER (KIND = GRID_SI)                                            :: i_count, i_pnt, i_tim, i_nc
	LOGICAL                                            :: l_fine
	TYPE (elmt), POINTER                               :: p_etmp
! not yet used	TYPE (edge), POINTER                               :: p_gtmp
	TYPE (node), POINTER                               :: p_ntmp
	TYPE (tetra), POINTER                              :: p_ttmp

!---------- set time tag

	i_tim= p_mesh%i_timetag

!---------- set fine grid toggle (default is 'true')

	fine_set: IF(present(l_finelevel)) THEN
	  l_fine= l_finelevel
	ELSE fine_set
	  l_fine= .TRUE.
	END IF fine_set

!---------- action depends on dummy argument given; set nodal values, default is tracer value

	action_type:  IF(present(r_nodevalues)) THEN
	  IF(present(i_valpoint)) THEN
	    i_pnt= i_valpoint
	  ELSE
	    i_pnt= DEF_tracer
	  END IF
	  DO i_count= 1, p_mesh%i_nnumber
	    p_ntmp => p_nhash(i_ngrid(i_count,i_tim))%np
	    p_ntmp%att%r_vals(i_pnt,i_tim)= r_nodevalues(i_count)
	  END DO

!---------- set element's node values (array of (3,m)), default: tracer value

	ELSE IF(present(r_elementvalues)) THEN action_type
	  IF(present(i_valpoint)) THEN
	    i_pnt= i_valpoint
	  ELSE
	    i_pnt= DEF_tracer
	  END IF
	  IF(l_fine) THEN
	    DO i_count= 1, p_mesh%i_enumfine
	      p_etmp => p_ehash(i_efine(i_count,i_tim))%ep
	      DO i_nc=1,DEF_elnodes
	        p_ntmp=> p_nhash(p_etmp%def%p_node(i_nc))%np
	        p_ntmp%att%r_vals(i_pnt,i_tim)= r_elementvalues(i_nc,i_count)
	      END DO
	    END DO
	  ELSE
	    DO i_count= 1, p_mesh%i_enumber
	      p_etmp => p_ehash(i_egrid(i_count,i_tim))%ep
	      DO i_nc=1,DEF_elnodes
	        p_ntmp=> p_nhash(p_etmp%def%p_node(i_nc))%np
	        p_ntmp%att%r_vals(i_pnt,i_tim)= r_elementvalues(i_nc,i_count)
	      END DO
	    END DO
	  END IF
	
!---------- set tetrahedron's node values (array of (4,m)), default: tracer value

	ELSE IF(present(r_tetravalues)) THEN action_type
	  IF(present(i_valpoint)) THEN
	    i_pnt= i_valpoint
	  ELSE
	    i_pnt= DEF_tracer
	  END IF
	  IF(l_fine) THEN
	    DO i_count= 1, p_mesh%i_tnumfine
	      p_ttmp => p_thash(i_tfine(i_count,i_tim))%tp
	      DO i_nc=1,DEF_tetnodes
	        p_ntmp=> p_nhash(p_ttmp%def%p_node(i_nc))%np
	        p_ntmp%att%r_vals(i_pnt,i_tim)= r_tetravalues(i_nc,i_count)
	      END DO
	    END DO
	  ELSE
	    DO i_count= 1, p_mesh%i_tnumber
	      p_ttmp => p_thash(i_tgrid(i_count,i_tim))%tp
	      DO i_nc=1,DEF_tetnodes
	        p_ntmp=> p_nhash(p_ttmp%def%p_node(i_nc))%np
	        p_ntmp%att%r_vals(i_pnt,i_tim)= r_tetravalues(i_nc,i_count)
	      END DO
	    END DO
	  END IF

!---------- set element's status (array of (m))

	ELSE IF(present(i_elementstatus)) THEN action_type
	  IF(l_fine) THEN
	    DO i_count= 1, p_mesh%i_enumfine
	      p_etmp => p_ehash(i_efine(i_count,i_tim))%ep
	      p_etmp%att%i_stat(i_tim)= i_elementstatus(i_count)
	    END DO
	  ELSE
	    DO i_count= 1, p_mesh%i_enumber
	      p_etmp => p_ehash(i_egrid(i_count,i_tim))%ep
	      p_etmp%att%i_stat(i_tim)= i_elementstatus(i_count)
	    END DO
	  END IF

!---------- set element's status (array of (m))

        ELSE IF(present(i_tetrastatus)) THEN action_type
          IF(l_fine) THEN
            DO i_count= 1, p_mesh%i_tnumfine
              p_ttmp => p_thash(i_tfine(i_count,i_tim))%tp
              p_ttmp%att%i_stat(i_tim)= i_tetrastatus(i_count)
            END DO
          ELSE
            DO i_count= 1, p_mesh%i_tnumber
              p_ttmp => p_thash(i_tgrid(i_count,i_tim))%tp
              p_ttmp%att%i_stat(i_tim)= i_tetrastatus(i_count)
            END DO
          END IF

!---------- default... 

	ELSE action_type
	  IF(GRID_parameters%iolog > 0) THEN
	    WRITE(GRID_parameters%iolog,*) 'GRID_api [grid_putinfo]: no action requested'
	  END IF
	END IF action_type

	RETURN
	END SUBROUTINE grid_putinfo
!*****************************************************************
	SUBROUTINE grid_getiteminfo(i_itemindex, c_itemtype, i_arrlen, r_values, &
	  r_coordinates, i_nodes, i_edges, i_elements, i_status, i_statustime, &
	  i_level, i_patch, i_adjacency)

!---------- local declarations

	IMPLICIT NONE

	INTEGER (KIND = GRID_SI), INTENT(in)                      :: i_itemindex
	CHARACTER (LEN=4), INTENT(in)            :: c_itemtype
	INTEGER (KIND = GRID_SI), OPTIONAL                        :: i_arrlen
	REAL (KIND = GRID_SR), DIMENSION(:), OPTIONAL             :: r_values
	REAL (KIND = GRID_SR), DIMENSION(DEF_dimension), OPTIONAL :: r_coordinates
	INTEGER (KIND = GRID_SI), DIMENSION(:), OPTIONAL          :: i_nodes
	INTEGER (KIND = GRID_SI), DIMENSION(:), OPTIONAL          :: i_edges
	INTEGER (KIND = GRID_SI), DIMENSION(:), OPTIONAL          :: i_elements
	INTEGER (KIND = GRID_SI), OPTIONAL                        :: i_status
	INTEGER (KIND = GRID_SI), OPTIONAL                        :: i_statustime
	INTEGER (KIND = GRID_SI), OPTIONAL                        :: i_level
	INTEGER (KIND = GRID_SI), DIMENSION(:), OPTIONAL          :: i_patch
	INTEGER (KIND = GRID_SI), DIMENSION(:), OPTIONAL          :: i_adjacency
	INTEGER (KIND = GRID_SI)                                  :: i_len, i_stime

!---------- act according to the itemtype

	type_select: SELECT CASE (c_itemtype)
	CASE('tetr') type_select
	  tetr_info: IF(present(i_nodes)) THEN
	    IF(.NOT. present(i_arrlen)) THEN
	      IF(GRID_parameters%iolog > 0) THEN
	        WRITE(GRID_parameters%iolog,*) 'GRID_api [grid_getiteminfo]: assume default array length'
	      END IF
	      i_len= DEF_tetnodes
	    ELSE
	      i_len= i_arrlen
	    END IF
	    i_nodes(1:i_len)= p_thash(i_itemindex)%tp%def%p_node(1:i_len)
	  ELSE IF(present(i_edges)) THEN tetr_info
	    IF(.NOT. present(i_arrlen)) THEN
	      IF(GRID_parameters%iolog > 0) THEN
	        WRITE(GRID_parameters%iolog,*) 'GRID_api [grid_getiteminfo]: assume default array length'
	      END IF
	      i_len= DEF_tetedges
	    ELSE
	      i_len= i_arrlen
	    END IF
	    i_edges(1:i_len)= p_thash(i_itemindex)%tp%def%p_edge(1:i_len)
	  ELSE IF(present(i_elements)) THEN tetr_info
	    IF(.NOT. present(i_arrlen)) THEN
	      IF(GRID_parameters%iolog > 0) THEN
	        WRITE(GRID_parameters%iolog,*) 'GRID_api [grid_getiteminfo]: assume default array length'
	      END IF
	      i_len= DEF_tetelmts
	    ELSE
	      i_len= i_arrlen
	    END IF
	    i_elements(1:i_len)= p_thash(i_itemindex)%tp%def%p_elem(1:i_len)
	  ELSE IF(present(i_status)) THEN tetr_info
	    IF(.NOT. present(i_statustime)) THEN
	      IF(GRID_parameters%iolog > 0) THEN
	        WRITE(GRID_parameters%iolog,*) 'GRID_api [grid_getiteminfo]: assume default time (future)'
	      END IF
	      i_stime= i_futuretime
	    ELSE
	      i_stime= i_statustime
	    END IF
	    i_status= p_thash(i_itemindex)%tp%att%i_stat(i_stime)
	  ELSE IF(present(i_level)) THEN tetr_info
	    i_level= p_thash(i_itemindex)%tp%att%i_levl
	  ELSE IF(present(r_values)) THEN tetr_info
	    IF(.NOT. present(i_arrlen)) THEN
	      IF(GRID_parameters%iolog > 0) THEN
	        WRITE(GRID_parameters%iolog,*) 'GRID_api [grid_getiteminfo]: assume default array length'
	      END IF
	      i_len= DEF_evalsize
	    ELSE
	      i_len= i_arrlen
	    END IF
	    r_values(1:i_len)= p_thash(i_itemindex)%tp%att%r_vals(1:i_len)
	  ELSE tetr_info
	    IF(GRID_parameters%iolog > 0) THEN
	      WRITE(GRID_parameters%iolog,*) 'GRID_api [grid_getiteminfo]: this info is not available from type[tetra] item'
	    END IF
	  END IF tetr_info
	CASE('elmt') type_select
	  elmt_info: IF(present(r_values)) THEN
	    IF(.NOT. present(i_arrlen)) THEN
	      IF(GRID_parameters%iolog > 0) THEN
	        WRITE(GRID_parameters%iolog,*) 'GRID_api [grid_getiteminfo]: assume default array length'
	      END IF
	      i_len= DEF_evalsize
	    ELSE
	      i_len= i_arrlen
	    END IF
	    r_values(1:i_len)= p_ehash(i_itemindex)%ep%att%r_vals(1:i_len)
	  ELSE IF(present(i_nodes)) THEN elmt_info
	    IF(.NOT. present(i_arrlen)) THEN
	      IF(GRID_parameters%iolog > 0) THEN
	        WRITE(GRID_parameters%iolog,*) 'GRID_api [grid_getiteminfo]: assume default array length'
	      END IF
	      i_len= DEF_elnodes
	    ELSE
	      i_len= i_arrlen
	    END IF
	    i_nodes(1:i_len)= p_ehash(i_itemindex)%ep%def%p_node(1:i_len)
	  ELSE IF(present(i_edges)) THEN elmt_info
	    IF(.NOT. present(i_arrlen)) THEN
	      IF(GRID_parameters%iolog > 0) THEN
	        WRITE(GRID_parameters%iolog,*) 'GRID_api [grid_getiteminfo]: assume default array length'
	      END IF
	      i_len= DEF_eledges
	    ELSE
	      i_len= i_arrlen
	    END IF
	    i_edges(1:i_len)= p_ehash(i_itemindex)%ep%def%p_edge(1:i_len)
	           
	  ELSE IF(present(i_status)) THEN elmt_info
	    IF(.NOT. present(i_statustime)) THEN
	      IF(GRID_parameters%iolog > 0) THEN
	        WRITE(GRID_parameters%iolog,*) 'GRID_api [grid_getiteminfo]: assume default time (future)'
	      END IF
	      i_stime= i_futuretime
	    ELSE
	      i_stime= i_statustime
	    END IF
	    i_status= p_ehash(i_itemindex)%ep%att%i_stat(i_stime)
	  ELSE IF(present(i_level)) THEN elmt_info
	    i_level= p_ehash(i_itemindex)%ep%att%i_levl
	  ELSE elmt_info
	    IF(GRID_parameters%iolog > 0) THEN
	      WRITE(GRID_parameters%iolog,*) 'GRID_api [grid_getiteminfo]: this info is not available from type[element] item'
	    END IF
	  END IF elmt_info
	CASE('edge') type_select
	  edge_info: IF(present(i_nodes)) THEN
	    IF(.NOT. present(i_arrlen)) THEN
	      IF(GRID_parameters%iolog > 0) THEN
	        WRITE(GRID_parameters%iolog,*) 'GRID_api [grid_getiteminfo]: assume default array length'
	      END IF
	      i_len= DEF_egnodes
	    ELSE
	      i_len= i_arrlen
	    END IF
	    i_nodes(1:i_len)= p_ghash(i_itemindex)%gp%def%p_node(1:i_len)
	  ELSE IF(present(i_elements)) THEN edge_info
	    IF(.NOT. present(i_arrlen)) THEN
	      IF(GRID_parameters%iolog > 0) THEN
	        WRITE(GRID_parameters%iolog,*) 'GRID_api [grid_getiteminfo]: assume default array length'
	      END IF
	      i_len= DEF_egelems
	    ELSE
	      i_len= i_arrlen
	    END IF
	    i_elements(1:i_len)= p_ghash(i_itemindex)%gp%att%p_elem(1:i_len)
	  ELSE IF(present(i_status)) THEN edge_info
	    IF(.NOT. present(i_statustime)) THEN
	      IF(GRID_parameters%iolog > 0) THEN
	        WRITE(GRID_parameters%iolog,*) 'GRID_api [grid_getiteminfo]: assume default time (future)'
	      END IF
	      i_stime= i_futuretime
	    ELSE
	      i_stime= i_statustime
	    END IF
	    i_status= p_ghash(i_itemindex)%gp%att%i_stat(i_stime)
	  ELSE edge_info
	    IF(GRID_parameters%iolog > 0) THEN
	      WRITE(GRID_parameters%iolog,*) 'GRID_api [grid_getiteminfo]: this info is not available from type[edge] item'
	    END IF
	  END IF edge_info
	CASE('node') type_select
	  node_info: IF(present(r_coordinates)) THEN
	    r_coordinates= p_nhash(i_itemindex)%np%def%r_coor
	  ELSE IF(present(r_values)) THEN node_info
	    IF(.NOT. present(i_arrlen)) THEN
	      IF(GRID_parameters%iolog > 0) THEN
	        WRITE(GRID_parameters%iolog,*) 'GRID_api [grid_getiteminfo]: assume default array length'
	      END IF
	      i_len= DEF_nvalsize
	    ELSE
	      i_len= i_arrlen
	    END IF
	    IF(.NOT. present(i_statustime)) THEN
	      IF(GRID_parameters%iolog > 0) THEN
	        WRITE(GRID_parameters%iolog,*) 'GRID_api [grid_getiteminfo]: assume default time (future)'
	      END IF
	      i_stime= i_futuretime
	    ELSE
	      i_stime= i_statustime
	    END IF
	    r_values(1:i_len)= p_nhash(i_itemindex)%np%att%r_vals(1:i_len,i_stime)
	  ELSE IF(present(i_patch)) THEN node_info
	    IF(.NOT. present(i_arrlen)) THEN
	      IF(GRID_parameters%iolog > 0) THEN
	        WRITE(GRID_parameters%iolog,*) 'GRID_api [grid_getiteminfo]: assume default array length'
	      END IF
	      i_len= DEF_ndpatch
	    ELSE
	      i_len= i_arrlen
	    END IF
	    IF(.NOT. present(i_statustime)) THEN
	      IF(GRID_parameters%iolog > 0) THEN
	        WRITE(GRID_parameters%iolog,*) 'GRID_api [grid_getiteminfo]: assume default time (future)'
	      END IF
	      i_stime= i_futuretime
	    ELSE
	      i_stime= i_statustime
	    END IF
	    i_patch(1:i_len)= p_nhash(i_itemindex)%np%att%p_ptch(1:i_len,i_stime)
	  ELSE node_info
	    IF(GRID_parameters%iolog > 0) THEN
	      WRITE(GRID_parameters%iolog,*) 'GRID_api [grid_getiteminfo]: this info is not available from type[node] item'
	    END IF
	  END IF node_info
	CASE DEFAULT type_select
	  IF(GRID_parameters%iolog > 0) THEN
	    WRITE(GRID_parameters%iolog,*) 'GRID_api [grid_getiteminfo]: this item type is not available'
	  END IF
	END SELECT type_select
	RETURN
	END SUBROUTINE grid_getiteminfo
!*****************************************************************
	END MODULE FEM_dataretrieve


