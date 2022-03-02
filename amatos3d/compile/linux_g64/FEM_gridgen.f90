!*****************************************************************
!
! MODULE NAME:
!	FEM_gridgen
! FUNCTION:
!	FEM grid generation routines (create elements, nodes, refine, etc.)
! CONTAINS:
!-----------------------------------------------------------------
!
! NAME:
!	refine_tetra
! FUNCTION:
!	refine a tetrahedron
! SYNTAX:
!	CALL refine_tetra(tetra.ptr, grid)
! ON INPUT:
!	p_thisisme: tetrahedron to be refined	TYPE (tetra) POINTER
!	p_ghand: grid handle for linked list	TYPE (grid_handle)
! ON OUTPUT:
!	p_thisisme: modified tetrahedron		TYPE (tetra) POINTER
! CALLS:
!	refine_element, create_elmt, create_tetra
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!	refine_element
! FUNCTION:
!	refine an element (by bisection)
! SYNTAX:
!	CALL refine_element(elmt.ptr, grid, i_newnodeindx, i_gchil1,
!			    i_gchil2, i_newedge, i_echil1, i_echil2)
! ON INPUT:
!	p_thisisme: element to be refined	TYPE (elmt) POINTER
!	p_ghand: grid handle for linked list	TYPE (grid_handle)
! ON OUTPUT:
!	p_thisisme: modified element		TYPE (elmt) POINTER
!	i_newnodeindx: index of new created node	INTEGER (KIND = GRID_SI)
!	i_gchil1: index of new created child edge 1	INTEGER (KIND = GRID_SI)
!	i_gchil2: index of new created child edge 2	INTEGER (KIND = GRID_SI)
!	i_newedge: index of new created edge		INTEGER (KIND = GRID_SI)
!	i_echil1: index of new created child element 1	INTEGER (KIND = GRID_SI)
!	i_echil2: index of new created child element 2	INTEGER (KIND = GRID_SI)
! CALLS:
!	create_node, create_edge, create_elmt
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!	coarse_element
! FUNCTION:
!	coarsen a given refined element
! SYNTAX:
!	CALL coarse_element(elmt.ptr, grid)
! ON INPUT:
!	p_thisisme: element to be coarsened	TYPE (elmt) POINTER
!	p_ghand: grid handle			TYPE (grid_handle)
! ON OUTPUT:
!	p_thisisme: modified element		TYPE (elmt) POINTER
! CALLS:
!	erase_elmt, erase_edge
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!	coarse_edge
! FUNCTION:
!	coarsen a given refined edge
! SYNTAX:
!	CALL coarse_element(elmt.ptr, grid)
! ON INPUT:
!	p_thisisme: element to be coarsened	TYPE (edge) POINTER
!	p_ghand: grid handle			TYPE (grid_handle)
! ON OUTPUT:
!	p_thisisme: modified edge		TYPE (edge) POINTER
! CALLS:
!	erase_edge, erase_node
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!	grid_refine
! FUNCTION:
!	refine given triangulation according to flags in elements
! SYNTAX:
!	CALL grid_refine(grid)
! ON INPUT:
!	p_ghandle: grid handle data struct.	TYPE (grid_handle)
! ON OUTPUT:
!	p_ghandle: modified grid handle		TYPE (grid_handle)
! CALLS:
!	print_error, grid_extract, refine_element, hash_fineupdate,
!	check_triangulation, renew_patches
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!	find_resolvpatch
! FUNCTION:
!	find all resolvable patches for coarsening
! SYNTAX:
!	CALL find_resolvpatch(grid, int)
! ON INPUT:
!	p_ghand: grid handle data struct.	TYPE (grid_handle)
! ON OUTPUT:
!	p_ghand: modified grid handle		TYPE (grid_handle)
!	i_count: counter of all edges		INTEGER (KIND = GRID_SI)
! CALLS:
!
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!	grid_coarse
! FUNCTION:
!	coarsen a given triangulation according to the element's flags
! SYNTAX:
!	CALL grid_coarse(grid)
! ON INPUT:
!	p_ghandle: grid handle data struct.	TYPE (grid_handle)
! ON OUTPUT:
!	p_ghandle: modified grid handle		TYPE (grid_handle)
! CALLS:
!	coarse_element, coarse_edge, clearup_grid, renew_patches,
!	find_resolvpatch
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!	check_triangulation
! FUNCTION:
!	look for nonconforming elements and flag them for refinement
! SYNTAX:
!	CALL check_triangulation(grid, logical)
! ON INPUT:
!	p_ghandle: first element in linked list		TYPE (grid_handle)
! ON OUTPUT:
!	l_nonconforming: true if nonconforming elements present	LOGICAL
! CALLS:
!
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!	grid_globalrefine
! FUNCTION:
!	globally refine a given initial triangulation
! SYNTAX:
!	CALL grid_globalrefine(grid)
! ON INPUT:
!	p_ghandle: grid handle data struct.	TYPE (grid_handle)
! ON OUTPUT:
!	p_ghandle: modified grid handle		TYPE (grid_handle)
! CALLS:
!	print_error, grid_update, grid_refine
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!	grid_timeduplicate
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
!	grid_initfromsave
! FUNCTION:
!	initialize grid data structures after reading a saveset
! SYNTAX:
!	CALL grid_initfromsave(grid)
! ON INPUT:
!	p_ghand: handle to grid			TYPE (grid_handle)
! ON OUTPUT:
!	p_ghand: handle to modified grid	TYPE (grid_handle)
! CALLS:
!	grid_timeduplicate, hash_fineupdate
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!	grid_remove
! FUNCTION:
!	remove this whole grid and free memory
! SYNTAX:
!	CALL grid_remove(grid)
! ON INPUT:
!	p_ghand: handle to grid		TYPE (grid_handle)
! ON OUTPUT:
!
! CALLS:
!
! COMMENTS:
!
!-----------------------------------------------------------------
!	
! PUBLIC:
!	grid_refine, grid_coarse, grid_timeduplicate, grid_globalrefine,
!	grid_remove, grid_initfromsave
! COMMENTS:
!
! USES:
!	MISC_globalparam, MISC_error, FEM_define, FEM_handle, FEM_create,
!	FEM_gridmanag
! LIBRARIES:
!
! REFERENCES:
!	the grid refinement strategy has been taken from
!	M.C. Rivara: "Algorithms for Refining triangular grids suitable
!	for adaptive and multigrid techniques", INT. JOU. NUM. METH. ENG.,
!	Vol. 20, pp. 745-756 (1984)
!	E. Baensch: "Local mesh refinement in 2 and 3 dimensions",
!	IMPACT COMP. SCI. ENG., Vol. 3, pp. 181-191 (1991)
! VERSION(S):
!	1. original version			j. behrens	8/96
!	2. now with edges			j. behrens	10/96
!	3. now with time and handles, clearup_grid and
!	   grid_timeduplicate added, renamed 	j. behrens	10/96
!	4. grid_clean added, clearup_grid
!	    modified				j. behrens	5/97
!	5. hashing tables			j. behrens	7/97
!	6. many changes and bug-fixes		j. behrens	8/97
!	7. grid_timetoggle moved to GRID_api	j. behrens	9/97
!	8. extracted FEM_create			j. behrens	10/97
![	9. parallel version			j. behrens	1/98]
!	10. find_resolvpatch added		j. behrens	1/98
!	11. find_resolvpatch alg. changed	j. behrens	2/98
!	12. changed edge data struct.		j. behrens	2/98
!
!*****************************************************************
	MODULE FEM_gridgen
	  USE MISC_globalparam
	  USE MISC_error
	  USE FEM_define
	  USE FEM_handle
	  USE FEM_create
	  USE FEM_gridmanag
	  PRIVATE
	  PUBLIC  :: grid_refine, grid_coarse, grid_timeduplicate, grid_globalrefine, &
	             grid_remove, grid_initfromsave &
	             ! inserted for testing by T. Landes:
	             ,refine_tetra
	  CONTAINS
	
!*****************************************************************
	SUBROUTINE get_tetra_color(p_thisisme, i_color)

!---------- local declarations

	IMPLICIT NONE
	
	TYPE (tetra), POINTER	:: p_thisisme
	INTEGER (KIND = GRID_SI), INTENT(out)	:: i_color
	INTEGER (KIND = GRID_SI)											:: i_cnt, j_cnt, i_OK, i_merk, &
													   j_merk, k_merk, i_ndx
	TYPE (edge_ptr_arr), DIMENSION(DEF_tetelmts)	:: p_marked_edges
	TYPE (elmt), POINTER                            :: p_elmt, p_refelmt_1, p_refelmt_2, &
													   p_norefelmt_1, p_norefelmt_2, &
													   p_elmt1, p_elmt2, p_elmt3, p_elmt4
	TYPE (edge), POINTER							:: p_refside, &
													   p_edge1, p_edge2, p_edge3, p_edge4, &
													   p_edge5, p_edge6, p_tmp1, p_tmp2
	TYPE (node), POINTER							:: p_node1, p_node2, p_node3, p_node4
	INTEGER (KIND = GRID_SI)											:: i_ind1, i_ind2, i_refside

!---------- collect marked edges in elements of tetrahedron

 	DO i_cnt=1,DEF_tetelmts
 	  p_elmt                   => p_ehash(p_thisisme%def%p_elem(i_cnt))%ep
 	  p_marked_edges(i_cnt)%gp => p_ghash(p_elmt%def%p_edge(p_elmt%att%i_mark))%gp
 	END DO

 !---------- find the two elements that have marked the same edge,
 !           since these are to refine

 	i_OK= 0
 	find_elmts_1: DO i_cnt=1,(DEF_tetelmts-1)
 	  find_elmts_2: DO j_cnt=(i_cnt+1),DEF_tetelmts
 	    IF (p_marked_edges(i_cnt)%gp%def%i_indx == p_marked_edges(j_cnt)%gp%def%i_indx) THEN
	      p_refside => p_marked_edges(i_cnt)%gp
 	      p_refelmt_1 => p_ehash(p_thisisme%def%p_elem(i_cnt))%ep
 	      p_refelmt_2 => p_ehash(p_thisisme%def%p_elem(j_cnt))%ep
	      i_merk = i_cnt
	      j_merk = j_cnt
 	      i_OK = 1
 	      EXIT
 	    END IF
 	  END DO find_elmts_2
 	  IF (i_OK == 1) EXIT
 	END DO find_elmts_1
 	IF (i_OK == 0) THEN
	  CALL print_error(a_err='[get_tetra_color] No global refinement edge found')
 	END IF

!---------- find non-refined elements of tetrahedron

	DO i_cnt=1,DEF_tetelmts
	  IF (i_cnt /= i_merk) THEN
	    IF (i_cnt /= j_merk) THEN
	      p_norefelmt_1 => p_ehash(p_thisisme%def%p_elem(i_cnt))%ep
	      k_merk = i_cnt
	      EXIT
	    END IF
	  END IF
	END DO
	DO i_cnt=2,DEF_tetelmts
	  IF (i_cnt /= i_merk) THEN
	    IF (i_cnt /= j_merk) THEN
	      IF (i_cnt /= k_merk) THEN
	      	p_norefelmt_2 => p_ehash(p_thisisme%def%p_elem(i_cnt))%ep
	      END IF
	    END IF
	  END IF
	END DO

!---------- find shared edge of the non-refined elements

	i_OK= 0
	find_edge_1: DO i_cnt=1,DEF_eledges
	  find_edge_2: DO j_cnt=1,DEF_eledges
	    IF (p_norefelmt_1%def%p_edge(i_cnt) == p_norefelmt_2%def%p_edge(j_cnt)) THEN
	      i_ndx = p_norefelmt_1%def%p_edge(i_cnt);
	      i_OK = 1
	      EXIT
	    END IF
	  END DO find_edge_2
	  IF (i_OK == 1) EXIT
	END DO find_edge_1
	IF (i_OK == 0) THEN
	  CALL print_error(a_err='[get_tetra_color] No shared edge found')
	END IF

!---------- normalize tetra: set some pointers, initialize some indices and flags

	p_elmt3		=> p_refelmt_1
	p_elmt4		=> p_refelmt_2	

	i_refside	=  abs(p_elmt4%att%i_mark)          ! nur innerhalb von p_elmt4 g�ltig!!!
	i_ind1		=  mod(i_refside, DEF_eledges)+ 1   ! nur innerhalb von p_elmt4 g�ltig!!!
	i_ind2		=  mod(i_refside+1, DEF_eledges)+ 1 ! nur innerhalb von p_elmt4 g�ltig!!!
		
	p_edge2		=> p_ghash(p_elmt4%def%p_edge(i_ind2))%gp
	
	i_OK = 0
	DO i_cnt=1,DEF_eledges
	  IF (p_norefelmt_1%def%p_edge(i_cnt) == p_edge2%def%i_indx) THEN
	    p_elmt2 => p_norefelmt_1
	    p_elmt1 => p_norefelmt_2
	    i_OK = 1
	    EXIT
	  END IF
	END DO
	IF (i_OK == 0) THEN
	  p_elmt1 => p_norefelmt_1
	  p_elmt2 => p_norefelmt_2
	END IF

	p_edge1		=> p_refside
	p_edge3		=> p_ghash(p_elmt4%def%p_edge(i_ind1))%gp
	p_edge6		=> p_ghash(i_ndx)%gp ! shared edge from above
	p_node1		=> p_nhash(p_elmt4%def%p_node(i_ind1))%np
	p_node2		=> p_nhash(p_elmt4%def%p_node(i_ind2))%np
	p_node3		=> p_nhash(p_elmt4%def%p_node(i_refside))%np

	i_refside	=  abs(p_elmt3%att%i_mark)           ! nur innerhalb von p_elmt3 g�ltig!!!
	i_ind1		=  mod(i_refside, DEF_eledges)+ 1    ! nur innerhalb von p_elmt3 g�ltig!!!
	i_ind2		=  mod(i_refside+ 1, DEF_eledges)+ 1 ! nur innerhalb von p_elmt3 g�ltig!!!

	p_edge4		=> p_ghash(p_elmt3%def%p_edge(i_ind1))%gp
	p_edge5		=> p_ghash(p_elmt3%def%p_edge(i_ind2))%gp
	p_node4		=> p_nhash(p_elmt3%def%p_node(i_refside))%np

	! LRZpatra
        ! is p_edge4 an edge of p_elmt2?, otherwise swap p_edge4 and p_edge5
        ! to avoid crossover by above arbitrary assignment
        i_OK = 0
        DO i_cnt=1,DEF_eledges
                IF ( p_ghash(p_elmt2%def%p_edge(i_cnt))%gp%def%i_indx == p_edge4%def%i_indx) THEN
                        i_OK = 1
                END IF
        END DO
        IF ( i_OK == 0 ) THEN
                p_tmp1  => p_edge4
                p_edge4 => p_edge5
                p_edge5 => p_tmp1
        END IF
        ! \LRZpatra
	
!---------- determine color

	! colors: 1=black, 2=red, 0=none
	
	i_color = 0 ! initialize to no color
	
	p_tmp1 => p_ghash(p_elmt1%def%p_edge(p_elmt1%att%i_mark))%gp ! marked edge of elmt1
	p_tmp2 => p_ghash(p_elmt2%def%p_edge(p_elmt2%att%i_mark))%gp ! marked edge of elmt2

	IF (p_tmp1%def%i_indx == p_edge3%def%i_indx) THEN ! edge3 is marked
		IF (p_tmp2%def%i_indx == p_edge2%def%i_indx) THEN ! edge3 and edge2 are marked
			i_color = 1 ! black
		ELSE
			IF (p_tmp2%def%i_indx == p_edge4%def%i_indx) THEN ! edge3 and edge4 are marked
				i_color = 2 ! red
			END IF
		END IF
	ELSE
		IF (p_tmp2%def%i_indx == p_edge2%def%i_indx) THEN ! edge2 is marked
			IF (p_tmp1%def%i_indx == p_edge5%def%i_indx) THEN ! edge2 and edge5 are marked
				i_color = 2 ! red
			END IF
		ELSE
			IF (p_tmp2%def%i_indx == p_edge4%def%i_indx) THEN ! edge4 is marked
				IF (p_tmp1%def%i_indx == p_edge5%def%i_indx) THEN ! edge4 and edge5 are marked
					i_color = 1 ! black
				END IF
			END IF
		END IF
	END IF
	
	RETURN
	END SUBROUTINE get_tetra_color
		
!*****************************************************************
	SUBROUTINE refine_tetra(p_thisisme, p_ghand)

!---------- local declarations

	IMPLICIT NONE
	
	TYPE (tetra), POINTER			:: p_thisisme, p_tnew1, p_tnew2
	TYPE (grid_handle)			:: p_ghand
	INTEGER (KIND = GRID_SI)					:: i_cnt, j_cnt, i_OK, i_merk, &
						   j_merk, k_merk, i_ndx, i_tmp, i_tim
	TYPE (edge_ptr_arr), DIMENSION(DEF_tetelmts) :: p_marked_edges
	TYPE (elmt), POINTER                         :: p_elmt, p_refelmt_1, p_refelmt_2, &
						   p_norefelmt_1, p_norefelmt_2, &
						   p_elmt1, p_elmt2, p_elmt3, p_elmt4, &
						   p_newelmt, p_echil1, p_echil2, p_echil3, p_echil4
	INTEGER (KIND = GRID_SI), DIMENSION(DEF_eledges)		:: p_eledges
	INTEGER (KIND = GRID_SI), DIMENSION(DEF_elnodes)		:: p_elnodes
	INTEGER (KIND = GRID_SI), DIMENSION(DEF_tetedges)	:: p_tetedges
	INTEGER (KIND = GRID_SI), DIMENSION(DEF_tetnodes)	:: p_tetnodes
	INTEGER (KIND = GRID_SI), DIMENSION(DEF_tetelmts)	:: p_tetelmts
	TYPE (edge), POINTER			:: p_refside, p_gtmp, p_tmp, &
						   p_edge1, p_edge2, p_edge3, p_edge4, &
						   p_edge5, p_edge6
	TYPE (node), POINTER			:: p_node1, p_node2, p_node3, p_node4
	INTEGER (KIND = GRID_SI)					:: i_newnode, i_gchil1, i_gchil2, &
						   i_newedge1, i_newedge2,  &
						   i_echil1, i_echil2, i_echil3, i_echil4
	INTEGER (KIND = GRID_SI)					:: i_ind1, i_ind2, i_lvl, i_dummy1, i_dummy2, &
						   i_color_father, i_color_this, i_dummy3
	INTEGER (KIND = GRID_SI)					:: i_refside, i_indx1, i_gtmp
	TYPE (edge), POINTER			:: p_frnt

!----------  initialize time

	i_tim = p_ghand%i_timetag

!---------- collect marked edges in elements of tetrahedron

 	DO i_cnt=1,DEF_tetelmts
 	  p_elmt                   => p_ehash(p_thisisme%def%p_elem(i_cnt))%ep
 	  p_marked_edges(i_cnt)%gp => p_ghash(p_elmt%def%p_edge(p_elmt%att%i_mark))%gp
 	END DO

!---------- find the two elements that have marked the same edge,
!           since these are to refine

 	i_OK= 0
 	find_elmts_1: DO i_cnt=1,(DEF_tetelmts-1)
 	  find_elmts_2: DO j_cnt=(i_cnt+1),DEF_tetelmts
 	    IF (p_marked_edges(i_cnt)%gp%def%i_indx == p_marked_edges(j_cnt)%gp%def%i_indx) THEN
	      p_refside => p_marked_edges(i_cnt)%gp
 	      p_refelmt_1 => p_ehash(p_thisisme%def%p_elem(i_cnt))%ep
 	      p_refelmt_2 => p_ehash(p_thisisme%def%p_elem(j_cnt))%ep
	      i_merk = i_cnt
	      j_merk = j_cnt
 	      i_OK = 1
 	      EXIT
 	    END IF
 	  END DO find_elmts_2
 	  IF (i_OK == 1) EXIT
 	END DO find_elmts_1
 	IF (i_OK == 0) THEN
	  CALL print_error(a_err='[refine_tetra] No global refinement edge found')
 	END IF

!---------- find non-refined elements of tetrahedron

	DO i_cnt=1,DEF_tetelmts
	  IF (i_cnt /= i_merk) THEN
	    IF (i_cnt /= j_merk) THEN
	      p_norefelmt_1 => p_ehash(p_thisisme%def%p_elem(i_cnt))%ep
	      k_merk = i_cnt
	      EXIT
	    END IF
	  END IF
	END DO
	DO i_cnt=2,DEF_tetelmts
	  IF (i_cnt /= i_merk) THEN
	    IF (i_cnt /= j_merk) THEN
	      IF (i_cnt /= k_merk) THEN
	      	p_norefelmt_2 => p_ehash(p_thisisme%def%p_elem(i_cnt))%ep
	      END IF
	    END IF
	  END IF
	END DO

!---------- find shared edge of the non-refined elements

	i_OK= 0
	find_edge_1: DO i_cnt=1,DEF_eledges
	  find_edge_2: DO j_cnt=1,DEF_eledges
	    IF (p_norefelmt_1%def%p_edge(i_cnt) == p_norefelmt_2%def%p_edge(j_cnt)) THEN
	      i_ndx = p_norefelmt_1%def%p_edge(i_cnt);
	      i_OK = 1
	      EXIT
	    END IF
	  END DO find_edge_2
	  IF (i_OK == 1) EXIT
	END DO find_edge_1
	IF (i_OK == 0) THEN
	  CALL print_error(199)
	END IF

!---------- normalize tetrahedron: set some pointers, initialize some indices and flags

    ! set arbitrary correspondation of p_elmt3/p_elmt4 and p_refelmt_1/p_refelmt_2
    ! everything else will be set on the base of this definition
	p_elmt3 => p_refelmt_1
	p_elmt4 => p_refelmt_2	

	i_refside	= abs(p_elmt4%att%i_mark)           ! nur innerhalb von p_elmt4 g�ltig!!!
	i_ind1		= mod(i_refside, DEF_eledges) + 1   ! nur innerhalb von p_elmt4 g�ltig!!!
	i_ind2		= mod(i_refside+1, DEF_eledges) + 1 ! nur innerhalb von p_elmt4 g�ltig!!!
		
	p_edge2		=> p_ghash(p_elmt4%def%p_edge(i_ind2))%gp
	
	i_OK = 0
	DO i_cnt=1,DEF_eledges
		IF (p_norefelmt_1%def%p_edge(i_cnt) == p_edge2%def%i_indx) THEN
			p_elmt2 => p_norefelmt_1
			p_elmt1 => p_norefelmt_2
			i_OK = 1
			EXIT
		END IF
	END DO
	IF (i_OK == 0) THEN
		DO i_cnt=1,DEF_eledges
			IF (p_norefelmt_2%def%p_edge(i_cnt) == p_edge2%def%i_indx) THEN
				p_elmt1 => p_norefelmt_1
				p_elmt2 => p_norefelmt_2
				i_OK = 1
				EXIT
			END IF
		END DO
		IF (i_OK == 0) THEN
			PRINT *, "hier is der Fehler"		
		END IF
	END IF

	p_edge1	=> p_refside
	p_edge3	=> p_ghash(p_elmt4%def%p_edge(i_ind1))%gp
	p_edge6	=> p_ghash(i_ndx)%gp ! shared edge from above
	p_node1	=> p_nhash(p_elmt4%def%p_node(i_ind1))%np
	p_node2	=> p_nhash(p_elmt4%def%p_node(i_ind2))%np
	p_node3	=> p_nhash(p_elmt4%def%p_node(i_refside))%np

	i_refside	= abs(p_elmt3%att%i_mark)           ! nur innerhalb von p_elmt3 g�ltig!!!
	i_ind1		= mod(i_refside, DEF_eledges) + 1   ! nur innerhalb von p_elmt3 g�ltig!!!
	i_ind2		= mod(i_refside+1, DEF_eledges) + 1 ! nur innerhalb von p_elmt3 g�ltig!!!

	p_edge4	=> p_ghash(p_elmt3%def%p_edge(i_ind1))%gp
	p_edge5	=> p_ghash(p_elmt3%def%p_edge(i_ind2))%gp
	p_node4	=> p_nhash(p_elmt3%def%p_node(i_refside))%np

	! LRZpatra
        ! is p_edge4 an edge of p_elmt2?, otherwise swap p_edge4 and p_edge5
        ! to avoid crossover by above arbitrary assignment
        i_OK = 0
        DO i_cnt=1,DEF_eledges
                IF ( p_ghash(p_elmt2%def%p_edge(i_cnt))%gp%def%i_indx == p_edge4%def%i_indx) THEN
                        i_OK = 1
                END IF
        END DO
        IF ( i_OK == 0 ) THEN
                p_tmp   => p_edge4
                p_edge4 => p_edge5
                p_edge5 => p_tmp
        END IF
        ! \LRZpatra

!---------- refine the elements, but check if elements are refined already!

	IF(p_elmt4%att%i_stat(i_tim) == DEF_refined) THEN
	  i_refside=  abs(p_elmt4%att%i_mark)
	  i_indx1  =  mod(i_refside, DEF_eledges)+ 1
	  p_frnt   => p_ghash(p_elmt4%def%p_edge(i_refside))%gp
	  i_gchil1 =  p_frnt%lnk%p_chil(1,i_tim)
	  i_gchil2 =  p_frnt%lnk%p_chil(2,i_tim)
	  i_tmp= p_elmt4%def%p_node(i_indx1)
	  IF(p_frnt%def%p_node(1) /= i_tmp) THEN
	    i_gtmp  = i_gchil1
	    i_gchil1= i_gchil2
	    i_gchil2= i_gtmp
	  END IF
	  i_newedge1= p_elmt4%att%i_edge(i_tim)
	  i_tmp = p_frnt%att%i_node(i_tim)
	  IF(i_tmp == 0) THEN
	    CALL print_error(199)
	  END IF
	  i_newnode= i_tmp
	  i_echil1 = p_elmt4%lnk%p_chil(1,i_tim)
	  i_echil2 = p_elmt4%lnk%p_chil(2,i_tim)
	ELSE
	  CALL refine_element(p_elmt4, p_ghand, i_newnode, i_gchil1, i_gchil2, &
                              i_newedge1, i_echil1, i_echil2)
	ENDIF
	
	IF(p_elmt3%att%i_stat(i_tim) == DEF_refined) THEN
	  i_refside=  abs(p_elmt3%att%i_mark)
	  i_indx1  =  mod(i_refside, DEF_eledges)+ 1
	  p_frnt   => p_ghash(p_elmt3%def%p_edge(i_refside))%gp
	  i_dummy2 =  p_frnt%lnk%p_chil(1,i_tim)
	  i_dummy1 =  p_frnt%lnk%p_chil(2,i_tim)
	  i_tmp= p_elmt3%def%p_node(i_indx1)
	  IF(p_frnt%def%p_node(1) /= i_tmp) THEN
	    i_gtmp  = i_dummy2
	    i_dummy2= i_dummy1
	    i_dummy1= i_gtmp
	  END IF
	  i_newedge2= p_elmt3%att%i_edge(i_tim)
	  i_tmp = p_frnt%att%i_node(i_tim)
	  IF(i_tmp == 0) THEN
	    CALL print_error(199)
	  END IF
	  i_dummy3 = i_tmp
	  i_echil3 = p_elmt3%lnk%p_chil(1,i_tim)
	  i_echil4 = p_elmt3%lnk%p_chil(2,i_tim)
	ELSE
	  CALL refine_element(p_elmt3, p_ghand, i_dummy3, i_dummy2, i_dummy1, &
                                          i_newedge2, i_echil3, i_echil4)
	ENDIF

!---------- order new edges and elements

	DO i_cnt=1,2
		IF (p_ghash(i_gchil2)%gp%def%p_node(i_cnt) == p_node1%def%i_indx) THEN
			i_dummy3 = i_gchil1
			i_gchil1 = i_gchil2
			i_gchil2 = i_dummy3
			EXIT
		END IF
	END DO
	
	DO i_cnt=1,DEF_elnodes
		IF (p_ehash(i_echil2)%ep%def%p_node(i_cnt) == p_node1%def%i_indx) THEN
			i_dummy3 = i_echil1
			i_echil1 = i_echil2
			i_echil2 = i_dummy3
			EXIT
		END IF
	END DO
	
	DO i_cnt=1,DEF_elnodes
		IF (p_ehash(i_echil4)%ep%def%p_node(i_cnt) == p_node2%def%i_indx) THEN
			i_dummy3 = i_echil3
			i_echil3 = i_echil4
			i_echil4 = i_dummy3
			EXIT
		END IF
	END DO

	p_echil1 => p_ehash(i_echil1)%ep
	p_echil2 => p_ehash(i_echil2)%ep
	p_echil3 => p_ehash(i_echil3)%ep
	p_echil4 => p_ehash(i_echil4)%ep

!---------- nodes of new element

	! remark: element standing upright and viewed from the right
	p_elnodes(1) = p_node3%def%i_indx
	p_elnodes(2) = p_node4%def%i_indx
	p_elnodes(3) = i_newnode
	
!---------- edges of new element

	! remark: element standing upright and viewed from the right
	p_eledges(1) = i_newedge2         ! new dividing edge of elmt3
	p_eledges(2) = i_newedge1         ! new dividing edge of elmt4
	p_eledges(3) = p_edge6%def%i_indx ! shared edge of non-refined elements (edge6)

!---------- determine edge of new element to be marked for refinement
!           (based on "Local Mesh Refinement in 2 and 3 Dimensions" by Eberhard B�nsch)
!---------- and create new element dividing the tetrahedron

	! colors: 1=black, 2=red, 0=none

	CALL get_tetra_color(p_thisisme, i_color_this)
	
	IF (i_color_this == 2) THEN ! if this tetrahedron is red
		! mark edge6
		p_newelmt => create_elmt(p_elnodes, p_eledges, i_mark=3, i_boundary=DEF_inner, i_stat=DEF_unrefined)
	ELSE ! this tetrahedron is not red
		IF (i_color_this == 1) THEN ! this tetrahedron is black
			IF (p_thisisme%att%i_levl > 1) THEN ! if this tetrahedron has a father
				CALL get_tetra_color(p_thash(p_thisisme%lnk%p_prnt(1))%tp, i_color_father)
				IF (i_color_father == 2) THEN ! if father is red
					! mark edge6
					p_newelmt => create_elmt(p_elnodes, p_eledges, i_mark=3, i_boundary=DEF_inner, i_stat=DEF_unrefined)
				ELSE ! father is black
					p_gtmp => p_ghash(p_elmt2%def%p_edge(p_elmt2%att%i_mark))%gp
					IF (p_gtmp%def%i_indx == p_edge2%def%i_indx) THEN ! if edge2 marked in elmt2
						! mark newedge1
						p_newelmt => create_elmt(p_elnodes, p_eledges, i_mark=2, i_boundary=DEF_inner, i_stat=DEF_unrefined)
					ELSE
						! mark newedge2
						p_newelmt => create_elmt(p_elnodes, p_eledges, i_mark=1, i_boundary=DEF_inner, i_stat=DEF_unrefined)
					END IF
				END IF
			ELSE ! there is no father
				! mark edge6
				p_newelmt => create_elmt(p_elnodes, p_eledges, i_mark=3, i_boundary=DEF_inner, i_stat=DEF_unrefined)
			END IF
		ELSE ! this tetrahedron is neither red nor black
			! mark edge6
			p_newelmt => create_elmt(p_elnodes, p_eledges, i_mark=3, i_boundary=DEF_inner, i_stat=DEF_unrefined)
		END IF
	END IF

!---------- elements of new tetrahedron 1 (left)

	! remark: echil1 is bottom element
	p_tetelmts(1) = p_newelmt%def%i_indx
	p_tetelmts(2) = p_elmt2%def%i_indx
	p_tetelmts(3) = i_echil4
	p_tetelmts(4) = i_echil1

!---------- edges of new tetrahedron 1 (left)

	! remark: echil1 is bottom element
	p_tetedges(1) = i_gchil1
	p_tetedges(2) = p_edge2%def%i_indx
	p_tetedges(3) = i_newedge1
	p_tetedges(4) = p_edge4%def%i_indx
	p_tetedges(5) = i_newedge2
	p_tetedges(6) = p_edge6%def%i_indx

!---------- nodes of new tetrahedron 1 (left)

	! remark: echil1 is bottom element
	p_tetnodes(1) = p_node1%def%i_indx
	p_tetnodes(2) = i_newnode
	p_tetnodes(3) = p_node3%def%i_indx
	p_tetnodes(4) = p_node4%def%i_indx

!---------- create new tetrahedron 1 (left)

	i_lvl = p_thisisme%att%i_levl + 1
	p_ghand%i_minlvl = MIN(p_ghand%i_minlvl, i_lvl)
	p_ghand%i_maxlvl = MAX(p_ghand%i_maxlvl, i_lvl)

	p_tnew1 => create_tetra(p_tetnodes, p_tetedges, p_tetelmts, i_level=i_lvl, &
	                        i_stat=DEF_unrefined, p_mother=p_thisisme%def%i_indx)

!---------- elements of new tetrahedron 2 (right)

	! remark: echil2 is bottom element
	p_tetelmts(1) = p_elmt1%def%i_indx
	p_tetelmts(2) = p_newelmt%def%i_indx
	p_tetelmts(3) = i_echil3
	p_tetelmts(4) = i_echil2

!---------- edges of new tetrahedron 2 (right)

	! remark: echil2 is bottom element
	p_tetedges(1) = i_gchil2
	p_tetedges(2) = i_newedge1
	p_tetedges(3) = p_edge3%def%i_indx
	p_tetedges(4) = i_newedge2
	p_tetedges(5) = p_edge5%def%i_indx
	p_tetedges(6) = p_edge6%def%i_indx

!---------- nodes of new tetrahedron 2 (right)

	! remark: echil2 is bottom element
	p_tetnodes(1) = i_newnode
	p_tetnodes(2) = p_node2%def%i_indx
	p_tetnodes(3) = p_node3%def%i_indx
	p_tetnodes(4) = p_node4%def%i_indx

!---------- create new tetrahedron 2 (right)

	p_tnew2 => create_tetra(p_tetnodes, p_tetedges, p_tetelmts, i_level=i_lvl, &
	                        i_stat=DEF_unrefined, p_mother=p_thisisme%def%i_indx)

!---------- update mother-tetra (me)

	p_thisisme%lnk%p_chil(1, i_tim)	= p_tnew1%def%i_indx
	p_thisisme%lnk%p_chil(2, i_tim)	= p_tnew2%def%i_indx
	p_thisisme%att%i_stat(i_tim) 	= DEF_refined
	p_thisisme%att%i_elmt(i_tim)	= p_newelmt%def%i_indx

!---------- update elements

	p_newelmt%att%p_tets(1) = p_tnew1%def%i_indx
	p_newelmt%att%p_tets(2) = p_tnew2%def%i_indx

	DO i_cnt=1,DEF_elmtets
	  IF (p_echil1%att%p_tets(i_cnt) == 0) THEN
	    p_echil1%att%p_tets(i_cnt) = p_tnew1%def%i_indx
	    EXIT
	  END IF
	END DO

	DO i_cnt=1,DEF_elmtets
	  IF (p_echil2%att%p_tets(i_cnt) == 0) THEN
	    p_echil2%att%p_tets(i_cnt) = p_tnew2%def%i_indx
	    EXIT
	  END IF
	END DO

	DO i_cnt=1,DEF_elmtets
	  IF (p_echil3%att%p_tets(i_cnt) == 0) THEN
	    p_echil3%att%p_tets(i_cnt) = p_tnew2%def%i_indx
	    EXIT
	  END IF
	END DO

	DO i_cnt=1,DEF_elmtets
	  IF (p_echil4%att%p_tets(i_cnt) == 0) THEN
	    p_echil4%att%p_tets(i_cnt) = p_tnew1%def%i_indx
	    EXIT
	  END IF
	END DO

	DO i_cnt=1,DEF_elmtets
	  i_tmp = p_elmt1%att%p_tets(i_cnt)
	  IF (i_tmp /= 0) THEN
	    IF ( p_thash(i_tmp)%tp%def%i_indx == p_thisisme%def%i_indx) THEN
	      p_elmt1%att%p_tets(i_cnt) = p_tnew2%def%i_indx
	      EXIT
	    END IF
	  END IF
	END DO

	DO i_cnt=1,DEF_elmtets
	  i_tmp = p_elmt2%att%p_tets(i_cnt)
	  IF (i_tmp /= 0) THEN
	    IF ( p_thash(i_tmp)%tp%def%i_indx == p_thisisme%def%i_indx) THEN
	      p_elmt2%att%p_tets(i_cnt) = p_tnew1%def%i_indx
	      EXIT
	    END IF
	  END IF
	END DO


	RETURN
	END SUBROUTINE refine_tetra

!*****************************************************************
	SUBROUTINE refine_element(p_thisisme, p_ghand, i_newnodeindx, i_gchil1, &
	                          i_gchil2, i_newedge, i_echil1, i_echil2)

!---------- local declarations

	IMPLICIT NONE

	TYPE (elmt), POINTER                  :: p_thisisme
	TYPE (grid_handle)                    :: p_ghand
	INTEGER (KIND = GRID_SI)                               :: i_refside, i_ind, i_lvl, i_tmp, i_tim, &
											 i_ind1, i_ind2, i_me, i_bnd, i_cnt
	TYPE (edge), POINTER                  :: p_rigt, p_left, p_frnt
	TYPE (edge), POINTER                  :: p_gchil1, p_gchil2, p_newedge
	TYPE (elmt), POINTER                  :: p_echil1, p_echil2
	TYPE (node), POINTER                  :: p_node1, p_node2, p_node3
	TYPE (node), POINTER                  :: p_newnode
	TYPE (elmt), POINTER                  :: p_etmp
	TYPE (edge), POINTER                  :: p_gtmp
	REAL (KIND = GRID_SR), DIMENSION(DEF_dimension)        :: r_newcoord
	INTEGER (KIND = GRID_SI)                               :: i_gtmp, i_gtmp1, i_gtmp2
	INTEGER (KIND = GRID_SI)                               :: i_gchil1, i_gchil2
	INTEGER (KIND = GRID_SI)                               :: i_echil1, i_echil2
	INTEGER (KIND = GRID_SI)                               :: i_newnodeindx, i_newedge
	INTEGER (KIND = GRID_SI), DIMENSION(DEF_egnodes)       :: i_gnod
	INTEGER (KIND = GRID_SI), DIMENSION(DEF_elnodes)       :: p_enod
	INTEGER (KIND = GRID_SI), DIMENSION(DEF_eledges)       :: p_eedg

!---------- set some pointers, initialize some indices and flags

	i_refside=  abs(p_thisisme%att%i_mark)
	i_tim    =  p_ghand%i_timetag
	i_ind1   =  mod(i_refside, DEF_eledges)+ 1
	i_ind2   =  mod(i_refside+ 1, DEF_eledges)+ 1
	i_me     =  p_thisisme%def%i_indx
	p_rigt   => p_ghash(p_thisisme%def%p_edge(i_ind1))%gp
	p_left   => p_ghash(p_thisisme%def%p_edge(i_ind2))%gp
	p_frnt   => p_ghash(p_thisisme%def%p_edge(i_refside))%gp
	p_node1  => p_nhash(p_thisisme%def%p_node(i_ind1))%np
	p_node2  => p_nhash(p_thisisme%def%p_node(i_ind2))%np
	p_node3  => p_nhash(p_thisisme%def%p_node(i_refside))%np
	i_bnd    =  DEF_inner

!---------- initialize some pointers

	i_gtmp1= 0; i_gtmp2= 0

!---------- is this side refined already (then there's a node)

	  refined_me: IF(p_frnt%att%i_stat(i_tim) == DEF_refined) THEN

!---------- determine existing edge children (indices)

	    i_gchil1 = p_frnt%lnk%p_chil(1,i_tim)
	    i_gchil2 = p_frnt%lnk%p_chil(2,i_tim)

!---------- order them

	      i_tmp= p_node1%def%i_indx
	      IF(p_frnt%def%p_node(1) /= i_tmp) THEN
	        i_gtmp  = i_gchil1
	        i_gchil1= i_gchil2
	        i_gchil2= i_gtmp
	      END IF

!---------- watch out for the indices and determine existing node

	    i_ind = p_frnt%att%i_node(i_tim)
	    IF(i_ind == 0) THEN
	      CALL print_error(199)
	    END IF
	    i_newnodeindx= i_ind

!---------- if this side is not yet refined

	  ELSE refined_me

!---------- create a new node: first determine position

	    DO i_cnt=1,DEF_dimension
	      r_newcoord(i_cnt)= (p_node1%def%r_coor(i_cnt)+ &
	                          p_node2%def%r_coor(i_cnt))* 0.5
	    END DO

!---------- now create it

	    p_newnode => create_node(r_newcoord, i_stat=DEF_unrefined, &
	                             p_edge= p_frnt%def%i_indx)
	    i_newnodeindx= p_newnode%def%i_indx

!---------- and create children edges correspondingly

	    i_gnod(1)= p_node1%def%i_indx
	    i_gnod(2)= i_newnodeindx
	    p_gchil1 => create_edge(i_gnod, i_boundary=DEF_inner, i_stat=DEF_unrefined)
	    i_gchil1 = p_gchil1%def%i_indx

	    i_gnod(1)= i_newnodeindx
	    i_gnod(2)= p_node2%def%i_indx
	    p_gchil2 => create_edge(i_gnod, i_boundary=DEF_inner, i_stat=DEF_unrefined)
	    i_gchil2 = p_gchil2%def%i_indx

!---------- update mother edge

	    i_tmp= p_node1%def%i_indx
	    IF(p_frnt%def%p_node(1) == i_tmp) THEN
	      p_frnt%lnk%p_chil(1,i_tim)= i_gchil1
	      p_frnt%lnk%p_chil(2,i_tim)= i_gchil2
	    ELSE
	      p_frnt%lnk%p_chil(1,i_tim)= i_gchil2
	      p_frnt%lnk%p_chil(2,i_tim)= i_gchil1
	    END IF
	    p_frnt%att%i_stat(i_tim)= DEF_refined
	    p_frnt%att%i_node(i_tim)= i_newnodeindx

	  END IF refined_me

!---------- set level and update global info

	i_lvl = p_thisisme%att%i_levl+ 1
	
! now done on refine_tetra (T. Landes):
!	p_ghand%i_minlvl= MIN(p_ghand%i_minlvl, i_lvl)
!	p_ghand%i_maxlvl= MAX(p_ghand%i_maxlvl, i_lvl)
	
!---------- create new edge between new elements

	i_gnod(1)= p_node3%def%i_indx
	i_gnod(2)= i_newnodeindx
	p_newedge => create_edge(i_gnod, i_boundary=DEF_inner, i_stat=DEF_unrefined)
	i_newedge = p_newedge%def%i_indx

!---------- create new triangles

	p_enod(i_ind1)   = p_node1%def%i_indx
	p_enod(i_ind2)   = i_newnodeindx
	p_enod(i_refside)= p_node3%def%i_indx
	p_eedg(i_ind1)   = p_newedge%def%i_indx
	p_eedg(i_ind2)   = p_left%def%i_indx
	p_eedg(i_refside)= i_gchil1
	p_echil1 => create_elmt(p_enod, p_eedg, i_mark= i_ind2, i_level= i_lvl, &
	                        i_stat= DEF_unrefined, p_mother= i_me)
	i_echil1 = p_echil1%def%i_indx
	p_echil1%att%i_boun = p_thisisme%att%i_boun

	p_enod(i_ind1)   = i_newnodeindx
	p_enod(i_ind2)   = p_node2%def%i_indx
	p_enod(i_refside)= p_node3%def%i_indx
	p_eedg(i_ind1)   = p_rigt%def%i_indx
	p_eedg(i_ind2)   = p_newedge%def%i_indx
	p_eedg(i_refside)= i_gchil2
	p_echil2 => create_elmt(p_enod, p_eedg, i_mark= i_ind1, i_level= i_lvl, &
	                        i_stat= DEF_unrefined, p_mother= i_me)
	i_echil2 = p_echil2%def%i_indx
	p_echil2%att%i_boun = p_thisisme%att%i_boun

!---------- update mother

	p_thisisme%lnk%p_chil(1,i_tim)= p_echil1%def%i_indx
	p_thisisme%lnk%p_chil(2,i_tim)= p_echil2%def%i_indx
	p_thisisme%att%i_stat(i_tim)= DEF_refined
	p_thisisme%att%i_edge(i_tim)= p_newedge%def%i_indx

!---------- update edges

	p_newedge%att%p_elem(1)= p_echil1%def%i_indx
	p_newedge%att%p_elem(2)= p_echil2%def%i_indx
	p_newedge%att%i_elem=    2
	p_newedge%att%i_boun = p_thisisme%att%i_boun

	p_gchil1 => p_ghash(i_gchil1)%gp
	p_gchil1%att%i_boun = p_frnt%att%i_boun
	IF(i_gtmp1 /= 0) p_gchil1 => p_ghash(i_gtmp1)%gp
	i_tmp= p_gchil1%att%i_elem+ 1
	IF(i_tmp > DEF_egelems) THEN
	  CALL print_error(a_err='[refine_element]: No. of elements sharing edge exceeds limit')
	END IF
	p_gchil1%att%p_elem(i_tmp)= p_echil1%def%i_indx
	p_gchil1%att%i_elem       = i_tmp
! 	DO i_cnt=1,DEF_egelems
! 	  IF(p_gchil1%att%p_elem(i_cnt) == 0) THEN
! 	    p_gchil1%att%p_elem(i_cnt)= p_echil1%def%i_indx
! 	    EXIT
! 	  END IF
! 	END DO

	p_gchil2 => p_ghash(i_gchil2)%gp
	p_gchil2%att%i_boun = p_frnt%att%i_boun
	IF(i_gtmp2 /= 0) p_gchil2 => p_ghash(i_gtmp2)%gp
	i_tmp= p_gchil2%att%i_elem+ 1
	IF(i_tmp > DEF_egelems) THEN
	  CALL print_error(a_err='[refine_element]: No. of elements sharing edge exceeds limit')
	END IF
	p_gchil2%att%p_elem(i_tmp)= p_echil2%def%i_indx
	p_gchil2%att%i_elem       = i_tmp
! 	DO i_cnt=1,DEF_egelems
! 	  IF(p_gchil2%att%p_elem(i_cnt) == 0) THEN
! 	    p_gchil2%att%p_elem(i_cnt)= p_echil2%def%i_indx
! 	    EXIT
! 	  END IF
! 	END DO

	DO i_cnt=1,p_rigt%att%i_elem
	  i_tmp= p_rigt%att%p_elem(i_cnt)
	  IF(i_tmp /= 0) THEN
	    p_etmp=> p_ehash(i_tmp)%ep
	    i_ind =  p_etmp%def%i_indx
	    IF(i_ind == i_me) THEN
	      p_rigt%att%p_elem(i_cnt)= p_echil2%def%i_indx
	      EXIT
	    END IF
	  END IF
	END DO

	DO i_cnt=1,p_left%att%i_elem
	  i_tmp= p_left%att%p_elem(i_cnt)
	  IF(i_tmp /= 0) THEN
	    p_etmp=> p_ehash(i_tmp)%ep
	    i_ind =  p_etmp%def%i_indx
	    IF(i_ind == i_me) THEN
	      p_left%att%p_elem(i_cnt)= p_echil1%def%i_indx
	      EXIT
	    END IF
	  END IF
	END DO
	

	RETURN
	END SUBROUTINE refine_element


!*****************************************************************
	SUBROUTINE coarse_tetra(p_thisisme, p_ghand)

!---------- local declarations

	IMPLICIT NONE

	TYPE (tetra), POINTER                 :: p_thisisme
	TYPE (grid_handle)                    :: p_ghand

	TYPE (tetra), POINTER                 :: p_ttmp
	TYPE (elmt), POINTER                  :: p_etmp
	TYPE (elmt), POINTER                  :: p_rigt, p_left, p_midl
	INTEGER (KIND = GRID_SI)                               :: i_cnt, &
	  i_lftch, i_rgtch, i_tim, i_tmp, i_me

!---------- save pointers for linked list recovery, and edge update

	i_tim      =  p_ghand%i_timetag
	i_me       =  p_thisisme%def%i_indx
	i_lftch    =  p_thisisme%lnk%p_chil(1,i_tim)
	i_rgtch    =  p_thisisme%lnk%p_chil(2,i_tim)

!---------- my two unrefined faces(elements)

	NULLIFY(p_rigt, p_left)
	DO i_cnt=1,DEF_tetelmts
	  p_etmp=> p_ehash(p_thisisme%def%p_elem(i_cnt))%ep
	  IF(p_etmp%att%i_stat(i_tim) == DEF_unrefined) THEN
	    IF(ASSOCIATED(p_rigt)) THEN
	      p_left=> p_ehash(p_thisisme%def%p_elem(i_cnt))%ep
	    ELSE
	      p_rigt=> p_ehash(p_thisisme%def%p_elem(i_cnt))%ep
	    END IF
	  END IF
	END DO

!---------- determine element between children (has to be erased)

	p_midl     => p_ehash(p_thisisme%att%i_elmt(i_tim))%ep

!---------- update neigboring face information

	face_loop: DO i_cnt=1,DEF_elmtets
	  i_tmp= p_rigt%att%p_tets(i_cnt)
	  IF((i_tmp == i_rgtch) .OR. (i_tmp == i_lftch)) THEN
	      p_rigt%att%p_tets(i_cnt)= i_me
	  END IF
	  i_tmp= p_left%att%p_tets(i_cnt)
	  IF((i_tmp == i_rgtch) .OR. (i_tmp == i_lftch)) THEN
	      p_left%att%p_tets(i_cnt)= i_me
	  END IF
	END DO face_loop

!---------- destroy children

	child_loop: DO i_cnt=1, DEF_tetchild
	  p_ttmp => p_thash(p_thisisme%lnk%p_chil(i_cnt,i_tim))%tp
	  CALL erase_tetra(p_ttmp, p_ghand)
	  p_thisisme%lnk%p_chil(i_cnt,i_tim)= 0
	END DO child_loop

!---------- destroy element between them

	CALL erase_elmt(p_midl, p_ghand)

!---------- set attributes ...

	p_thisisme%att%i_stat(i_tim) = DEF_unrefined
	p_thisisme%att%i_elmt(i_tim) = 0

	
	RETURN
	END SUBROUTINE coarse_tetra

!*****************************************************************
	SUBROUTINE coarse_element(p_thisisme, p_ghand)

!---------- local declarations

	IMPLICIT NONE

	TYPE (elmt), POINTER                  :: p_thisisme
	TYPE (grid_handle)                    :: p_ghand

	TYPE (elmt), POINTER                  :: p_etmp
	TYPE (edge), POINTER                  :: p_rigt, p_left, p_midl
	INTEGER (KIND = GRID_SI)                               :: i_cnt, i_rgt, i_lft, &
	  i_lftch, i_rgtch, i_tim, i_tmp, i_me

!---------- save pointers for linked list recovery, and edge update

	i_tim      =  p_ghand%i_timetag
	i_me       =  p_thisisme%def%i_indx
	i_lftch    =  p_thisisme%lnk%p_chil(1,i_tim)
	i_rgtch    =  p_thisisme%lnk%p_chil(2,i_tim)

!---------- some indices

	i_rgt      =  mod(p_thisisme%att%i_mark,DEF_eledges)+ 1
	i_lft      =  mod(p_thisisme%att%i_mark+ 1,DEF_eledges)+ 1

!---------- determine edge between children (has to be erased)

	p_midl     => p_ghash(p_thisisme%att%i_edge(i_tim))%gp

!---------- update edge information

	p_rigt     => p_ghash(p_thisisme%def%p_edge(i_rgt))%gp
	p_left     => p_ghash(p_thisisme%def%p_edge(i_lft))%gp
	edge_loop: DO i_cnt=1,DEF_egelems
	  i_tmp= p_rigt%att%p_elem(i_cnt)
	  IF(i_tmp == i_rgtch) THEN
	      p_rigt%att%p_elem(i_cnt)= i_me
	  END IF
	  i_tmp= p_left%att%p_elem(i_cnt)
	  IF(i_tmp == i_lftch) THEN
	      p_left%att%p_elem(i_cnt)= i_me
	  END IF
	END DO edge_loop

!---------- destroy children

	child_loop: DO i_cnt=1, DEF_elchild
	  p_etmp => p_ehash(p_thisisme%lnk%p_chil(i_cnt,i_tim))%ep
	  CALL erase_elmt(p_etmp, p_ghand)
	  p_thisisme%lnk%p_chil(i_cnt,i_tim)= 0
	END DO child_loop

!---------- destroy edge between them

	CALL erase_edge(p_midl, p_ghand)

!---------- set attributes ...

	p_thisisme%att%i_stat(i_tim) = DEF_unrefined
	p_thisisme%att%i_edge(i_tim) = 0

	
	RETURN
	END SUBROUTINE coarse_element

!*****************************************************************
	SUBROUTINE coarse_edge(p_thisisme, p_ghand)

!---------- local declarations

	IMPLICIT NONE

	TYPE (edge), POINTER                  :: p_thisisme
	TYPE (grid_handle)                    :: p_ghand
	TYPE (edge), POINTER                  :: p_gtmp, p_gsplit
	TYPE (node), POINTER                  :: p_ntmp
	INTEGER (KIND = GRID_SI)                               :: i_cnt, i_tim

!---------- set time

	i_tim= p_ghand%i_timetag

!---------- destroy children

	child_loop: DO i_cnt=1, DEF_egchild
	  p_gtmp => p_ghash(p_thisisme%lnk%p_chil(i_cnt,i_tim))%gp

!---------- determine the node

	  only_once: IF(i_cnt == 1) THEN
	    p_ntmp   => p_nhash(p_gtmp%def%p_node(1))%np
	    IF(p_ntmp%att%p_edge == 0) THEN
	      p_ntmp   => p_nhash(p_gtmp%def%p_node(2))%np
	      IF(p_ntmp%att%p_edge == 0) THEN
	        CALL print_error(199)
	      END IF
	    END IF
	    p_gsplit => p_ghash(p_ntmp%att%p_edge)%gp
	    IF(p_gsplit%def%i_indx /= p_thisisme%def%i_indx) &
	      p_ntmp => p_nhash(p_gtmp%def%p_node(2))%np
	  END IF only_once
	  CALL erase_edge(p_gtmp, p_ghand)
	  p_thisisme%lnk%p_chil(i_cnt,i_tim)= 0
	END DO child_loop

!---------- destroy the node

	CALL erase_node(p_ntmp, p_ghand)

!---------- set attributes ...

	p_thisisme%att%i_stat(i_tim)= DEF_unrefined
	p_thisisme%att%i_node(i_tim)= 0

	RETURN
	END SUBROUTINE coarse_edge

!*****************************************************************
	SUBROUTINE grid_refine(p_ghand)

!---------- local declarations

	IMPLICIT NONE

	TYPE (grid_handle)                       :: p_ghand
	INTEGER (KIND = GRID_SI), DIMENSION(:), ALLOCATABLE       :: i_refindex
	INTEGER (KIND = GRID_SI)                                  :: i_refcount, i_alct, i_tim, i_cnt
	LOGICAL                                  :: l_nonconforming
	TYPE (tetra), POINTER                    :: p_tref
	INTEGER (KIND = GRID_SI)					 				 :: i_newnodeindx, i_gchil1, i_gchil2, &
	                                            i_newedge, i_echil1, i_echil2

!---------- set time

	i_tim = p_ghand%i_timetag

!---------- allocate array for grid management

	allocate(i_refindex(p_ghand%i_tnumfine), stat=i_alct)
	not_alloc: IF(i_alct /= 0) THEN
	  CALL print_error(28)
	END IF not_alloc

!---------- extract flags for refinement into array and count tetrahedrons to be refined

	CALL grid_extract(p_ghand, c_action='tffl', i_array1=i_refindex)
	i_refcount= count(i_refindex == DEF_pleasrefine)

!---------- deallocate temporary array

	deallocate(i_refindex)

!---------- refinement iteration loop

	IF(i_refcount /= 0) l_nonconforming= .TRUE.
	ref_loop: DO WHILE(l_nonconforming)

!---------- loop through the list and refine those triangles flagged for refinement

	  list_loop: DO i_cnt= 1, p_ghand%i_tnumfine
	    p_tref => p_thash(i_tfine(i_cnt,i_tim))%tp
	    IF(p_tref%att%i_stat(i_tim) == DEF_pleasrefine) THEN
	      CALL refine_tetra(p_tref, p_ghand)
	    END IF
	  END DO list_loop

!---------- update fine and boundary index tables

	  CALL hash_fineupdate(p_ghand)

!---------- look for nonconforming nodes and mark corresponding elements

	  CALL check_triangulation(p_ghand, l_nonconforming)
	
	END DO ref_loop

!---------- renew patch links for nodes

	CALL renew_patches(p_ghand)

	
	RETURN
	END SUBROUTINE grid_refine

!*****************************************************************
	SUBROUTINE find_resolvpatch(p_ghand, i_count)

!---------- local declarations

	IMPLICIT NONE

	TYPE (grid_handle)                       :: p_ghand
	INTEGER (KIND = GRID_SI), INTENT(out)                     :: i_count
	TYPE (tetra), POINTER                    :: p_ttmp
	TYPE (edge), POINTER                     :: p_refedge, p_gpartner
	TYPE (node), POINTER                     :: p_crsnode, p_npartner
	INTEGER (KIND = GRID_SI)                                  :: i_cnt, i_crslvl, i_tim

!---------- initialize output

	i_count= 0

!---------- set time tag

	i_tim= p_ghand%i_timetag

!---------- this is the coarsest level allowed

	i_crslvl= p_ghand%i_crslvlbnd

!---------- loop through nodes and look for resolvable patches

	main_loop: DO i_cnt= 1, p_ghand%i_nnumber
	  p_crsnode => p_nhash(i_ngrid(i_cnt, i_tim))%np
	  no_edge: IF(p_crsnode%att%p_edge == 0) THEN
	    CYCLE main_loop
	  END IF no_edge
	  p_refedge => p_ghash(p_crsnode%att%p_edge)%gp

!---------- if the edge is marked already, we can save work

	  save_edge: IF(p_refedge%att%i_stat(i_tim) == DEF_pleascoarse) THEN
	    CYCLE main_loop
	  END IF save_edge

!---------- if node does not lie at a periodic boundry

	  bound_peri: IF(p_refedge%att%i_boun <= DEF_inner) THEN

	    CALL check_resolvpatch(p_crsnode, p_refedge, i_tim, i_crslvl, i_count)

!---------- if we're at a periodic boundary !CAUTION: THIS MIGHT BE STILL WRONG

	  ELSE  bound_peri

!---------- this is my partner node and partner edge

	    p_npartner=> p_nhash(p_crsnode%lnk%p_peri)%np
	    p_gpartner=> p_ghash(p_refedge%lnk%p_peri)%gp

	    CALL check_resolvpatch(p_crsnode, p_refedge, i_tim, i_crslvl, i_count, &
	                           p_perinode= p_npartner, p_periedge= p_gpartner)

	  END IF bound_peri
	END DO main_loop

!---------- reset flags

	reset_loop: DO i_cnt= 1,p_ghand%i_tnumfine
	  p_ttmp => p_thash(i_tfine(i_cnt,i_tim))%tp
	  IF(p_ttmp%att%i_stat(i_tim) == DEF_pleascoarse) THEN
	    p_ttmp%att%i_stat(i_tim)= DEF_unrefined
	  END IF
	END DO reset_loop


	RETURN
	END SUBROUTINE find_resolvpatch

!*****************************************************************
	SUBROUTINE check_resolvpatch(p_chknode, p_chkedge, i_time, i_crslvl, &
	                             i_count, p_perinode, p_periedge)

!---------- local declarations

	IMPLICIT NONE

	TYPE (edge), POINTER                     :: p_chkedge
	TYPE (node), POINTER                     :: p_chknode
	INTEGER (KIND = GRID_SI), INTENT(inout)                   :: i_count
	INTEGER (KIND = GRID_SI), INTENT(in)                      :: i_time, i_crslvl
	TYPE (node), POINTER, OPTIONAL           :: p_perinode
	TYPE (edge), POINTER, OPTIONAL           :: p_periedge
	TYPE (tetra), POINTER                    :: p_ttmp
	TYPE (elmt), POINTER                     :: p_etmp, p_echl
	INTEGER (KIND = GRID_SI)                                  :: j_cnt, i_totcnt, &
	  i_mothcnt, i_crscnt, i_npt, i_cnt
	INTEGER (KIND = GRID_SI), DIMENSION(1)                    :: i_loc
	INTEGER (KIND = GRID_SI), DIMENSION(DEF_ndpatch)          :: i_mothers, i_mothind
	REAL (KIND = GRID_SR)                                     :: r_frac, r_port=0.75
	LOGICAL                                  :: l_patchfound

!---------- initialize pointers, etc.

	i_crscnt= 0; i_totcnt= 0; i_mothcnt= 0
	i_mothers=0; i_mothind=0

!---------- if node lies at the boundry, only two elements are in his patch

	bound_node: IF(p_chkedge%att%i_boun <= DEF_inner) THEN

!---------- the number of adjacent elements to the edge tells us the number
!           of tetrahedra in the patch of the node: 2 * #elems      if inner
!                                                   2 * (#elems -1) if boundary

	  i_npt= p_chkedge%att%i_elem* 2
	  IF(p_chkedge%att%i_boun < DEF_inner) i_npt= (p_chkedge%att%i_elem- 1)* 2

!---------- error if counted and calculated patch sizes differ

	  IF(p_chknode%att%i_ptch(i_time) /= i_npt) THEN
	    RETURN
!	    CALL print_error(a_err='[check_resolvpatch]: Patch size of pot. res. patch does not match calculated size')
	  END IF

!---------- loop over patch and count number of flagged, and number of mothers

	  patch_loop: DO j_cnt=1,i_npt
	    p_ttmp => p_thash(p_chknode%att%p_ptch(j_cnt,i_time))%tp
	    i_totcnt= i_totcnt+ 1

!---------- if tetrahedron is still refined, then there is an error!

	    IF(p_ttmp%lnk%p_chil(1,i_time) /= 0) THEN
	      CALL print_error(a_err='[check_resolvpatch]: A tetra in patch has an illegal child!')
	    END IF

!---------- count flag

	    flagged: IF((p_ttmp%att%i_stat(i_time) == DEF_pleascoarse) .AND. &
	                (p_ttmp%att%i_levl > i_crslvl)) THEN
	      i_crscnt= i_crscnt+ 1
	    END IF flagged

!---------- register mother and count her appearence (should be 2)

	    IF(.NOT. ANY(i_mothers(1:i_mothcnt) == p_ttmp%lnk%p_prnt(i_time))) THEN
	      i_mothcnt= i_mothcnt+1
	      i_mothers(i_mothcnt)= p_ttmp%lnk%p_prnt(i_time)
	      i_mothind(i_mothcnt)= 1
	    ELSE
	      i_loc= MINLOC(i_mothers(1:i_mothcnt), i_mothers(1:i_mothcnt)==p_ttmp%lnk%p_prnt(i_time))
	      i_mothind(i_loc(1))= i_mothind(i_loc(1))+1
	    END IF

!---------- reset tetra's flag

	      p_ttmp%att%i_stat(i_time)= DEF_unrefined
	  END DO patch_loop

!---------- calculate fraction of flagged tetras

	  r_frac=REAL(i_crscnt, GRID_SR)/REAL(i_totcnt, GRID_SR)

!---------- if potential patch was found, test if surrounding faces are refined

	  ptch_found: IF((ALL(i_mothind(1:i_mothcnt) == 2)) .AND. &
	                 (i_mothcnt > 0) .AND. &
			 (r_frac > r_port)) THEN
	    l_patchfound= .TRUE.
	    face_loop: DO j_cnt=1,p_chkedge%att%i_elem
	      p_etmp=> p_ehash(p_chkedge%att%p_elem(j_cnt))%ep
	      DO i_cnt=1, DEF_elchild
	        p_echl=> p_ehash(p_etmp%lnk%p_chil(i_cnt,i_time))%ep
	        IF(p_echl%att%i_stat(i_time) == DEF_refined) l_patchfound= .FALSE.
	      END DO
	    END DO face_loop

!---------- if patch was found, mark edge for coarsening

	    IF(l_patchfound) THEN
	      p_chkedge%att%i_stat(i_time)= DEF_pleascoarse
	      i_count= i_count+ 1
	    END IF
	  END IF ptch_found

!---------- if we're at a periodic boundary !CAUTION: THIS MIGHT BE STILL WRONG

	ELSE bound_node

	  CALL print_error(a_err='[check_resolvpatch]: Periodic boundaries not yet supported')

	END IF bound_node

	RETURN
	END SUBROUTINE check_resolvpatch

!*****************************************************************
	SUBROUTINE grid_coarse(p_ghand)

!---------- local declarations

	IMPLICIT NONE

	TYPE (grid_handle)                       :: p_ghand

	TYPE (tetra), POINTER                    :: p_ttmp
	TYPE (elmt), POINTER                     :: p_etmp
	TYPE (edge), POINTER                     :: p_gtmp
	INTEGER (KIND = GRID_SI)                                  :: i_count, i_cnt, i_tim, &
	  j_cnt, i_tmp, i_tnc

!---------- set time tag

	i_tim= p_ghand%i_timetag

!---------- find resolvapble patches

	CALL find_resolvpatch(p_ghand, i_count)

!---------- loop through the edges and coarsen the resolvable patches

	to_do: IF(i_count > 0) THEN

!---------- edge loop

	  edge_loop: DO j_cnt= 1, p_ghand%i_gnumber
	    p_gtmp => p_ghash(i_ggrid(j_cnt, i_tim))%gp

!---------- if edge is flaged, find triangles (faces) to be coarsened

	    coarsen: IF(p_gtmp%att%i_stat(i_tim) == DEF_pleascoarse) THEN
	      elem_loop: DO i_cnt=1, p_gtmp%att%i_elem
	        i_tmp= p_gtmp%att%p_elem(i_cnt)
	        IF(i_tmp /= 0) THEN
	          p_etmp=> p_ehash(i_tmp)%ep

!---------- for each triangle find adjacent tetrahedra for coarsening

		  tetra_loop: DO i_tnc=1,DEF_elmtets
		    i_tmp= p_etmp%att%p_tets(i_tnc)
		    IF(i_tmp /= 0) THEN
		      p_ttmp=> p_thash(i_tmp)%tp

!---------- if tetrahedron has not already been coarsened from oposite face, coarsen

		      IF(p_ttmp%att%i_stat(i_tim) == DEF_refined) THEN
	                CALL coarse_tetra(p_ttmp, p_ghand)
		      END IF
		    END IF
		  END DO tetra_loop

!---------- if face has not already been coarsened, then coarsen

		  IF(p_etmp%att%i_stat(i_tim) == DEF_refined) THEN
	            CALL coarse_element(p_etmp, p_ghand)
		  END IF
	        END IF
	      END DO elem_loop

!---------- finally coarsen the edge

	      CALL coarse_edge(p_gtmp, p_ghand)
	    END IF coarsen
	  END DO edge_loop
	END IF to_do

!---------- clear up grid before updating indices

	CALL clearup_grid(p_ghand)

!---------- renew patch links for nodes

	CALL renew_patches(p_ghand)

	RETURN
	END SUBROUTINE grid_coarse

!*****************************************************************
	SUBROUTINE check_triangulation(p_ghand, l_nonconforming)

!---------- local declarations

	IMPLICIT NONE

	TYPE (grid_handle)	:: p_ghand
	LOGICAL, INTENT(out)	:: l_nonconforming
	TYPE (edge), POINTER	:: p_gtmp
	TYPE (tetra), POINTER	:: p_ttmp
	INTEGER (KIND = GRID_SI)			:: i_cnt, j_cnt, i_tim

!---------- initialize output variable

	l_nonconforming= .FALSE.
	i_tim= p_ghand%i_timetag

!---------- loop through the list

	tetra_loop: DO j_cnt= 1, p_ghand%i_tnumfine
	  p_ttmp => p_thash(i_tfine(j_cnt,i_tim))%tp
	  side_loop: DO i_cnt=1,DEF_tetedges
	    p_gtmp => p_ghash(p_ttmp%def%p_edge(i_cnt))%gp
	    nonconforming: IF(p_gtmp%att%i_stat(i_tim) == DEF_refined) THEN
	      l_nonconforming = .TRUE.
	      p_ttmp%att%i_stat(i_tim)= DEF_pleasrefine
	    END IF nonconforming
	  END DO side_loop	
	END DO tetra_loop

	RETURN
	END SUBROUTINE check_triangulation

!*****************************************************************
	SUBROUTINE grid_globalrefine(p_ghand)

!---------- local declarations

	IMPLICIT NONE

	TYPE (grid_handle)					:: p_ghand
	INTEGER (KIND = GRID_SI)							:: i_alct, i_size
	INTEGER (KIND = GRID_SI), DIMENSION(:), ALLOCATABLE			:: i_aux


!---------- loop over levels
	
	level_loop: DO WHILE(p_ghand%i_maxlvl < p_ghand%i_crslvlbnd)
	
!---------- create array of flags

	  i_size= p_ghand%i_tnumfine
	  allocate(i_aux(i_size), stat=i_alct)
	  not_alloc: IF(i_alct /= 0) THEN
	    CALL print_error(28)
	  END IF not_alloc
	  i_aux= DEF_pleasrefine

!---------- update list of tetrahedrons

	  CALL grid_update(p_ghand, c_action='tffl', i_array1=i_aux)

!---------- deallocate temporary array

	  deallocate(i_aux)

!---------- do the refinement

	  CALL grid_refine(p_ghand)
	
	END DO level_loop

	RETURN
	END SUBROUTINE grid_globalrefine

!*****************************************************************
	SUBROUTINE grid_timeduplicate(p_grid1, p_grid2)

!---------- local declarations

	IMPLICIT NONE

	TYPE (grid_handle), INTENT(in)           :: p_grid1
	TYPE (grid_handle), INTENT(inout)        :: p_grid2

	TYPE (tetra), POINTER                    :: p_ttmp
	TYPE (elmt), POINTER                     :: p_etmp
	TYPE (edge), POINTER                     :: p_gtmp
	TYPE (node), POINTER                     :: p_ntmp
	INTEGER (KIND = GRID_SI)                                  :: i_curtim, i_futtim, i_cnt

!---------- copy grid handle information

	p_grid2%i_etotal    =  p_grid1%i_etotal
	p_grid2%i_enumber   =  p_grid1%i_enumber
	p_grid2%i_enumfine  =  p_grid1%i_enumfine
	p_grid2%i_enumboun  =  p_grid1%i_enumboun
	
	p_grid2%i_ttotal    =  p_grid1%i_ttotal
	p_grid2%i_tnumber   =  p_grid1%i_tnumber
	p_grid2%i_tnumfine  =  p_grid1%i_tnumfine
	
	p_grid2%i_gtotal    =  p_grid1%i_gtotal
	p_grid2%i_gnumber   =  p_grid1%i_gnumber
	p_grid2%i_gnumfine  =  p_grid1%i_gnumfine
	p_grid2%i_gnumboun  =  p_grid1%i_gnumboun
	
	p_grid2%i_ntotal    =  p_grid1%i_ntotal
	p_grid2%i_nnumber   =  p_grid1%i_nnumber
	
	p_grid2%i_minlvl    =  p_grid1%i_minlvl
	p_grid2%i_maxlvl    =  p_grid1%i_maxlvl
	p_grid2%i_crslvlbnd =  p_grid1%i_crslvlbnd
	p_grid2%i_reflvlbnd =  p_grid1%i_reflvlbnd

!---------- set time tags

	i_curtim = p_grid1%i_timetag
	i_futtim = p_grid2%i_timetag

!---------- copy index tables

	i_cnt                     = p_grid2%i_tnumber
	i_tgrid(1:i_cnt,i_futtim) = i_tgrid(1:i_cnt,i_curtim)
	i_cnt                     = p_grid2%i_tnumfine
	i_tfine(1:i_cnt,i_futtim) = i_tfine(1:i_cnt,i_curtim)

	i_cnt                     = p_grid2%i_enumber
	i_egrid(1:i_cnt,i_futtim) = i_egrid(1:i_cnt,i_curtim)
	i_cnt			  = p_grid2%i_enumboun
	i_eboun(1:i_cnt,i_futtim) = i_eboun(1:i_cnt,i_curtim)
	i_cnt			  = p_grid2%i_enumfine
	i_efine(1:i_cnt,i_futtim) = i_efine(1:i_cnt,i_curtim)

	i_cnt                     = p_grid2%i_gnumber
	i_ggrid(1:i_cnt,i_futtim) = i_ggrid(1:i_cnt,i_curtim)
	i_cnt                     = p_grid2%i_gnumboun
	i_gboun(1:i_cnt,i_futtim) = i_gboun(1:i_cnt,i_curtim)
	i_cnt                     = p_grid2%i_gnumfine
	i_gfine(1:i_cnt,i_futtim) = i_gfine(1:i_cnt,i_curtim)

	i_cnt                     = p_grid2%i_nnumber
	i_ngrid(1:i_cnt,i_futtim) = i_ngrid(1:i_cnt,i_curtim)

!---------- loop through the list of tetrahedrons

	tetra_loop: DO i_cnt=1, p_grid2%i_tnumber
	  p_ttmp=> p_thash(i_tgrid(i_cnt,i_futtim))%tp
	  p_ttmp%lnk%p_prnt(i_futtim)  = p_ttmp%lnk%p_prnt(i_curtim)
	  p_ttmp%lnk%p_chil(:,i_futtim)= p_ttmp%lnk%p_chil(:,i_curtim)
	  p_ttmp%att%i_time            = i_futtim
	  p_ttmp%att%i_stat(i_futtim)  = p_ttmp%att%i_stat(i_curtim)
	  p_ttmp%att%i_elmt(i_futtim)  = p_ttmp%att%i_elmt(i_curtim)
	END DO tetra_loop

!---------- loop through the list of elements

	element_loop: DO i_cnt=1, p_grid2%i_enumber
	  p_etmp=> p_ehash(i_egrid(i_cnt,i_futtim))%ep
	  p_etmp%lnk%p_prnt(i_futtim)  = p_etmp%lnk%p_prnt(i_curtim)
	  p_etmp%lnk%p_chil(:,i_futtim)= p_etmp%lnk%p_chil(:,i_curtim)
	  p_etmp%att%i_time            = i_futtim
	  p_etmp%att%i_stat(i_futtim)  = p_etmp%att%i_stat(i_curtim)
	  p_etmp%att%i_edge(i_futtim)  = p_etmp%att%i_edge(i_curtim)
	END DO element_loop

!---------- loop through the list of edges

	edge_loop: DO i_cnt=1, p_grid2%i_gnumber
	  p_gtmp=> p_ghash(i_ggrid(i_cnt,i_futtim))%gp
	  p_gtmp%att%i_time            = i_futtim
	  p_gtmp%att%i_stat(i_futtim)  = p_gtmp%att%i_stat(i_curtim)
	  p_gtmp%att%i_node(i_futtim)  = p_gtmp%att%i_node(i_curtim)
	  p_gtmp%lnk%p_chil(:,i_futtim)= p_gtmp%lnk%p_chil(:,i_curtim)
	END DO edge_loop

!---------- loop through the list of nodes

	node_loop: DO i_cnt=1, p_grid2%i_nnumber
	  p_ntmp=> p_nhash(i_ngrid(i_cnt,i_futtim))%np
	  p_ntmp%att%i_time            = i_futtim
	  p_ntmp%att%r_vals(:,i_futtim)= p_ntmp%att%r_vals(:,i_curtim)
	  p_ntmp%att%i_ptch(i_futtim)  = p_ntmp%att%i_ptch(i_curtim)
	  p_ntmp%att%p_ptch(:,i_futtim)= p_ntmp%att%p_ptch(:,i_curtim)
	END DO node_loop

	RETURN
	END SUBROUTINE grid_timeduplicate

!*****************************************************************
	SUBROUTINE grid_initfromsave(p_ghand)

!---------- local declarations

	IMPLICIT NONE

	TYPE (grid_handle), DIMENSION(DEF_timesteps), INTENT(inout)    :: p_ghand

!---------- duplicate the old to the new grid

	CALL grid_timeduplicate(p_ghand(i_pasttime), p_ghand(i_futuretime))

!---------- restore finest grid index arrays

	CALL hash_fineupdate(p_ghand(i_futuretime))

	RETURN
	END SUBROUTINE grid_initfromsave

!*****************************************************************
	SUBROUTINE grid_remove(p_ghand)

!---------- local declarations

	IMPLICIT NONE

	TYPE (grid_handle), DIMENSION(DEF_timesteps) :: p_ghand

	TYPE (elmt), POINTER             :: p_eclr
	TYPE (edge), POINTER             :: p_gclr
	TYPE (node), POINTER             :: p_nclr
	INTEGER (KIND = GRID_SI)                          :: i_cnt

!---------- loop through elements

	DO i_cnt=1, p_ghand(i_futuretime)%i_etotal
	  p_eclr => p_ehash(i_cnt)%ep
	  IF(associated(p_eclr)) deallocate(p_eclr)
	END DO

!---------- loop through edges

	DO i_cnt=1, p_ghand(i_futuretime)%i_gtotal
	  p_gclr => p_ghash(i_cnt)%gp
	  IF(associated(p_gclr)) deallocate(p_gclr)
	END DO

!---------- loop through nodes

	DO i_cnt=1, p_ghand(i_futuretime)%i_ntotal
	  p_nclr => p_nhash(i_cnt)%np
	  IF(associated(p_nclr)) deallocate(p_nclr)
	END DO

!---------- deallocate hashing and index tables

	deallocate(p_ehash, p_ghash, p_nhash)
	deallocate(i_egrid, i_ggrid, i_ngrid)
	deallocate(i_efine, i_gfine, i_gboun)

!---------- print logging information, if required

	IF(GRID_parameters%iolog > 0) THEN
	  WRITE(GRID_parameters%iolog,*) ' LOGINFO: hashing and index tables deallocated,'
	END IF

!---------- update handle

	p_ghand(:)%i_enumber   =  0
	p_ghand(:)%i_gnumber   =  0
	p_ghand(:)%i_enumfine  =  0
	p_ghand(:)%i_gnumfine  =  0
	p_ghand(:)%i_gnumboun  =  0
	p_ghand(:)%i_nnumber   =  0
	p_ghand(:)%i_etotal    =  0
	p_ghand(:)%i_gtotal    =  0
	p_ghand(:)%i_ntotal    =  0

	RETURN
	END SUBROUTINE grid_remove


	END MODULE FEM_gridgen



































































