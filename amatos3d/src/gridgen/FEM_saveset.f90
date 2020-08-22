!*****************************************************************
!
! MODULE NAME:
!	FEM_saveset
! FUNCTION:
!	read and write complete data structures for saving
! CONTAINS:
!-----------------------------------------------------------------
!
! NAME:
!	grid_readsaveset
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
!	grid_writesaveset
! FUNCTION:
!	write a saveset from disc
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
!	grid_readdomain
! FUNCTION:
!	read domain polyline information from disc
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
! PUBLIC:
!	grid_writesaveset, grid_readsaveset, grid_readdomain
! COMMENTS:
!
! USES:
!	MISC_globalparam, MISC_error, FEM_define, FEM_handle
! LIBRARIES:
!
! REFERENCES:
!
! VERSION(S):
!	1. original version			j. behrens	10/96
!	2. FEM_handle added			j. behrens	7/97
!	3. hashing tables			j. behrens	7/97
!	4. updated for amatos version 1.0	j. behrens	11/2000
!
!*****************************************************************
	MODULE FEM_saveset
	  USE MISC_globalparam
	  USE MISC_error
	  USE FEM_define
	  USE FEM_handle
	  USE FEM_create
	  PRIVATE
	  PUBLIC :: grid_wsaveset, grid_rsaveset, grid_readdomain
	  CONTAINS
!*****************************************************************
	  SUBROUTINE grid_wsaveset(c_file, p_handle)

!---------- local declarations

	  IMPLICIT NONE
	  CHARACTER (len=32)                             :: c_file
	  TYPE (grid_handle), DIMENSION(DEF_timesteps)   :: p_handle

	  INTEGER (KIND = GRID_SI), DIMENSION(17)                         :: i_hand
	  INTEGER (KIND = GRID_SI)                                        :: i_tcn, i_ind, i_fst, i_unit
	  TYPE (elmt), POINTER                           :: p_etmp
	  TYPE (edge), POINTER                           :: p_gtmp
	  TYPE (node), POINTER                           :: p_ntmp
	  TYPE (tetra), POINTER                          :: p_ttmp

!---------- open file

	  i_unit= 17
	  open(i_unit, file= c_file, form= 'unformatted', iostat= i_fst)
	  not_opened: IF(i_fst /= 0) THEN
	    CALL print_error(96)
	  END IF not_opened
	  IF(GRID_parameters%iolog > 0) &
	    write(GRID_parameters%iolog,*) 'INFO: Filename: ', c_file, ' opened on unit: ', i_unit

!---------- write handle information to disc

	  time_handl: DO i_tcn=1, DEF_timesteps
	    i_hand(1) = p_handle(i_tcn)%i_timetag
	    i_hand(2) = p_handle(i_tcn)%i_enumber
	    i_hand(3) = p_handle(i_tcn)%i_gnumber
	    i_hand(4) = p_handle(i_tcn)%i_enumfine
	    i_hand(5) = p_handle(i_tcn)%i_gnumfine
	    i_hand(6) = p_handle(i_tcn)%i_gnumboun
	    i_hand(7) = p_handle(i_tcn)%i_nnumber
	    i_hand(8) = p_handle(i_tcn)%i_etotal
	    i_hand(9) = p_handle(i_tcn)%i_gtotal
	    i_hand(10)= p_handle(i_tcn)%i_ntotal
	    i_hand(11)= p_handle(i_tcn)%i_minlvl
	    i_hand(12)= p_handle(i_tcn)%i_maxlvl
	    i_hand(13)= p_handle(i_tcn)%i_crslvlbnd
	    i_hand(14)= p_handle(i_tcn)%i_reflvlbnd
	    i_hand(15)= p_handle(i_tcn)%i_tnumber
	    i_hand(16)= p_handle(i_tcn)%i_ttotal
	    i_hand(17)= p_handle(i_tcn)%i_tnumfine
	    write(i_unit) i_hand
	  END DO time_handl
	  write(i_unit) i_futuretime, i_currenttime, i_pasttime

!---------- write index tables

	  write(i_unit) i_thashmax, i_ehashmax, i_ghashmax, i_nhashmax, &
	                i_tgridmax, i_egridmax, i_ggridmax, i_ngridmax, &
	                i_tfinemax, i_efinemax, i_gfinemax, &
			i_ebounmax, i_gbounmax
	  write(i_unit) ((i_tgrid(i_ind, i_tcn), i_ind= 1, i_tgridmax), &
	                                         i_tcn= 1, DEF_timesteps)
	  write(i_unit) ((i_egrid(i_ind, i_tcn), i_ind= 1, i_egridmax), &
	                                         i_tcn= 1, DEF_timesteps)
	  write(i_unit) ((i_ggrid(i_ind, i_tcn), i_ind= 1, i_ggridmax), &
	                                         i_tcn= 1, DEF_timesteps)
	  write(i_unit) ((i_ngrid(i_ind, i_tcn), i_ind= 1, i_ngridmax), &
	                                         i_tcn= 1, DEF_timesteps)
	  write(i_unit) ((i_tfine(i_ind, i_tcn), i_ind= 1, i_tfinemax), &
	                                         i_tcn= 1, DEF_timesteps)
	  write(i_unit) ((i_efine(i_ind, i_tcn), i_ind= 1, i_efinemax), &
	                                         i_tcn= 1, DEF_timesteps)
	  write(i_unit) ((i_gfine(i_ind, i_tcn), i_ind= 1, i_gfinemax), &
	                                         i_tcn= 1, DEF_timesteps)
	  write(i_unit) ((i_eboun(i_ind, i_tcn), i_ind= 1, i_ebounmax), &
	                                         i_tcn= 1, DEF_timesteps)
	  write(i_unit) ((i_gboun(i_ind, i_tcn), i_ind= 1, i_gbounmax), &
	                                         i_tcn= 1, DEF_timesteps)

!---------- write nodes

	  node_write: DO i_ind=1, p_handle(1)%i_ntotal
	    p_ntmp=> p_nhash(i_ind)%np
	    write(i_unit) p_ntmp%def%r_coor, p_ntmp%att%r_vals, &
	              p_ntmp%def%i_indx, p_ntmp%att%i_time, p_ntmp%lnk%p_peri, &
	              p_ntmp%att%p_edge, p_ntmp%att%i_stat, p_ntmp%att%i_ptch, &
	              p_ntmp%att%p_ptch
	  END DO node_write

!---------- write edges

	  edge_write: DO i_ind=1, p_handle(1)%i_gtotal
	    p_gtmp=> p_ghash(i_ind)%gp
	    write(i_unit) p_gtmp%def%i_indx, p_gtmp%def%p_node, p_gtmp%att%i_time, &
	              p_gtmp%att%i_boun, p_gtmp%att%i_elem, p_gtmp%att%i_stat, &
	              p_gtmp%att%i_node, p_gtmp%att%p_elem, p_gtmp%lnk%p_peri, &
	              p_gtmp%lnk%p_chil
	  END DO edge_write

!---------- write elements

	  elmt_write: DO i_ind=1, p_handle(1)%i_etotal
	    p_etmp=> p_ehash(i_ind)%ep
	    write(i_unit) p_etmp%att%r_vals, p_etmp%def%i_indx, p_etmp%def%p_node, &
	              p_etmp%def%p_edge, p_etmp%att%i_time, p_etmp%att%i_stat, &
	              p_etmp%att%i_mark, p_etmp%att%i_levl, p_etmp%att%i_edge, &
	              p_etmp%lnk%p_prnt, p_etmp%lnk%p_chil, p_etmp%att%i_boun, &
		      p_etmp%att%p_tets
!		      p_etmp%att%i_tets, p_etmp%att%p_tets
	  END DO elmt_write

!---------- write tetras

	  tet_write: DO i_ind=1, p_handle(1)%i_ttotal
	    p_ttmp=> p_thash(i_ind)%tp
	    write(i_unit) p_ttmp%att%r_vals, p_ttmp%def%i_indx, p_ttmp%def%p_node, &
	              p_ttmp%def%p_edge, p_ttmp%def%p_elem, p_ttmp%att%i_time, &
	              p_ttmp%att%i_stat, p_ttmp%att%i_levl, p_ttmp%att%i_elmt, &
	              p_ttmp%lnk%p_prnt, p_ttmp%lnk%p_chil
	  END DO tet_write

!---------- close file

	  close(i_unit)
	  IF(GRID_parameters%iolog > 0) &
	    write(GRID_parameters%iolog,*) 'INFO: Closed file on unit: ', i_unit

	  RETURN
	  END SUBROUTINE grid_wsaveset

!*****************************************************************
	  SUBROUTINE grid_rsaveset(c_file, p_handle)

!---------- local declarations

	  IMPLICIT NONE
	  CHARACTER (len=32)                             :: c_file
	  TYPE (grid_handle), DIMENSION(DEF_timesteps)   :: p_handle

	  INTEGER (KIND = GRID_SI), DIMENSION(17)                         :: i_hand
	  INTEGER (KIND = GRID_SI)                                        :: i_alct, i_tcn, i_ind, i_fst, i_unit
	  TYPE (elmt), POINTER                           :: p_etmp
	  TYPE (edge), POINTER                           :: p_gtmp
	  TYPE (node), POINTER                           :: p_ntmp
	  TYPE (tetra), POINTER                          :: p_ttmp

!---------- open file

	  i_unit= 17
	  open(i_unit, file= c_file, form= 'unformatted', iostat= i_fst)
	  not_opened: IF(i_fst /= 0) THEN
	    CALL print_error(97)
	  END IF not_opened
	  IF(GRID_parameters%iolog > 0) &
	    write(GRID_parameters%iolog,*) 'INFO: Filename: ', c_file, ' opened on unit: ', i_unit

!---------- read handle information from disc

	  time_handl: DO i_tcn=1, DEF_timesteps
	    read(i_unit) i_hand
	    p_handle(i_tcn)%i_timetag  = i_hand(1)
	    p_handle(i_tcn)%i_enumber  = i_hand(2)
	    p_handle(i_tcn)%i_gnumber  = i_hand(3)
	    p_handle(i_tcn)%i_enumfine = i_hand(4)
	    p_handle(i_tcn)%i_gnumfine = i_hand(5)
	    p_handle(i_tcn)%i_gnumboun = i_hand(6)
	    p_handle(i_tcn)%i_nnumber  = i_hand(7)
	    p_handle(i_tcn)%i_etotal   = i_hand(8)
	    p_handle(i_tcn)%i_gtotal   = i_hand(9)
	    p_handle(i_tcn)%i_ntotal   = i_hand(10)
	    p_handle(i_tcn)%i_minlvl   = i_hand(11)
	    p_handle(i_tcn)%i_maxlvl   = i_hand(12)
	    p_handle(i_tcn)%i_crslvlbnd= i_hand(13)
	    p_handle(i_tcn)%i_reflvlbnd= i_hand(14)
	    p_handle(i_tcn)%i_tnumber  = i_hand(15)
	    p_handle(i_tcn)%i_ttotal   = i_hand(16)
	    p_handle(i_tcn)%i_tnumfine = i_hand(17)
	  END DO time_handl
	  read(i_unit) i_futuretime, i_currenttime, i_pasttime

!---------- read index tables

	  read(i_unit) i_thashmax, i_ehashmax, i_ghashmax, i_nhashmax, &
	               i_tgridmax, i_egridmax, i_ggridmax, i_ngridmax, &
	               i_tfinemax, i_efinemax, i_gfinemax, &
		       i_ebounmax, i_gbounmax

!---------- allocate hashing and index tables with appropriate sizes

	  CALL grid_initdatastruct(c_action='size')

!---------- fill hashing and index tables

	  read(i_unit) ((i_tgrid(i_ind, i_tcn), i_ind= 1, i_tgridmax), &
	                                        i_tcn= 1, DEF_timesteps)
	  read(i_unit) ((i_egrid(i_ind, i_tcn), i_ind= 1, i_egridmax), &
	                                        i_tcn= 1, DEF_timesteps)
	  read(i_unit) ((i_ggrid(i_ind, i_tcn), i_ind= 1, i_ggridmax), &
	                                        i_tcn= 1, DEF_timesteps)
	  read(i_unit) ((i_ngrid(i_ind, i_tcn), i_ind= 1, i_ngridmax), &
	                                        i_tcn= 1, DEF_timesteps)
	  read(i_unit) ((i_tfine(i_ind, i_tcn), i_ind= 1, i_tfinemax), &
	                                        i_tcn= 1, DEF_timesteps)
	  read(i_unit) ((i_efine(i_ind, i_tcn), i_ind= 1, i_efinemax), &
	                                        i_tcn= 1, DEF_timesteps)
	  read(i_unit) ((i_gfine(i_ind, i_tcn), i_ind= 1, i_gfinemax), &
	                                        i_tcn= 1, DEF_timesteps)
	  read(i_unit) ((i_eboun(i_ind, i_tcn), i_ind= 1, i_ebounmax), &
	                                        i_tcn= 1, DEF_timesteps)
	  read(i_unit) ((i_gboun(i_ind, i_tcn), i_ind= 1, i_gbounmax), &
	                                        i_tcn= 1, DEF_timesteps)

!---------- read nodes

	  node_read: DO i_ind=1, p_handle(1)%i_ntotal
	    allocate(p_ntmp, stat=i_alct)
	    node_notalloc: IF(i_alct /= 0) THEN
	      CALL print_error(98)
	    END IF node_notalloc
	    read(i_unit) p_ntmp%def%r_coor, p_ntmp%att%r_vals, &
	              p_ntmp%def%i_indx, p_ntmp%att%i_time, p_ntmp%lnk%p_peri, &
	              p_ntmp%att%p_edge, p_ntmp%att%i_stat, p_ntmp%att%i_ptch, &
	              p_ntmp%att%p_ptch
	    p_nhash(i_ind)%np=> p_ntmp
	  END DO node_read

!---------- read edges

	  edge_read: DO i_ind=1, p_handle(1)%i_gtotal
	    allocate(p_gtmp, stat=i_alct)
	    edge_notalloc: IF(i_alct /= 0) THEN
	      CALL print_error(99)
	    END IF edge_notalloc
	    read(i_unit) p_gtmp%def%i_indx, p_gtmp%def%p_node, p_gtmp%att%i_time, &
	              p_gtmp%att%i_boun, p_gtmp%att%i_elem, p_gtmp%att%i_stat, &
	              p_gtmp%att%i_node, p_gtmp%att%p_elem, p_gtmp%lnk%p_peri, &
	              p_gtmp%lnk%p_chil
	    p_ghash(i_ind)%gp=> p_gtmp
	  END DO edge_read

!---------- read elements

	  elmt_read: DO i_ind=1, p_handle(1)%i_etotal
	    allocate(p_etmp, stat=i_alct)
	    elmt_notalloc: IF(i_alct /= 0) THEN
	      CALL print_error(100)
	    END IF elmt_notalloc
	    read(i_unit) p_etmp%att%r_vals, p_etmp%def%i_indx, p_etmp%def%p_node, &
	              p_etmp%def%p_edge, p_etmp%att%i_time, p_etmp%att%i_stat, &
	              p_etmp%att%i_mark, p_etmp%att%i_levl, p_etmp%att%i_edge, &
	              p_etmp%lnk%p_prnt, p_etmp%lnk%p_chil, p_etmp%att%i_boun, &
		      p_etmp%att%p_tets
!		      p_etmp%att%i_tets, p_etmp%att%p_tets
	    p_ehash(i_ind)%ep=> p_etmp
	  END DO elmt_read

!---------- read tetras

	  tet_write: DO i_ind=1, p_handle(1)%i_ttotal
	    allocate(p_ttmp, stat=i_alct)
	    tet_notalloc: IF(i_alct /= 0) THEN
	      CALL print_error(100)
	    END IF tet_notalloc
	    read(i_unit) p_ttmp%att%r_vals, p_ttmp%def%i_indx, p_ttmp%def%p_node, &
	              p_ttmp%def%p_edge, p_ttmp%def%p_elem, p_ttmp%att%i_time, &
	              p_ttmp%att%i_stat, p_ttmp%att%i_levl, p_ttmp%att%i_elmt, &
	              p_ttmp%lnk%p_prnt, p_ttmp%lnk%p_chil
	    p_thash(i_ind)%tp=> p_ttmp
	  END DO tet_write

!---------- close file

	  close(i_unit)
	  IF(GRID_parameters%iolog > 0) &
	    write(GRID_parameters%iolog,*) 'INFO: Closed file on unit: ', i_unit

	  RETURN
	  END SUBROUTINE grid_rsaveset

!*****************************************************************
	  SUBROUTINE grid_readdomain(i_arrlen, r_polyar, c_readfile)

!---------- local declarations

	  IMPLICIT NONE
	  INTEGER (KIND = GRID_SI), INTENT(out)               :: i_arrlen
	  REAL (KIND = GRID_SR), DIMENSION(:,:), POINTER      :: r_polyar
	  CHARACTER (len=32), OPTIONAL       :: c_readfile
	  CHARACTER (len=32)                 :: c_filnam
	  CHARACTER (len=80)                 :: c_filrow
	  INTEGER (KIND = GRID_SI)                            :: i_unit, i_fst, i_alct, i_cln, i_cdt, &
	    i_ioend, i_cnt, j_cnt, i_polylines, i_dimen
	  INTEGER (KIND = GRID_SI), DIMENSION(:), ALLOCATABLE :: i_numvert

!---------- set filename

	  fil_present: IF(present(c_readfile)) THEN
	    c_filnam= c_readfile
	  ELSE fil_present
	    write(c_filnam,*) trim(GRID_parameters%program_name), '_domain.dat'
	    c_filnam= adjustl(c_filnam)
	    IF(GRID_parameters%iolog > 0) THEN
	      write(GRID_parameters%iolog,*) 'GRID_api [grid_readdomain]: default file is: ',c_filnam
	    END IF
	  END IF fil_present

!---------- open file

	  i_cln= 0
	  i_cdt= 0

!---------- initialize counters, etc.

	  i_unit= 17
	  open(i_unit, file= c_filnam, action= 'read', form= 'formatted', iostat= i_fst)
	  not_opened: IF(i_fst /= 0) THEN
	    CALL print_error(101)
	  END IF not_opened
	  IF(GRID_parameters%iolog > 0) &
	    write(GRID_parameters%iolog,*) 'GRID_api [grid_readdomain]: Filename: ', c_filnam, ' opened on unit: ', i_unit

!---------- we're using a special data format

	  read_loop: DO
	    read(i_unit,2000,iostat=i_ioend) c_filrow

!---------- if file ended

	    file_end: IF(i_ioend /= 0) THEN
	      close(i_unit)
	      IF(GRID_parameters%iolog > 0) &
	        write(GRID_parameters%iolog,*) 'GRID_api [grid_readdomain]: Closed file on unit: ', i_unit
	      EXIT read_loop
	    ELSE file_end

!---------- decide what to DO with line according to first character

	      comment_line: IF(c_filrow(1:1) == '#' .or. c_filrow(1:1) == '!') THEN
	        CYCLE read_loop

!---------- read the number of dimensions, and check it

	      ELSE IF(c_filrow(1:11) == 'NUMBER_OF_D') THEN comment_line
	        read(i_unit,*) i_dimen
	        dim_check: IF(i_dimen /= DEF_dimension) THEN
	          CALL print_error(102)
	        END IF dim_check

!---------- read the number of polygons, and check it

	      ELSE IF(c_filrow(1:11) == 'NUMBER_OF_P') THEN comment_line
	        read(i_unit,*) i_polylines
	        lin_check: IF(i_polylines <= 0) THEN
	          CALL print_error(103)
	        END IF lin_check

!---------- at the moment, only one line is supported (no islands)

	        isl_check: IF(i_polylines > 1) THEN
	          i_polylines= 1
	          IF(GRID_parameters%iolog > 0) &
	            write(GRID_parameters%iolog,*) 'GRID_api [grid_readinitial]: Number of polylines set to 1'
	          EXIT
	        END IF isl_check

!---------- allocate auxiliary array, if i_polylines is unique

	        uniq_check: IF(ALLOCATED(i_numvert)) THEN
	          CALL print_error(104)
	        ELSE uniq_check
	          ALLOCATE(i_numvert(i_polylines), stat= i_alct)
	          IF(i_alct /= 0) THEN
	            CALL print_error(105)
	          END IF
	        END IF uniq_check
!	        i_arrlen= i_polylines

!---------- read number of vertices for one polygon

	      ELSE IF(c_filrow(1:11) == 'NUMBER_OF_V') THEN comment_line
	        i_cln= i_cln+ 1
	        over_check: IF(i_cln > i_polylines) THEN
	          CALL print_error(106)
	        END IF over_check
	        read(i_unit,*) i_numvert(i_cln)
	        i_arrlen= i_numvert(i_cln)  ! ATTENTION THIS IS BUGGY!!!!!!!

!---------- allocate data array

!	        IF(ASSOCIATED(r_polyar)) DEALLOCATE(r_polyar)
	        ALLOCATE(r_polyar(DEF_dimension,i_numvert(i_cln)), stat=i_alct)
	        IF(i_alct /= 0) THEN
	          CALL print_error(107)
	        END IF

!---------- read data block

	      ELSE IF(c_filrow(1:11) == 'VERTEX_DATA') THEN comment_line
	        i_cdt= i_cdt+ 1
	        ovr_check: IF((i_cdt > i_polylines) .OR. (i_cdt > i_cln)) THEN
	          CALL print_error(108)
	        END IF ovr_check
	        vert_loop: DO i_cnt=1,i_numvert(i_cdt)
	          dimn_loop: DO j_cnt= 1,DEF_dimension
	            read(i_unit,*) r_polyar(j_cnt,i_cnt)
	          END DO dimn_loop
	        END DO vert_loop
	      END IF comment_line
	    END IF file_end
	  END DO read_loop

!---------- deallocate aux array

	  IF(ALLOCATED(i_numvert)) DEALLOCATE(i_numvert)

 2000	  FORMAT(a80)
 
	  RETURN
	  END SUBROUTINE grid_readdomain

	END MODULE FEM_saveset
