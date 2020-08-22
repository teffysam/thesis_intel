!*****************************************************************
!
! MODULE NAME:
!	IO_visnetplot (formerly known as IO_bjuglplot)
! FUNCTION:
!	perform screen rendering by calling BJUGL
! CONTAINS:
!-----------------------------------------------------------------
!
! NAME:
!	visnet_plot (formerly known as bjg_plot)
! FUNCTION:
!	plot scalar- or vector-field, grid, etc. in real time
! SYNTAX:
!	call bjg_plot(int, grid, char, int, ...)
! ON INPUT:
!	i_pflag: flag for the specified plot type   (required)	integer
!	p_handle:grid handle for the linked lists   (required)	type (grid_handle)
!	c_action:make 'plot' to behave accordingly  (optional)	character
!	i_tstep: timestep number                    (optional)	integer
!	i_scal:  (array-) size of the scalar field  (optional)	integer
!	r_scal:  scalar field to plot               (optional)	real
!	i_vect:  (array-) size of the vector field  (optional)	integer
!	r_vec1:  first component of vector-field    (optional)	real
!	r_vec2:  second component of vector-field   (optional)	real
! ON OUTPUT:
!       0-all right; 1-gfx process has exited, quit main prog!
! CALLS:
!
! COMMENTS:
!	this routine changes behaviour, corresponding to the
!	optional arguments given.
!	main steering argument is 'c_action'. it reacts according
!	to the following keywords:
!	  init:  activates initialization of the graphics environment
!	  plot:  (default) activates plotting activity
!	  quit:  deactivate graphics environment
!
!-----------------------------------------------------------------
!
! PUBLIC:
!	plot
! COMMENTS:
!
! USES:
!	MISC_globalparam, MISC_error, VisNET, IO_plotdefine, GRID_api
! LIBRARIES:
!
! REFERENCES:
!	this module is based on the original implementations in the
!	splash/fe project (fortran 77 version), but wildly modified!
! VERSION(S):
!	1. original version			j. behrens	9/96
!	2. grid handles, time added		j. behrens	10/96
!	3. new matlab support			j. behrens	10/96
!	4. matlab_plot changed,
!	   poly_line moved to grid module 	j. behrens	7/97
!	5. changed to use GRID_api		j. behrens	11/97
!	6. adapted to BJuGL			j. behrens	1/2000
!	7. compliant to amatos 1.0		j. behrens	12/2000
!	8. compliant to amatos 1.2		j. behrens	3/2002
!       9. adapted for visnet plotting          f. klaschka     4/2002
!
!*****************************************************************
	MODULE IO_visnetplot
	  USE FLASH_parameters
!	  USE BJUGL
          USE VisNET4Flash
!	  USE IO_plotdefine
	  USE GRID_api
	  PRIVATE
	  PUBLIC :: visnet_plot
	  CONTAINS
!*****************************************************************
	  FUNCTION visnet_plot(p_control, p_handle, c_action) RESULT (i_plotquit)

!---------- local declarations

	  IMPLICIT NONE

	  TYPE (control_struct), INTENT(in)       :: p_control
	  TYPE (grid_handle), INTENT(in)          :: p_handle
	  CHARACTER (len=4), OPTIONAL, INTENT(in) :: c_action
	  INTEGER                                 :: i_plotquit
	  REAL, DIMENSION(:), ALLOCATABLE, SAVE   :: r_xbnd, r_ybnd
	  REAL, DIMENSION(:,:), ALLOCATABLE       :: r_xytmp
	  INTEGER, SAVE                           :: i_tiff    ! screenshot ?
 	  CHARACTER (len=32), SAVE                :: c_wintext  ! window title
 	  CHARACTER (len=1), SAVE                 :: c_polyfile  ! dummy polygon file
	  CHARACTER (len=32), SAVE                :: c_hrdcpyfile  ! file name
	  INTEGER                                 :: i_alct, i_count, I
	  INTEGER, SAVE                           :: i_filecount
	  INTEGER                                 :: i_ml, i_wl, i_ll, i_cnt, i_tl
	  CHARACTER (len=15)                      :: c_prgname
	  CHARACTER (len=28)                      :: c_tmp
	  REAL, DIMENSION(:,:,:), ALLOCATABLE     :: r_aux
	  REAL, DIMENSION(:,:), ALLOCATABLE       :: r_cox
	  INTEGER                                 :: i_efine, i_nnum, i_2d3d
	  REAL                                    :: r_sec= 1./3600.

          INTEGER                                 :: i_gfine

!          REAL, DIMENSION(:,:), ALLOCATABLE, SAVE             :: r_coords_tmp
          REAL, DIMENSION(:,:), ALLOCATABLE, SAVE, TARGET     :: r_coords0, r_coords1
          REAL, DIMENSION(:,:), SAVE, POINTER                 :: r_coords
!          REAL, DIMENSION(:), ALLOCATABLE, SAVE, TARGET       :: r_tracer0, r_tracer1
!          REAL, DIMENSION(:), SAVE, POINTER                   :: r_tracer
!          REAL, DIMENSION(:), ALLOCATABLE, SAVE, TARGET       :: r_ucomp0, r_ucomp1
!          REAL, DIMENSION(:), SAVE, POINTER                   :: r_ucomp
!          REAL, DIMENSION(:), ALLOCATABLE, SAVE, TARGET       :: r_vcomp0, r_vcomp1
!          REAL, DIMENSION(:), SAVE, POINTER                   :: r_vcomp

          REAL, DIMENSION(:), ALLOCATABLE, SAVE               :: r_vals_tmp
          REAL, DIMENSION(:,:), ALLOCATABLE, SAVE, TARGET     :: r_vals0, r_vals1
          REAL, DIMENSION(:,:), SAVE, POINTER                 :: r_vals

          INTEGER, DIMENSION(:,:), ALLOCATABLE, SAVE, TARGET  :: i_edges0, i_edges1
          INTEGER, DIMENSION(:,:), SAVE, POINTER              :: i_edges
          INTEGER, DIMENSION(:,:), ALLOCATABLE, SAVE, TARGET  :: i_elems0, i_elems1
          INTEGER, DIMENSION(:,:), SAVE, POINTER              :: i_elems
	  INTEGER, DIMENSION(4)                               :: i_valind

          REAL, DIMENSION(2,4)                    :: extremes

          INTEGER, SAVE :: switch = 0
          
          INTEGER :: j

          REAL, DIMENSION(3)                                  :: r_n1c, r_n2c, r_vec
          REAL                                                :: len

!---------- initialize return value

	  i_plotquit= 0

          IF(.NOT.p_control%io%l_visnet) RETURN


!---------- determine mode for plotting (into a file)

          IF(p_control%io%l_visnet_tiff) THEN
             i_tiff= 1
          ELSE
             i_tiff= 0
          END IF

!---------- set constant

	  i_efine= p_handle%i_enumfine
	  i_nnum = p_handle%i_nnumber
          i_gfine = p_handle%i_gnumfine

          !WRITE(*,'(A)') '# of nodes:'
          !WRITE(*,'(I5)') i_nnum
          !WRITE(*,'(A)') '# of edges:'
          !WRITE(*,'(I5)') i_gfine
          !WRITE(*,'(A)') '# of elements:'
          !WRITE(*,'(I5)') i_efine

!---------- choose by supplied action

	  action_present: IF(present(c_action))  THEN

!---------- default is plotting, see below...
!---------- do i have to initialize?

	  initialize: IF(c_action=='init') THEN

!---------- set dimension of plot


            i_2d3d = 3

!---------- initialize file numbering

             i_filecount=0

!---------- allocate boundary coordinate array (temporary)

	    i_cnt= i_nnum
	    allocate(r_xytmp(GRID_dimension,i_cnt), stat=i_alct)
	    not_alloc1: IF(i_alct /= 0) THEN
	      CALL grid_error(c_error='[visnet_plot]: Unable to allocate temporary boundary coordinate array!')
	    END IF not_alloc1

!---------- set up array for boundary representation (polygonal line)

	    CALL grid_getpolyline(p_handle, GRID_boundary, i_cnt, i_count, r_xytmp)

            !WRITE(*,'(F15.6,F15.6)') (r_xytmp(:,j),j=1,i_count)

!---------- allocate boundary coordinate array

	    allocate(r_xbnd(i_count), r_ybnd(i_count), stat=i_alct)
	    not_alloc3: IF(i_alct /= 0) THEN
	      CALL grid_error(c_error='[visnet_plot]: Unable to allocate boundary coordinate array!')
	    END IF not_alloc3

!---------- copy boundary coordinate array from temporary array

	    r_xbnd(1:i_count)= r_xytmp(1,1:i_count)
	    r_ybnd(1:i_count)= r_xytmp(2,1:i_count)
	    deallocate(r_xytmp)

!---------- window title text

	    c_prgname= adjustr(GRID_parameters%program_name)
	    WRITE(c_wintext,1000) trim(c_prgname)

!---------- init extremes
            extremes(1,1) = -5
            extremes(2,1) =  5
            extremes(1,2) = -5
            extremes(2,2) =  5
            extremes(1,3) = -5
            extremes(2,3) =  5
            extremes(1,4) = -10
            extremes(2,4) =  10

!---------- initialize graphics

	    i_wl= len_trim(c_wintext)
	    i_ml= 0 !len_trim(p_control%io%c_polygonfile)
	    i_plotquit= VisNET_GraphicsInit( &
                 i_count, &
                 r_xbnd, r_ybnd, &
                 i_wl, c_wintext, &
                 i_ml, c_polyfile, &
                 i_tiff, &
                 i_2d3d, &
                 0) !, extremes)

!---------- check if everything's ok

	    IF(i_plotquit /= 0) THEN
	      CALL grid_error(c_error='[visnet_plot]: Initializing VisNET failed!')
	    END IF
!---------- RETURN
	  END IF initialize

!---------- quit from plotting

	  quitting: IF(c_action=='quit') THEN

!---------- deallocate arrays

	    IF(allocated(r_xbnd)) THEN
	      deallocate(r_xbnd)
	    END IF
	    IF(allocated(r_ybnd)) THEN
	      deallocate(r_ybnd)
	    END IF
            IF(allocated(r_coords0)) THEN
               deallocate(r_coords0)
            END IF
            IF(allocated(r_coords1)) THEN
               deallocate(r_coords1)
            END IF
            IF(allocated(r_vals_tmp)) THEN
               deallocate(r_vals_tmp)
            END IF
            IF(allocated(r_vals0)) THEN
               deallocate(r_vals0)
            END IF
            IF(allocated(r_vals1)) THEN
               deallocate(r_vals1)
            END IF
            IF(allocated(i_edges0)) THEN
               deallocate(i_edges0)
            END IF
            IF(allocated(i_edges1)) THEN
               deallocate(i_edges1)
            END IF
            IF(allocated(i_elems0)) THEN
               deallocate(i_elems0)
            END IF
            IF(allocated(i_elems1)) THEN
               deallocate(i_elems1)
            END IF

	    CALL VisNET_GraphicsQuit

	    RETURN
	  END IF quitting

	  END IF action_present

!---------- plotting

!---------- allocate data arrays
             IF(allocated(r_vals_tmp)) THEN
                deallocate(r_vals_tmp)
             END IF
             allocate(r_vals_tmp(i_nnum), stat=i_alct)
             IF(i_alct /= 0) THEN
                CALL grid_error(c_error='[visnet_plot]: Allocation of temporary node value array failed!')
             END IF
!---------- switched arrays:
          IF(switch.eq.0) THEN
!---------- node coordinates
             IF(allocated(r_coords0)) THEN
                deallocate(r_coords0)
             END IF
             allocate(r_coords0(3, i_nnum), stat=i_alct)
             IF(i_alct /= 0) THEN
                CALL grid_error(c_error='[visnet_plot]: Allocation of vertex array 0 failed!')
             END IF
             r_coords => r_coords0
!---------- node values
             IF(allocated(r_vals0)) THEN
                deallocate(r_vals0)
             END IF
             allocate(r_vals0(5, i_nnum), stat=i_alct)
             IF(i_alct /= 0) THEN
                CALL grid_error(c_error='[visnet_plot]: Allocation of node value array 0 failed!')
             END IF
             r_vals => r_vals0
!---------- edges
             IF(allocated(i_edges0)) THEN
                deallocate(i_edges0)
             END IF
             allocate(i_edges0(GRID_edgenodes, i_gfine), stat=i_alct)
             IF(i_alct /= 0) THEN
                CALL grid_error(c_error='[visnet_plot]: Allocation of edge array 0 failed!')
             END IF
             i_edges => i_edges0
!---------- elements
             IF(allocated(i_elems0)) THEN
                deallocate(i_elems0)
             END IF
             allocate(i_elems0(GRID_elementnodes, i_efine), stat=i_alct)
             IF(i_alct /= 0) THEN
                CALL grid_error(c_error='[visnet_plot]: Allocation of element array 0 failed!')
             END IF
             i_elems => i_elems0

          ELSE ! switch.eq.0

!---------- node coordinates
             IF(allocated(r_coords1)) THEN
                deallocate(r_coords1)
             END IF
             allocate(r_coords1(3, i_nnum), stat=i_alct)
             IF(i_alct /= 0) THEN
                CALL grid_error(c_error='[visnet_plot]: Allocation of vertex array 1 failed!')
             END IF
             r_coords => r_coords1
!---------- node values
             IF(allocated(r_vals1)) THEN
                deallocate(r_vals1)
             END IF
             allocate(r_vals1(5, i_nnum), stat=i_alct)
             IF(i_alct /= 0) THEN
                CALL grid_error(c_error='[visnet_plot]: Allocation of node value array 1 failed!')
             END IF
             r_vals => r_vals1
!---------- edges
             IF(allocated(i_edges1)) THEN
                deallocate(i_edges1)
             END IF
             allocate(i_edges1(GRID_edgenodes, i_gfine), stat=i_alct)
             IF(i_alct /= 0) THEN
                CALL grid_error(c_error='[visnet_plot]: Allocation of edge array 1 failed!')
             END IF
             i_edges => i_edges1
!---------- elements
             IF(allocated(i_elems1)) THEN
                deallocate(i_elems1)
             END IF
             allocate(i_elems1(GRID_elementnodes, i_efine), stat=i_alct)
             IF(i_alct /= 0) THEN
                CALL grid_error(c_error='[visnet_plot]: Allocation of element array 1 failed!')
             END IF
             i_elems => i_elems1

          ENDIF

!-- Amatos 2.0
!---------- get grid info (node coordinates, tracer, edges, elements)
!	  i_valind= (/GRID_tracer, GRID_ucomp, GRID_vcomp, GRID_wcomp/)
!          r_vals(4,:) = (/ (0,I=1,i_nnum) /)
!	  CALL grid_getinfo(p_handle, l_finelevel=.TRUE., &
!                            i_arraypoint = i_valind, &
!                            r_nodecoordinates = r_coords, &
!                            r_nodevalues = r_vals, &
!                            i_edgenodes = i_edges, &
!	                    i_elementnodes= i_elems)

!-- Amatos 1.x
!---------- get grid info (elements)
	  CALL grid_getinfo(p_handle, i_efine, l_finelevel=.TRUE., &
	                    i_elementnodes= i_elems)
!---------- get grid info (edges)
	  CALL grid_getinfo(p_handle, i_gfine, l_finelevel=.TRUE., &
                            i_edgenodes = i_edges)
!---------- get grid info (node coordinates, tracer)
	  CALL grid_getinfo(p_handle, i_nnum, l_finelevel=.TRUE., &
                            r_nodecoordinates = r_coords)
!---------- get grid info (node coordinates, tracer)
	  CALL grid_getinfo(p_handle, i_nnum, l_finelevel=.TRUE., i_valpoint=GRID_tracer, &
                            r_nodevalues = r_vals_tmp)
          r_vals(1,:) = r_vals_tmp
!---------- get ucomp
          CALL grid_getinfo(p_handle, i_nnum, l_finelevel=.TRUE., i_valpoint=GRID_ucomp, &
                            r_nodevalues = r_vals_tmp)
          r_vals(2,:) = r_vals_tmp
!---------- get vcomp
          CALL grid_getinfo(p_handle, i_nnum, l_finelevel=.TRUE., i_valpoint=GRID_vcomp, &
                            r_nodevalues = r_vals_tmp)
          r_vals(3,:) = r_vals_tmp
!---------- get wcomp
          CALL grid_getinfo(p_handle, i_nnum, l_finelevel=.TRUE., i_valpoint=GRID_wcomp, &
                            r_nodevalues = r_vals_tmp)
          r_vals(4,:) = r_vals_tmp

!---------- tiff file name preparation

          DO I=1, i_gfine
             IF(i_edges(1,I) < 1) THEN
                write(*, '(A)') 'node 1 index < 1'
                write(*, '(A,I)') 'node 1 index=', i_edges(1,I)
                write(*, '(A,I,A,I)') 'edge   index=', I, ', i_gfine=', i_gfine
            END IF
             IF(i_edges(1,I) > i_nnum) THEN
                write(*, '(A)') 'node 1 index > i_nnum'
                write(*, '(A,I,A,I)') 'node 1 index=', i_edges(1,I), ', i_nnum=', i_nnum
                write(*, '(A,I,A,I)') 'edge   index=', I, ', i_gfine=', i_gfine
             END IF
             IF(i_edges(2,I) < 1) THEN
                write(*, '(A)') 'node 2 index < 1'
                write(*, '(A,I)') 'node 2 index=', i_edges(2,I)
                write(*, '(A,I,A,I)') 'edge   index=', I, ', i_gfine=', i_gfine
             END IF
             IF(i_edges(2,I) > i_nnum) THEN
                write(*, '(A)') 'node 2 index > i_nnum'
                write(*, '(A,I,A,I)') 'node 2 index=', i_edges(2,I), ', i_nnum=', i_nnum
                write(*, '(A,I,A,I)') 'edge   index=', I, ', i_gfine=', i_gfine
             END IF
!             r_n1c = r_coords(:,i_edges(1,I))
!             r_n2c = r_coords(:,i_edges(2,I))
!             r_vec = r_n2c - r_n1c
!             len = sqrt(r_vec(1)*r_vec(1) + r_vec(2)*r_vec(2) + r_vec(3)*r_vec(3))
!             IF(len > 0.3) THEN
!                write(*, '(A,F)') 'len=', len
!                write(*, '(E,A,E,A,E,A,E,A)') r_n1c(1), ', ', r_n1c(2), ', ', r_n1c(3), ', [', r_vals(1,i_edges(1,I)), ']'
!                write(*, '(E,A,E,A,E,A,E,A)') r_n2c(1), ', ', r_n2c(2), ', ', r_n2c(3), ', [', r_vals(1,i_edges(2,I)), ']'
!             END IF
          END DO


	  write(c_tmp,*) trim(GRID_parameters%program_name), '_hrdcpy_'
	  write(c_hrdcpyfile,1010) trim(c_tmp), i_filecount
	  c_hrdcpyfile= adjustl(c_hrdcpyfile)
	  i_ll= len_trim(c_hrdcpyfile)
          i_filecount= i_filecount + 1


!---------- plot data

	  i_plotquit= VisNET_GraphicsDraw(i_nnum, i_gfine, i_efine, &
                                          r_coords, r_vals, &
                                          i_edges, i_elems, &
                                          i_ll, c_hrdcpyfile)

!---------- check if everything's ok

	  IF(i_plotquit.eq.-1) THEN
             CALL grid_error(c_error='[visnet_plot]: Drawing data with VisNET failed!')
	  END IF

!---------- deallocate temporary arrays

!---------- switch to next arrays
          switch = MODULO(switch+1,2)

	  RETURN

 1000	  FORMAT(a,': Graphics Window')
 1010	  FORMAT(a28,i4.4)

	  END FUNCTION visnet_plot
	  
	END MODULE IO_visnetplot
