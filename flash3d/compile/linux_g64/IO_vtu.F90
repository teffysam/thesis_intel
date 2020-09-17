!-----------------------------------------------------------------------
!> \file IO_vtu.F90
!> \brief Contains the module IO_vtu
!-----------------------------------------------------------------------
!> The MODULE IO_vtu create a vtu file, unstructered grid, readable by
!!                     paraview
!
!> Actually, amatos' structure denies a unified handling for
!! 2d, 3d and spherical case. Therefore the corresponding definition
!! has to be uncommented
!-----------------------------------------------------------------------

!#define VTU_OUTPUT2D
#define VTU_OUTPUT3D

MODULE IO_vtu

USE FEM_param
USE MISC_error
USE GRID_api

PUBLIC :: plot_vtu, t_vtu_data

#ifdef VTU_OUTPUT2D
  INTEGER(KIND=GRID_SI), PARAMETER         :: i_nodespercell = GRID_elementnodes
  INTEGER(KIND=GRID_SI), PARAMETER         :: i_nodesperface = GRID_edgenodes

  ! This defines a triangle in VTK format
  INTEGER(KIND=GRID_SI), PARAMETER         :: i_vtucelltype = 5
#elif defined VTU_OUTPUT3D
  INTEGER(KIND=GRID_SI), PARAMETER         :: i_nodespercell = GRID_tetranodes
  INTEGER(KIND=GRID_SI), PARAMETER         :: i_nodesperface = GRID_elementnodes

  ! This defines a tetrahedron in VTK format
  INTEGER(KIND=GRID_SI), PARAMETER         :: i_vtucelltype = 10
#endif


!> Structure for variable information
TYPE t_vtu_data
  CHARACTER(LEN=32)                              :: c_name  !< VTU variable name
  INTEGER(KIND=GRID_SI)                          :: i_size  !< data dimension
  REAL(KIND=GRID_SR),POINTER, DIMENSION(:,:)     :: p_vdata !< pointer to data
END TYPE t_vtu_data

!> Very small values can cause errors when read in paraview.
!! This is the threshold for filtering.
REAL(KIND = GRID_SR), PARAMETER                  :: r_vtueps = 1e-12
CONTAINS

!---------------------------------------------------------------------
!> \brief Writes a VTU file
!> Use this routine to plot continuous or discontinuous data.
!> \param[in] p_mesh          The mesh
!> \param[in] c_filename      The name of the file to write to
!> \param[in] i_nodedata      Size of the array p_nodedata
!> \param[in] p_nodedata      Array of type t_vtu_data for node
!!                            data. For each node one entry has to exist in
!!                            p_vdata.
!> \param[in] i_celldata      Size of the array p_celldata
!> \param[in] p_nodedata      Array of type t_vtu_data for cell
!!                            data. For each tetrahedron one entry has to
!!                            exist in p_vdata.
#ifdef VTU_OUTPUT2D
!> \param[in] r_zcoordinate   The z-coordinate values
!> \param[in] i_zcoordinate   Use the i_zcoordinate component of p_nodedata
!!                            also as z-coordinate
#endif
!> \param[in] l_continuous    Write continuous node data.
!!                            This needs less space than discontinuous.
!!                            default: .true.
!> \param[in] l_grid_info     Write grid numbering information
!!                            default: .false.
!---------------------------------------------------------------------
SUBROUTINE plot_vtu(p_mesh, c_filename, &
                    i_nodedata, p_nodedata, i_celldata, p_celldata, &
#ifdef VTU_OUTPUT2D
                    r_zcoordinate, i_zcoordinate, &
#endif
                    l_continuous, l_grid_info)
  IMPLICIT NONE

  TYPE (grid_handle), INTENT(in)                 :: p_mesh
  CHARACTER (len=*), INTENT(IN)                  :: c_filename
  INTEGER(KIND=GRID_SI), INTENT(IN), OPTIONAL    :: i_nodedata
  TYPE(t_vtu_data), INTENT(IN), DIMENSION(:), OPTIONAL &
                                                 :: p_nodedata
  INTEGER(KIND=GRID_SI), INTENT(IN), OPTIONAL    :: i_celldata
  TYPE(t_vtu_data), INTENT(IN), DIMENSION(:), OPTIONAL &
                                                 :: p_celldata
#ifdef VTU_OUTPUT2D
  REAL(KIND=GRID_SR), POINTER, DIMENSION(:,:), OPTIONAL, INTENT(IN) &
                                                 :: r_zcoordinate
  INTEGER(KIND=GRID_SI), OPTIONAL, INTENT(IN)    :: i_zcoordinate
#endif

  LOGICAL, INTENT(IN), OPTIONAL                  :: l_grid_info
  LOGICAL, INTENT(IN), OPTIONAL                  :: l_continuous

  INTEGER( KIND = GRID_SI)                       :: i_alct
  INTEGER( KIND = GRID_SI)                       :: i_cnt
  INTEGER( KIND = GRID_SI)                       :: i_fst
  INTEGER( KIND = GRID_SI)                       :: i_ncnt
  INTEGER( KIND = GRID_SI), PARAMETER            :: i_fhandle = 89

  ! this variables result from different naming conventions in two and
  ! three dimensions
  INTEGER( KIND = GRID_SI)                       :: i_numberofcells
  INTEGER( KIND = GRID_SI)                       :: i_numberoffaces
  INTEGER( KIND = GRID_SI)                       :: i_numberofpoints

  INTEGER (KIND = GRID_SI), DIMENSION(:,:), ALLOCATABLE     :: i_cellnodes
  REAL (KIND = GRID_SR), DIMENSION(:,:), ALLOCATABLE        :: r_nodecoor
  INTEGER (KIND = GRID_SI), DIMENSION(:), ALLOCATABLE       :: i_nodes
  INTEGER (KIND = GRID_SI)                                  :: i_size

  LOGICAL                                        :: l_write_grid_info
  LOGICAL                                        :: l_continuous_data


#ifdef VTU_OUTPUT2D
  IF(PRESENT(i_zcoordinate) .AND. PRESENT(r_zcoordinate)) THEN
    CALL GRID_error(c_error='[plot_vtu] z-coordinate is defined twice')
  END IF
#endif

  ! set the value of l_continuous. The default is .true.
  IF(PRESENT(l_continuous)) THEN
    l_continuous_data = l_continuous
  ELSE
    l_continuous_data = .TRUE.
  END IF

  ! set the value of l_grid_info. The default is .false..
  IF(PRESENT(l_grid_info)) THEN
    l_write_grid_info = l_grid_info
  ELSE
    l_write_grid_info = .FALSE.
  END IF

  ! Set dimension depending types
#ifdef  VTU_OUTPUT2D
  i_numberofcells = p_mesh%i_enumfine
  i_numberoffaces = p_mesh%i_gnumfine
#elif defined VTU_OUTPUT3D
  i_numberofcells = p_mesh%i_tnumfine
  i_numberoffaces = p_mesh%i_enumfine
#endif

  IF(l_continuous_data) THEN
    i_numberofpoints = p_mesh%i_nnumber
  ELSE
    i_numberofpoints = i_numberofcells * i_nodespercell
  END IF

  ! We only need the node coordinates and the nodes per tetra
  ! as additional information.
  ALLOCATE(r_nodecoor(GRID_dimension, p_mesh%i_nnumber), &
           i_cellnodes(i_nodespercell, i_numberofcells), &
           stat = i_alct)

  IF(i_alct /= 0) THEN
    CALL grid_error(c_error='[write_vtu]: could not allocate data arrays')
  END IF



#ifdef VTU_OUTPUT2D
                CALL GRID_getinfo(p_mesh,r_nodecoordinates = r_nodecoor,i_elementnodes = i_cellnodes)
#elif defined VTU_OUTPUT3D
                CALL GRID_getinfo(p_mesh,i_numberofpoints, r_nodecoordinates = r_nodecoor)
                CALL GRID_getinfo(p_mesh, i_numberofcells, l_finelevel=.TRUE., l_relative=.TRUE., i_tetranodes= i_cellnodes)
#endif


  ! open the file and write header information
  OPEN(i_fhandle, file = c_filename, iostat= i_fst)
  IF(i_fst /= 0) THEN
    RETURN
  END IF

  WRITE(i_fhandle, "(A)") '<?xml version="1.0"?>'
  WRITE(i_fhandle, "(A)") '<VTKFile type="UnstructuredGrid" version="0.1" byte_order="LittleEndian">'
  WRITE(i_fhandle, *) '<UnstructuredGrid>'
  WRITE(i_fhandle, "(A,I8,A,I8,A)") '<Piece NumberOfPoints="', &
                        i_numberofpoints, &
                        '" NumberOfCells="', &
                        i_numberofcells, &
                        '">'
  
  ! write the node coordinates to VTU file
  WRITE(i_fhandle, *) '<Points>'
  WRITE(i_fhandle, "(A,I1,A)") '<DataArray type="Float32" NumberOfComponents="', &
  3, '" format="ascii">'

  ! Distunguish between continuous and discontinuous case
  IF(l_continuous_data) THEN
    !> The continuous case is represented by amatos' grid topology.
    DO i_cnt = 1, p_mesh%i_nnumber
#ifdef VTU_OUTPUT3D
      WRITE(i_fhandle, *) r_nodecoor(:, i_cnt)
#elif defined  VTU_OUTPUT2D
    IF(PRESENT(r_zcoordinate)) THEN
      WRITE(i_fhandle, *) r_nodecoor(:, i_cnt), r_zcoordinate(1, i_cnt)
    ELSE IF(PRESENT(i_zcoordinate)) THEN
      WRITE(i_fhandle, *) r_nodecoor(:, i_cnt), &
                          p_nodedata(i_zcoordinate)%p_vdata(1, i_cnt)
    ELSE
      WRITE(i_fhandle, *) r_nodecoor(:, i_cnt), 0.0
    END IF
#endif
    END DO

  ELSE
    ! Write the node coordinates. In this discontinuous case one node in
    ! paraview belongs only to one cell.
    DO i_cnt = 1, i_numberofcells
      DO i_ncnt = 1, i_nodespercell
#ifdef VTU_OUTPUT3D
        WRITE(i_fhandle, *) r_nodecoor(:, i_cellnodes(i_ncnt, i_cnt))
#elif defined VTU_OUTPUT2D
        IF(PRESENT(r_zcoordinate)) THEN
          WRITE(i_fhandle, *) r_nodecoor(:, i_cellnodes(i_ncnt, i_cnt)), &
                              r_zcoordinate(1, i_cellnodes(i_ncnt, i_cnt))
        ELSE IF(PRESENT(i_zcoordinate)) THEN
          WRITE(i_fhandle, *) r_nodecoor(:, i_cellnodes(i_ncnt, i_cnt)), &
              p_nodedata(i_zcoordinate)%p_vdata(1, i_cellnodes(i_ncnt, i_cnt))
        ELSE
          WRITE(i_fhandle, *) r_nodecoor(:, i_cellnodes(i_ncnt, i_cnt)), 0.0
        END IF
#endif
      END DO
    END DO
  END IF

  WRITE(i_fhandle, *) '</DataArray>'
  WRITE(i_fhandle, *) '</Points>'

  ! Write the node connectivity
  WRITE(i_fhandle, *) '<Cells>'
  WRITE(i_fhandle, *) '<DataArray type="Int32" Name="connectivity" format="ascii">'

  ! check wether continuous data is written
  IF(l_continuous_data) THEN
    ! The indexing in paraview starts with 0. Therefore we subtract one from
    ! the node connectivity information.
#ifdef VTU_OUTPUT2D
    DO i_cnt = 1, p_mesh%i_enumfine
#elif defined VTU_OUTPUT3D
    DO i_cnt = 1, p_mesh%i_tnumfine
#endif
      WRITE(i_fhandle, *) i_cellnodes(:, i_cnt) - 1
    END DO
  ELSE
    ! Write element nodes (indexing starts by 0!)
    ! In the discontinuous case each tetrahedron has its own nodes
    ALLOCATE(i_nodes(i_nodespercell), stat = i_alct)

    IF(i_alct /= 0) THEN
      CALL grid_error(c_error='[write_vtu]: could not allocate node array for VTU output')
    END IF
 
    DO i_cnt = 1, i_numberofcells * i_nodespercell, i_nodespercell

      DO i_ncnt = 1, i_nodespercell
        i_nodes(i_ncnt) = i_ncnt + i_cnt - 2
      END DO

      WRITE(i_fhandle, *) i_nodes
    END DO

    DEALLOCATE(i_nodes)
  END IF

  WRITE(i_fhandle, *) '</DataArray>'
  
  ! write the cell type. Tetrahedra are represented by 10 and triangles by 5
  WRITE(i_fhandle, *) '<DataArray type="UInt8" Name="types" format="ascii">'
  DO i_cnt = 1, i_numberofcells
    WRITE(i_fhandle, *) i_vtucelltype
  END DO
  WRITE(i_fhandle, *) '</DataArray>'
  WRITE(i_fhandle, *) '<DataArray type="Int32" Name="offsets" format="ascii">'
  DO i_cnt = 1, i_numberofcells
    WRITE(i_fhandle, *) i_cnt * i_nodespercell
  END DO
  WRITE(i_fhandle, *) '</DataArray>'
  WRITE(i_fhandle, *) '</Cells>'
 
  ! Write the point data / node data
  ! If the grid information is requested save the node numbering of amatos.
  ! Since every node number in paraview is decreased by one, this option is
  ! in continuous plotting not too useful.
  WRITE(i_fhandle, *) '<PointData>'

  IF(l_write_grid_info) THEN
    WRITE(i_fhandle, *) '<DataArray type="Int32" Name="Nodenumber" format="ascii">'
    IF(l_continuous) THEN
      DO i_cnt = 1, p_mesh%i_nnumber
        WRITE(i_fhandle, *) i_cnt
      END DO
    ELSE
      ! In this discontinuous case one node in
      ! paraview belongs only to one cell.
      DO i_cnt = 1, i_numberofcells
        DO i_ncnt = 1, i_nodespercell
          WRITE(i_fhandle, *) i_cellnodes(i_ncnt, i_cnt)
        END DO
      END DO
    END IF

    WRITE(i_fhandle, *) '</DataArray>'
  END IF

  ! When variables for the node data are present, save them.
  IF(PRESENT(i_nodedata) .AND. PRESENT(p_nodedata)) THEN
    DO i_cnt = 1, i_nodedata
      CALL write_vtu_data(i_fhandle, p_nodedata(i_cnt))
    END DO
  END IF

  WRITE(i_fhandle, *) '</PointData>'



  ! write the cell data.
  WRITE(i_fhandle, *) '<CellData>'

  ! Write the element numbers when requested.
  IF(l_write_grid_info) THEN
    WRITE(i_fhandle, *) '<DataArray type="Int32" Name="Elementnumber" format="ascii">'
  
    DO i_cnt = 1, i_numberofcells
      WRITE(i_fhandle, *) i_cnt
    END DO

    WRITE(i_fhandle, *) '</DataArray>'
  END IF

  ! Write the variable belonging to the tetradra.
  IF(PRESENT(i_celldata) .AND. PRESENT(p_celldata)) THEN
    DO i_cnt = 1, i_celldata
      CALL write_vtu_data(i_fhandle, p_celldata(i_cnt))
    END DO
  END IF

  ! write the end of cell data and the footer.
  WRITE(i_fhandle, *) '</CellData>'
  WRITE(i_fhandle, *) '</Piece>'
  WRITE(i_fhandle, *) '</UnstructuredGrid>'
  WRITE(i_fhandle, *) '</VTKFile>'

  ! tidy up
  CLOSE(i_fhandle)
  DEALLOCATE(r_nodecoor, i_cellnodes)
END SUBROUTINE plot_vtu


#ifdef VTU_OUTPUT3D
!---------------------------------------------------------------------
!> Use this routine to plot the mesh consisting of face and
!! continuous data on it.
!> \param p_mesh      - the mesh
!> \param c_filename    - the name of the file to write to
!> \param i_nodedata    - (optional) size of the array p_nodedata
!> \param p_nodedata    - (optional) array of type t_vtu_data for node
!!                        data. For each node one entry has to exist in
!!                        p_vdata.
!> \param i_celldata    - (optional) size of the array p_celldata
!> \param p_nodedata    - (optional) array of type t_vtu_data for cell
!!                        data. For each face one entry has to
!!                        exist in p_vdata.
!> \param l_grid_info   - (optional) Write grid numbering information
!!                        default: .false.
!---------------------------------------------------------------------
SUBROUTINE plot_vtu_elements(p_mesh, c_filename, i_nodedata, &
                             p_nodedata, i_celldata, p_celldata, &
                             l_grid_info)
  IMPLICIT NONE

  TYPE (grid_handle), INTENT(in)                 :: p_mesh
  CHARACTER (len=*), INTENT(IN)                  :: c_filename
  INTEGER(KIND=GRID_SI), INTENT(IN), OPTIONAL    :: i_nodedata
  TYPE(t_vtu_data), INTENT(IN), DIMENSION(:), OPTIONAL &
                                                 :: p_nodedata
  INTEGER(KIND=GRID_SI), INTENT(IN), OPTIONAL    :: i_celldata
  TYPE(t_vtu_data), INTENT(IN), DIMENSION(:), OPTIONAL &
                                                 :: p_celldata

  LOGICAL, INTENT(IN), OPTIONAL                  :: l_grid_info

  LOGICAL                                        :: l_write_grid_info

  INTEGER( KIND = GRID_SI)             :: i_alct, i_cnt, i_fst
  INTEGER( KIND = GRID_SI)             :: i_fhandle = 89

  INTEGER (KIND = GRID_SI), DIMENSION(:,:), ALLOCATABLE     :: i_enodes
  REAL (KIND = GRID_SR), DIMENSION(:,:), ALLOCATABLE        :: r_nodecoor
  INTEGER (KIND = GRID_SI)                                  :: i_size

  !> set the optional data
  IF(PRESENT(l_grid_info)) THEN
    l_write_grid_info = l_grid_info
  ELSE
    l_write_grid_info = .FALSE.
  END IF

  ALLOCATE(r_nodecoor(GRID_dimension, p_mesh%i_nnumber), &
           i_enodes(DEF_elnodes, p_mesh%i_enumfine), &
           stat = i_alct)

   IF(i_alct /= 0) THEN
     CALL grid_error(c_error='[write_vtu]: could not allocate data arrays')
   END IF

  i_size = p_mesh%i_nnumber
  CALL GRID_getinfo(p_mesh,i_size, r_nodecoordinates = r_nodecoor, &
                   i_elementnodes = i_enodes)


  OPEN(i_fhandle, file = c_filename, iostat= i_fst)
  IF(i_fst /= 0) THEN
    RETURN
  END IF

  !> Write the VTK header
  WRITE(i_fhandle, "(A)") '<?xml version="1.0"?>'
  WRITE(i_fhandle, "(A)") '<VTKFile type="UnstructuredGrid" version="0.1" byte_order="LittleEndian">'
  WRITE(i_fhandle, *) '<UnstructuredGrid>'
  WRITE(i_fhandle, "(A,I8,A,I8,A)") '<Piece NumberOfPoints="', &
                        p_mesh%i_nnumber, &
                        '" NumberOfCells="', &
                        p_mesh%i_enumfine, &
                        '">'

  !> Save the coordinates of all nodes
  WRITE(i_fhandle, *) '<Points>'
  WRITE(i_fhandle, "(A,I1,A)") '<DataArray type="Float32" NumberOfComponents="', &
 3 , '" format="ascii">'
  DO i_cnt = 1, p_mesh%i_nnumber
    WRITE(i_fhandle, *) r_nodecoor(:, i_cnt)
  END DO
  WRITE(i_fhandle, *) '</DataArray>'
  WRITE(i_fhandle, *) '</Points>'

  !> Write the cell (element) information
  WRITE(i_fhandle, *) '<Cells>'

  !> Save the node connectivity
  WRITE(i_fhandle, *) '<DataArray type="Int32" Name="connectivity" format="ascii">'

   
  !> write element nodes.  Write connectivity (indexing starts by 0!)
  DO i_cnt = 1, p_mesh%i_enumfine
    WRITE(i_fhandle, *) i_enodes(:, i_cnt) - 1
  END DO
  WRITE(i_fhandle, *) '</DataArray>'
  
  !> Write the cell type (triangle = 5, tetrahedron = 10).
  WRITE(i_fhandle, *) '<DataArray type="UInt8" Name="types" format="ascii">'
  DO i_cnt = 1, p_mesh%i_enumfine
    WRITE(i_fhandle, *) '5'
  END DO
  WRITE(i_fhandle, *) '</DataArray>'
  WRITE(i_fhandle, *) '<DataArray type="Int32" Name="offsets" format="ascii">'
  DO i_cnt = 1, p_mesh%i_enumfine
    WRITE(i_fhandle, *) i_cnt * DEF_elnodes
  END DO
  WRITE(i_fhandle, *) '</DataArray>'
  WRITE(i_fhandle, *) '</Cells>'
 
  !> write point data
  WRITE(i_fhandle, *) '<PointData>'

  !> Save the node numbers, when requested.
  IF(l_write_grid_info) THEN
    WRITE(i_fhandle, *) '<DataArray type="Int32" Name="Nodenumber" format="ascii">'
  
    DO i_cnt = 1, p_mesh%i_nnumber
      WRITE(i_fhandle, *) i_cnt
    END DO

    WRITE(i_fhandle, *) '</DataArray>'
  END IF

  !> Write the nodal variables, when present.
  IF(PRESENT(i_nodedata) .AND. PRESENT(p_nodedata)) THEN
    DO i_cnt = 1, i_nodedata
      CALL write_vtu_data(i_fhandle, p_nodedata(i_cnt))
    END DO
  END IF

  WRITE(i_fhandle, *) '</PointData>'



  !> write cell data
  WRITE(i_fhandle, *) '<CellData>'

  !> save the element numbers, when requested.
  IF(l_write_grid_info) THEN
    WRITE(i_fhandle, *) '<DataArray type="Int32" Name="Elementnumber" format="ascii">'
  
    DO i_cnt = 1, p_mesh%i_enumfine
      WRITE(i_fhandle, *) i_cnt
    END DO

    WRITE(i_fhandle, *) '</DataArray>'
  END IF

  !> Write the cell variables (when present)
  IF(PRESENT(i_celldata) .AND. PRESENT(p_celldata)) THEN
    DO i_cnt = 1, i_celldata
      CALL write_vtu_data(i_fhandle, p_celldata(i_cnt))
    END DO
  END IF

  !> write the footer.
  WRITE(i_fhandle, *) '</CellData>'
  WRITE(i_fhandle, *) '</Piece>'
  WRITE(i_fhandle, *) '</UnstructuredGrid>'
  WRITE(i_fhandle, *) '</VTKFile>'

  !> finally close the file and deallocate the data
  CLOSE(i_fhandle)
  DEALLOCATE(r_nodecoor, i_enodes)
END SUBROUTINE plot_vtu_elements
#endif !----3D output only

!---------------------------------------------------------------------
!> write_vtu_data writes a single variable (node or cell) to the
!! VTK file.
!> \param i_fhandle - the file handle
!> \param p_data    - the variable
!---------------------------------------------------------------------
SUBROUTINE write_vtu_data(i_fhandle, p_data)
  IMPLICIT NONE

  INTEGER(KIND=GRID_SI), INTENT(IN)             :: i_fhandle
  TYPE(t_vtu_data)                              :: p_data
  INTEGER(KIND=GRID_SI)                         :: i_size
  INTEGER(KIND=GRID_SI)                         :: i_cnt
  INTEGER(KIND=GRID_SI)                         :: i_cnt2

  !> Special treatment for vector valued data
  IF(p_data%i_size > 1) THEN
    WRITE(i_fhandle, "(A,A,A,I1, A)") &
                '<DataArray type="Float32" Name="', &
                TRIM(p_data%c_name), '" NumberOfComponents="', &
                p_data%i_size, '" format="ascii">'
  ELSE
    WRITE(i_fhandle, "(A,A,A)") '<DataArray type="Float32" Name="', &
                TRIM(p_data%c_name), '" format="ascii">'
  END IF
  i_size = SIZE(p_data%p_vdata, 2)
  
  !> Write the data to file with respect to the threshold value
  DO i_cnt = 1, i_size
    DO i_cnt2 = 1, p_data%i_size
      IF(ABS(p_data%p_vdata(i_cnt2, i_cnt)) < r_vtueps) &
          p_data%p_vdata(i_cnt2, i_cnt) = 0
    END DO

    WRITE(i_fhandle, "(10e15.7)"), p_data%p_vdata(1:p_data%i_size, i_cnt)
  END DO
  WRITE(i_fhandle, *) '</DataArray>'
  
END SUBROUTINE write_vtu_data

END MODULE IO_vtu
