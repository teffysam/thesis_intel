!*****************************************************************
!
! MODULE NAME:
!	FEM_basis
! FUNCTION:
!	contains definitions for basisfunctions on elements
! CONTAINS:
!-----------------------------------------------------------------
!
! NAME:
!	grid_linearbasis
! FUNCTION:
!	evaluate linear basis function at a coordinate and return coefficients
! SYNTAX:
!	CALL grid_linearbasis(elmt, real.arr, int, real.arr)
! ON INPUT:
!	p_elem:  element data struct			TYPE(elmt)
!	r_coord: coordinate of evaluation point		REAL (KIND = GRID_SR)
! ON OUTPUT:
!	i_order: order of element (no. of coeff's)	INTEGER (KIND = GRID_SI)
!	r_basis: coefficients at nodes			REAL (KIND = GRID_SR)
! CALLS:
!
! COMMENTS:
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
!
! USES:
!
! LIBRARIES:
!
! REFERENCES:
!
! VERSION(S):
!	1. original version	j. behrens	4/98
!
!*****************************************************************
	MODULE FEM_basis
	  USE MISC_globalparam
	  USE MISC_error
	  USE FEM_define
	PRIVATE
	PUBLIC :: grid_linearbasis
        PUBLIC :: grid3D_linearbasis
	  CONTAINS
!*****************************************************************
	SUBROUTINE grid_linearbasis(p_elmt, r_coord, i_order, r_basis)

!---------- local declarations

	IMPLICIT NONE
	TYPE (elmt), POINTER                        :: p_elmt
	REAL (KIND = GRID_SR), DIMENSION(DEF_dimension)              :: r_coord
	INTEGER (KIND = GRID_SI), INTENT(out)                        :: i_order
	REAL (KIND = GRID_SR), DIMENSION(:), POINTER                 :: r_basis
	TYPE (node), POINTER                        :: p_ntmp
	REAL (KIND = GRID_SR), DIMENSION(DEF_dimension, DEF_elnodes) :: r_nodes
	REAL (KIND = GRID_SR)                                        :: r_fac
	INTEGER (KIND = GRID_SI)                                     :: i_alct, i_cnt, i_1, i_2, i_3

!---------- set order (for linear elements it is 3)

	i_order= 3

!---------- allocate array if necessary

	coef_check: IF(.NOT. associated(r_basis)) THEN
	  allocate(r_basis(i_order), stat=i_alct)
	  IF(i_alct /= 0) THEN
	    CALL print_error(199)
	  END IF
	ELSE coef_check
	  IF(size(r_basis) < i_order) THEN
	    CALL print_error(199)
	  END IF
	END IF coef_check

!---------- find coordinates of vertices

	DO i_cnt=1,DEF_elnodes
	  p_ntmp=> p_nhash(p_elmt%def%p_node(i_cnt))%np
	  r_nodes(:,i_cnt)= p_ntmp%def%r_coor
	END DO

!---------- basis function (ONLY 2D!)

	DO i_cnt=1,DEF_elnodes
	  i_1= i_cnt
	  i_2= mod(i_cnt,DEF_elnodes)+ 1
	  i_3= mod(i_cnt+1,DEF_elnodes)+ 1
	  r_fac= 1./((r_nodes(1,i_1)- r_nodes(1,i_2))* (r_nodes(2,i_3)- r_nodes(2,i_2))- &
	             (r_nodes(1,i_3)- r_nodes(1,i_2))* (r_nodes(2,i_1)- r_nodes(2,i_2)))
	  r_basis(i_1)= r_fac* &
	            ((r_coord(1)- r_nodes(1,i_2))* (r_nodes(2,i_3)- r_nodes(2,i_2))- &
	             (r_nodes(1,i_3)- r_nodes(1,i_2))* (r_coord(2)- r_nodes(2,i_2)))
	END DO

	RETURN
	END SUBROUTINE grid_linearbasis



 
SUBROUTINE grid3D_linearbasis(p_tetra, r_coord, i_order, r_basis)

!---------- local declarations

	IMPLICIT NONE
	TYPE (tetra), POINTER                        :: p_tetra
	REAL (KIND = GRID_SR), DIMENSION(DEF_dimension)              :: r_coord
	INTEGER (KIND = GRID_SI), INTENT(out)                        :: i_order
	REAL (KIND = GRID_SR), DIMENSION(:), POINTER                 :: r_basis
	TYPE (node), POINTER                        :: p_ntmp
	REAL (KIND = GRID_SR), DIMENSION(DEF_dimension, DEF_tetnodes) :: r_nodes
	REAL (KIND = GRID_SR)                                        :: r_fac
	INTEGER (KIND = GRID_SI)                                     :: i_alct, i_cnt, i_1, i_2, i_3, i_4

!---------- set order (for linear elements it is 4)

	i_order= 4

!---------- allocate array if necessary

	coef_check: IF(.NOT. associated(r_basis)) THEN
	  allocate(r_basis(i_order), stat=i_alct)
	  IF(i_alct /= 0) THEN
	    CALL print_error(199)
	  END IF
	ELSE coef_check
	  IF(size(r_basis) < i_order) THEN
	    CALL print_error(199)
	  END IF
	END IF coef_check

!---------- find coordinates of vertices

	DO i_cnt=1,DEF_tetnodes
	  p_ntmp=> p_nhash(p_tetra%def%p_node(i_cnt))%np
	  r_nodes(:,i_cnt)= p_ntmp%def%r_coor
	END DO

!---------- basis function 

	DO i_cnt = 1,DEF_tetnodes
           i_1 = i_cnt
           i_2 = mod(i_cnt,DEF_tetnodes)+1
           i_3 = mod(i_cnt+1,DEF_tetnodes)+1
           i_4 = mod(i_cnt+2,DEF_tetnodes)+1
 
        r_fac = 1./ (  (r_nodes(1,i_1)-r_nodes(1,i_2))* (r_nodes(2,i_3)-r_nodes(2,i_2)) * (r_nodes(3,i_4)-r_nodes(3,i_2) ) + &
 (r_nodes(2,i_1)-r_nodes(2,i_2))*(r_nodes(3,i_3)-r_nodes(3,i_2))*(r_nodes(1,i_4)-r_nodes(1,i_2))  + &
  (r_nodes(3,i_1)-r_nodes(3,i_2))*(r_nodes(1,i_3)-r_nodes(1,i_2))*(r_nodes(2,i_4)-r_nodes(2,i_2))  - &
 (r_nodes(1,i_4)-r_nodes(1,i_2))*(r_nodes(2,i_3)-r_nodes(2,i_2))*(r_nodes(3,i_1)-r_nodes(3,i_2))   - &
 (r_nodes(2,i_4)-r_nodes(2,i_2))*(r_nodes(3,i_3)-r_nodes(3,i_2))*(r_nodes(1,i_1)-r_nodes(1,i_2))   - &
 (r_nodes(2,i_1)-r_nodes(2,i_2))*(r_nodes(1,i_3)-r_nodes(1,i_2))*(r_nodes(3,i_4)-r_nodes(3,i_2))   )

      r_basis(i_1) = r_fac *  &
(  (r_coord(1)-r_nodes(1,i_2))* (r_nodes(2,i_3)-r_nodes(2,i_2)) * (r_nodes(3,i_4)-r_nodes(3,i_2) ) + &
 (r_coord(2)-r_nodes(2,i_2))*(r_nodes(3,i_3)-r_nodes(3,i_2))*(r_nodes(1,i_4)-r_nodes(1,i_2))  + &
  (r_coord(3)-r_nodes(3,i_2))*(r_nodes(1,i_3)-r_nodes(1,i_2))*(r_nodes(2,i_4)-r_nodes(2,i_2))  - &
 (r_nodes(1,i_4)-r_nodes(1,i_2))*(r_nodes(2,i_3)-r_nodes(2,i_2))*(r_coord(3)-r_nodes(3,i_2))   - &
 (r_nodes(2,i_4)-r_nodes(2,i_2))*(r_nodes(3,i_3)-r_nodes(3,i_2))*(r_coord(1)-r_nodes(1,i_2))   - &
 (r_coord(2)-r_nodes(2,i_2))*(r_nodes(1,i_3)-r_nodes(1,i_2))*(r_nodes(3,i_4)-r_nodes(3,i_2))   )

       END DO

	RETURN
	END SUBROUTINE grid3D_linearbasis

!*****************************************************************
!	SUBROUTINE 
!
!---------- local declarations
!
!	IMPLICIT NONE
!
!	RETURN
!	END SUBROUTINE 

	END MODULE FEM_basis
