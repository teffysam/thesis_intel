!*****************************************************************
!
! MODULE NAME:
!        MISC_datafun
! FUNCTION:
!        provide some dummy functions for testing amatos
! CONTAINS:
!-----------------------------------------------------------------
!
! VERSION(S):
!        1. original version                    j. behrens        3/2001
!       2. adapted some routine to 3d       o. kunst    1/2010
!
!*****************************************************************
        MODULE MISC_datafun
        USE MAIN_parameters
        USE GRID_api
        PRIVATE
        PUBLIC :: veclen, calc_femsol, calc_femrhs
        CONTAINS
!*****************************************************************
        FUNCTION veclen(r_vec) RESULT(r_rst)

!---------- local declarations

        IMPLICIT NONE
        REAL, DIMENSION(GRID_dimension) :: r_vec
        REAL                                :: r_rst, r_tmp

!---------- calculate vector dotproduct

        r_tmp= dot_product(r_vec, r_vec)
        r_rst= sqrt(r_tmp)

        END FUNCTION veclen

!*****************************************************************
        FUNCTION calc_femrhs(r_coord) RESULT (r_val)

!---------- local declarations

        IMPLICIT NONE

        REAL (KIND = GRID_SR), DIMENSION(GRID_dimension), INTENT(in) :: r_coord
        REAL (KIND = GRID_SR)                                             :: r_val
        REAL (KIND = GRID_SR)                                             :: r_x, r_y, r_z



!---------- f=-2*(yz(1-y)(1-z)+xz(1-x)(1-z)+xy(1-y)(1-z)) !!! ONLY 3D !!!

        r_x= r_coord(1); r_y= r_coord(2); r_z = r_coord(3)
        r_val= 2.0_GRID_SR*(r_y*r_z*(1-r_y)*(1-r_z)+ &
                            r_x*r_z*(1-r_x)*(1-r_z)+ &
                            r_x*r_y*(1-r_x)*(1-r_y))

        RETURN
        END FUNCTION calc_femrhs


!*****************************************************************
        FUNCTION calc_femsol(r_coord) RESULT (r_val)

!---------- local declarations

        IMPLICIT NONE

        REAL (KIND = GRID_SR), DIMENSION(GRID_dimension), INTENT(in) :: r_coord
        REAL (KIND = GRID_SR)                                        :: r_val
        REAL (KIND = GRID_SR)                                        :: r_x, r_y, r_z

!---------- no solution available

!         r_val= 0.0_GRID_SR

!---------- Solution corresponding to rhs:
!---------- f=-2*(yz(1-y)(1-z)+xz(1-x)(1-z)+xy(1-y)(1-z))  !! 3D ONLY
        r_x= r_coord(1); r_y= r_coord(2); r_z= r_coord(3)
        r_val= r_x* r_y* r_z* &
               (1.0_GRID_SR- r_x)* (1.0_GRID_SR- r_y)*(1.0_GRID_SR- r_z)

        RETURN
        END FUNCTION calc_femsol

        END MODULE MISC_datafun
