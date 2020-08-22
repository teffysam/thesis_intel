!*****************************************************************
!
! MODULE NAME:
!        MISC_testfem
! FUNCTION:
!        solve a Poisson problem for test reasons
! CONTAINS:
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
!        MAIN_parameters, GRID_api
! LIBRARIES:
!
! REFERENCES:
!
! VERSION(S):
!        1. original version                                j. behrens        6/2003
!        2. using Frank Giraldo's generic FEM support
!           and Andre Riedmueller's boundary values        j. behrens        7/2005
!        3. adaption to 3d                                  o. kunst         1/2010
!
!*****************************************************************
! preprocessor directives

        MODULE MISC_poisson
        USE MAIN_parameters
        USE MISC_datafun
        USE GRID_api
        PRIVATE
        PUBLIC :: poisson_test
        INTEGER (KIND = GRID_SI), PARAMETER :: i_bandwidth= 100_GRID_SI !384_GRID_SI
        REAL(KIND=GRID_SR), DIMENSION(:,:), ALLOCATABLE, TARGET &
        & :: r_def_psiksi, r_def_psieta, r_def_psizeta, r_def_qweig, r_def_coeff
        CONTAINS

!*****************************************************************
! simple roiutine to get boundary dofs. has to be replaced in
! comming versions
        SUBROUTINE boundary_nodes(i_dofnum, r_xyz, i_dofboundary)
              IMPLICIT NONE 

              INTEGER(KIND=GRID_SI), INTENT(IN)                :: i_dofnum
              REAL(KIND=GRID_SR), DIMENSION(GRID_dimension,i_dofnum), &
                                             INTENT(IN)   :: r_xyz

              INTEGER(KIND=GRID_SI), DIMENSION(i_dofnum), INTENT(OUT) &
                                                           :: i_dofboundary


              INTEGER(KIND=GRID_SI)                        :: i_cnt
              INTEGER(KIND=GRID_SI)                        :: i_coor

              DO i_cnt = 1, i_dofnum
                   i_dofboundary(i_cnt) = 0;
                   DO i_coor = 1, GRID_dimension
                       IF((r_xyz(i_coor, i_cnt) .EQ. 1.0_GRID_SR) .OR. &
                             (r_xyz(i_coor, i_cnt) .EQ. 0.0_GRID_SR)) THEN
                                i_dofboundary(i_cnt) = -1_GRID_SI
                       END IF
                   END DO
              END DO
        END SUBROUTINE

!*****************************************************************
        SUBROUTINE poisson_test(p_ghand,i_femtype)

!---------- local declarations

        IMPLICIT NONE

        TYPE (grid_handle), INTENT(in)                        :: p_ghand
        INTEGER (KIND = GRID_SI)                              :: i_femtype
        
        REAL (KIND = GRID_SR), DIMENSION(:,:), ALLOCATABLE    :: r_xyz
        INTEGER (KIND = GRID_SI), DIMENSION(:,:), ALLOCATABLE :: i_tdofs, i_tnods  ! element dof's
        INTEGER (KIND = GRID_SI), DIMENSION(:), ALLOCATABLE   :: i_nboun, i_dofboun
        INTEGER (KIND = GRID_SI)                              :: i_unkn, i_tnum, &
          i_tim, i_alct, i_nnum, i, i_cnt, i_doftetra, i_qpnt
        REAL (KIND = GRID_SR), DIMENSION(:,:), ALLOCATABLE    :: r_matrix
        INTEGER (KIND = GRID_SI), DIMENSION(:,:), ALLOCATABLE :: i_matind
        INTEGER (KIND = GRID_SI), DIMENSION(:,:,:), ALLOCATABLE :: i_allpiv
        REAL (KIND = GRID_SR), DIMENSION(:), ALLOCATABLE      :: r_vec
        REAL (KIND = GRID_SR), DIMENSION(:), ALLOCATABLE      :: r_sol
        REAL (KIND = GRID_SR), DIMENSION(:,:), ALLOCATABLE    :: r_do
        REAL (KIND = GRID_SR)                                 :: r_cgtol, r_l2
        REAL (KIND = GRID_SR), DIMENSION(:,:), ALLOCATABLE    :: r_jac
        REAL (KIND=GRID_SR), DIMENSION(:,:,:,:), ALLOCATABLE  :: r_jmat

        LOGICAL                                           :: l_finelevel=.TRUE.
!---------- for MATLAB output of matrix structure:
         REAL (KIND = GRID_SR), DIMENSION(:,:), ALLOCATABLE    :: r_MLmat
         INTEGER (KIND = GRID_SI)                              :: ii

        r_cgtol= GRID_EPS

!---------- get element coordinates, element info, and boundary info
!-- hard coded constants
        i_unkn= p_ghand%i_nnumber
        i_tnum= p_ghand%i_tnumfine
        i_tim=  p_ghand%i_timetag
        i_doftetra= 4_GRID_SI
        i_qpnt= 4_GRID_SI
	
        allocate(r_xyz(GRID_dimension,p_ghand%i_nnumber), &
           & i_tdofs(DEF_tetnodes,i_tnum), &
           & r_jmat(GRID_dimension, GRID_dimension, i_doftetra, i_tnum), &
           & r_jac(i_qpnt, i_tnum), &
           & i_allpiv(GRID_dimension, i_doftetra, i_tnum), &
           & stat=i_alct)

        IF(i_alct /= 0) CALL grid_error(c_error= & 
           '[poisson_test]: could not allocate coordinate arrays')

        allocate(i_dofboun(i_unkn), stat=i_alct)
        IF(i_alct /= 0) CALL grid_error(c_error= &
           '[poisson_test]: could not allocate boundary array for dofs')

        CALL grid_getinfo(p_ghand, p_ghand%i_nnumber, &
                          r_nodecoordinates= r_xyz)

        CALL grid_getinfo(p_ghand, p_ghand%i_tnumfine, &
                          i_tetranodes=i_tdofs)


        CALL  boundary_nodes(p_ghand%i_nnumber, r_xyz, i_dofboun)

!        CALL grid_getinfo(p_ghand, i_femtype=i_femtype, r_dofcoordinates= r_xy, &
!                          i_elementdofs=i_edofs, i_dofboundary= i_dofboun)

!- hard coded matrices, should be replaced by signature file
        ALLOCATE(r_def_psiksi(4,4), r_def_psieta(4,4), r_def_psizeta(4,4), &
         & r_def_qweig(4,1), r_def_coeff(4,4))

!---------- allocate metric term arrays

        IF(i_alct /= 0) CALL grid_error(c_error='[asmbl_stiff]: could not allocate metric arrays')

!        r_def_psiksi = RESHAPE((/-1, -1, -1, -1, & 
!                                  1,  1,  1,  1, &
!                                  0,  0,  0,  0, &
!                                  0,  0,  0,  0/), (/4,4/))
!
!        r_def_psieta = RESHAPE((/-1, -1, -1, -1, & 
!                                   0,  0,  0,  0, &
!                                   1,  1,  1,  1, &
!                                   0,  0,  0,  0/), (/4,4/))
!
!        r_def_psizeta = RESHAPE((/-1, -1, -1, -1, & 
!                                  0,  0,  0,  0, &
!                                  0,  0,  0,  0, &
!                                  1,  1,  1,  1/), (/4,4/))

        r_def_psiksi = RESHAPE((/-1, 1, 0, 0, & 
                                 -1, 1, 0, 0, &
                                 -1, 1, 0, 0, &
                                 -1, 1, 0, 0/), (/4,4/))

        r_def_psieta = RESHAPE((/-1, 0, 1, 0, & 
                                 -1, 0, 1, 0, &
                                 -1, 0, 1, 0, &
                                 -1, 0, 1, 0/), (/4,4/))

        r_def_psizeta = RESHAPE((/-1, 0, 0, 1, & 
                                  -1, 0, 0, 1, &
                                  -1, 0, 0, 1, &
                                  -1, 0, 0, 1/), (/4,4/))

        r_def_coeff   = RESHAPE((/1, 0, 0, 0, & 
                                  0, 1, 0, 0, &
                                  0, 0, 1, 0, &
                                  0, 0, 0, 1/), (/4,4/))

        r_def_qweig(1, 1) = 5 ! 1./6. !.25    
        r_def_qweig(2, 1) = r_def_qweig(1, 1)
        r_def_qweig(3, 1) = r_def_qweig(1, 1)
        r_def_qweig(4, 1) = r_def_qweig(1, 1)


!---------- compute metric terms

        CALL asmbl_metrics(p_ghand, i_femtype, i_unkn, r_xyz, i_tnum, i_tdofs, &
                           i_allpiv, r_jmat , r_jac)

!---------- allocate global matrix

        allocate(r_matrix(i_bandwidth,i_unkn), i_matind(i_bandwidth,i_unkn), stat=i_alct)
!        allocate(r_matrix(i_unkn,i_unkn), i_matind(i_bandwidth,i_unkn), stat=i_alct)
        IF(i_alct /= 0) THEN
          CALL grid_error(c_error='[poisson_test]: could not allocate global matrix')
        END IF

!---------- building the Stiffnes Matrix

!        CALL asmbl_full(p_ghand, i_femtype, i_unkn, r_xyz, i_dofboun, i_tnum, &
!                         i_tdofs, i_allpiv, r_jmat, r_jac, &
!                         r_matrix)

       CALL asmbl_stiff(p_ghand, i_femtype, i_unkn, r_xyz, i_dofboun, i_tnum, &
                         i_tdofs, i_allpiv, r_jmat, r_jac, &
                         i_bandwidth, r_matrix, i_matind)

!---------- allocate right hand side vector

        allocate(r_vec(i_unkn), stat=i_alct)
        IF(i_alct /= 0) THEN
          CALL grid_error(c_error='[poisson_test]: could not allocate rhs vector')
        END IF

!---------- assemble right hand side

        CALL asmbl_rhs(p_ghand, i_femtype, i_unkn, r_xyz, i_dofboun, i_tnum, &
                       i_tdofs, r_jac, r_vec)

!---------- allocate solution vector

        allocate(r_sol(i_unkn), stat=i_alct)
        IF(i_alct /= 0) THEN
          CALL grid_error(c_error='[poisson_test]: could not allocate solution vector')
        END IF
        r_sol= 0.0_GRID_SR

!---------- for MATLAB output of matrix structure:

!         ALLOCATE(r_MLmat(0:i_unkn, i_unkn), stat=i_alct)
!         r_MLmat= 0.0
!         DO i_alct=1,i_unkn
!          DO ii = 1, i_bandwidth
!           r_MLmat(i_matind(ii, i_alct), i_alct) = r_matrix(ii, i_alct)
!          END DO
!           r_MLmat(i_matind(1:i_bandwidth,i_alct),i_alct)= r_matrix(1:i_bandwidth,i_alct)
!         END DO
!         open(33,file='MLMatrix.dat',form='formatted')
!---     write header (needed for octave)
!         WRITE(33,*) '# Created by amatos3d'
!         WRITE(33,*) '# name: ML'
!         WRITE(33,*) '# type: matrix'
!         WRITE(33,*) '# rows:', i_unkn
!         WRITE(33,*) '# columns:', i_unkn
!         DO i_alct=1,i_unkn
!!!------           write(33,*) r_matrix(i_alct, :)
!          write(33,*) (r_MLmat(ii,i_alct), ii=1,i_unkn)
!!----	  WRITE(*,*) i_alct, i_dofboun(i_alct)
!         END DO
!         close(33)


!---------- solve system of equations

        CALL cg_solve(i_unkn, i_bandwidth, r_matrix, i_matind, r_vec, r_CGtol, r_sol)

!---------- store solution for later scatter step

        r_matrix(1,:)= r_sol(:)

!---------- calculate exact solution or error

        DO i_cnt=1,i_unkn
          r_matrix(2,i_cnt)= (r_sol(i_cnt)- calc_femsol(r_xyz(:,i_cnt)))**2
        END DO

!---------- put new values to grid, we re-use r_matrix here, since we need a 2D-array...

        CALL grid_putinfo(p_ghand, i_unkn, i_valpoint = DEF_zeta, r_nodevalues=r_matrix(1,:))
        CALL grid_putinfo(p_ghand, i_unkn, i_valpoint = DEF_phi, r_nodevalues=r_matrix(2,:))

        !DEALLOCATE(r_xyz, r_matrix, r_vec, r_sol, i_matind)
        DEALLOCATE(r_xyz)
        DEALLOCATE(r_matrix)
        DEALLOCATE(r_vec)
        DEALLOCATE(r_sol)
        DEALLOCATE(i_matind)
        DEALLOCATE(i_tdofs, i_dofboun)
        DEALLOCATE(r_jac, r_jmat)
        DEALLOCATE(r_def_psiksi, r_def_psieta, r_def_psizeta, r_def_qweig)

!---------- calculate l2-norm

!        r_l2= grid_globalintegral(p_ghand,AMAT_hi2)
        WRITE(*,*) '------ POISSON: L2-Norm: ', r_l2

        RETURN
        END SUBROUTINE poisson_test

!*****************************************************************
        SUBROUTINE sparse_matmul(i_len, i_bndw, r_matrix, i_matind, r_rhs, r_result)

!---------- local declarations

        IMPLICIT NONE
        INTEGER (KIND = GRID_SI), INTENT(in)                          :: i_len
        INTEGER (KIND = GRID_SI), INTENT(in)                          :: i_bndw
        REAL (KIND = GRID_SR), DIMENSION(i_bndw,i_len), INTENT(in)    :: r_matrix
        INTEGER (KIND = GRID_SI), DIMENSION(i_bndw,i_len), INTENT(in) :: i_matind
        REAL (KIND = GRID_SR), DIMENSION(i_len), INTENT(in)           :: r_rhs
        REAL (KIND = GRID_SR), DIMENSION(i_len), INTENT(out)          :: r_result
        INTEGER (KIND = GRID_SI)                                      :: i_cnt, j_cnt, j_len

        DO i_cnt= 1,i_len
          r_result(i_cnt)= 0.0_GRID_SR
          len_loop: DO j_cnt=1,i_bndw
            IF(i_matind(j_cnt,i_cnt) == 0) THEN
              j_len= j_cnt-1
              exit len_loop
            END IF
          END DO len_loop
          r_result(i_cnt)= dot_product(r_matrix(1:j_len,i_cnt), r_rhs(i_matind(1:j_len,i_cnt)))
        END DO

        RETURN
        END SUBROUTINE sparse_matmul

!*****************************************************************
        SUBROUTINE cg_solve(i_len, i_bndw, r_matrix, i_matind, r_rhs, r_tol, r_result)

!---------- local declarations

        IMPLICIT NONE
        INTEGER (KIND = GRID_SI), INTENT(in)                          :: i_len
        INTEGER (KIND = GRID_SI), INTENT(in)                          :: i_bndw
        REAL (KIND = GRID_SR), DIMENSION(i_bndw,i_len), INTENT(in)    :: r_matrix
        INTEGER (KIND = GRID_SI), DIMENSION(i_bndw,i_len), INTENT(in) :: i_matind
        REAL (KIND = GRID_SR), DIMENSION(i_len), INTENT(in)           :: r_rhs
        REAL (KIND = GRID_SR), INTENT(in)                             :: r_tol
        REAL (KIND = GRID_SR), DIMENSION(i_len), INTENT(inout)        :: r_result
        INTEGER (KIND = GRID_SI)                                      :: i_cnt, j_cnt, j_len
        REAL (KIND = GRID_SR), DIMENSION(i_len)                       :: r_h, r_r, r_xp ! implicit allocation
        INTEGER (KIND = GRID_SI)                                      :: i_maxit, i_itr, i_ntimes
        REAL (KIND = GRID_SR)                                         :: r_s1, r_s2, r_s3, r_tr, r_ak, r_tto, r_bk
        EXTERNAL :: saxpy

        CALL sparse_matmul(i_len, i_bndw, r_matrix, i_matind, r_result, r_h)

        DO i_cnt=1,i_len
          r_r(i_cnt)=r_rhs(i_cnt)-r_h(i_cnt)
          r_xp(i_cnt)=r_r(i_cnt)
        END DO

        i_maxit= 2 * i_len
        iter_loop: DO i_itr=1,i_maxit
          CALL sparse_matmul(i_len, i_bndw, r_matrix, i_matind, r_xp, r_h)

          r_s1= dot_product(r_r,r_r)
          r_s2= dot_product(r_xp,r_h)
          r_tr= r_s1

          IF(r_tr.eq.0._GRID_SR) THEN
             WRITE(*,*) '------- CG - residual becomes self-orthogonal...'
             WRITE(*,*) '------- CG - tol/ residuum: ',r_tol,' / ', r_tto
             WRITE(*,*) '------- CG - Iterations: ',i_itr
            RETURN
          END IF
          r_ak=r_tr/r_s2
          CALL saxpy(i_len,r_ak,r_xp,1,r_result,1)
          CALL saxpy(i_len,(-r_ak),r_h,1,r_r,1)

          r_tto= maxval(abs(r_r))

          r_s3= dot_product(r_r,r_r)
          IF(r_tto < r_tol)THEN
             WRITE(*,*) '------- CG - tol/ residuum: ',r_tol,' / ', r_tto
             WRITE(*,*) '------- CG - Iterations: ',i_itr
             RETURN
          END IF


          r_bk= r_s3/r_tr
          r_xp(:)= r_r(:)+ r_bk* r_xp(:)
        END DO iter_loop

        RETURN
        END SUBROUTINE cg_solve

!*****************************************************************
        SUBROUTINE asmbl_metrics(p_ghand, i_femtype, i_unkn, r_xyz, &
                   i_tnum, i_tdofs, i_allpiv, r_jmat, r_jac)

!---------- local declarations

        IMPLICIT NONE
        TYPE (grid_handle), INTENT(in)                          :: p_ghand
        INTEGER (KIND = GRID_SI), INTENT(in)                    :: i_femtype
        INTEGER (KIND = GRID_SI), INTENT(in)                    :: i_unkn
        REAL (KIND = GRID_SR), DIMENSION(:,:)                   :: r_xyz
        INTEGER (KIND = GRID_SI), INTENT(in)                    :: i_tnum
        INTEGER (KIND = GRID_SI), DIMENSION(:,:)                :: i_tdofs
        INTEGER (KIND = GRID_SI), DIMENSION(:,:,:), INTENT(OUT) :: i_allpiv
        REAL (KIND = GRID_SR), DIMENSION(:,:,:,:), INTENT(INOUT):: r_jmat
        REAL (KIND = GRID_SR), DIMENSION(:,:), INTENT(INOUT)    :: r_jac

        INTEGER (KIND = GRID_SI)                                :: i_qpnt, i_nods, &
          i_tet, i_outer, i_inner, i_alct, i_egdof, i_info
        INTEGER (KIND = GRID_SI), DIMENSION(GRID_dimension)     :: i_piv
        REAL (KIND = GRID_SR), DIMENSION(:,:), POINTER          :: r_psiksi, r_psieta, r_psizeta
        REAL (KIND = GRID_SR), DIMENSION(:), POINTER            :: r_qweig
        REAL (KIND = GRID_SR)                                   :: r_tmpj, r_tmpi
        REAL (KIND = GRID_SR), DIMENSION(:), ALLOCATABLE        :: r_xksi, &
          r_xeta, r_xzeta, r_yksi, r_yeta, r_yzeta, r_zksi, r_zeta, r_zzeta

        REAL(KIND=GRID_SR), DIMENSION(3,3)                      :: r_mata

!---------- initialize fem values and coefficients

        i_qpnt= 4
        i_nods= 4
        i_egdof= 0
        r_psiksi=>  r_def_psiksi
        r_psieta=>  r_def_psieta
        r_psizeta=> r_def_psizeta
        r_qweig=>   r_def_qweig(:,1)


! Initiliaze cubature weights
!        ALLOCATE(r_xksi(i_qpnt), r_xeta(i_qpnt), &
!                 r_yksi(i_qpnt), r_yeta(i_qpnt), stat=i_alct)

!        IF(i_alct /= 0) CALL grid_error(c_error= &
!          & '[asmbl_metrics]: could not allocate aux. arrays')

!---------- compute metric terms: first dx/dxi, dx/deta, dy/dxi, dy/deta
!---------- loop over all elements

        main_loop: DO i_tet=1,i_tnum
          DO i_outer=1,i_qpnt
            r_mata = 0.0_GRID_SR
            DO i_inner=1,i_nods !!! CAUTION: 3D Only !!!
              r_mata(1, 1)=r_mata(1, 1) &
               &+ r_psiksi(i_inner,i_outer)* r_xyz(1,i_tdofs(i_inner,i_tet))
              r_mata(2, 1)=r_mata(2, 1) &
               &+ r_psieta(i_inner,i_outer)* r_xyz(1,i_tdofs(i_inner,i_tet))
              r_mata(3, 1)= r_mata(3,1) &
               &+ r_psizeta(i_inner,i_outer)* r_xyz(1,i_tdofs(i_inner,i_tet))
              r_mata(1, 2)=r_mata(1, 2) &
               &+ r_psiksi(i_inner,i_outer)* r_xyz(2,i_tdofs(i_inner,i_tet))
              r_mata(2, 2)=r_mata(2, 2) &
               &+ r_psieta(i_inner,i_outer)* r_xyz(2,i_tdofs(i_inner,i_tet))
              r_mata(3, 2)=r_mata(3, 2) &
               &+ r_psizeta(i_inner,i_outer)* r_xyz(2,i_tdofs(i_inner,i_tet))
              r_mata(1, 3)=r_mata(1, 3) &
               &+ r_psiksi(i_inner,i_outer)* r_xyz(3,i_tdofs(i_inner,i_tet))
              r_mata(2, 3)=r_mata(2, 3) &
               &+ r_psieta(i_inner,i_outer)* r_xyz(3,i_tdofs(i_inner,i_tet))
              r_mata(3, 3)=r_mata(3, 3) &
               &+ r_psizeta(i_inner,i_outer)* r_xyz(3,i_tdofs(i_inner,i_tet))
            END DO ! i_inner


            !- perform lu decomposition
            CALL SGETRF(3,3,r_mata,3,i_piv,i_info)

            IF(i_info .NE. 0) THEN
                 WRITE(*,*) r_mata
                 WRITE(*,*) 'i_info:', i_info
            END IF

            i_allpiv(1:3, i_outer, i_tet) = i_piv
            r_jmat(1:3,1:3,i_outer, i_tet) = r_mata

            r_jac(i_outer,i_tet)= r_qweig(i_outer)* &
               ABS(r_mata(1,1)*r_mata(2,2)*r_mata(3,3))
          END DO ! i_outer


!---------- compute metric terms: the Jacobian determinant
	END DO main_loop
   END SUBROUTINE asmbl_metrics

!*****************************************************************
        SUBROUTINE asmbl_full(p_ghand, i_femtype, i_unkn, r_xyz, &
                   i_dofboun, i_tnum, i_tdofs, &
                   i_allpiv, r_jmat, r_jac, r_matrix)

!---------- local declarations

        IMPLICIT NONE
        TYPE (grid_handle), INTENT(in)                    :: p_ghand
        INTEGER (KIND = GRID_SI), INTENT(in)              :: i_femtype
        INTEGER (KIND = GRID_SI), INTENT(in)              :: i_unkn
        REAL (KIND = GRID_SR), DIMENSION(:,:)             :: r_xyz
        INTEGER (KIND = GRID_SI), DIMENSION(:)            :: i_dofboun
        INTEGER (KIND = GRID_SI), INTENT(in)              :: i_tnum
        INTEGER (KIND = GRID_SI), DIMENSION(:,:)          :: i_tdofs
        REAL (KIND = GRID_SR), DIMENSION(:,:), INTENT(IN) :: r_jac
        INTEGER (KIND = GRID_SI), DIMENSION(:,:,:), INTENT(IN) :: i_allpiv
        REAL (KIND = GRID_SR), DIMENSION(:,:,:,:),INTENT(IN)   :: r_jmat
        REAL (KIND = GRID_SR), DIMENSION(:,:)             :: r_matrix
        REAL(KIND=GRID_SR), DIMENSION(3,3)                :: r_mata
        REAL(KIND=GRID_SR), DIMENSION(3,1)                :: r_matbinner
        REAL(KIND=GRID_SR), DIMENSION(3,1)                :: r_matbouter
        INTEGER(KIND=GRID_SI), DIMENSION(3)               :: i_piv
        INTEGER(KIND=GRID_SI)                             :: i_info

        INTEGER (KIND = GRID_SI)                                :: i_cnt, i_inner, &
          i_outer, i_tet, i_ind, j_ind, i_spr, j_pos, i_q, i_qpnt, i_nods, i_alct, &
          i_egdof
        REAL (KIND = GRID_SR)                                   :: r_dldxi, r_dldxj, r_dldyi, r_dldyj
        REAL (KIND = GRID_SR), DIMENSION(:,:), POINTER          :: r_psiksi, r_psieta, r_psizeta
        LOGICAL                                                 :: l_notfound

!---------- initialize global matrix

        r_matrix= 0.0_GRID_SR

!---------- initialize fem values and coefficients

        i_qpnt= 4
        i_nods= 4
        i_egdof= 0
        r_psiksi=> r_def_psiksi
        r_psieta=> r_def_psieta
        r_psizeta=> r_def_psizeta

!---------- loop over all elements

        main_loop: DO i_tet=1,i_tnum

!---------- assemble global stiffness matrix: first loop over quadrature points

          emat_quad: DO i_q=1,i_qpnt

!---------- outer loop over element dofs

            r_mata = r_jmat(1:3,1:3,i_q,i_tet)
            i_piv = i_allpiv(:,i_q,i_tet)

            emat_outer: DO i_outer=1,i_nods
              i_ind= i_tdofs(i_outer,i_tet)
              r_matbouter(1,1) = r_psiksi(i_outer, i_q)
              r_matbouter(2,1) = r_psieta(i_outer, i_q)
              r_matbouter(3,1) = r_psizeta(i_outer, i_q)

              CALL SGETRS('N', 3, 1, r_mata, 3, i_piv, r_matbouter, 3, i_info)
              IF(i_info .NE. 0) WRITE(*,*) 'SGETRS:', i_info


!---------- inner loop over element dofs

              r_mata = r_jmat(:,:,i_q,i_tet)
              emat_inner: DO i_inner=1,i_nods
                j_ind= i_tdofs(i_inner,i_tet)
                r_matbinner(1,1) = r_psiksi(i_inner,i_q)
                r_matbinner(2,1) = r_psieta(i_inner,i_q)
                r_matbinner(3,1) = r_psizeta(i_inner,i_q)

                CALL SGETRS('N', 3, 1, r_mata, 3, i_piv, r_matbinner, 3, i_info)


!---------- compute contribution to matrix entry

                r_matrix(j_ind,i_ind)= r_matrix(j_ind,i_ind)+r_jac(i_q,i_tet)* &
                            & DOT_PRODUCT(r_matbinner(:,1), r_matbouter(:,1))

              END DO emat_inner
            END DO emat_outer
          END DO emat_quad

        END DO main_loop

        
!---------- modify stiffness matrix at boundary
        CALL modify_full_Matrix(i_unkn,i_dofboun, r_matrix)
        

        RETURN
        END SUBROUTINE asmbl_full


!*****************************************************************
        SUBROUTINE asmbl_stiff(p_ghand, i_femtype, i_unkn, r_xyz, &
                   i_dofboun, i_tnum, i_tdofs, &
                   i_allpiv, r_jmat, r_jac, &
                   i_bndw, r_matrix, i_matind)

!---------- local declarations

        IMPLICIT NONE
        TYPE (grid_handle), INTENT(in)                    :: p_ghand
        INTEGER (KIND = GRID_SI), INTENT(in)              :: i_femtype
        INTEGER (KIND = GRID_SI), INTENT(in)              :: i_unkn
        REAL (KIND = GRID_SR), DIMENSION(:,:)             :: r_xyz
        INTEGER (KIND = GRID_SI), DIMENSION(:)            :: i_dofboun
        INTEGER (KIND = GRID_SI), INTENT(in)              :: i_tnum
        INTEGER (KIND = GRID_SI), DIMENSION(:,:)          :: i_tdofs
        REAL (KIND = GRID_SR), DIMENSION(:,:), INTENT(IN) :: r_jac
        INTEGER (KIND = GRID_SI), DIMENSION(:,:,:), INTENT(IN) :: i_allpiv
        REAL (KIND = GRID_SR), DIMENSION(:,:,:,:),INTENT(IN)   :: r_jmat
        INTEGER (KIND = GRID_SI), INTENT(in)              :: i_bndw
        REAL (KIND = GRID_SR), DIMENSION(:,:)             :: r_matrix
        INTEGER (KIND = GRID_SI), DIMENSION(:,:)          :: i_matind
        REAL(KIND=GRID_SR), DIMENSION(3,3)                :: r_mata
        REAL(KIND=GRID_SR), DIMENSION(3,1)                :: r_matbinner
        REAL(KIND=GRID_SR), DIMENSION(3,1)                :: r_matbouter
        INTEGER(KIND=GRID_SI), DIMENSION(3)               :: i_piv
        INTEGER(KIND=GRID_SI)                             :: i_info

        INTEGER (KIND = GRID_SI)                                :: i_cnt, i_inner, &
          i_outer, i_tet, i_ind, j_ind, i_spr, j_pos, i_q, i_qpnt, i_nods, i_alct, &
          i_egdof
        REAL (KIND = GRID_SR)                                   :: r_dldxi, r_dldxj, r_dldyi, r_dldyj
        REAL (KIND = GRID_SR), DIMENSION(:,:), POINTER          :: r_psiksi, r_psieta, r_psizeta
        LOGICAL                                                 :: l_notfound

!---------- initialize global matrix

        r_matrix= 0.0_GRID_SR
        i_matind= 0_GRID_SI

!---------- initialize fem values and coefficients

        i_qpnt= 4
        i_nods= 4
        i_egdof= 0
        r_psiksi=> r_def_psiksi
        r_psieta=> r_def_psieta
        r_psizeta=> r_def_psizeta

!---------- loop over all elements

        main_loop: DO i_tet=1,i_tnum

!---------- assemble global stiffness matrix: first loop over quadrature points

          emat_quad: DO i_q=1,i_qpnt

!---------- outer loop over element dofs

            r_mata = r_jmat(1:3,1:3,i_q,i_tet)
            i_piv = i_allpiv(:,i_q,i_tet)

            emat_outer: DO i_outer=1,i_nods
              i_ind= i_tdofs(i_outer,i_tet)
              r_matbouter(1,1) = r_psiksi(i_outer, i_q)
              r_matbouter(2,1) = r_psieta(i_outer, i_q)
              r_matbouter(3,1) = r_psizeta(i_outer, i_q)

              CALL SGETRS('N', 3, 1, r_mata, 3, i_piv, r_matbouter, 3, i_info)
              IF(i_info .NE. 0) WRITE(*,*) 'SGETRS:', i_info


!---------- inner loop over element dofs

              r_mata = r_jmat(:,:,i_q,i_tet)
              emat_inner: DO i_inner=1,i_nods
                j_ind= i_tdofs(i_inner,i_tet)
                r_matbinner(1,1) = r_psiksi(i_inner,i_q)
                r_matbinner(2,1) = r_psieta(i_inner,i_q)
                r_matbinner(3,1) = r_psizeta(i_inner,i_q)

                CALL SGETRS('N', 3, 1, r_mata, 3, i_piv, r_matbinner, 3, i_info)

                IF(i_info .NE. 0) THEN
                     WRITE(*,*) 'info:', i_info, 'outer:', r_matbouter, &
			 'inner:', r_matbinner
                END IF

!---------- determine index in sparse matrix storage format

                l_notfound= .TRUE.
                sparse_loop: DO i_spr=1,i_bndw
                  j_pos= i_matind(i_spr,i_ind)
                  IF(j_pos == 0 .OR. j_pos == j_ind) THEN
                    l_notfound= .FALSE.
                    exit sparse_loop
                  END IF
                END DO sparse_loop
                IF(l_notfound) THEN
                  CALL grid_error(i_error=1,c_error='[asmbl_stiff]: bandwidth exceeded - increase it!')
                END IF

!---------- compute contribution to matrix entry

                r_matrix(i_spr,i_ind)= r_matrix(i_spr,i_ind)+r_jac(i_q,i_tet)* &
                            & DOT_PRODUCT(r_matbinner(:,1), r_matbouter(:,1))

                i_matind(i_spr,i_ind)= j_ind
              END DO emat_inner
            END DO emat_outer
          END DO emat_quad

        END DO main_loop

        
!---------- modify stiffness matrix at boundary
        CALL modify_global_Matrix(i_unkn,i_dofboun,i_bndw, r_matrix, i_matind)
        

        RETURN
        END SUBROUTINE asmbl_stiff

!*****************************************************************
        SUBROUTINE asmbl_rhs(p_ghand, i_femtype, i_unkn, r_xyz, i_dofboun, &
                             i_tnum, i_tdofs, r_jac, r_vec)

!---------- local declarations

        IMPLICIT NONE
        TYPE (grid_handle), INTENT(in)                          :: p_ghand
        INTEGER (KIND = GRID_SI), INTENT(in)                    :: i_femtype
        INTEGER (KIND = GRID_SI), INTENT(in)                    :: i_unkn
        REAL (KIND = GRID_SR), DIMENSION(:,:)                   :: r_xyz
        INTEGER (KIND = GRID_SI), DIMENSION(:)                  :: i_dofboun
        INTEGER (KIND = GRID_SI), INTENT(in)                    :: i_tnum
        INTEGER (KIND = GRID_SI), DIMENSION(:,:)                 :: i_tdofs
        REAL (KIND = GRID_SR), DIMENSION(:,:)                   :: r_jac
        REAL (KIND = GRID_SR), DIMENSION(:)                     :: r_vec

        INTEGER (KIND = GRID_SI)                                :: i_cnt, &
          i_tet, i_ind, i_q, i_qpnt, i_nods, i_alct, i_egdof
        REAL (KIND = GRID_SR)                                   :: r_val
        REAL (KIND = GRID_SR), DIMENSION(:,:), POINTER          :: r_psi
        REAL (KIND = GRID_SR), DIMENSION(GRID_dimension)        :: r_loccoor

!---------- initialize new right hand side vector

        r_vec= 0.0_GRID_SR

!---------- initialize fem values and coefficients

!-        i_qpnt= GRID_femtypes%p_type(i_femtype)%sig%i_quadpoints
!-        i_nods= GRID_femtypes%p_type(i_femtype)%sig%i_unknowns
!-        i_egdof= GRID_femtypes%p_type(i_femtype)%sig%i_edgepoints
!-        r_psi=> GRID_femtypes%p_type(i_femtype)%sig%r_coef

        r_psi=> r_def_coeff

        i_qpnt  = 4
        i_nods  = 4
        i_egdof = 0

	r_vec = 0.0

!---------- loop over all elements

        main_loop: DO i_tet=1,i_tnum

!---------- loop over all quadrature points in each element

          quad_loop: DO i_q=1,i_qpnt

!---------- compute quadrature locations

            r_loccoor= 0.0_GRID_SR
            DO i_cnt=1,i_nods
              r_loccoor= r_loccoor+ r_xyz(:,i_tdofs(i_cnt,i_tet))* r_psi(i_cnt,i_q)
            END DO

!---------- evaluate right hand side and quadpoint

            r_val= calc_femrhs(r_loccoor)

!---------- now compute the vector value

            DO i_cnt=1,i_nods
              i_ind= i_tdofs(i_cnt,i_tet)
              r_vec(i_ind)= r_vec(i_ind)+ r_jac(i_q,i_tet)* r_psi(i_cnt,i_q)* r_val
	      !--- WRITE(*,*) 'VEC(', i_ind, '): ', r_vec(i_ind), r_jac(i_q,i_tet), r_psi(i_cnt,i_q), r_val
            END DO

          END DO quad_loop
        END DO main_loop
        
!---------- modify vector at boundary

        bound_loop: DO i_ind=1,i_unkn
          IF(i_dofboun(i_ind) == -1) THEN
            r_vec(i_ind)= 0.0_GRID_SR
          END IF
        END DO bound_loop

        RETURN
        END SUBROUTINE asmbl_rhs
        
!***********************************************************************************************

        SUBROUTINE modify_global_Matrix(i_nod_dof_num,i_bounarray,i_bndw, r_matrix, i_matind)

!----- Andre Riedm"uller, TUM, 15.6.2004
!----- modifying stiffnes matrix at boundary
!----- INPUT  :
        IMPLICIT NONE
        INTEGER (KIND = GRID_SI), INTENT(in)                    :: i_nod_dof_num
        INTEGER (KIND = GRID_SI), DIMENSION(i_nod_dof_num)      :: i_bounarray
        INTEGER (KIND = GRID_SI), INTENT(in)                    :: i_bndw
        REAL (KIND = GRID_SR), DIMENSION(i_bndw,i_nod_dof_num)  :: r_matrix
        INTEGER (KIND = GRID_SI),DIMENSION(i_bndw,i_nod_dof_num):: i_matind
        
!----- OUTPUT : !nothing ; r_matrix and i_matind get modification

!----- LOCAL  :
        INTEGER (KIND = GRID_SI)                                :: i_ind, i_k, i_help,i_alct,&
                                                                   i_notzeros, i_outindex
        INTEGER (KIND = GRID_SI)                                :: i_column
        INTEGER (KIND = GRID_SI), DIMENSION(:), ALLOCATABLE        :: i_spaltennr
!--------------------------------------------------------------------------------------


!----- allocate :
        
            ALLOCATE(i_spaltennr(i_bndw+1),stat=i_alct)
            IF(i_alct /= 0) THEN
             CALL grid_error(c_error='[Routine: element]: could not allocate arrays')
            END IF

!----- make those entries to 0, which correspond to column Nr. i_ind 
!----- (A(:,i_ind)==0), except: A(i_ind, i_ind)==1

        DO i_ind =1,i_nod_dof_num
          IF (i_bounarray(i_ind) == -1) THEN
        
            i_spaltennr(:)=0 
        
!----------- finding column numbers in row Nr. i_ind :  ((getestet 17.6.2004))
            spalten_loop: DO i_k= 1,i_bndw
             i_help= i_matind(i_k,i_ind)

             IF (i_help .NE.0) THEN
              i_spaltennr(i_k+1)= i_help
             ELSE 
              i_spaltennr(1)= i_k-1        ! Anzahl der nichtnull Eintr"age in Zeile i_ind
              exit spalten_loop
             END IF
            END DO spalten_loop

             
!------------ Adapting entries to 0 :         
!------------------which columns of i_matind and r_matrix have to be considered ? :
!------------------those with Indices in i_spaltennr:

          i_notzeros = i_spaltennr(1) ! numb. of not zero entries in row i_ind
          
          column_loop: DO i_help = 2,i_notzeros+1
            i_column = i_spaltennr(i_help)
            
!---------------------- if i_column == i_ind: nothing to do (treatment: below):
            IF (i_column .NE. i_ind) THEN
             CALL which_entry(i_matind(:,i_column), i_ind, i_bndw, i_outindex)
           
!------------------------- delete entries r_matrix(i_outindex,i_column) and
!------------------------- i_matind(i_outindex,i_column). Also : adjusting of
!------------------------- datastructures necessary:
             CALL adapt_matrix(i_matind(:,i_column), r_matrix(:,i_column),&
                               i_outindex, i_nod_dof_num)          ! now we made it.
            END IF
          END DO column_loop
        
          END IF  
        END DO
        DEALLOCATE(i_spaltennr)
        
!----- make row Nr. i_ind to 0 (A(i_ind, :)==0), except: A(i_ind, i_ind)==1
        bound_loop: DO i_ind=1,i_nod_dof_num
          IF(i_bounarray(i_ind) == -1) THEN
            r_matrix(:,i_ind)= 0.0_GRID_SR
            i_matind(:,i_ind)= 0_GRID_SI
            r_matrix(1,i_ind)= 1.0_GRID_SR
            i_matind(1,i_ind)= i_ind
          END IF
        END DO bound_loop
        
        RETURN
        END SUBROUTINE modify_global_Matrix
        
!***********************************************************************************************

        SUBROUTINE modify_full_Matrix(i_nod_dof_num,i_bounarray,r_matrix)

!----- Oliver Kunst, UHH, 02.2010
!----- modifying stiffnes matrix at boundary
!----- INPUT  :
        IMPLICIT NONE
        INTEGER (KIND = GRID_SI), INTENT(in)                    :: i_nod_dof_num
        INTEGER (KIND = GRID_SI), DIMENSION(i_nod_dof_num)      :: i_bounarray
        REAL (KIND = GRID_SR), DIMENSION(i_nod_dof_num,i_nod_dof_num)  :: r_matrix
        
!----- OUTPUT : !nothing ; r_matrix and i_matind get modification

!----- LOCAL  :
        INTEGER (KIND = GRID_SI)                                :: i_ind, i_k, i_help,i_alct,&
                                                                   i_notzeros, i_outindex
        INTEGER (KIND = GRID_SI)                                :: i_column
        INTEGER (KIND = GRID_SI), DIMENSION(:), ALLOCATABLE        :: i_spaltennr
!--------------------------------------------------------------------------------------


!----- make those entries to 0, which correspond to column Nr. i_ind 
!----- (A(:,i_ind)==0), except: A(i_ind, i_ind)==1

        DO i_ind =1,i_nod_dof_num
          IF (i_bounarray(i_ind) == -1) THEN
              r_matrix(:, i_ind) = 0.0
              r_matrix(i_ind, i_ind) = 1.0;
              
          END IF
	END DO
        RETURN
        END SUBROUTINE modify_full_Matrix
        
!***********************************************************************************************
!***********************************************************************************************
 
        SUBROUTINE which_entry(i_invector, i_compare_index, i_bndw, i_index)

!------- Andre Riedmueller, TUM, 18.6.2004
!------- returns the (unique!)Index of that element (of in_vector(:))
!------- which is equal to i_compar_index
!------- or returns 0 if there is no such element. If that happens there occured an
!------- error in some datastructures. (should never be the case)

!------- INPUT  :
        INTEGER (KIND = GRID_SI), DIMENSION(:)                :: i_invector 
        INTEGER (KIND = GRID_SI), INTENT(in)                :: i_bndw, i_compare_index
        
!------- OUTPUT :
        INTEGER (KIND = GRID_SI), INTENT(out)                :: i_index
        
!------- LOCAL  :  
        INTEGER (KIND = GRID_SI)                        :: i_com
        
!------------------------------------------------------------------------------------
        
        compare_loop: DO i_com =1,i_bndw
          IF(i_invector(i_com)==0) exit compare_loop
          IF(i_invector(i_com)==i_compare_index) i_index = i_com
        END DO compare_loop
        
        RETURN
        END SUBROUTINE which_entry

!***********************************************************************************************
        
        
        SUBROUTINE adapt_matrix(i_matindvector, r_matrixvector, i_index,&
                        i_nod_dof_num)

!------- Andre Riedmueller, TUM, 15.6.2004
!------- adapts datastructure r_matrix, i_matind and reorders them


!------- INPUT  :
        INTEGER (KIND = GRID_SI), DIMENSION(:)                :: i_matindvector
        INTEGER (KIND = GRID_SI), INTENT(in)                :: i_index, i_nod_dof_num
        REAL (KIND = GRID_SR), DIMENSION(:)                 :: r_matrixvector
        
        
!------- OUTPUT :
        !nothing; entries in datastructures r_matrixvector and i_matindvector are shifted
        
!------- LOCAL  :  
        INTEGER (KIND = GRID_SI)                        :: i_control
                
!------------------------------------------------------------------------------------
        r_matrixvector(i_index)=0
        i_matindvector(i_index)=0          ! ^= i_matind(i_index,i_k)==0
        
!------------- adaptation of datastructures :
        
        i_control =0
        
!- Strange and now obsolete feature, replaced by while loop
   DO WHILE(i_matindvector(i_index +1+i_control) .NE. 0)
        i_matindvector(i_index+i_control)= i_matindvector(i_index +1+i_control)
        r_matrixvector(i_index+i_control)=r_matrixvector(i_index+1+i_control)
        i_control = i_control+1
   END DO
         i_matindvector(i_index + i_control) =0
         r_matrixvector(i_index + i_control)=0

        RETURN
        END SUBROUTINE adapt_matrix
        

        END MODULE MISC_poisson
