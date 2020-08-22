!*****************************************************************
!
! MODULE NAME:
!	FEM_interpolation
! FUNCTION:
!	low- and high-order interpolation of values
! CONTAINS:
!-----------------------------------------------------------------
!
! NAME:
!	grid_hiorderinterpol
! FUNCTION:
!	provide the high order (bicubic) interpolation given the upstream
!	coordinates and the element containing them
! SYNTAX:
!	real= grid_hiorderinterpol(real.arr, elmt, char)
! ON INPUT:
!	r_upstr: coordinate array of upstream point	REAL (KIND = GRID_SR)
!	p_elem:  element containing upstream point	TYPE (elmt)
!	i_value: integer value describing value		INTEGER (KIND = GRID_SI)
! ON OUTPUT:
!	r_ivalue: interpolation value at upstr. point	REAL (KIND = GRID_SR)
! CALLS:
!
! COMMENTS:
!	currently supported values for i_value (defined in FEM_define):
!	  DEF_tracer: for the tracer variable
!	  DEF_ucomp : for the u velocity value
!	  DEF_vcomp : for the v velocity value
!	  DEF_phi   : for the geopot. height
!	  DEF_zeta  : for the vorticity
!
!-----------------------------------------------------------------
!
! NAME:
!	grid_loorderinterpol
! FUNCTION:
!	provide the low order (bilinear) interpolation given the upstream
!	coordinates and the element containing them
! SYNTAX:
!	real= grid_loorderinterpol(real.arr, elmt, char)
! ON INPUT:
!	r_upstr: coordinate array of upstream point	REAL (KIND = GRID_SR)
!	p_elem:  element containing upstream point	TYPE (elmt)
!	i_value: integer value describing value		INTEGER (KIND = GRID_SI)
! ON OUTPUT:
!	r_ivalue: interpolation value at upstr. point	REAL (KIND = GRID_SR)
! CALLS:
!
! COMMENTS:
!	currently supported values for i_value (defined in FEM_define):
!	  DEF_tracer: for the tracer variable
!	  DEF_ucomp : for the u velocity value
!	  DEF_vcomp : for the v velocity value
!	  DEF_phi   : for the geopot. height
!	  DEF_zeta  : for the vorticity
!
!-----------------------------------------------------------------
!
! NAME:
!	cubic_interpolation
! FUNCTION:
!	bi-cubic spline interpolation, requires values and derivatives at
!	triangle nodes surrounding interpolation point
! SYNTAX:
!	real= cubic_interpolation(real.arr, real.arr, real.arr, real.arr)
! ON INPUT:
!	r_upstr:  upstream point coordinate array	REAL (KIND = GRID_SR)
!	r_coords: coordinate array of surrounding nodes	REAL (KIND = GRID_SR)
!	r_values: values at surrounding nodes (array)	REAL (KIND = GRID_SR)
!	r_derivs: derivatives (x,y) at surroun. nodes	REAL (KIND = GRID_SR)
! ON OUTPUT:
!	r_ivalue: interpolated value at upstr. point	REAL (KIND = GRID_SR)
! CALLS:
!
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!	estmt_gradient
! FUNCTION:
!	estimate the gradient at a node (from differences to surrounding nodes)
! SYNTAX:
!	real.arr= estmt_gradient(node, char)
! ON INPUT:
!	p_inode: pointer to node I am					TYPE (node)
!	i_value: integer value describing value				INTEGER (KIND = GRID_SI)
! ON OUTPUT:
!	r_deriv: array of derivatives (in coordinate directions)	REAL (KIND = GRID_SR)
! CALLS:
!
! COMMENTS:
!	currently supported values for i_value (defined in FEM_define):
!	  DEF_tracer: for the tracer variable
!	  DEF_ucomp : for the u velocity value
!	  DEF_vcomp : for the v velocity value
!	  DEF_phi   : for the geopot. height
!	  DEF_zeta  : for the vorticity
!
!
! PUBLIC:
!
! COMMENTS:
!
! USES:
!	MISC_globalparam, MISC_error, FEM_define, FEM_basis
! LIBRARIES:
!
! REFERENCES:
!	Gradient estimation is based on:
!	AKIMA, H.: "A method of of bivariate interpolation and smooth
!	surface fitting for irregularly distributed data points",
!	ACM TOMS 4, 148-159 (1978)
!	Bi-cubic interpolation is based on:
!	robert renkas algorithm TOMS624:
!	ACM-Trans. Math. Software, Vol. 10, No. 4, p.453 (1984)
! VERSION(S):
!	1. original version			j. behrens	10/96
!	2. FEM_handle added			j. behrens	7/97
!	3. changed for GRID, renamed...		j. behrens	9/97
!	4. major bug fix in low ord. interpol.	j. behrens	4/98
!	5. 3D Version from M.B.Pham and		j. behrens	5/2000-5/2001
!	6. many bug fixes and modifications	j. behrens	7/2001
!
!*****************************************************************
	MODULE FEM_interpolation
	  USE MISC_globalparam
	  USE MISC_error
	  USE FEM_define
	  USE FEM_basis
	  PRIVATE
	  REAL (KIND = GRID_SR), DIMENSION(DEF_dimension) :: r_centr
	  PUBLIC :: grid_hiorderinterpol,grid_loorderinterpol
	  CONTAINS
!*****************************************************************
	  FUNCTION grid_loorderinterpol(r_upstr, p_tetra, i_value, i_itime) RESULT (r_ivalue)

!---------- local declarations

	  IMPLICIT NONE
	  REAL (KIND = GRID_SR), DIMENSION(DEF_dimension), INTENT(in) :: r_upstr
	  TYPE (tetra), POINTER                       :: p_tetra
	  INTEGER (KIND = GRID_SI), INTENT(in)                        :: i_value
	  INTEGER (KIND = GRID_SI), INTENT(in)                        :: i_itime
	  REAL (KIND = GRID_SR)                                       :: r_ivalue
	  INTEGER (KIND = GRID_SI)                                    :: i_cnt
	  TYPE (node), POINTER                       :: p_ntmp
	  REAL (KIND = GRID_SR), DIMENSION(DEF_tetnodes)               :: r_values
	  REAL (KIND = GRID_SR)                                       :: r_reci
	  REAL (KIND = GRID_SR), DIMENSION(:), POINTER                :: r_bas
	  INTEGER (KIND = GRID_SI)                                    :: i_ord
          REAL (KIND = GRID_SR), DIMENSION(:,:), ALLOCATABLE         :: r_coords
!---------- loop over nodes of tetrahedon (get values)

	  elmt_nodes: DO i_cnt=1, DEF_tetnodes
	    p_ntmp=> p_nhash(p_tetra%def%p_node(i_cnt))%np
	    r_values(i_cnt)   = p_ntmp%att%r_vals(i_value,i_itime)
          END DO elmt_nodes

!---------- calculate linear baw  asis functions at r_upstr

	  CALL grid3D_linearbasis(p_tetra, r_upstr, i_ord, r_bas)

!---------- calculate interpolant

	  r_ivalue= DOT_PRODUCT(r_values, r_bas)

!---------- deallocate data

	  deallocate(r_bas)

	  RETURN
	  END FUNCTION grid_loorderinterpol
! ************************************************************	 

	  FUNCTION grid_hiorderinterpol(r_upstr, p_tetra, i_value, i_itime) RESULT (r_ivalue)

!---------- local declarations

	  IMPLICIT NONE
	  REAL (KIND = GRID_SR), DIMENSION(DEF_dimension), INTENT(in) :: r_upstr
	  TYPE (tetra), POINTER                      :: p_tetra
	  INTEGER (KIND = GRID_SI), INTENT(in)                        :: i_value
	  INTEGER (KIND = GRID_SI), INTENT(in)                        :: i_itime
	  REAL (KIND = GRID_SR)                                       :: r_ivalue

	  INTEGER (KIND = GRID_SI), PARAMETER                         :: DEF_MAXNODES = 30
	  INTEGER (KIND = GRID_SI)                                    :: i_cnt, i_numnod
	  INTEGER (KIND = GRID_SI)                                    :: i_nodecnt
	  TYPE (node), POINTER                       :: p_ntmp
          INTEGER (KIND = GRID_SI)                                    :: NQ
          INTEGER (KIND = GRID_SI)                                    :: NW
          INTEGER (KIND = GRID_SI)                                    :: NR
          INTEGER (KIND = GRID_SI)                                    :: IER
          INTEGER (KIND = GRID_SI),DIMENSION (DEF_MAXNODES,DEF_MAXNODES,DEF_MAXNODES) :: LCELL
	  INTEGER (KIND = GRID_SI),DIMENSION (DEF_MAXNODES)           :: LNEXT
	  REAL (KIND = GRID_SR),DIMENSION(DEF_MAXNODES)	             :: X
	  REAL (KIND = GRID_SR),DIMENSION(DEF_MAXNODES)               :: Y
	  REAL (KIND = GRID_SR),DIMENSION(DEF_MAXNODES)	             :: Z
	  REAL (KIND = GRID_SR),DIMENSION(DEF_MAXNODES)               :: F
	  REAL (KIND = GRID_SR),DIMENSION(3) 	                     :: XYZMIN
	  REAL (KIND = GRID_SR),DIMENSION(3)     		     :: XYZDEL 
	  REAL (KIND = GRID_SR) 	                                     :: RMAX 
	  REAL (KIND = GRID_SR),DIMENSION(DEF_MAXNODES)	             :: RSQ 
	  REAL (KIND = GRID_SR),DIMENSION(9,DEF_MAXNODES)             :: A 
	  INTEGER (KIND = GRID_SI), DIMENSION(:), POINTER             :: i_nodeindc

	  EXTERNAL                                   :: QSHEP3
	  REAL (KIND = GRID_SR), EXTERNAL                             :: QS3VAL

!---------- loop over nodes of tetrahedron 

	  i_nodecnt  = 0

	  CALL get_surnodes(p_tetra, i_itime, i_numnod, i_nodeindc)
	  IF(i_numnod > DEF_MAXNODES) THEN
	    r_centr= r_upstr
	    CALL sort_nearest(i_numnod, i_nodeindc)
	    i_nodecnt= DEF_MAXNODES
	  ELSE
	    i_nodecnt= i_numnod
	  END IF

	  array_loop: DO i_cnt=1, i_nodecnt
	    p_ntmp  => p_nhash(i_nodeindc(i_cnt))%np
	    X(i_cnt)=  p_ntmp%def%r_coor(1)
	    Y(i_cnt)=  p_ntmp%def%r_coor(2)
	    Z(i_cnt)=  p_ntmp%def%r_coor(3)
	    F(i_cnt)=  p_ntmp%att%r_vals(i_value,i_itime)
	  END DO array_loop

	  NQ = MIN(17,i_numnod-2)
	  NW = MIN(DEF_MAXNODES-1, i_numnod-1)
	  NR = (i_numnod/3.0)**(1.0/3.0)

	  CALL QSHEP3(i_nodecnt, X, Y, Z, F, NQ, NW, NR, LCELL, LNEXT, &
	              XYZMIN, XYZDEL, RMAX, RSQ, A, IER) 
	  r_ivalue= QS3VAL(r_upstr(1), r_upstr(2), r_upstr(3),i_nodecnt, &
	                   X, Y, Z, F, NR, LCELL, LNEXT, XYZMIN, & 
	                   XYZDEL, RMAX, RSQ, A)

          RETURN
	  END FUNCTION grid_hiorderinterpol

! ************************************************************	 
	  SUBROUTINE get_surnodes(p_tetra, i_time, i_numnod, i_indices)

!---------- local declarations

	  IMPLICIT NONE
	  TYPE (tetra), POINTER                       :: p_tetra
	  INTEGER (KIND = GRID_SI), INTENT(in)                         :: i_time
	  INTEGER (KIND = GRID_SI), INTENT(out)                        :: i_numnod
	  INTEGER (KIND = GRID_SI), DIMENSION(:), POINTER              :: i_indices
	  
	  TYPE (tetra), POINTER                       :: p_ttmp
	  TYPE (node), POINTER                        :: p_ntmp, p_nnod
	  INTEGER (KIND = GRID_SI), PARAMETER                          :: i_alctsize=32
	  INTEGER (KIND = GRID_SI)                                     :: i_alct, i_currsize, &
	    i_siz, i_cn1, i_cn2, i_cn3, i_cnt
	  INTEGER (KIND = GRID_SI), DIMENSION(:), POINTER              :: i_auxarr
	  LOGICAL                                     :: l_notinlist

!---------- allocate index-array

	  IF(associated(i_indices)) THEN
	    deallocate(i_indices)
	  END IF
	  allocate(i_indices(i_alctsize), stat=i_alct)
	  IF(i_alct /= 0) THEN
	    CALL print_error(a_err='[get_surnodes]: could not allocate index array')
	  END IF
	  i_currsize= i_alctsize
	  i_numnod= 0

!---------- now find neigboring nodes, first the local tetrahedrons nodes

	  tetranode_loop1: DO i_cnt=1,DEF_tetnodes
	    i_numnod= i_numnod+1

!---------- reallocate index array if necessary

	    re_alloc: IF(i_numnod > i_currsize) THEN
	      i_siz= i_currsize+i_alctsize
	      allocate(i_auxarr(i_siz), stat=i_alct)
	      IF(i_alct /= 0) THEN
	        CALL print_error(a_err='[get_surnodes]: could not reallocate index array')
	      END IF
	      i_auxarr(1:i_currsize)= i_indices(1:i_currsize)
	      i_auxarr(i_currsize+1:i_siz)= 0
	      deallocate (i_indices)
	      i_indices=> i_auxarr
	      nullify(i_auxarr)
	      i_currsize= i_siz
	    END IF re_alloc

	    i_indices(i_numnod)= p_tetra%def%p_node(i_cnt)
	    p_nnod=> p_nhash(p_tetra%def%p_node(i_cnt))%np
	  END DO tetranode_loop1

!---------- collect surrounding nodes in a pointer array

	  tetranode_loop2: DO i_cnt=1,DEF_tetnodes
	    p_nnod=> p_nhash(p_tetra%def%p_node(i_cnt))%np
	    patch_loop: DO i_cn1=1,p_nnod%att%i_ptch(i_time)
	      p_ttmp=> p_thash(p_nnod%att%p_ptch(i_cn1,i_time))%tp
              node_loop: do i_cn2=1, DEF_tetnodes
	        p_ntmp=> p_nhash(p_ttmp%def%p_node(i_cn2))%np
                l_notinlist = .TRUE.
	        collect_loop: do i_cn3= 1, i_numnod
	          decide: IF(i_indices(i_cn3) == p_ntmp%def%i_indx) THEN
	            l_notinlist= .FALSE.
	            EXIT collect_loop
	          END IF decide
	        END DO collect_loop
	        put_in: IF(l_notinlist) THEN
	          i_numnod= i_numnod+1

!---------- reallocate index array if necessary

	          re2_alloc: IF(i_numnod > i_currsize) THEN
	            i_siz= i_currsize+i_alctsize
	            allocate(i_auxarr(i_siz), stat=i_alct)
	            IF(i_alct /= 0) THEN
	              CALL print_error(a_err='[get_surnodes]: could not reallocate index array')
	            END IF
	            i_auxarr(1:i_currsize)= i_indices(1:i_currsize)
	            i_auxarr(i_currsize+1:i_siz)= 0
	            deallocate (i_indices)
	            i_indices=> i_auxarr
	            nullify(i_auxarr)
	            i_currsize= i_siz
	          END IF re2_alloc
	          i_indices(i_numnod)= p_ntmp%def%i_indx
	        END IF put_in
	      END DO node_loop
	    END DO patch_loop
	  END DO tetranode_loop2
	  
	  RETURN
	  END SUBROUTINE get_surnodes

! ************************************************************	 
	  SUBROUTINE sort_nearest(i_numnod, i_nodeindc)

!---------- local declarations

	  IMPLICIT NONE
	  INTEGER (KIND = GRID_SI), INTENT(in)                  :: i_numnod
	  INTEGER (KIND = GRID_SI), DIMENSION(:), POINTER       :: i_nodeindc
          EXTERNAL :: qsort
	  
	  CALL qsort(i_nodeindc, i_numnod, 4, compr)
	  RETURN
	  END SUBROUTINE sort_nearest

!*****************************************************************
	FUNCTION compr(i_1, i_2) RESULT(i_pm)

!---------- local declarations

	IMPLICIT NONE
	INTEGER (KIND = GRID_SI)   :: i_1, i_2
	INTEGER (KIND = GRID_SI)   :: i_pm ! original: (KIND=2)
	REAL (KIND = GRID_SR), DIMENSION(DEF_dimension) :: r_coo1, r_coo2, r_tmp
	REAL (KIND = GRID_SR)                           :: r_d1, r_d2
	TYPE (node), POINTER           :: p_ntmp

!---------- find coordinates corresponding to indices i_1, i_2

	p_ntmp=> p_nhash(i_1)%np
	r_coo1=  p_ntmp%def%r_coor
	p_ntmp=> p_nhash(i_2)%np
	r_coo2=  p_ntmp%def%r_coor

!---------- calculate distances to center

	r_tmp =  r_coo1- r_centr
	r_d1  =  dot_product(r_tmp, r_tmp)
	r_tmp =  r_coo2- r_centr
	r_d2  =  dot_product(r_tmp, r_tmp)
	
	IF(r_d1 == r_d2) THEN
	  i_pm= 0
	ELSE IF (r_d1 < r_d2) THEN
	  i_pm= -1
	ELSE
	  i_pm= 1
	END IF
	RETURN
	END FUNCTION compr
! ************************************************************	 
! 
! 	SUBROUTINE QSHEP3 (N,X,Y,Z,F,NQ,NW,NR, LCELL,LNEXT,XYZMIN,XYZDEL,RMAX,RSQ,A,IER)
! 	INTEGER (KIND = GRID_SI) N, NQ, NW, NR, LCELL(NR,NR,NR), LNEXT(N), IER , ml, nl , ol
! 	REAL (KIND = GRID_SR)    X(N), Y(N), Z(N), F(N), XYZMIN(3), XYZDEL(3), RMAX, RSQ(N), A(9,N)
! 
! 
! 	INTEGER (KIND = GRID_SI) I, IB, IERR, IP1, IRM1, IROW, J, JP1, K, LMAX, LNP, NEQ, NN, NNQ, NNR, NNW, NP, NPTS(40),   NQWMAX
! 	REAL (KIND = GRID_SR)    AV, AVSQ, B(10,10), C, DMIN, DTOL, FK, RQ, &
! 	  RS, XYZMN(3) ,  RSMX, RSOLD, RTOL, RWS, S, SF, SUM, T, &
! 	  XK, YK,  ZK, XYZDL(3)
! 
! 	DATA    RTOL/1.E-5/, DTOL/.01/, SF/1./
! 
!       NN = N
!       NNQ = NQ
!       NNW = NW
!       NNR = NR
!       NQWMAX = MAX0(NNQ,NNW)
!       LMAX = MIN0(40,NN-1)
! 
! 
! 
!       IF (9 .GT. NNQ  .OR.  1 .GT. NNW	.OR.  NQWMAX .GT. LMAX	.OR.  NNR .LT. 1) GO TO 20
! 
! 
! 
!   CALL STORE3 (NN,X,Y,Z,NNR, LCELL,LNEXT,XYZMN,XYZDL,IERR)
!   
!    
! 
! 
! 
!  
! 
! 
!      IF (IERR .NE. 0) GO TO 22
!       RSMX = 0.
! 
!  
! 
! DO 18 K = 1,NN
! 	XK = X(K)
! 	YK = Y(K)
! 	ZK = Z(K)
! 	FK = F(K)
! 
! 
! LNEXT(K) = -LNEXT(K)
! 
!          RS = 0.
! 	SUM = 0.
! 	RWS = 0.
! 	RQ = 0.
! 	LNP = 0
! 
!  1	SUM = SUM + RS
! 	  IF (LNP .EQ. LMAX) GO TO 3
! 	  LNP = LNP + 1
! 	  RSOLD = RS
! 	  CALL GETNP3 (XK,YK,ZK,X,Y,Z,NNR,LCELL,LNEXT,XYZMN , XYZDL, NP,RS)
!            
! 
! 
!   
!          
! 	  IF (RS .EQ. 0.) GO TO 21
! 	  NPTS(LNP) = NP
! 	  IF ( (RS-RSOLD)/RS .LT.  RTOL ) GO TO 1
! 	  IF (RWS .EQ. 0.  .AND.  LNP .GT. NNW) RWS = RS
! 	  IF (RQ .NE. 0.  .OR.	LNP .LE. NNQ) GO TO 2
! 
!            NEQ = LNP - 1
! 	  RQ = SQRT(RS)
! 	  AVSQ = SUM/FLOAT(NEQ)
! 
! 
!  2	  IF (LNP .GT. NQWMAX) GO TO 4
! 	  GO TO 1
! 
! 
!     3	IF (RWS .EQ. 0.) RWS = 1.1*RS
! 	IF (RQ .NE. 0.) GO TO 4
! 	  NEQ = LMAX
! 	  RQ = SQRT(1.1*RS)
!         
! 
!  4	RSQ(K) = RWS
! 
!    
!   
! 	IF (RWS .GT. RSMX) RSMX = RWS
! 	AV = SQRT(AVSQ)
!       
!        
! 
!        I = 0
! 
!     5	  I = I + 1
! 	  NP = NPTS(I)
! 	  IROW = MIN0(I,10)
! 	  CALL SETUP3 (XK,YK,ZK,FK,X(NP),Y(NP),Z(NP),F(NP),  AV,AVSQ,RQ, B(1,IROW))
! 	  IF (I .EQ. 1) GO TO 5
! 	  IRM1 = IROW-1
! 	  DO 6 J = 1,IRM1
! 	    JP1 = J + 1
! 	    CALL GIVENS (B(J,J),B(J,IROW),C,S)
!             
!     6	    CALL ROTATE (10-J,C,S,B(JP1,J),B(JP1,IROW))
! 	  IF (I .LT. NEQ) GO TO 5
! 
! 
! DMIN = AMIN1( ABS(B(1,1)),ABS(B(2,2)),ABS(B(3,3)),  ABS(B(4,4)),ABS(B(5,5)),ABS(B(6,6)), ABS(B(7,7)),ABS(B(8,8)),ABS(B(9,9)) )
! 	IF (DMIN*RQ .GE. DTOL) GO TO 13
! 	IF (NEQ .EQ. LMAX) GO TO 10
! 
! 
!  7	RSOLD = RS
! 	NEQ = NEQ + 1
! 	IF (NEQ .EQ. LMAX) GO TO 9
! 	IF (NEQ .EQ. LNP) GO TO 8
! 
! 
!         NP = NPTS(NEQ+1)
! 	RS = (X(NP)-XK)**2 + (Y(NP)-YK)**2 + (Z(NP)-ZK)**2
! 	IF ( (RS-RSOLD)/RS .LT. RTOL ) GO TO 7
! 	RQ = SQRT(RS)
! 	GO TO 5
! 
!   8	LNP = LNP + 1
! 	CALL GETNP3 (XK,YK,ZK,X,Y,Z,NNR,LCELL,LNEXT,XYZMN, XYZDL, NP,RS)
! 	IF (NP .EQ. 0) GO TO 21
! 	NPTS(LNP) = NP
! 	IF ( (RS-RSOLD)/RS .LT. RTOL ) GO TO 7
! 	RQ = SQRT(RS)
! 	GO TO 5
! 
! 
!     9	RQ = SQRT(1.1*RS)
! 	GO TO 5
! 
! 10	DO 12 I = 1,6
! 	  B(I,10) = SF
! 	  IP1 = I + 1
! 	  DO 11 J = IP1,10
!    11	    B(J,10) = 0.
! 	  DO 12 J = I,9
! 	    JP1 = J + 1
! 	    CALL GIVENS (B(J,J),B(J,10),C,S)
!    12	    CALL ROTATE (10-J,C,S,B(JP1,J),B(JP1,10))
! 
! DMIN = AMIN1( ABS(B(1,1)),ABS(B(2,2)),ABS(B(3,3)),   ABS(B(4,4)),ABS(B(5,5)),ABS(B(6,6)),   ABS(B(7,7)),ABS(B(8,8)),ABS(B(9,9)) )
! 	IF (DMIN*RQ .LT. DTOL) GO TO 22
! 
! 
! 
! 13	DO 15 IB = 1,9
! 	  I = 10-IB
! 	  T = 0.
! 	  IF (I .EQ. 9) GO TO 15
! 	  IP1 = I + 1
! 	  DO 14 J = IP1,9
!    14	    T = T + B(J,I)*A(J,K)
!    15	  A(I,K) = (B(10,I)-T)/B(I,I)
! 
!         DO 16 I = 1,6
!    16	  A(I,K) = A(I,K)/AVSQ
! 	A(7,K) = A(7,K)/AV
! 	A(8,K) = A(8,K)/AV
! 	A(9,K) = A(9,K)/AV
! 
! 	LNEXT(K) = -LNEXT(K)
! 	DO 17 I = 1,LNP
! 	  NP = NPTS(I)
!    17	  LNEXT(NP) = -LNEXT(NP)
!    18	CONTINUE
! 
!  DO 19 I = 1,3
! 	XYZMIN(I) = XYZMN(I)
!    19	XYZDEL(I) = XYZDL(I)
!       RMAX = SQRT(RSMX)
!       
!  
!      IER = 0
!       RETURN
! 
!    20 IER = 1
!       RETURN
! 
!    21 IER = 2
!       RETURN
! 
!    22 DO 23 I = 1,3
! 	XYZMIN(I) = XYZMN(I)
!    23	XYZDEL(I) = XYZDL(I)
!       IER = 3
!       RETURN
!      
! 
!   END SUBROUTINE QSHEP3
! ! ************************************************************	 
! 
!  FUNCTION QS3VAL (PX,PY,PZ,N,X,Y,Z,F,NR,LCELL,LNEXT,   XYZMIN,XYZDEL,RMAX,RSQ,A) RESULT (qs3_value)
!       INTEGER (KIND = GRID_SI) N, NR, LCELL(NR,NR,NR), LNEXT(N)
!       REAL (KIND = GRID_SR)    PX, PY, PZ, X(N), Y(N), Z(N), F(N), XYZMIN(3), XYZDEL(3), RMAX, RSQ(N), A(9,N), Qsqs3_value
! 
!  
!       XP = PX
!       YP = PY
!       ZP = PZ
!       XMIN = XYZMIN(1)
!       YMIN = XYZMIN(2)
!       ZMIN = XYZMIN(3)
!       DX = XYZDEL(1)
!       DY = XYZDEL(2)
!       DZ = XYZDEL(3)
! 
!  
! 
! 
! 
! 
!  
!       IF (N .LT. 10  .OR.  NR .LT. 1  .OR.  DX .LE. 0.  .OR. DY .LE. 0.  .OR.  DZ .LE. 0.  .OR.  RMAX .LT. 0.) RETURN
!     
! 
!       IMIN = IFIX((XP-XMIN-RMAX)/DX) + 1
!       IMAX = IFIX((XP-XMIN+RMAX)/DX) + 1
!       IF (IMIN .LT. 1) IMIN = 1
!       IF (IMAX .GT. NR) IMAX = NR
!       JMIN = IFIX((YP-YMIN-RMAX)/DY) + 1
!       JMAX = IFIX((YP-YMIN+RMAX)/DY) + 1
!       IF (JMIN .LT. 1) JMIN = 1
!       IF (JMAX .GT. NR) JMAX = NR
!       KMIN = IFIX((ZP-ZMIN-RMAX)/DZ) + 1
!       KMAX = IFIX((ZP-ZMIN+RMAX)/DZ) + 1
!       IF (KMIN .LT. 1) KMIN = 1
!       IF (KMAX .GT. NR) KMAX = NR
! 
! 
! 
! 
! 	   IF (IMIN .GT. IMAX  .OR.	JMIN .GT. JMAX	.OR. KMIN .GT. KMAX) GO TO 5
!  
! 
!       SW = 0.
!       SWQ = 0.
! 
!       DO 3 K = KMIN,KMAX
! 	DO 3 J = JMIN,JMAX
! 	  DO 3 I = IMIN,IMAX
! 	    L = LCELL(I,J,K)
! 	    IF (L .EQ. 0) GO TO 3
!  
! 
!  1	    DELX = XP - X(L)
! 	    DELY = YP - Y(L)
! 	    DELZ = ZP - Z(L)
! 	    DXSQ = DELX*DELX
! 	    DYSQ = DELY*DELY
! 	    DZSQ = DELZ*DELZ
! 	    DS = DXSQ + DYSQ + DZSQ
! 	    RS = RSQ(L)
! 
! 
! 	    IF (DS .GE. RS) GO TO 2
! 	    IF (DS .EQ. 0.) GO TO 4
! 	    RDS = RS*DS
! 	    RD = SQRT(RDS)
! 	    W = (RS+DS-RD-RD)/RDS
! 	    SW = SW + W
! 	    SWQ = SWQ + W*( A(1,L)*DXSQ + A(2,L)*DELX*DELY + &
!        A(3,L)*DYSQ + A(4,L)*DELX*DELZ + &
!       A(5,L)*DELY*DELZ + A(6,L)*DZSQ +  &
!       A(7,L)*DELX + A(8,L)*DELY +       &
!         A(9,L)*DELZ + F(L) )
! 
!            
! 
!  2	    LP = L
! 	    L = LNEXT(LP)
! 	    IF (L .NE. LP) GO TO 1
!     3	    CONTINUE
! 
!       IF (SW .EQ. 0.) GO TO 5
!       qs3_value = SWQ/SW
!       RETURN
! 
! 
! IF (SW .EQ. 0.) GO TO 5
!       qs3_value= SWQ/SW
!       RETURN
! 
!     4 qs3_value = F(L)
!       RETURN
! 
!     5 qs3_value  = 0.
!       RETURN
!       END FUNCTION QS3VAL
! ! ************************************************************	 
! 
!  SUBROUTINE GETNP3 (PX,PY,PZ,X,Y,Z,NR,LCELL,LNEXT, XYZMIN,XYZDEL, NP,DSQ)
!       INTEGER (KIND = GRID_SI) NR, LCELL(NR,NR,NR), LNEXT(1), NP
!       REAL (KIND = GRID_SR)    PX, PY, PZ, X(1), Y(1), Z(1), XYZMIN(3),  XYZDEL(3), DSQ
! 
! 
!  LOGICAL FIRST
!       XP = PX
!       YP = PY
!       ZP = PZ
!       DX = XYZDEL(1)
!       DY = XYZDEL(2)
!       DZ = XYZDEL(3)
! 
!       IF (NR .LT. 1  .OR.  DX .LE. 0.  .OR.  DY .LE. 0.  .OR.	DZ .LE. 0.) GO TO 10
! 
! 
! FIRST = .TRUE.
!       IMIN = 1
!       IMAX = NR
!       JMIN = 1
!       JMAX = NR
!       KMIN = 1
!       KMAX = NR
!       DELX = XP - XYZMIN(1)
!       DELY = YP - XYZMIN(2)
!       DELZ = ZP - XYZMIN(3)
!       I0 = IFIX(DELX/DX) + 1
!       IF (I0 .LT. 1) I0 = 1
!       IF (I0 .GT. NR) I0 = NR
!       J0 = IFIX(DELY/DY) + 1
!       IF (J0 .LT. 1) J0 = 1
!       IF (J0 .GT. NR) J0 = NR
!       K0 = IFIX(DELZ/DZ) + 1
!       IF (K0 .LT. 1) K0 = 1
!       IF (K0 .GT. NR) K0 = NR
!       I1 = I0
!       I2 = I0
!       J1 = J0
!       J2 = J0
!       K1 = K0
!       K2 = K0
! 
! 
! 1 DO 7 K = K1,K2
! 	IF (K .GT. KMAX) GO TO 8
! 	IF (K .LT. KMIN) GO TO 7
! 	DO 6 J = J1,J2
! 	  IF (J .GT. JMAX) GO TO 7
! 	  IF (J .LT. JMIN) GO TO 6
! 	  DO 5 I = I1,I2
! 	    IF (I .GT. IMAX) GO TO 6
! 	    IF (I .LT. IMIN) GO TO 5
! 	    IF (K .NE. K1  .AND.  K .NE. K2  .AND. J .NE. J1  .AND.  J .NE. J2  .AND. I .NE. I1  .AND.  I .NE. I2) GO TO 5
! 
! 
!             L = LCELL(I,J,K)
! 	    IF (L .EQ. 0) GO TO 5
! 
! 
!  2	    LN = LNEXT(L)
! 	    IF (LN .LT. 0) GO TO 4
! 
! 	    RSQ = (X(L)-XP)**2 + (Y(L)-YP)**2 + (Z(L)-ZP)**2
! 	    IF (.NOT. FIRST) GO TO 3
! 
! 
!             LMIN = L
! 	    RSMIN = RSQ
! 	    R = SQRT(RSMIN)
! 	    IMIN = IFIX((DELX-R)/DX) + 1
! 	    IF (IMIN .LT. 1) IMIN = 1
! 	    IMAX = IFIX((DELX+R)/DX) + 1
! 	    IF (IMAX .GT. NR) IMAX = NR
! 	    JMIN = IFIX((DELY-R)/DY) + 1
! 	    IF (JMIN .LT. 1) JMIN = 1
! 	    JMAX = IFIX((DELY+R)/DY) + 1
! 	    IF (JMAX .GT. NR) JMAX = NR
! 	    KMIN = IFIX((DELZ-R)/DZ) + 1
! 	    IF (KMIN .LT. 1) KMIN = 1
! 	    KMAX = IFIX((DELZ+R)/DZ) + 1
! 	    IF (KMAX .GT. NR) KMAX = NR
! 	    FIRST = .FALSE.
! 	    GO TO 4
! 
! 
! 
!  3	    IF (RSQ .GE. RSMIN) GO TO 4
! 
! 	    LMIN = L
! 	    RSMIN = RSQ
! 
!     4	    IF (IABS(LN) .EQ. L) GO TO 5
! 	    L = IABS(LN)
! 	    GO TO 2
!     5	    CONTINUE
!     6	  CONTINUE
!     7	CONTINUE
! 
!     8 IF (I1 .LE. IMIN	.AND.  I2 .GE. IMAX  .AND. &
!        J1 .LE. JMIN .AND.  J2 .GE. JMAX  .AND.  K1 .LE. KMI  .AND.  & 
!        K2 .GE. KMAX) GO TO 9
!       I1 = I1 - 1
!       I2 = I2 + 1
!       J1 = J1 - 1
!       J2 = J2 + 1
!       K1 = K1 - 1
!       K2 = K2 + 1
!       GO TO 1
! 
!     9 IF (FIRST) GO TO 10
!       NP = LMIN
!       DSQ = RSMIN
!       LNEXT(LMIN) = -LNEXT(LMIN)
!       RETURN
! 
!    10 NP = 0
!       DSQ = 0.
!       RETURN
! 
!  END SUBROUTINE GETNP3
! ! ************************************************************	 
! 
! SUBROUTINE GIVENS ( A,B, C,S)
!       REAL (KIND = GRID_SR) A, B, C, S
! 
! 
!  REAL (KIND = GRID_SR) AA, BB, R, U, V
! 
! 
!      AA = A
!       BB = B
!       IF (ABS(AA) .LE. ABS(BB)) GO TO 1
! 
!       U = AA + AA
!       V = BB/U
!       R = SQRT(.25 + V*V) * U
!       C = AA/R
!       S = V * (C + C)
!      
! 
!       B = S
!       A = R
!       RETURN
! 
!     1 IF (BB .EQ. 0.) GO TO 2
!       U = BB + BB
!       V = AA/U
! 
!       A = SQRT(.25 + V*V) * U
!       S = BB/A
!       C = V * (S + S)
! 
!       B = 1.
!       IF (C .NE. 0.) B = 1./C
!       RETURN
! 
!     2 C = 1.
!       S = 0.
!       RETURN
!       
! 
! 
!   END SUBROUTINE GIVENS
! ! ************************************************************	 
! 
! 	SUBROUTINE ROTATE (N,C,S, X,Y )
! 	INTEGER (KIND = GRID_SI) N
! 	REAL (KIND = GRID_SR)    C, S, X(N), Y(N)
! 
! 	INTEGER (KIND = GRID_SI) I
! 	REAL (KIND = GRID_SR)    XI, YI
! 
!  IF (N .LE. 0 .OR. (C .EQ. 1. .AND. S .EQ. 0.)) RETURN
!       DO 1 I = 1,N
! 	XI = X(I)
! 	YI = Y(I)
! 	X(I) = C*XI + S*YI
! 	Y(I) = -S*XI + C*YI
!     1	CONTINUE
!       RETURN
!      
! 
! 	END SUBROUTINE ROTATE
! ! ************************************************************	 
! 
! 	SUBROUTINE SETUP3 (XK,YK,ZK,FK,XI,YI,ZI,FI,S1,S2, R, ROW)
! 	REAL (KIND = GRID_SR) XK, YK, ZK, FK, XI, YI, ZI, FI, S1, S2,  R, ROW(10)
! 
! 
! 	INTEGER (KIND = GRID_SI) I
! 	REAL (KIND = GRID_SR)    DX, DY, DZ, DXSQ, DYSQ, DZSQ, D, W, W1, W2
! 
!       DX = XI - XK
!       DY = YI - YK
!       DZ = ZI - ZK
!       DXSQ = DX*DX
!       DYSQ = DY*DY
!       DZSQ = DZ*DZ
!       D = SQRT(DXSQ + DYSQ + DZSQ)
!       IF (D .LE. 0.  .OR.  D .GE. R) GO TO 1
!       W = (R-D)/R/D
!       W1 = W/S1
!       W2 = W/S2
!       ROW(1) = DXSQ*W2
!       ROW(2) = DX*DY*W2
!       ROW(3) = DYSQ*W2
!       ROW(4) = DX*DZ*W2
!       ROW(5) = DY*DZ*W2
!       ROW(6) = DZSQ*W2
!       ROW(7) = DX*W1
!       ROW(8) = DY*W1
!       ROW(9) = DZ*W1
!       ROW(10) = (FI - FK)*W
!       RETURN
! 
!     1 DO 2 I = 1,10
!     2	ROW(I) = 0.
!       RETURN
!       
! 
! 	END SUBROUTINE SETUP3
! ! ************************************************************	 
! 
! 	SUBROUTINE STORE3 (N,X,Y,Z,NR, LCELL,LNEXT,XYZMIN, XYZDEL,IER)
!       INTEGER (KIND = GRID_SI) N, NR, LCELL(NR,NR,NR), LNEXT(N), IER
!       REAL (KIND = GRID_SR)    X(N), Y(N), Z(N), XYZMIN(3), XYZDEL(3)
! 
! 
! 
!   NN = N
!       NNR = NR
!       IF (NN .LT. 2  .OR.  NNR .LT. 1) GO TO 4
! 
! 
!       XMN = X(1)
!       XMX = XMN
!       YMN = Y(1)
!       YMX = YMN
!       ZMN = Z(1)
!       ZMX = ZMN
!       DO 1 L = 2,NN
! 	IF (X(L) .LT. XMN) XMN = X(L)
! 	IF (X(L) .GT. XMX) XMX = X(L)
! 	IF (Y(L) .LT. YMN) YMN = Y(L)
! 	IF (Y(L) .GT. YMX) YMX = Y(L)
! 	IF (Z(L) .LT. ZMN) ZMN = Z(L)
!     1	IF (Z(L) .GT. ZMX) ZMX = Z(L)
!       XYZMIN(1) = XMN
!       XYZMIN(2) = YMN
!       XYZMIN(3) = ZMN
! 
! 
! 
!       DELX = (XMX-XMN)/FLOAT(NNR)
!       DELY = (YMX-YMN)/FLOAT(NNR)
!       DELZ = (ZMX-ZMN)/FLOAT(NNR)
!       XYZDEL(1) = DELX
!       XYZDEL(2) = DELY
!       XYZDEL(3) = DELZ
!       IF (DELX .EQ. 0.	.OR.  DELY .EQ. 0.  .OR. DELZ .EQ. 0.) GO TO 5
! 
! 
! 
!  DO 2 K = 1,NNR
! 	DO 2 J = 1,NNR
! 	  DO 2 I = 1,NNR
!     2	    LCELL(I,J,K) = 0
! 
!       NP1 = NN + 1
!       DO 3 LL = 1,NN
! 	LB = NP1 - LL
! 	I = IFIX((X(LB)-XMN)/DELX) + 1
! 	IF (I .GT. NNR) I = NNR
! 	J = IFIX((Y(LB)-YMN)/DELY) + 1
! 	IF (J .GT. NNR) J = NNR
! 	K = IFIX((Z(LB)-ZMN)/DELZ) + 1
! 	IF (K .GT. NNR) K = NNR
! 	L = LCELL(I,J,K)
! 	LNEXT(LB) = L
! 	IF (L .EQ. 0) LNEXT(LB) = LB
!     3	LCELL(I,J,K) = LB
! 
!  IER = 0
!       RETURN
! 
!     4 IER = 1
!       RETURN
! 
!     5 IER = 2
!       RETURN
! 
! 	END SUBROUTINE STORE3 

END MODULE FEM_interpolation












