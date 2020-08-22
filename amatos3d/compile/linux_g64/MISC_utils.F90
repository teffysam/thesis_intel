!*****************************************************************
!
! MODULE NAME:
!	MICS_utils
! FUNCTION:
!	provide miscelaneous routines
! CONTAINS:
!-----------------------------------------------------------------
!
! NAME:
!	geo_kart
! FUNCTION:
!	spherical (geographical) to karthesian transform
! SYNTAX:
!	real.arr= geo_kart(real.arr)
! ON INPUT:
!	r_lamphi: 2-D vector with lambda and phi
! ON OUTPUT:
!	r_xyz:    3-D vector with karthesian coordinates
! CALLS:
!
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!	kart_geo
! FUNCTION:
!	karthesian to spherical (geographical) transform
! SYNTAX:
!	real.arr= kart_geo(real.arr)
! ON INPUT:
!	r_xyz:    3-D vector with karthesian coordinates
! ON OUTPUT:
!	r_lamphi: 2-D vector with lambda and phi
! CALLS:
!
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!	bary
! FUNCTION:
!	calculate brycentric coordinates
! SYNTAX:
!	real.arr= bary(real.arr, elmt)
! ON INPUT:
!	r_xyz:	coordinate array			real
!	p_etmp:	element					type(elmt)
! ON OUTPUT:
!	r_rst:	resulting barycentric coordinate	real
! CALLS:
!
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!	proj2triang
! FUNCTION:
!	project a given point to a plane, defined by an element
! SYNTAX:
!	real.arr= proj2triang(real.arr, elmt)
! ON INPUT:
!	r_xyz:	coordinate array			real
!	p_etmp:	element					type(elmt)
! ON OUTPUT:
!	r_rst:	resulting coordinate			real
! CALLS:
!
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!	crossprod
! FUNCTION:
!	crossproduct of two vectors
! SYNTAX:
!	real.arr= crossprod(real.arr, real.arr)
! ON INPUT:
!	r_x1:	first vector				real
!	r_x2:	second vector				real
! ON OUTPUT:
!	r_rst:	resulting vector			real
! CALLS:
!
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!	twonorm
! FUNCTION:
!	euklidean norm of a vector
! SYNTAX:
!	real= twonorm(real.arr)
! ON INPUT:
!	r_vec:	vector				real
! ON OUTPUT:
!	r_rst:	norm				real
! CALLS:
!
! COMMENTS:
!
!-----------------------------------------------------------------
!
! PUBLIC:
!	cross_product, euklid_norm
! COMMENTS:
!
! USES:
!	FEM_define
! LIBRARIES:
!
! REFERENCES:
!
! VERSION(S):
!	1. original version for amatos-1.0	J. Behrens	11/2000
!
!*****************************************************************
#if !defined(SGL_DBL) && !defined(DBL_DBL) && !defined(DBL_QUAD)
#define SGL_DBL
#endif
#if defined(SGL_DBL) || defined(DBL_QUAD)
#define PREC_DIFF
#endif
!*****************************************************************
MODULE MISC_utils
  USE FEM_define

#ifdef PREC_DIFF
  INTERFACE euklid_norm
    MODULE PROCEDURE s_twonorm, d_twonorm
  END INTERFACE ! euklid_norm
#else
  INTERFACE euklid_norm
    MODULE PROCEDURE s_twonorm
  END INTERFACE ! euklid_norm
#endif
  PUBLIC :: euklid_norm
  CONTAINS

!*****************************************************************
  FUNCTION s_twonorm(r_vec) RESULT(r_rst)

!---------- local declarations

    IMPLICIT NONE
    REAL (KIND = GRID_SR), DIMENSION(DEF_dimension) :: r_vec
    REAL (KIND = GRID_SR)                           :: r_rst, r_tmp

!---------- calculate vector crossproduct, 3D only

    r_tmp= dot_product(r_vec, r_vec)
    r_rst= sqrt(r_tmp)

  END FUNCTION s_twonorm


!*****************************************************************
  FUNCTION d_twonorm(r_vec) RESULT(r_rst)

!---------- local declarations

    IMPLICIT NONE
    DOUBLE PRECISION, DIMENSION(DEF_dimension) :: r_vec
    DOUBLE PRECISION                           :: r_rst, r_tmp

!---------- calculate vector crossproduct, 3D only

    r_tmp= dot_product(r_vec, r_vec)
    r_rst= sqrt(r_tmp)

  END FUNCTION d_twonorm

END MODULE MISC_utils
