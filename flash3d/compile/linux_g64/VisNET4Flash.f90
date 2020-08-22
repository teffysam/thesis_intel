!*****************************************************************
!
! MODULE NAME:
!	VISNET4Flash
! FUNCTION:
!	provides Fortran 90 Module with mode definitions
! PUBLIC:
!	BJG_FLAGGRID, BJG_FLAGCONTINENT, BJG_FLAGVECTOR, BJG_FLAGSCALAR
!	BJG_DRAWMODE_GL, BJG_DRAWMODE_TIFF, BJG_DRAWMODE_PS
! COMMENTS:
!
! USES:
!
! LIBRARIES:
!	libBJUGL.a
! REFERENCES:
!	BJUGL was written by Jörn Behrens and Bertram Lückehe
! VERSION(S):
!	1. original version	j. behrens	1/2000
!
!*****************************************************************
	MODULE VisNET4Flash

!---------- define drawing modes and types

	  INTEGER, EXTERNAL    :: VisNET_GraphicsDraw

          INTERFACE
             FUNCTION VisNET_GraphicsInit(nn, &
                                          xx, yy, &
                                          wtl, wtext, &
                                          mtl, mskfile, &
                                          tiff, &
                                          initial_dimension, &
                                          i_extremes, r_extremes) RESULT (ret)

               INTEGER, INTENT(IN)            :: nn                ! number of boundary nodes
               REAL, DIMENSION(*)             :: xx                ! x-coordinates of bounds
               REAL, DIMENSION(*)             :: yy                ! y-coordinates of bounds
               INTEGER, INTENT(IN)            :: wtl               ! window description text length
               CHARACTER                      :: wtext             ! window description text
               INTEGER, INTENT(IN)            :: mtl               ! land data file name length
               CHARACTER                      :: mskfile           ! land data file name
               INTEGER, INTENT(IN)            :: tiff              ! screenshot?
               INTEGER, INTENT(IN)            :: initial_dimension ! dimension
               INTEGER, INTENT(in)            :: i_extremes        ! use extremes
               REAL, DIMENSION(3,*), OPTIONAL :: r_extremes        ! minimal and maximal coord and nodal values
               INTEGER                        :: ret
             END FUNCTION VisNET_GraphicsInit
          END INTERFACE
               

!          TYPE descr
!            SEQUENCE
!            INTEGER            :: l1,l2,l3
!            CHARACTER (LEN=32) :: s1,s2,s3
!          END TYPE
          
!          TYPE (descr)         :: BJG_descript

	END MODULE VisNET4Flash


