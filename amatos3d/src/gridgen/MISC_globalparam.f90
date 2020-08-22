!*****************************************************************
!
! MODULE NAME:
!	MISC_globalparam
! FUNCTION:
!	global parameter definitions
! CONTAINS:
!
! PUBLIC:
!	all
! COMMENTS:
!
! LIBRARIES:
!
! REFERENCES:
!
! VERSION(S):
!	1. original version		j. behrens	8/96
!	2. structures for parameters
!	   and command line information	j. behrens	11/96
!	3. change to rt_info		j. behrens	1/97
!	4. version 0.3 with parallel
!	   extensions			j. behrens	8/97
!	5. completely stripped for
!	   amatos version 1.0		j. behrens	11/00
!	6. bug fix for Solaris f90	j. behrens	01/01
!
!*****************************************************************
	MODULE MISC_globalparam
               	USE FEM_define

	IMPLICIT NONE


	PRIVATE
	PUBLIC :: grid_param, GRID_parameters

!---------- Program specification

	INTEGER (KIND = GRID_SI), PARAMETER                   :: io_fillen=48
	CHARACTER (len=io_fillen), PARAMETER :: prg_name= 'AMATOS3D                                        '

!---------- Author

	CHARACTER (len=io_fillen), PARAMETER :: aut_name= 'Jörn Behrens                                    '
	CHARACTER (len=io_fillen), PARAMETER :: aut_aff1= 'Munich University of Technology                 '
	CHARACTER (len=io_fillen), PARAMETER :: aut_aff2= 'Chair for Scientific Computing                  '
	CHARACTER (len=io_fillen), PARAMETER :: aut_aff3= 'D-80290 Munich, Germany                         '
	CHARACTER (len=io_fillen), PARAMETER :: aut_mail= 'behrens@mathematik.tu-muenchen.de               '

!---------- Version specification

	INTEGER (KIND = GRID_SI), PARAMETER :: i_mver= 0
	INTEGER (KIND = GRID_SI), PARAMETER :: i_sver= 1
	INTEGER (KIND = GRID_SI), PARAMETER :: i_pver= 0
	INTEGER (KIND = GRID_SI), PARAMETER :: i_month= 05
	INTEGER (KIND = GRID_SI), PARAMETER :: i_year= 2001

!---------- I/O Specifications

	INTEGER (KIND = GRID_SI), PARAMETER :: i_ioin= 5

!---------- Parameter data structure

	TYPE grid_param
	  INTEGER (KIND = GRID_SI)                   :: i_stringlength
	  CHARACTER (len=io_fillen) :: program_name
	  CHARACTER (len=io_fillen) :: author_name
	  CHARACTER (len=io_fillen) :: author_affil1
	  CHARACTER (len=io_fillen) :: author_affil2
	  CHARACTER (len=io_fillen) :: author_affil3
	  CHARACTER (len=io_fillen) :: author_email
	  INTEGER (KIND = GRID_SI)                   :: version
	  INTEGER (KIND = GRID_SI)                   :: subversion
	  INTEGER (KIND = GRID_SI)                   :: patchversion
	  INTEGER (KIND = GRID_SI)                   :: datemonth
	  INTEGER (KIND = GRID_SI)                   :: dateyear
	  INTEGER (KIND = GRID_SI)                   :: ioin
	  INTEGER (KIND = GRID_SI)                   :: ioout
	  INTEGER (KIND = GRID_SI)                   :: iolog
	END TYPE grid_param
	TYPE (grid_param)         :: GRID_parameters &
	& = grid_param(io_fillen, prg_name, aut_name, &
	  aut_aff1, aut_aff2, aut_aff3, aut_mail, i_mver, i_sver, i_pver, &
	  i_month, i_year, i_ioin, 6, -1)
	!DATA GRID_parameters /grid_param(io_fillen, prg_name, aut_name, &
	  !aut_aff1, aut_aff2, aut_aff3, aut_mail, i_mver, i_sver, i_pver, &
	  !i_month, i_year, i_ioin, 6, -1)/

	END MODULE MISC_globalparam
	
