!*****************************************************************
!
! MODULE NAME:
!	MISC_system
! FUNCTION:
!	contains system dependent functions (e.g. command line io)
! CONTAINS:
!-----------------------------------------------------------------
!
! NAME:
!	sys_getcmdargs
! FUNCTION:
!	same as getarg (read command line option)
! SYNTAX:
!	call sys_getcmdargs(int, char)
! ON INPUT:
!	i_pos:	position of command line option		integer
! ON OUTPUT:
!	c_opt:	option read from command line		character string
! CALLS:
!
! COMMENTS:
!	i_pos=0 returns the command itself
!-----------------------------------------------------------------
!
! NAME:
!	sys_numcmdargs
! FUNCTION:
!	same as iargc (tell number of command line options)
! SYNTAX:
!	int= sys_numcmdargs
! ON INPUT:
!
! ON OUTPUT:
!	i_argnum: number of command line items		integer
! CALLS:
!
! COMMENTS:
!	i_argnum excludes the command itself, thus i_argnum=0
!	indicates that there are no command line options given.
!-----------------------------------------------------------------
!
! PUBLIC:
!	sys_getcmdargs, sys_numcmdargs
! COMMENTS:
!	this is for the CRAY CF90 compiler,
!	supporting POSIX ipxfargc and pxfgetarg
! USES:
!
! LIBRARIES:
!
! REFERENCES:
!
! VERSION(S):
!	1. original version	j. behrens	1/97
!
!*****************************************************************
	MODULE MISC_system
	  PRIVATE
	  PUBLIC :: sys_getcmdargs, sys_numcmdargs
	  CONTAINS
!*****************************************************************
	  SUBROUTINE sys_getcmdargs(i_pos, c_opt, i_len)

!---------- local declarations

	  IMPLICIT NONE

	  INTEGER, INTENT(in) :: i_pos
	  CHARACTER (len=80), INTENT(out) :: c_opt
	  INTEGER, OPTIONAL               :: i_len
	  INTEGER                         :: i_err
	  EXTERNAL pxfgetarg

!---------- this uses a POSIX routine. no error code is checked, no matching
!---------- of length is checked!!!

	  CALL pxfgetarg(i_pos, c_opt, i_len, i_err)

	  RETURN
	  END SUBROUTINE sys_getcmdargs
!*****************************************************************
	  FUNCTION sys_numcmdargs() RESULT(i_argnum)

!---------- local declarations

	  IMPLICIT NONE

	  INTEGER :: i_argnum
	  EXTERNAL   ipxfargc
	  INTEGER :: ipxfargc

	  i_argnum= ipxfargc()

	  RETURN
	  END FUNCTION sys_numcmdargs
	END MODULE MISC_system
