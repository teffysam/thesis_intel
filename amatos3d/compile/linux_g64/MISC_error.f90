!*****************************************************************
!
! MODULE NAME:
!	MISC_error
! FUNCTION:
!	error handling routine and error messages
! CONTAINS:
!-----------------------------------------------------------------
!
! NAME:
!	print_error
! FUNCTION:
!	print out error or warning message and terminate if error
! SYNTAX:
!	call print_error(int)
! ON INPUT:
!	i_err:	error or warning index	integer
! ON OUTPUT:
!
! CALLS:
!
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!	init_error
! FUNCTION:
!	initialize error or warning messages
! SYNTAX:
!	call init_error
! ON INPUT:
!
! ON OUTPUT:
!
! CALLS:
!
! COMMENTS:
!
!-----------------------------------------------------------------
! PUBLIC:
!	print_error
! COMMENTS:
!	error code conventions:
!	    1 -  20 : Warnings (not terminating the program)
!	   21 -  22 : FEM_basis
!	   23 -  46 : FEM_create
!	   47 -  52 : FEM_gridgen
!	   53 -  86 : FEM_gridmanag
!	   87 -  95 : FEM_inittriang
!	   96 - 108 : FEM_saveset
!	  109 - 120 : GRID_api
!	  121 - 200 : Error (reserved)
!               999 : default (no error code given)
! USES:
!	MISC_globalparam, MISC_deferror
! LIBRARIES:
!
! REFERENCES:
!
! VERSION(S):
!	1. original version			j. behrens	9/96
!	2. error message includes routine name	j. behrens	7/97
!	3. updated, rewritten for amatos v.1.0	j. behrens	11/2000
!       4. error number and message optional    n. rakowsky     4/2001
!*****************************************************************
	MODULE MISC_error
	  USE MISC_globalparam
          USE FEM_define
	  PRIVATE
	  PUBLIC :: print_error
	  INTEGER (KIND = GRID_SI), PARAMETER                        :: i_textlen= 80
	  INTEGER (KIND = GRID_SI), PARAMETER                        :: i_ioerr=0
	  CONTAINS
!*****************************************************************
	SUBROUTINE print_error(i_err,a_err)

!---------- local declarations

	IMPLICIT NONE

	INTEGER (KIND = GRID_SI), INTENT(in), optional     :: i_err
	CHARACTER(*),intent(in), optional :: a_err

	CHARACTER(len=i_textlen)          :: a_text
        INTEGER (KIND = GRID_SI)                           :: i_iolog
        logical                           :: l_warning = .false.

        i_iolog = GRID_parameters%iolog

        if (present(a_err)) then
           a_text = a_err
           if (a_text(1:1) /= ':') a_text = ': '//a_err
        elseif (present(i_err)) then
           a_text = p_error(i_err)
        endif

!---------- error indices <= 20 are just warnings
        if (present(i_err)) l_warning = (i_err <= 20)

        fatal_error: IF(l_warning) THEN
           
           write(i_ioerr,2000) i_err, a_text
           IF(i_iolog > 0) THEN
              write(i_iolog,*) trim(GRID_parameters%program_name), ' ERROR HANDLING INVOKED!'
              write(i_iolog,2000) i_err, a_text
           END IF

!---------- error indices > 20 are errors that cause program termination
        ELSE fatal_error
           if (present(i_err)) then
              write(i_ioerr,2010) i_err, a_text
           else
              write(i_ioerr,2011) a_text
           endif
              
           IF(GRID_parameters%ioout > 6) THEN
              IF(i_iolog > 0) &
                   write(i_iolog,*) 'INFO: Prematurely closed file on unit: ', GRID_parameters%ioout
              close(GRID_parameters%ioout)
           END IF
           IF(i_iolog > 0) THEN
              write(i_iolog,*) trim(GRID_parameters%program_name), ' ERROR HANDLING INVOKED!'
              if (present(i_err)) then
                 write(i_iolog,2010) i_err, a_text
              else
                 write(i_iolog,2011) a_text
              endif
              write(i_iolog,*) 'INFO: Prematurely closed file on unit: ', i_iolog
              close(i_iolog)
           END IF
           STOP
              
        END IF fatal_error
     
	RETURN

!---------- formats

 2000	format(1x,'WARNING (',i3.3,')',a80)
 2010	format(1x,'ERROR   (',i3.3,')',a80,/,'... program STOPPED!')
 2011	format(1x,'ERROR ', a80,/,'... program STOPPED!')

	END SUBROUTINE print_error

!*****************************************************************

	FUNCTION p_error(i_err)

!---------- Warning messages

	IMPLICIT NONE
        integer                  :: i_err
        character(len=i_textlen) :: p_error

        set_error_messages: select case (i_err)

!---------- set error messages

        case default
           p_error = ': Sorry, no explanation available ...                                           '
        case (999)
           p_error = ': Sorry, no explanation available ...                                           '

	case(  1)
           p_error = ': DUMMY yeah!                                                                   '
	case( 21)
           p_error = ': [grid_linearbasis:89] Could not allocate vector for basis function            '
	case( 22)
           p_error = ': [grid_linearbasis:93] Vector lenght too short for desired order of accuracy   '
	case( 23)
           p_error = ': [create_elmt:364] Could not allocate element (need more memory?)              '
	case( 24)
           p_error = ': [create_edge:516] Could not allocate edge (need more memory?)                 '
	case( 25)
           p_error = ': [create_node:639] Could not allocate node (need more memory?)                 '
	case( 26)
           p_error = ': [hash_initalloc:955] Allocation of hash table failed (p_ehash)                '
	case( 27)
           p_error = ': [hash_initalloc:974] Allocation of hash table failed (p_ghash)                '
	case( 28)
           p_error = ': [hash_initalloc:993] Allocation of hash table failed (p_nhash)                '
	case( 29)
           p_error = ': [hash_initalloc:1012] Allocation of index list failed (i_egrid)               '
	case( 30)
           p_error = ': [hash_initalloc:1032] Allocation of index list failed (i_efine)               '
	case( 31)
           p_error = ': [hash_initalloc:1052] Allocation of index list failed (i_ggrid)               '
	case( 32)
           p_error = ': [hash_initalloc:1072] Allocation of index list failed (i_gfine)               '
	case( 33)
           p_error = ': [hash_initalloc:1092] Allocation of index list failed (i_gboun)               '
	case( 34)
           p_error = ': [hash_initalloc:1112] Allocation of index list failed (i_ngrid)               '
	case( 35)
           p_error = ': [hash_realloc:1172] Reallocation of hash table p_ehash failed, memory short   '
	case( 36)
           p_error = ': [hash_realloc:1215] Reallocation of hash table p_ghash failed, memory short   '
	case( 37)
           p_error = ': [hash_realloc:1258] Reallocation of hash table p_nhash failed, memory short   '
	case( 38)
           p_error = ': [hash_realloc:1301] Reallocation of ind. list i_egrid failed, memory short    '
	case( 39)
           p_error = ': [hash_realloc:1349] Reallocation of ind. list i_efine failed, memory short    '
	case( 40)
           p_error = ': [hash_realloc:1392] Reallocation of ind. list i_ggrid failed, memory short    '
	case( 41)
           p_error = ': [hash_realloc:1440] Reallocation of ind. list i_gfine failed, memory short    '
	case( 42)
           p_error = ': [hash_realloc:1483] Reallocation of ind. list i_gboun failed, memory short    '
	case( 43)
           p_error = ': [hash_realloc:1531] Reallocation of ind. list i_ngrid failed, memory short    '
	case( 44)
           p_error = ': [hash_pack:1613] Allocation of auxiliary temporary arrays failed              '
	case( 45)
           p_error = ': [hash_gridupdate:1807] Allocation of auxiliary temporary arrays failed        '
	case( 46)
           p_error = ': [renew_patches:2132] Too many elements in patch, change DEF_ndpatch           '
	case( 47)
           p_error = ': [refine_element:315] Periodic partner node not found                          '
	case( 48)
           p_error = ': [refine_element:441] Existing node not found                                  '
	case( 49)
           p_error = ': [coarse_edge:676] Coarsening node not found                                   '
	case( 50)
           p_error = ': [refine_grid:721] Allocation of auxiliary array failed, buy more memory       '
	case( 51)
           p_error = ': [check_resolvpatch:902] Periodic information must be provided                 '
	case( 52)
           p_error = ': [grid_globalrefine:1124] Allocation of auxiliary array failed                 '
	case( 53)
           p_error = ': [grid_extract:255] Inadequate array type for requested action (xcoo)          '
	case( 54)
           p_error = ': [grid_extract:267] Inadequate array type for requested action (ycoo)          '
	case( 55)
           p_error = ': [grid_extract:279] Inadequate array type for requested action (uval)          '
	case( 56)
           p_error = ': [grid_extract:291] Inadequate array type for requested action (vval)          '
	case( 57)
           p_error = ': [grid_extract:303] Inadequate array type for requested action (pval)          '
	case( 58)
           p_error = ': [grid_extract:315] Inadequate array type for requested action (vort)          '
	case( 59)
           p_error = ': [grid_extract:327] Inadequate array type for requested action (trac)          '
	case( 60)
           p_error = ': [grid_extract:339] Inadequate array type for requested action (nodi)          '
	case( 61)
           p_error = ': [grid_extract:354] Inadequate array type for requested action (nodx)          '
	case( 62)
           p_error = ': [grid_extract:369] Inadequate array type for requested action (nody)          '
	case( 63)
           p_error = ': [grid_extract:384] Inadequate array type for requested action (nodu)          '
	case( 64)
           p_error = ': [grid_extract:399] Inadequate array type for requested action (nodv)          '
	case( 65)
           p_error = ': [grid_extract:414] Inadequate array type for requested action (nodp)          '
	case( 66)
           p_error = ': [grid_extract:429] Inadequate array type for requested action (nodt)          '
	case( 67)
           p_error = ': [grid_extract:444] Inadequate array type for requested action (nodz)          '
	case( 68)
           p_error = ': [grid_extract:459] Inadequate array type for requested action (elfl)          '
	case( 69)
           p_error = ': [grid_extract:471] Inadequate array type for requested action (effl)          '
	case( 70)
           p_error = ': [grid_extract:483] Inadequate array type for requested action (elme)          '
	case( 71)
           p_error = ': [grid_extract:495] Inadequate array type for requested action (ellv)          '
	case( 72)
           p_error = ': [grid_extract:507] Inadequate array type for requested action (eflv)          '
	case( 73)
           p_error = ': [grid_extract:516] Action not supported for array extraction                  '
	case( 74)
           p_error = ': [grid_update:553] Inadequate array type for requested action (xcoo)           '
	case( 75)
           p_error = ': [grid_update:565] Inadequate array type for requested action (ycoo)           '
	case( 76)
           p_error = ': [grid_update:577] Inadequate array type for requested action (uval)           '
	case( 77)
           p_error = ': [grid_update:589] Inadequate array type for requested action (vval)           '
	case( 78)
           p_error = ': [grid_update:601] Inadequate array type for requested action (pval)           '
	case( 79)
           p_error = ': [grid_update:613] Inadequate array type for requested action (vort)           '
	case( 80)
           p_error = ': [grid_update:625] Inadequate array type for requested action (trac)           '
	case( 81)
           p_error = ': [grid_update:637] Inadequate array type for requested action (elfl)           '
	case( 82)
           p_error = ': [grid_update:649] Inadequate array type for requested action (effl)           '
	case( 83)
           p_error = ': [grid_update:661] Inadequate array type for requested action (elin)           '
	case( 84)
           p_error = ': [grid_update:673] Inadequate array type for requested action (egin)           '
	case( 85)
           p_error = ': [grid_update:685] Inadequate array type for requested action (ndin)           '
	case( 86)
           p_error = ': [grid_update:694] Action not supported for array update                       '
	case( 87)
           p_error = ': [grid_create:110] Could not open initial triangulation file                   '
	case( 88)
           p_error = ': [grid_create:136] Dimension does not match precompiled dimension setting      '
	case( 89)
           p_error = ': [grid_create:141] No. of nodes per edge does not match precompiled setting    '
	case( 90)
           p_error = ': [grid_create:147] Could not allocate coordinate array                         '
	case( 91)
           p_error = ': [grid_create:154] Could not allocate auxiliary edge arrays                    '
	case( 92)
           p_error = ': [grid_create:161] Could not allocate auxiliary element arrays                 '
	case( 93)
           p_error = ': [grid_create:172] Node index is larger than number of nodes                   '
	case( 94)
           p_error = ': [grid_create:177] Edge index is larger than number of edges                   '
	case( 95)
           p_error = ': [grid_create:182] Element index is larger than number of elements             '
	case( 96)
           p_error = ': [grid_wsaveset:106] Saveset file could not be opened for writing              '
	case( 97)
           p_error = ': [grid_rsaveset:205] Saveset file could not be opened for reading              '
	case( 98)
           p_error = ': [grid_rsaveset:258] Allocation of node failed                                 '
	case( 99)
           p_error = ': [grid_rsaveset:272] Allocation of edge failed                                 '
	case(100)
           p_error = ': [grid_rsaveset:258] Allocation of element failed                              '
	case(101)
           p_error = ': [grid_readdomain:340] Domain file could not be opened for reading             '
	case(102)
           p_error = ': [grid_readdomain:369] Dimension does not match precompiled dimension setting  '
	case(103)
           p_error = ': [grid_readdomain:377] Number of polylines zero or less!                       '
	case(104)
           p_error = ': [grid_readdomain:392] Vertex index array exists!                              '
	case(105)
           p_error = ': [grid_readdomain:396] Could not allocate vertex index array                   '
	case(106)
           p_error = ': [grid_readdomain:406] Number of polylines exeeds previously given number      '
	case(107)
           p_error = ': [grid_readdomain:416] Could not allocate coordinate array                     '
	case(108)
           p_error = ': [grid_readdomain:424] Number of coordinates exeeds previously given vertex no.'
	case(109)
           p_error = ': [grid_definegeometry:646] Could not allocate boundary polygon                 '
	case(110)
           p_error = ': [grid_createinitial:708] Illegal settings for ref/crs levels in mesh handle   '
	case(111)
           p_error = ': [grid_initialize:1432] Could not open log file for errors and warnings        '
	case(112)
           p_error = ': [grid_initialize:1453] Could not open file for redirected standard output     '
	case(113)
           p_error = ': [grid_adapt:1526] Allocation of auxiliary array failed, buy memory            '
	case(114)
           p_error = ': [grid_getpolyline:1603] Supplied array too short for the boundary polygon     '
	case(115)
           p_error = ': [grid_domaincheck:1861] Domain too complicated... Change i_maxsect            '
	case(116)
           p_error = ': [grid_createdual:1971] Allocation of auxiliary array failed                   '
	case(117)
           p_error = ': [grid_createdual:1979] Allocation of coordinate array failed                  '
	case(118)
           p_error = ': [grid_createdual:2023] Allocation of edges array failed                       '
	case(119)
           p_error = ': [grid_edgelength:2120] Allocation of auxiliary (coordinate) array failed      '
	case(120)
           p_error = ': [grid_edgelength:2151] Supplied array too short for the edge length array     '
	case(200) !200
           p_error = ': [grid_create] Could not open file, containing initial triangulation           '
	case(201) !201
           p_error = ': [grid_create] Dimension mismatch                                              '
	case(202) !202
           p_error = ': [grid_create] Allocation of temporary array failed                            '
	case(203) !203
           p_error = ': [grid_create] Number of given nodes exceeds the initially given number        '
	case(204) !204
           p_error = ': [grid_create] Number of given edges exceeds the initially given number        '
	case(205) !205
           p_error = ': [grid_create] Number of given elements exceeds the initially given number     '
	case(206) !206
           p_error = ': [grid_create] Number of given tetrahedrons exceeds the initially given number '

        end select set_error_messages
	
      END FUNCTION p_error

	END MODULE MISC_error
