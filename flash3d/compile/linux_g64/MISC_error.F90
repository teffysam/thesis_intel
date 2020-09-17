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
	  USE GRID_api
	  PRIVATE
	  PUBLIC :: print_error
	  INTEGER, PARAMETER                        :: i_textlen= 80
	  INTEGER, PARAMETER                        :: i_ioerr=0
	  CONTAINS
!*****************************************************************
	SUBROUTINE print_error(i_err,a_err)

!---------- local declarations

	IMPLICIT NONE

	INTEGER, INTENT(in), optional     :: i_err
	CHARACTER(*),intent(in), optional :: a_err

	CHARACTER(len=i_textlen)          :: a_text
        INTEGER                           :: i_iolog
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
	case( 11)
           p_error = ': [plot_gmv:103] Could not allocate r_cox                                       '
	case( 12)
           p_error = ': [plot_gmv:120] Could not allocate i_tets                                      '
	case( 13)
           p_error = ': [plot_gmv:134] Could not allocate i_ids                                       '
	case( 14)
           p_error = ': [plot_gmv:148] Could not allocate i_ids                                       '
	case( 15)
           p_error = ': [plot_gmv:161] Could not allocate r_ttmp                                      '
	case( 16)
           p_error = ': [plot_matlab:120] Could not allocate r_xytmp                                  '
	case( 17)
           p_error = ': [plot_matlab:129] Could not allocate i_val                                    '
	case( 18)
           p_error = ': [plot_matlab:136] Could not allocate i_val                                    '
	case( 19)
           p_error = ': [plot_matlab:143] Could not allocate i_val                                    '
	case( 20)
           p_error = ': [io_getinterinput:463] No values found in input ... oops                      '
	case( 21)
           p_error = ': [io_getbatchinput:560] Could not open File, perhaps wrong filename?           '
	case( 22)
           p_error = ': [io_getbatchinput:660] No values found in input file ... oops                 '
	case( 23)
           p_error = ': [io_putinputfile:870] Could not open file for writing parameters              '
	case( 30)
           p_error = ': [slm_windfield:103] Could not open Wind file for reading                      '
	case( 31)
           p_error = ': [slm_windinit:172] Could not open Wind parameters file                        '
	case( 32)
           p_error = ': [slm_windinit:230] Could not allocate work space for wind data arrays         '
	case( 33)
           p_error = ': [poly_1d:660] Requested polynomial order exceeds maximum order                '
	case( 34)
           p_error = ': [poly_1d:709] Numerical Error in poly_1d (div. by zero)                       '
	case( 35)
           p_error = ': [slm_adapt:342] Could not allocate work array                                 '
	case( 36)
           p_error = ': [slm_diagnostics:447] Could not open diagnostics output file                  '
	case( 37)
           p_error = ': [slm_diagnostics:458] Could not allocate temporary workspace                  '
	case( 38)
           p_error = ': [slm_timestepping:877] Could not allocate temporary workspace                 '
	case( 39)
           p_error = ': [slm_upstream:344]  Could not allocate temporary workspace                    '
	case( 40)
           p_error = ': [slm_timestepping:1021] Failed to allocate workspace for dual mesh            '
	case( 41)
           p_error = ': [bjg_plot:185f] Could not allocate array for visualization initialization     '
	case( 42)
           p_error = ': [bjg_plot:221] Initialization of BJUGL Plotting failed                        '
	case( 43)
           p_error = ': [bjg_plot:274f] Could not allocate array for online visualization             '
	case( 44)
           p_error = ': [bjg_plot:395] Finalizing visualization failed                                '
	case( 45)
           p_error = ': [plot_gridded:91] Could not allocate coordinate array                         '
	case( 46)
           p_error = ': [plot_matlab:122] Failed to allocate data array for matlab compatible plotting'
	case( 47)
           p_error = ': [plot_netcdf:172] Failed to allocate data array for netcdf compatible plotting'
	case( 48)
           p_error = ': [slm_errorest:110] Array size incompatible with no. of elements in mesh       '
	case( 49)
           p_error = ': [slm_errorest:115] Could not allocate temorary arrays                         '
	case( 50)
           p_error = ': [slm_initialvalues:91] Could not allocate arrays                              '
	case( 51)
           p_error = ': [slm_initbar:153] Failed to allocate arrays                                   '
	case( 52)
           p_error = ': [slm_analyticsolution:248] Dimension mismatch                                 '
	case( 53)
           p_error = ': [slm_analyticsolution:253] Could not allocate arrays                          '
	case( 54)
           p_error = ': [slm_initslot:403] Failed to allocate arrays                                  '
	case( 55)
           p_error = ': [slm_initcylndr:461] Failed to allocate arrays                                '
	case( 56)
           p_error = ': [slm_initialvalues:199] Could not open file                                   '
	case( 57)
           p_error = ': [slm_initialvalues:230] Input file inconsistent                               '
	case( 58)
           p_error = ': [slm_initialvalues:236] Array allocation failed                               '
	case( 59)
           p_error = ': [slm_initm3:303] Array allocation failed                                      '
	case( 60)
           p_error = ': [slm_interpolate:103] Array allocation failed                                 '
	case( 61)
           p_error = ': [slm_interpolinit:312] Array allocation failed                                '

        end select set_error_messages
	
      END FUNCTION p_error

	END MODULE MISC_error
