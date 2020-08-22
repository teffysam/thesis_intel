!*****************************************************************
!
! MODULE NAME:
!	IO_utils
! FUNCTION:
!	input/output routines (io_get...something, io_put...something)
! CONTAINS:
!-----------------------------------------------------------------
!
! NAME:
!	io_getcmdline
! FUNCTION:
!	read options from command line
! SYNTAX:
!	call io_getcmdline(param)
! ON INPUT:
!
! ON OUTPUT:
!	p_param: control parameters			TYPE(control_struct)
! CALLS:
!
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!	io_getinterinput
! FUNCTION:
!	get user input interactively
! SYNTAX:
!	call io_getinterinput(param)
! ON INPUT:
!
! ON OUTPUT:
!	p_param: control parameters			TYPE(control_struct)
! CALLS:
!
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!	io_getbatchinput
! FUNCTION:
!	read user input from file
! SYNTAX:
!	call io_getbatchinput(param)
! ON INPUT:
!
! ON OUTPUT:
!	p_param: control parameters			TYPE(control_struct)
! CALLS:
!
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!	io_putparameters
! FUNCTION:
!	write out parameters in a nice way
! SYNTAX:
!	call io_putparameters(param)
! ON INPUT:
!	p_param: data structure containing parameters	TYPE(control_struct)
! ON OUTPUT:
!	
! CALLS:
!
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!	io_putruntimeinfo
! FUNCTION:
!	print some information on the current run time status
! SYNTAX:
!	call io_putruntimeinfo(grid, info)
! ON INPUT:
!	p_ghand: grid handle for no. of elements...	TYPE(grid_handle)
!	p_info:  structure containing other info	TYPE(rt_info)
! ON OUTPUT:
!	p_info:  structure reseted			TYPE(rt_info)
! CALLS:
!
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!	io_putinputfile
! FUNCTION:
!	print an input file conforming to io_getbatchinput
! SYNTAX:
!	call io_putinputfile(param)
! ON INPUT:
!	p_param: global parameter data structure	TYPE(control_struct)
! ON OUTPUT:
!
! CALLS:
!
! COMMENTS:
!
!-----------------------------------------------------------------
!
! PUBLIC:
!	io_getcmdline, io_getinterinput, io_getbatchinput,
!	io_putparameters, io_putruntimeinfo, io_putinputfile
! COMMENTS:
!
! USES:
!	MISC_globalparam, MISC_error, MISC_timing, IO_plotdefine,
!	FEM_handle
! LIBRARIES:
!
! REFERENCES:
!
! VERSION(S):
!	1. original version			j. behrens	7/96
!	2. largely extended			j. behrens	11/96
!	3. changed command line io		j. behrens	1/97
!	4. changed runtime output (adaptations)	j. behrens	1/97
!	5. control data struct, less command io	j. behrens	12/97
!	6. tiff file plotting included		j. behrens	1/98
!	7. adapted to BJuGL			j. behrens	1/2000
!	8. compliant to amatos 1.0		j. behrens	12/2000
!
!*****************************************************************
	MODULE IO_utils
	  USE FLASH_parameters
	  USE MISC_timing
	  USE MISC_system
	  USE GRID_api
	  PRIVATE
	  INTEGER, PARAMETER :: i_ioerr=0
	  PUBLIC :: io_getcmdline, io_getinterinput, io_getbatchinput, &
	    io_putparameters, io_putruntimeinfo, io_putinputfile, io_initparams
	  CONTAINS
!*****************************************************************
	SUBROUTINE io_getcmdline(p_cmd)

!---------- local declarations

	IMPLICIT NONE

	TYPE (control_struct), INTENT(out) :: p_cmd

	LOGICAL                            :: l_ict
	LOGICAL                            :: l_bat
	LOGICAL                            :: l_mtl
	LOGICAL                            :: l_dia
	CHARACTER (len=io_fillen)          :: a_infln
	INTEGER                            :: numargs
	INTEGER                            :: i= 1
	INTEGER                            :: i_fst
	LOGICAL                            :: help= .true.
	LOGICAL                            :: shoversion= .false.
	CHARACTER (len=2)                  :: option
	CHARACTER (len=15)                 :: comdnam
	CHARACTER (len=32)                 :: c_file
	CHARACTER (len=80)                 :: c_dummy

!---------- initialize output variables

	a_infln= 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
	l_ict= .false.
        l_bat= .false.
	l_mtl= .false.
	l_dia= .false.

!---------- read number commandline arguments
!           this is possibly non standard f90, but definitely quasi standard

	numargs= sys_numcmdargs()
	CALL sys_getcmdargs(0,c_dummy,i_len=len(c_dummy))
	comdnam= c_dummy(1:15)
	check_args: IF(numargs < 1) THEN
	  GOTO 100 ! print_help
	ELSE check_args

!---------- read command line arguments one by one

	  DO WHILE (i <= numargs)
	    CALL sys_getcmdargs(i,c_dummy,i_len=len(c_dummy))
	    option= c_dummy(1:2)

!---------- select the CASEs for command line options

	    eval_option: SELECT CASE (option)
	      CASE('-h') eval_option !--- request for help ---
	        help= .true.
	        i= i+1
	      CASE('-?') eval_option !--- request for help ---
	        help= .true.
	        i= i+1
	      CASE('-r') eval_option !--- print release information ---
	        shoversion= .true.
	        i= i+1
	      CASE('-i') eval_option !--- select interactive input mode ---
	        help= .false.
	        l_ict= .true.
	        i= i+1
	      CASE('-d') eval_option !--- switch on diagnostics ---
	        help= .false.
	        l_dia= .true.
	        i= i+1
	      CASE('-l') eval_option !--- switch on logging ---
	        help= .false.
	        p_cmd%cmd%l_logging= .true.
	        i= i+1
	      CASE('-o') eval_option !--- redirect output into file ---
	        help= .false.
	        p_cmd%cmd%l_output= .true.
	        i= i+1
	      CASE('-b') eval_option !--- select batch input mode ---
	        help= .false.
	        l_bat= .true.
	        IF(a_infln == 'xxxxxxxxxxxxxxxxxxxx') &
	          a_infln= 'Parameters.in' !--- default input file name ---
	        i= i+1
	      CASE('-f') eval_option !--- supply input file name ---
	        i= i+1
	        CALL sys_getcmdargs(i,c_dummy,i_len=len(c_dummy))
	        a_infln= c_dummy(1:io_fillen)
	        IF(a_infln(1:1) == '-') THEN  !--- check correctnes of file name ---
	          help= .true.
	          GOTO 100 ! print_help
	        ELSE
	          i= i+1
	        END IF
	      CASE DEFAULT eval_option !--- default CASE: show help ---
	        help= .true.
	        GOTO 100 ! print_help
	    END SELECT eval_option
	  END DO
	END IF check_args

!---------- update output structure

	p_cmd%cmd%c_batchfile = a_infln
	p_cmd%cmd%l_interactive= l_ict
	p_cmd%cmd%l_diagnostics= l_dia
        p_cmd%cmd%l_batchmode= l_bat

!---------- print help information

 100	print_help: IF(help) THEN
	  IF(shoversion) THEN
	    write(GRID_parameters%ioout,1001) GRID_parameters%program_name, GRID_parameters%version, GRID_parameters%subversion, &
	                        GRID_parameters%patchversion, GRID_parameters%datemonth, GRID_parameters%dateyear, &
	                        GRID_parameters%author_name, GRID_parameters%author_email, GRID_parameters%author_affil1, &
	                        GRID_parameters%author_affil2, GRID_parameters%author_affil3
	    write(GRID_parameters%ioout,1002) comdnam
	    write(i_ioerr,*) 'STOPPED ... this is all I can say'
	    STOP
	  ELSE
	    write(GRID_parameters%ioout,1010) comdnam
	    write(GRID_parameters%ioout,1011) GRID_parameters%author_name
	    write(i_ioerr,*) 'STOPPED ... hope this made it clear'
	    STOP
	  END IF
	END IF print_help

!---------- print version information

	print_version: IF(shoversion) THEN
	  write(GRID_parameters%ioout,1001) GRID_parameters%program_name, GRID_parameters%version, GRID_parameters%subversion, &
	                        GRID_parameters%patchversion, GRID_parameters%datemonth, GRID_parameters%dateyear, &
	                        GRID_parameters%author_name, GRID_parameters%author_email, GRID_parameters%author_affil1, &
	                        GRID_parameters%author_affil2, GRID_parameters%author_affil3
	  IF(GRID_parameters%iolog > 0) &
	    write(GRID_parameters%iolog,1001) GRID_parameters%program_name, GRID_parameters%version, GRID_parameters%subversion, &
	                        GRID_parameters%patchversion, GRID_parameters%datemonth, GRID_parameters%dateyear, &
	                        GRID_parameters%author_name, GRID_parameters%author_email, GRID_parameters%author_affil1, &
	                        GRID_parameters%author_affil2, GRID_parameters%author_affil3
	END IF print_version

	RETURN

 1001	FORMAT(1x,'***** ***** ***** ***** ***** ***** ***** ***** ***** *****',/ &
	       1x,'***** PROGRAM: ',a15,24x,'*****',/ &
	       1x,'***** VERSION: ',i2.2,'.',i2.2,'.',i2.2,31x,'*****',/ &
	       1x,'***** DATE:    ',i2.2,'/',i4.4,32x,'*****',/ &
	       1x,'***** AUTHOR:  ',a12,27x,'*****',/ &
	       1x,'***** E-MAIL:  ',a39,'*****',/ &
	       1x,'***** ADDRESS: ',a39,'*****',/ &
	       1x,'*****          ',a39,'*****',/ &
	       1x,'*****          ',a39,'*****',/ &
	       1x,'***** ***** ***** ***** ***** ***** ***** ***** ***** *****')
 1002	FORMAT(1x,'***** TYPE ',a15,' -h, to get help',12x,'*****',/ &
	       1x,'***** ***** ***** ***** ***** ***** ***** ***** ***** *****')
 1010	FORMAT(1x,'USAGE: ',a15,' [-i|-b] {-d} {-f name} {-h} {-l} {-o} {-r}',/ &
	       1x,'       -i: interactive input mode',/ &
	       1x,'       -b: batch input mode (read from file)',/ &
	       1x,'       -d: switch on diagnostics',/ &
	       1x,'       -f: input filename is << name >>',/ &
	       1x,'       -h: help information (this output)',/ &
	       1x,'       -l: switch on log file output',/ &
	       1x,'       -o: redirect standard output to a file',/ &
	       1x,'       -r: release information')
 1011	FORMAT(1x,'Copyright (c) 1996, 1997, 1998, ',a13)
	END SUBROUTINE io_getcmdline
!*****************************************************************
	SUBROUTINE io_initparams(p_param)
	
!---------- local declarations

	IMPLICIT NONE

	TYPE (control_struct), INTENT(out) :: p_param

!---------- initialize

	p_param%phy%r_deltatime     = -1.0
	p_param%phy%r_reftolerance  = -1.0
	p_param%phy%r_crstolerance  = -1.0
	p_param%phy%r_refwatermark  = -1.0
	p_param%phy%r_crswatermark  = -1.0
	p_param%phy%i_experiment    = -1
	p_param%phy%i_crslevel      = -1
	p_param%phy%i_reflevel      = -1
	p_param%phy%i_frsttimestep  = -1
	p_param%phy%i_lasttimestep  = -1
	p_param%phy%i_adviterations = -1
	p_param%io%i_plotoffset     = -1
	p_param%io%i_saveoffset     = -1
	p_param%io%i_savelast       = -1
	p_param%io%l_matlab         = .FALSE.
	p_param%io%l_gmv            = .FALSE.
!-- BEGIN added for visnetplot [flo]
	p_param%io%l_visnet         = .FALSE.
	p_param%io%l_visnet_tiff    = .FALSE.
!-- END
	p_param%io%c_domainfile     = 'Domain.dat'
	p_param%io%c_triangfile     = 'Triang.dat'
	p_param%io%c_windfile       = 'Flow.dat'

	END SUBROUTINE io_initparams
!*****************************************************************
	SUBROUTINE io_getinterinput(p_param)

!---------- local declarations

	IMPLICIT NONE

	TYPE (control_struct), INTENT(out) :: p_param
	INTEGER                            :: i_iost, i_tmp, i_cln
	CHARACTER (len=io_fillen)          :: c_tmp

!---------- initialize

	CALL io_initparams(p_param)
	i_cln= MAX(GRID_parameters%i_stringlength,io_fillen)

!---------- prompt user for input (loop in case of wrong input)
        
	write(GRID_parameters%ioout,1000) GRID_parameters%program_name, GRID_parameters%version, &
	                    GRID_parameters%subversion, GRID_parameters%patchversion

!---------- Experiment Control

	write(GRID_parameters%ioout,1001)
	write(GRID_parameters%ioout,1010)
	write(GRID_parameters%ioout,1011,advance='NO')
	read(*,*) p_param%phy%i_experiment

!---------- Adaptivity Control

	write(GRID_parameters%ioout,1001)
	write(GRID_parameters%ioout,1020)
	write(GRID_parameters%ioout,1021,advance='NO')
	read(*,*) p_param%phy%i_reflevel
	write(GRID_parameters%ioout,1022,advance='NO')
	read(*,*) p_param%phy%i_crslevel
	write(GRID_parameters%ioout,1023,advance='NO')
	read(*,*) p_param%phy%r_reftolerance
	write(GRID_parameters%ioout,1024,advance='NO')
	read(*,*) p_param%phy%r_crstolerance
	write(GRID_parameters%ioout,1025,advance='NO')
	read(*,*) p_param%phy%r_refwatermark
	write(GRID_parameters%ioout,1026,advance='NO')
	read(*,*) p_param%phy%r_crswatermark

!---------- Time Step Control

	write(GRID_parameters%ioout,1001)
	write(GRID_parameters%ioout,1030)
	write(GRID_parameters%ioout,1031,advance='NO')
	read(*,*) p_param%phy%r_deltatime
	write(GRID_parameters%ioout,1032,advance='NO')
	read(*,*) p_param%phy%i_frsttimestep
	write(GRID_parameters%ioout,1033,advance='NO')
	read(*,*) p_param%phy%i_lasttimestep

!---------- Output Control

	write(GRID_parameters%ioout,1001)
	write(GRID_parameters%ioout,1040)
	write(GRID_parameters%ioout,1041,advance='NO')
	read(*,*) p_param%io%i_saveoffset
	write(GRID_parameters%ioout,1043,advance='NO')
	read(*,*) p_param%io%i_savelast
	write(GRID_parameters%ioout,1042,advance='NO')
	read(*,*) p_param%io%i_plotoffset
	write(GRID_parameters%ioout,1049,advance='NO')
	read(*,*) i_tmp
	IF(i_tmp /= 0) THEN
	  p_param%io%l_matlab= .True.
	END IF
	write(GRID_parameters%ioout,10491,advance='NO')
	read(*,*) i_tmp
	IF(i_tmp /= 0) THEN
	  p_param%io%l_gmv= .True.
	END IF
        !-- BEGIN added for visnetplot [flo]
	write(GRID_parameters%ioout,10492,advance='NO')
	read(*,*) i_tmp
	IF(i_tmp /= 0) THEN
	  p_param%io%l_visnet= .True.
	END IF
	write(GRID_parameters%ioout,10493,advance='NO')
	read(*,*) i_tmp
	IF(i_tmp /= 0) THEN
	  p_param%io%l_visnet_tiff= .True.
	END IF
        !-- END
	write(GRID_parameters%ioout,1047,advance='NO')
	read(*,2000,iostat=i_iost) c_tmp
	IF(i_iost == 0) THEN
	  p_param%io%c_triangfile(1:i_cln)= c_tmp(1:i_cln)
	END IF

!---------- Iteration Control

	write(GRID_parameters%ioout,1001)
	write(GRID_parameters%ioout,1050)
	write(GRID_parameters%ioout,1051,advance='NO')
	read(*,*) p_param%phy%i_adviterations
	write(GRID_parameters%ioout,1002)

!---------- error handling

	no_value: IF((p_param%phy%r_deltatime     < 0.0) .OR. &
	             (p_param%phy%r_reftolerance  < 0.0) .OR. &
	             (p_param%phy%r_crstolerance  < 0.0) .OR. &
	             (p_param%phy%r_refwatermark  < 0.0) .OR. &
	             (p_param%phy%r_crswatermark  < 0.0) .OR. &
	             (p_param%phy%i_experiment    < 0)   .OR. &
	             (p_param%phy%i_crslevel      < 0)   .OR. &
	             (p_param%phy%i_reflevel      < 0)   .OR. &
	             (p_param%phy%i_frsttimestep  < 0)   .OR. &
	             (p_param%phy%i_lasttimestep  < 0)   .OR. &
	             (p_param%io%i_plotoffset     < 0)   .OR. &
	             (p_param%io%i_saveoffset     < 0)   .OR. &
	             (p_param%phy%i_adviterations < 0)) THEN
	  CALL grid_error(20)
	END IF no_value
	RETURN

 1000	FORMAT(1x,'***** ***** ***** ***** ***** ***** ***** ***** ***** *****',/ &
	       1x,'***** PROGRAM: ',a15,24x,'*****',/ &
	       1x,'***** VERSION: ',i2.2,'.',i2.2,'.',i2.2,31x,'*****',/ &
	       1x,'*****          Started in INTERACTIVE input mode      *****',/ &
	       1x,'***** ***** ***** ***** ***** ***** ***** ***** ***** *****',/)
 1001	FORMAT(1x,'-----------------------------------------------------------',/)
 1002	FORMAT(1x,'---------------------- end of input -----------------------',/)
 1010	FORMAT(1x,'                        Experiment')
 1020	FORMAT(1x,'                    Adaptivity Control')
 1030	FORMAT(1x,'                     Timestep Control')
 1040	FORMAT(1x,'                   Input/Output Control')
 1050	FORMAT(1x,'                    Iteration Control')
 1011	FORMAT(1x,'INPUT: Experiment No. (first exp. = 0)              > ')
 1021	FORMAT(1x,'INPUT: Finest level of refinement                   > ')
 1022	FORMAT(1x,'INPUT: Coarsest level of refinement                 > ')
 1023	FORMAT(1x,'INPUT: Tolerance for refinement (|t_r| < 1)         > ')
 1024	FORMAT(1x,'INPUT: Tolerance for Coarsening (t_c < t_r)         > ')
 1025	FORMAT(1x,'INPUT: Watermark for refinement (|w_r| < 1)         > ')
 1026	FORMAT(1x,'INPUT: Watermark for coarsening (|w_c| < 1)         > ')
 1031	FORMAT(1x,'INPUT: Timestep length (delta t)                    > ')
 1032	FORMAT(1x,'INPUT: First timestep number                        > ')
 1033	FORMAT(1x,'INPUT: Last timestep number                         > ')
 1041	FORMAT(1x,'INPUT: Number of timesteps between saves            > ')
 1042	FORMAT(1x,'INPUT: Number of timesteps between plots            > ')
 1043	FORMAT(1x,'INPUT: Save last step for next experiment (no = 0)  > ')
 1045	FORMAT(1x,'INPUT: Plot polygonal outline (no = 0)              > ')
 1046	FORMAT(1x,'INPUT: Read wind data from file (no = 0)            > ')
 10461	FORMAT(1x,'INPUT: Filename for wind data       (Flow.dat)      > ')
 1047	FORMAT(1x,'INPUT: Filename for triangulation   (Triang.dat)    > ')
 1049	FORMAT(1x,'INPUT: Write matlab compatible output file (no = 0) > ')
 10491	FORMAT(1x,'INPUT: Write gmv compatible output file (no = 0)    > ')
 !-- BEGIN added for visnetplot [flo]:
 10492  FORMAT(1x,'INPUT: Plot to visnet window (no = 0)               > ')
 10493 FORMAT(1x,'INPUT: Make visnet screenshots (no = 0)             > ')
 !-- END
 1051	FORMAT(1x,'INPUT: Iterations in trajectory estimation          > ')

 2000	FORMAT(a32)

	END SUBROUTINE io_getinterinput
!*****************************************************************
	SUBROUTINE io_getbatchinput(p_param)

!---------- local declarations

	IMPLICIT NONE

	TYPE (control_struct), INTENT(out)         :: p_param
	INTEGER, PARAMETER                         :: i_iofil= 10
	CHARACTER (len=80)                         :: a_filrow
	CHARACTER (len=io_fillen)                  :: c_tmp
	INTEGER                                    :: i_iost, i_ioend, &
	  i_tmp, i_cln

!---------- initialize

	i_cln= MAX(32,io_fillen)

!---------- open input file

	open(unit= i_iofil, file= p_param%cmd%c_batchfile, status= 'OLD', &
	     action= 'READ', iostat= i_iost)
	file_notopen: IF(i_iost /= 0) THEN
	  write(i_ioerr,*) 'ERROR: Filename: ', p_param%cmd%c_batchfile
	  IF(GRID_parameters%iolog > 0) &
	    write(GRID_parameters%iolog,*) 'ERROR: Filename: ', p_param%cmd%c_batchfile
	  CALL grid_error(21)
	ELSE file_notopen
	  write(GRID_parameters%ioout,1000) GRID_parameters%program_name, GRID_parameters%version, GRID_parameters%subversion, &
	                      GRID_parameters%patchversion, p_param%cmd%c_batchfile
	  IF(GRID_parameters%iolog > 0) THEN
	    write(GRID_parameters%iolog,*) 'INFO: Filename: ', p_param%cmd%c_batchfile, ' opened on unit: ', i_iofil
	    write(GRID_parameters%iolog,1000) GRID_parameters%program_name, GRID_parameters%version, GRID_parameters%subversion, &
	                        GRID_parameters%patchversion, p_param%cmd%c_batchfile
	  END IF
	END IF file_notopen

!---------- read line by line

	read_loop: DO
	  read(i_iofil,2000,iostat=i_ioend) a_filrow

!---------- if file ended

	  file_end: IF(i_ioend /= 0) THEN
	    close(i_iofil)
	    IF(GRID_parameters%iolog > 0) &
	      write(GRID_parameters%iolog,*) 'INFO: Closed file on unit: ', i_iofil
	    EXIT
	  ELSE file_end

!---------- decide what to DO with line according to first character

	    comment_line: IF(a_filrow(1:1) == '#' .or. a_filrow(1:1) == '!') THEN
	      CYCLE read_loop
	    ELSE IF(a_filrow(1:14) == 'EXPERIMENT_NUM') THEN comment_line
	      read(i_iofil,*) p_param%phy%i_experiment
	    ELSE IF(a_filrow(1:14) == 'FINE_GRID_LEVE') THEN comment_line
	      read(i_iofil,*) p_param%phy%i_reflevel
	    ELSE IF(a_filrow(1:14) == 'COARSE_GRID_LE') THEN comment_line
	      read(i_iofil,*) p_param%phy%i_crslevel
	    ELSE IF(a_filrow(1:14) == 'TOLERANCE_OF_R') THEN comment_line
	      read(i_iofil,*) p_param%phy%r_reftolerance
	    ELSE IF(a_filrow(1:14) == 'TOLERANCE_OF_C') THEN comment_line
	      read(i_iofil,*) p_param%phy%r_crstolerance
	    ELSE IF(a_filrow(1:14) == 'WATERMARK_OF_R') THEN comment_line
	      read(i_iofil,*) p_param%phy%r_refwatermark
	    ELSE IF(a_filrow(1:14) == 'WATERMARK_OF_C') THEN comment_line
	      read(i_iofil,*) p_param%phy%r_crswatermark
	    ELSE IF(a_filrow(1:14) == 'TIMESTEP_LENGT') THEN comment_line
	      read(i_iofil,*) p_param%phy%r_deltatime
	    ELSE IF(a_filrow(1:14) == 'BEGINNING_TIME') THEN comment_line
	      read(i_iofil,*) p_param%phy%i_frsttimestep
	    ELSE IF(a_filrow(1:14) == 'FINISHING_TIME') THEN comment_line
	      read(i_iofil,*) p_param%phy%i_lasttimestep
	    ELSE IF(a_filrow(1:14) == 'STEPS_BTW_PLOT') THEN comment_line
	      read(i_iofil,*) p_param%io%i_plotoffset
	    ELSE IF(a_filrow(1:14) == 'MATLAB_PLOTTIN') THEN comment_line
	      read(i_iofil,*) i_tmp
	      IF(i_tmp /= 0) p_param%io%l_matlab= .TRUE.
	    ELSE IF(a_filrow(1:14) == 'GMV_FILE_PLOTT') THEN comment_line
	      read(i_iofil,*) i_tmp
	      IF(i_tmp /= 0) p_param%io%l_gmv= .TRUE.
            !-- BEGIN: added for visnetplot [flo]
	    ELSE IF(a_filrow(1:14) == 'VISNET_PLOTTIN') THEN comment_line
	      read(i_iofil,*) i_tmp
	      IF(i_tmp /= 0) p_param%io%l_visnet= .TRUE.
	    ELSE IF(a_filrow(1:14) == 'VISNET_SCREENS') THEN comment_line
	      read(i_iofil,*) i_tmp
	      IF(i_tmp /= 0) p_param%io%l_visnet_tiff= .TRUE.
            !-- END
	    ELSE IF(a_filrow(1:14) == 'STEPS_BTW_SAVE') THEN comment_line
	      read(i_iofil,*) p_param%io%i_saveoffset
	    ELSE IF(a_filrow(1:14) == 'SAVE_FINISH_CO') THEN comment_line
	      read(i_iofil,*) p_param%io%i_savelast
	    ELSE IF(a_filrow(1:14) == 'WIND_FILE_NAME') THEN comment_line
	      read(i_iofil,2010,iostat=i_tmp) c_tmp
	      IF(i_tmp == 0) p_param%io%c_windfile(1:i_cln)= c_tmp(1:i_cln)
	    ELSE IF(a_filrow(1:14) == 'DOMAIN_FILE_NA') THEN comment_line
	      read(i_iofil,2010,iostat=i_tmp) c_tmp
	      IF(i_tmp == 0) p_param%io%c_domainfile(1:i_cln)= c_tmp(1:i_cln)
	    ELSE IF(a_filrow(1:14) == 'TRIANG_FILE_NA') THEN comment_line
	      read(i_iofil,2010,iostat=i_tmp) c_tmp
	      IF(i_tmp == 0) p_param%io%c_triangfile(1:i_cln)= c_tmp(1:i_cln)
	    ELSE IF(a_filrow(1:14) == 'SLM_ITERATION_') THEN comment_line
	      read(i_iofil,*) p_param%phy%i_adviterations
	    END IF comment_line
	  END IF file_end
	END DO read_loop

!---------- error handling

	no_value: IF((p_param%phy%r_deltatime     < 0.0) .OR. &
	             (p_param%phy%r_reftolerance  < 0.0) .OR. &
	             (p_param%phy%r_crstolerance  < 0.0) .OR. &
	             (p_param%phy%r_refwatermark  < 0.0) .OR. &
	             (p_param%phy%r_crswatermark  < 0.0) .OR. &
	             (p_param%phy%i_experiment    < 0)   .OR. &
	             (p_param%phy%i_crslevel      < 0)   .OR. &
	             (p_param%phy%i_reflevel      < 0)   .OR. &
	             (p_param%phy%i_frsttimestep  < 0)   .OR. &
	             (p_param%phy%i_lasttimestep  < 0)   .OR. &
	             (p_param%io%i_plotoffset     < 0)   .OR. &
	             (p_param%io%i_saveoffset     < 0)   .OR. &
	             (p_param%phy%i_adviterations < 0)) THEN
	  CALL grid_error(22)
	END IF no_value

	RETURN

 1000	FORMAT(1x,'***** ***** ***** ***** ***** ***** ***** ***** ***** *****',/ &
	       1x,'***** PROGRAM:   ',a15,22x,'*****',/ &
	       1x,'***** VERSION:   ',i2.2,'.',i2.2,'.',i2.2,29x,'*****',/ &
	       1x,'*****            Started in BATCH input mode',10x,'*****',/ &
	       1x,'***** INPUTFILE: ',a20,17x,'*****',/ &
	       1x,'***** ***** ***** ***** ***** ***** ***** ***** ***** *****',/)
 2000	FORMAT(a80)
 2010	FORMAT(a32)

	END SUBROUTINE io_getbatchinput
!*****************************************************************
	SUBROUTINE io_putparameters(p_param)

!---------- local declarations

	IMPLICIT NONE

	TYPE (control_struct), INTENT(in) :: p_param
	INTEGER                         :: i1, i2, i3, i9, i10, i11, i12, i13, i14
	REAL                            :: r4, r5, r6, r7, r8

!---------- temporary store

	i1 = p_param%phy%i_experiment
	i2 = p_param%phy%i_reflevel
	i3 = p_param%phy%i_crslevel
	r4 = p_param%phy%r_reftolerance
	r5 = p_param%phy%r_crstolerance
	r6 = p_param%phy%r_refwatermark
	r7 = p_param%phy%r_crswatermark
	r8 = p_param%phy%r_deltatime
	i9 = p_param%phy%i_frsttimestep
	i10= p_param%phy%i_lasttimestep
	i11= p_param%io%i_saveoffset
	i12= p_param%io%i_plotoffset
	i13= p_param%io%i_savelast
	i14= p_param%phy%i_adviterations

!---------- write satement

	write(GRID_parameters%ioout,1000) i1, i2, i3, r4, r5, r6, r7, r8, i9, i10, i11, i12, &
	                    i13, i14
	IF(GRID_parameters%iolog > 0) &
	  write(GRID_parameters%iolog,1000) i1, i2, i3, r4, r5, r6, r7, r8, i9, i10, i11, i12, &
	                      i13, i14

	RETURN

 1000	FORMAT(1x,'***** ***** ***** ***** ***** ***** ***** ***** ***** *****',/ &
	       1x,'*****                Global Parameters',16x,'*****',/ &
	       1x,'***** ----- ----- ----- ----- ----- ----- ----- ----- *****',/ &
	       1x,'***** Experiment No.',25x,i8,' *****',/ &
	       1x,'***** ----- ----- ----- ----- ----- ----- ----- ----- *****',/ &
	       1x,'***** Finest grid level',22x,i8,' *****',/ &
	       1x,'***** Coarsest grid level',20x,i8,' *****',/ &
	       1x,'***** Refinement tolerance',15x,e12.4,' *****',/ &
	       1x,'***** Coarsening tolerance',15x,e12.4,' *****',/ &
	       1x,'***** Refinement watermark',15x,e12.4,' *****',/ &
	       1x,'***** Coarsening watermark',15x,e12.4,' *****',/ &
	       1x,'***** ----- ----- ----- ----- ----- ----- ----- ----- *****',/ &
	       1x,'***** Timestep length',20x,e12.4,' *****',/ &
	       1x,'***** First timestep',25x,i8,' *****',/ &
	       1x,'***** Last timestep',26x,i8,' *****',/ &
	       1x,'***** ----- ----- ----- ----- ----- ----- ----- ----- *****',/ &
	       1x,'***** Timesteps between save',17x,i8,' *****',/ &
	       1x,'***** Timesteps between plot',17x,i8,' *****',/ &
	       1x,'***** Flag for last save',21x,i8,' *****',/ &
	       1x,'***** ----- ----- ----- ----- ----- ----- ----- ----- *****',/ &
	       1x,'***** Iterations for trajectories',12x,i8,' *****',/ &
	       1x,'***** ***** ***** ***** ***** ***** ***** ***** ***** *****',/)

	END SUBROUTINE io_putparameters
!*****************************************************************
	SUBROUTINE io_putruntimeinfo(p_ghand, p_info, p_time)

!---------- local declarations

	IMPLICIT NONE

	TYPE (grid_handle), INTENT(in) :: p_ghand
	TYPE (rt_info), INTENT(inout)  :: p_info
	TYPE (sw_info), INTENT(inout)  :: p_time
	INTEGER                        :: i_cnt

!---------- output

	write(GRID_parameters%ioout,1000) p_info%i_step, p_info%r_modeltime, p_info%i_adapit, &
	                    p_ghand%i_tnumber, p_ghand%i_tnumfine, &
	                    p_ghand%i_enumber, p_ghand%i_enumfine, &
	                    p_ghand%i_gnumber, p_ghand%i_gnumfine, &
	                    p_ghand%i_nnumber
	IF(GRID_parameters%iolog > 0) &
	  write(GRID_parameters%iolog,1000) p_info%i_step, p_info%r_modeltime, p_info%i_adapit, &
	                      p_ghand%i_tnumber, p_ghand%i_tnumfine, &
	                      p_ghand%i_enumber, p_ghand%i_enumfine, &
	                      p_ghand%i_gnumber, p_ghand%i_gnumfine, &
	                      p_ghand%i_nnumber
	times_loop: DO i_cnt=1,p_time%i_num
	  write(GRID_parameters%ioout,1003) p_time%p_tim(i_cnt)%c_tim, p_time%p_tim(i_cnt)%r_tim
	  IF(GRID_parameters%iolog > 0) &
	    write(GRID_parameters%iolog,1003) p_time%p_tim(i_cnt)%c_tim, p_time%p_tim(i_cnt)%r_tim
	END DO times_loop
	save_perf: IF(p_info%l_saved) THEN
	  write(GRID_parameters%ioout,1002)
	  IF(GRID_parameters%iolog > 0) &
	    write(GRID_parameters%iolog,1002)
	  also_plot: IF(p_info%l_ploted) THEN
	    write(GRID_parameters%ioout,1005)
	    write(GRID_parameters%ioout,1006)
	    IF(GRID_parameters%iolog > 0) THEN
	      write(GRID_parameters%iolog,1005)
	      write(GRID_parameters%iolog,1006)
	    END IF
	  ELSE also_plot
	    write(GRID_parameters%ioout,1005)
	    IF(GRID_parameters%iolog > 0) &
	      write(GRID_parameters%iolog,1005)
	  END IF also_plot
	ELSE save_perf
	  but_plot: IF(p_info%l_ploted) THEN
	    write(GRID_parameters%ioout,1002)
	    write(GRID_parameters%ioout,1006)
	    IF(GRID_parameters%iolog > 0) THEN
	      write(GRID_parameters%iolog,1002)
	      write(GRID_parameters%iolog,1006)
	    END IF
	  END IF but_plot
	END IF save_perf
	write(GRID_parameters%ioout,1010)
	IF(GRID_parameters%iolog > 0) &
	  write(GRID_parameters%iolog,1010)

!---------- reset info structure

	p_info%i_step  = 0
	p_info%i_adapit= 0
	p_info%l_saved = .FALSE.
	p_info%l_ploted= .FALSE.

	RETURN
	
 1000	FORMAT(1x,'+++++ +++++ +++++ +++++ +++++ +++++ +++++ +++++ +++++ +++++',/ &
	       1x,'+++++            Runtime Information Output           +++++',/ &
	       1x,'+++++ ----- ----- ----- ----- ----- ----- ----- ----- +++++',/ &
	       1x,'+++++ Timestep Number',24x,i8,' +++++',/ &
	       1x,'+++++ Model time',25x,e12.4,' +++++',/ &
	       1x,'+++++ ----- ----- ----- ----- ----- ----- ----- ----- +++++',/ &
	       1x,'+++++ Inner iterations (for adaptation)',6x,i8,' +++++',/ &
	       1x,'+++++ ----- ----- ----- ----- ----- ----- ----- ----- +++++',/ &
	       1x,'+++++ Number of tetrahedra',19x,i8,' +++++',/ &
	       1x,'+++++ Number of tetrahedra (fine grid)',7x,i8,' +++++',/ &
	       1x,'+++++ Number of elements',21x,i8,' +++++',/ &
	       1x,'+++++ Number of elements (fine grid)',9x,i8,' +++++',/ &
	       1x,'+++++ Number of edges',24x,i8,' +++++',/ &
	       1x,'+++++ Number of edges (fine grid)',12x,i8,' +++++',/ &
	       1x,'+++++ Number of nodes',24x,i8,' +++++',/ &
	       1x,'+++++ ----- ----- ----- ----- ----- ----- ----- ----- +++++')
 1002	FORMAT(1x,'+++++ ----- ----- ----- ----- ----- ----- ----- ----- +++++')
 1003	FORMAT(1x,'+++++ Time spent in ',a16,5x,e12.4,' +++++')
 1005	FORMAT(1x,'+++++ Saveset transferred to disk in this step        +++++')
 1006	FORMAT(1x,'+++++ Plotting performed in this step                 +++++')
 1010	FORMAT(1x,'+++++ +++++ +++++ +++++ +++++ +++++ +++++ +++++ +++++ +++++',/)

	END SUBROUTINE io_putruntimeinfo
!*****************************************************************
	SUBROUTINE io_putinputfile(p_param)

!---------- local declarations

	IMPLICIT NONE

	TYPE (control_struct), INTENT(in) :: p_param
	INTEGER                         :: i1, i2, i3, i9, i10, i11, &
	  i12, i13, i14, i15, i16, i17, i18, i19, i20 &
          !-- added for visnetplot [flo]
          , i31, i32
	REAL                            :: r4, r5, r6, r7, r8
	INTEGER                         :: i_unit=15, i_fst
	CHARACTER (len=32)              :: c_file
	CHARACTER (len=28)              :: c_tmp

!---------- temporary store

	i1 = p_param%phy%i_experiment+ 1
	i2 = p_param%phy%i_reflevel
	i3 = p_param%phy%i_crslevel
	r4 = p_param%phy%r_reftolerance
	r5 = p_param%phy%r_crstolerance
	r6 = p_param%phy%r_refwatermark
	r7 = p_param%phy%r_crswatermark
	r8 = p_param%phy%r_deltatime
	i9 = p_param%phy%i_lasttimestep+ 1
	i10= i9+ (p_param%phy%i_lasttimestep- p_param%phy%i_frsttimestep)
	i12= 0
	IF(p_param%io%l_matlab) i12= 1
	i11= 0
	IF(p_param%io%l_gmv) i11= 1
        !-- BEGIN added for visnetplot [flo]
        i31= 0
        IF(p_param%io%l_visnet) i31= 1
        i32= 0
        IF(p_param%io%l_visnet_tiff) i32= 1
        !-- END
	i13= p_param%io%i_plotoffset
	i14= p_param%io%i_saveoffset
	i15= p_param%io%i_savelast

	i16= p_param%phy%i_adviterations

!---------- open file

	  write(c_tmp,*) trim(GRID_parameters%program_name), '_input.'
	  write(c_file,1010) trim(c_tmp), i1
	  c_file= adjustl(c_file)
	  open(i_unit, file= c_file, action= 'write', form= 'formatted', &
	       status='replace', iostat= i_fst)
	  not_opened: IF(i_fst /= 0) THEN
	    CALL grid_error(23)
	  END IF not_opened
	  IF(GRID_parameters%iolog > 0) &
	    write(GRID_parameters%iolog,*) 'INFO: Filename: ', c_file, ' opened on unit: ', i_unit

!---------- write satement

	write(i_unit,1001) c_file, GRID_parameters%program_name
	write(i_unit,1002) i1, i2, i3, r4, r5, r6, r7, r8, i9, i10
        !-- added for visnetplot [flo] ", i31, i32":
	write(i_unit,1003) i12, i11, i31, i32, i13, i14, &
	  i15, p_param%io%c_windfile, &
	  p_param%io%c_domainfile, p_param%io%c_triangfile, i16

!---------- open file

	close(i_unit)
	IF(GRID_parameters%iolog > 0) &
	  write(GRID_parameters%iolog,*) 'INFO: Closed file on unit: ', i_unit

	RETURN

 1001	FORMAT('# --- --- --- --- --- --- --- --- --- --- --- --- ---',/ &
	       '# Parameter file ',a32,/ &
	       '# created automatically by program ',a15,/ &
	       '# --- --- --- --- --- --- --- --- --- --- --- --- ---')
 1002	FORMAT('EXPERIMENT_NUMBER',/ &
	       i8,/ &
	       'FINE_GRID_LEVEL',/ &
	       i8,/ &
	       'COARSE_GRID_LEVEL',/ &
	       i8,/ &
	       'TOLERANCE_OF_REFINEMENT',/ &
	       e12.4,/ &
	       'TOLERANCE_OF_COARSENING',/ &
	       e12.4,/ &
	       'WATERMARK_OF_REFINEMENT',/ &
	       e12.4,/ &
	       'WATERMARK_OF_COARSENING',/ &
	       e12.4,/ &
	       'TIMESTEP_LENGTH',/ &
	       e12.4,/ &
	       'BEGINNING_TIMESTEP',/ &
	       i8,/ &
	       'FINISHING_TIMESTEP',/ &
	       i8)
 1003	FORMAT('MATLAB_PLOTTING',/ &
	       i8,/ &
	       'GMV_FILE_PLOTTING',/ &
	       i8,/ &
               !-- BEGIN added for visnetplot [flo]
	       'VISNET_PLOTTING',/ &
	       i8,/ &
	       'VISNET_SCREENSHOT',/ &
	       i8,/ &
               !-- END
	       'STEPS_BTW_PLOTS',/ &
	       i8,/ &
	       'STEPS_BTW_SAVES',/ &
	       i8,/ &
	       'SAVE_FINISH_CONFIGURATION',/ &
	       i8,/ &
	       'WIND_FILE_NAME',/ &
	       a32,/ &
	       'DOMAIN_FILE_NAME',/ &
	       a32,/ &
	       'TRIANG_FILE_NAME',/ &
	       a32,/ &
	       'SLM_ITERATION_NUMBER',/ &
	       i8,/ &
	       '# --- ---  End of parameter file  --- --- --- --- ---',/)
 1010	FORMAT(a28,i4.4)

	END SUBROUTINE io_putinputfile
	END MODULE IO_utils
