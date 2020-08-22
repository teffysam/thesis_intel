!*****************************************************************
!
! MODULE NAME:
!     IO_utils
! FUNCTION:
!     input/output routines (io_get...something, io_put...something)
! CONTAINS:
!-----------------------------------------------------------------
!
! NAME:
!     io_getcmdline
! FUNCTION:
!     read options from command line
! SYNTAX:
!     call io_getcmdline(param)
! ON INPUT:
!
! ON OUTPUT:
!     p_param: control parameters               TYPE(control_struct)
! CALLS:
!
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!     io_getinterinput
! FUNCTION:
!     get user input interactively
! SYNTAX:
!     call io_getinterinput(param)
! ON INPUT:
!
! ON OUTPUT:
!     p_param: control parameters               TYPE(control_struct)
! CALLS:
!
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!     io_getbatchinput
! FUNCTION:
!     read user input from file
! SYNTAX:
!     call io_getbatchinput(param)
! ON INPUT:
!
! ON OUTPUT:
!     p_param: control parameters               TYPE(control_struct)
! CALLS:
!
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!     io_putparameters
! FUNCTION:
!     write out parameters in a nice way
! SYNTAX:
!     call io_putparameters(param)
! ON INPUT:
!     p_param: data structure containing parameters     TYPE(control_struct)
! ON OUTPUT:
!     
! CALLS:
!
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!     io_putruntimeinfo
! FUNCTION:
!     print some information on the current run time status
! SYNTAX:
!     call io_putruntimeinfo(grid)
! ON INPUT:
!     p_ghand: grid handle for no. of elements...     TYPE(grid_handle)
! ON OUTPUT:
!
! CALLS:
!
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!     io_putinputfile
! FUNCTION:
!     print an input file conforming to io_getbatchinput
! SYNTAX:
!     call io_putinputfile(param)
! ON INPUT:
!     p_param: global parameter data structure     TYPE(control_struct)
! ON OUTPUT:
!
! CALLS:
!
! COMMENTS:
!
!-----------------------------------------------------------------
!
! PUBLIC:
!     io_getcmdline, io_getinterinput, io_getbatchinput,
!     io_putparameters, io_putruntimeinfo, io_putinputfile
! COMMENTS:
!
! USES:
!     MISC_globalparam, MISC_error, MISC_timing, IO_plotdefine,
!     FEM_handle
! LIBRARIES:
!
! REFERENCES:
!
! VERSION(S):
!     1. original version               j. behrens     7/96
!     2. largely extended               j. behrens     11/96
!     3. changed command line io          j. behrens     1/97
!     4. changed runtime output (adaptations)     j. behrens     1/97
!     5. control data struct, less command io     j. behrens     12/97
!     6. tiff file plotting included          j. behrens     1/98
!     7. updated for amatos version 1.0     j. behrens     11/2000
!
!*****************************************************************
     MODULE IO_utils
       USE MAIN_parameters
       USE MISC_timing
       USE MISC_system
       USE GRID_api
       PRIVATE
       PUBLIC :: io_getcmdline, io_getinterinput, io_getbatchinput, &
         io_putparameters, io_putruntimeinfo, io_putinputfile
       INTEGER (KIND = GRID_SI), PARAMETER :: i_ioin= 5
       CONTAINS
!*****************************************************************
     SUBROUTINE io_getcmdline(p_cmd)

!---------- local declarations

     IMPLICIT NONE

     TYPE (control_struct), INTENT(out) :: p_cmd

     LOGICAL                            :: l_ict
     LOGICAL                            :: l_mtl
     LOGICAL                            :: l_dia
     LOGICAL                            :: l_log
     LOGICAL                            :: l_red
     CHARACTER (len=io_fillen)          :: a_infln
     INTEGER (KIND = GRID_SI)                            :: numargs
     INTEGER (KIND = GRID_SI)                            :: i= 1
     INTEGER (KIND = GRID_SI)                            :: i_fst
     LOGICAL                            :: help= .true.
     LOGICAL                            :: shoversion= .false.
     CHARACTER (len=2)                  :: option
     CHARACTER (len=15)                 :: comdnam
     CHARACTER (len=32)                 :: c_file
     CHARACTER (len=80)                 :: c_dummy
     INTEGER (KIND = GRID_SI), PARAMETER                 :: i_ioout=6

!---------- initialize output variables

     a_infln= 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
     l_ict= .false.
     l_mtl= .false.
     l_dia= .false.
     l_log= .false.
     l_red= .false.

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
             IF(l_ict) THEN
               help= .true.
               GOTO 100 ! print_help
             ELSE
               help= .false.
               IF(a_infln == 'xxxxxxxxxxxxxxxxxxxx') &
                 a_infln= 'Parameters.in' !--- default input file name ---
               i= i+1
             END IF
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

!---------- print help information

 100     print_help: IF(help) THEN
       IF(shoversion) THEN
         write(i_ioout,1001) GRID_parameters%program_name, GRID_parameters%version, GRID_parameters%subversion, &
                             GRID_parameters%patchversion, GRID_parameters%datemonth, GRID_parameters%dateyear, &
                             GRID_parameters%author_name, GRID_parameters%author_email, GRID_parameters%author_affil1, &
                             GRID_parameters%author_affil2, GRID_parameters%author_affil3
         write(i_ioout,1002) comdnam
         write(i_ioout,*) 'STOPPED ... this is all I can say'
         STOP
       ELSE
         write(i_ioout,1010) comdnam
         write(i_ioout,1011) GRID_parameters%author_name
         write(i_ioout,*) 'STOPPED ... hope this made it clear'
         STOP
       END IF
     END IF print_help

!---------- print version information

     print_version: IF(shoversion) THEN
       write(i_ioout,1001) GRID_parameters%program_name, GRID_parameters%version, GRID_parameters%subversion, &
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

 1001     FORMAT(1x,'***** ***** ***** ***** ***** ***** ***** ***** ***** *****',/ &
            1x,'***** PROGRAM: ',a15,24x,'*****',/ &
            1x,'***** VERSION: ',i2.2,'.',i2.2,'.',i2.2,31x,'*****',/ &
            1x,'***** DATE:    ',i2.2,'/',i4.4,32x,'*****',/ &
            1x,'***** AUTHOR:  ',a12,27x,'*****',/ &
            1x,'***** E-MAIL:  ',a39,'*****',/ &
            1x,'***** ADDRESS: ',a39,'*****',/ &
            1x,'*****          ',a39,'*****',/ &
            1x,'*****          ',a39,'*****',/ &
            1x,'***** ***** ***** ***** ***** ***** ***** ***** ***** *****')
 1002     FORMAT(1x,'***** TYPE ',a15,' -h, to get help',12x,'*****',/ &
            1x,'***** ***** ***** ***** ***** ***** ***** ***** ***** *****')
 1010     FORMAT(1x,'USAGE: ',a15,' [-i|-b] {-d} {-f name} {-h} {-l} {-o} {-r}',/ &
            1x,'       -i: interactive input mode',/ &
            1x,'       -b: batch input mode (read from file)',/ &
            1x,'       -d: switch on diagnostics',/ &
            1x,'       -f: input filename is << name >>',/ &
            1x,'       -h: help information (this output)',/ &
            1x,'       -l: switch on log file output',/ &
            1x,'       -o: redirect standard output to a file',/ &
            1x,'       -r: release information')
 1011     FORMAT(1x,'Copyright (c) 1996, 1997, 1998, ',a13)
     END SUBROUTINE io_getcmdline
!*****************************************************************
     SUBROUTINE io_getinterinput(p_param)

!---------- local declarations

     IMPLICIT NONE

     TYPE (control_struct), INTENT(out) :: p_param
     INTEGER (KIND = GRID_SI)                            :: i_iost, i_tmp, i_cln
     CHARACTER (len=io_fillen)          :: c_tmp

!---------- initialize

     p_param%phy%i_crslevel      = -1
     p_param%phy%i_reflevel      = -1
     p_param%io%l_matlab         = .FALSE.
     p_param%io%l_gmv            = .FALSE.
     p_param%io%c_triangfile     = 'Triang.dat'

     i_cln= MAX(GRID_parameters%i_stringlength,io_fillen)

!---------- prompt user for input (loop in case of wrong input)

     write(GRID_parameters%ioout,1000) GRID_parameters%program_name, GRID_parameters%version, &
                         GRID_parameters%subversion, GRID_parameters%patchversion

!---------- Adaptivity Control

     write(GRID_parameters%ioout,1001)
     write(GRID_parameters%ioout,1020)
     write(GRID_parameters%ioout,1021,advance='NO')
     read(i_ioin,*) p_param%phy%i_reflevel
     write(GRID_parameters%ioout,1022,advance='NO')
     read(i_ioin,*) p_param%phy%i_crslevel

!---------- Output Control

     write(GRID_parameters%ioout,1001)
     write(GRID_parameters%ioout,1040)
     write(GRID_parameters%ioout,1049,advance='NO')
     read(i_ioin,*) i_tmp
     IF(i_tmp /= 0) THEN
       p_param%io%l_matlab= .True.
     END IF
     write(GRID_parameters%ioout,10491,advance='NO')
     read(i_ioin,*) i_tmp
     IF(i_tmp /= 0) THEN
       p_param%io%l_gmv= .True.
     END IF
     write(GRID_parameters%ioout,1047,advance='NO')
     read(i_ioin,2000,iostat=i_iost) c_tmp
     IF(i_iost == 0) THEN
       p_param%io%c_triangfile(1:i_cln)= c_tmp(1:i_cln)
     END IF

!---------- error handling

     no_value: IF((p_param%phy%i_crslevel      < 0)   .OR. &
                  (p_param%phy%i_reflevel      < 0)) THEN
       CALL grid_error(c_error='[io_getinterinput] No values found in input ... oops')
     END IF no_value
     RETURN

 1000     FORMAT(1x,'***** ***** ***** ***** ***** ***** ***** ***** ***** *****',/ &
            1x,'***** PROGRAM: ',a15,24x,'*****',/ &
            1x,'***** VERSION: ',i2.2,'.',i2.2,'.',i2.2,31x,'*****',/ &
            1x,'*****          Started in INTERACTIVE input mode      *****',/ &
            1x,'***** ***** ***** ***** ***** ***** ***** ***** ***** *****',/)
 1001     FORMAT(1x,'-----------------------------------------------------------',/)
 1002     FORMAT(1x,'---------------------- end of input -----------------------',/)
 1010     FORMAT(1x,'                        Experiment')
 1020     FORMAT(1x,'                    Adaptivity Control')
 1030     FORMAT(1x,'                     Timestep Control')
 1040     FORMAT(1x,'                   Input/Output Control')
 1050     FORMAT(1x,'                    Iteration Control')
 1011     FORMAT(1x,'INPUT: Experiment No. (first exp. = 0)              > ')
 1021     FORMAT(1x,'INPUT: Finest level of refinement                   > ')
 1022     FORMAT(1x,'INPUT: Coarsest level of refinement                 > ')
 1023     FORMAT(1x,'INPUT: Tolerance for refinement (|t_r| < 1)         > ')
 1024     FORMAT(1x,'INPUT: Tolerance for Coarsening (t_c < t_r)         > ')
 1025     FORMAT(1x,'INPUT: Watermark for refinement (|w_r| < 1)         > ')
 1026     FORMAT(1x,'INPUT: Watermark for coarsening (|w_c| < 1)         > ')
 1031     FORMAT(1x,'INPUT: Timestep length (delta t)                    > ')
 1032     FORMAT(1x,'INPUT: First timestep number                        > ')
 1033     FORMAT(1x,'INPUT: Last timestep number                         > ')
 1041     FORMAT(1x,'INPUT: Number of timesteps between saves            > ')
 1042     FORMAT(1x,'INPUT: Number of timesteps between plots            > ')
 1043     FORMAT(1x,'INPUT: Save last step for next experiment (no = 0)  > ')
 1044     FORMAT(1x,'INPUT: Plotting desired? I need the type of plot',/ &
            1x,'        0: no plot desired',/ &
            1x,'        7: plot the plain grid',/ &
            1x,'        8: plot the wind vector field',/ &
            1x,'       10: plot tracer scalar field                 > ')
 10441     FORMAT(1x,'INPUT: Plot screen to a TIFF file (no = 0)          > ')
 1045     FORMAT(1x,'INPUT: Plot polygonal outline (no = 0)              > ')
 10451     FORMAT(1x,'INPUT: Filename for polygon data    (Polyline.dat)  > ')
 1046     FORMAT(1x,'INPUT: Read wind data from file (no = 0)            > ')
 10461     FORMAT(1x,'INPUT: Filename for wind data       (Flow.dat)      > ')
 1047     FORMAT(1x,'INPUT: Filename for triangulation   (Triang.dat)    > ')
 1049     FORMAT(1x,'INPUT: Write matlab compatible output file (no = 0) > ')
 10491     FORMAT(1x,'INPUT: Write gmv compatible output file (no = 0)    > ')
 1051     FORMAT(1x,'INPUT: Iterations in trajectory estimation          > ')

 2000     FORMAT(a32)

     END SUBROUTINE io_getinterinput
!*****************************************************************
     SUBROUTINE io_getbatchinput(p_param)

!---------- local declarations

     IMPLICIT NONE

     TYPE (control_struct), INTENT(out)         :: p_param
     INTEGER (KIND = GRID_SI), PARAMETER                         :: i_iofil= 10
     CHARACTER (len=80)                         :: a_filrow
     CHARACTER (len=io_fillen)                  :: c_tmp
     INTEGER (KIND = GRID_SI)                                    :: i_iost, i_ioend, &
       i_tmp, i_cln

!---------- initialize

     p_param%phy%i_crslevel      = -1
     p_param%phy%i_reflevel      = -1
     p_param%io%l_matlab         = .FALSE.
     p_param%io%l_gmv            = .FALSE.
     p_param%io%c_triangfile     = 'Triang.dat'

     i_cln= MAX(GRID_parameters%i_stringlength,io_fillen)

!---------- open input file

     open(unit= i_iofil, file= p_param%cmd%c_batchfile, status= 'OLD', &
          action= 'READ', iostat= i_iost)
     file_notopen: IF(i_iost /= 0) THEN
       IF(GRID_parameters%iolog > 0) &
         write(GRID_parameters%iolog,*) 'ERROR: Filename: ', p_param%cmd%c_batchfile
       CALL grid_error(c_error='[io_getbatchinput] Could not open File, perhaps wrong filename?')
     ELSE file_notopen
       write(GRID_parameters%ioout,1000) GRID_parameters%program_name, GRID_parameters%version, &
                           GRID_parameters%subversion, GRID_parameters%patchversion, &
                     p_param%cmd%c_batchfile
       IF(GRID_parameters%iolog > 0) THEN
         write(GRID_parameters%iolog,*) 'INFO: Filename: ', p_param%cmd%c_batchfile, ' opened on unit: ', i_iofil
         write(GRID_parameters%iolog,1000) GRID_parameters%program_name, GRID_parameters%version, &
                             GRID_parameters%subversion, GRID_parameters%patchversion, &
                    p_param%cmd%c_batchfile
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
         ELSE IF(a_filrow(1:14) == 'FINE_GRID_LEVE') THEN comment_line
           read(i_iofil,*) p_param%phy%i_reflevel
         ELSE IF(a_filrow(1:14) == 'COARSE_GRID_LE') THEN comment_line
           read(i_iofil,*) p_param%phy%i_crslevel
         ELSE IF(a_filrow(1:14) == 'MATLAB_PLOTTIN') THEN comment_line
           read(i_iofil,*) i_tmp
           IF(i_tmp /= 0) p_param%io%l_matlab= .TRUE.
         ELSE IF(a_filrow(1:14) == 'GMV_FILE_PLOTT') THEN comment_line
           read(i_iofil,*) i_tmp
           IF(i_tmp /= 0) p_param%io%l_gmv= .TRUE.
         ELSE IF(a_filrow(1:14) == 'TRIANG_FILE_NA') THEN comment_line
           read(i_iofil,2010,iostat=i_tmp) c_tmp
           IF(i_tmp == 0) p_param%io%c_triangfile(1:i_cln)= c_tmp(1:i_cln)
         END IF comment_line
       END IF file_end
     END DO read_loop

!---------- error handling

     no_value: IF((p_param%phy%i_crslevel      < 0)   .OR. &
                  (p_param%phy%i_reflevel      < 0)) THEN
       CALL grid_error(c_error='[io_getbatchinput] No values found in input file ... oops')
     END IF no_value

     RETURN

 1000     FORMAT(1x,'***** ***** ***** ***** ***** ***** ***** ***** ***** *****',/ &
            1x,'***** PROGRAM:   ',a15,22x,'*****',/ &
            1x,'***** VERSION:   ',i2.2,'.',i2.2,'.',i2.2,29x,'*****',/ &
            1x,'*****            Started in BATCH input mode',10x,'*****',/ &
            1x,'***** INPUTFILE: ',a20,17x,'*****',/ &
            1x,'***** ***** ***** ***** ***** ***** ***** ***** ***** *****',/)
 2000     FORMAT(a80)
 2010     FORMAT(a32)

     END SUBROUTINE io_getbatchinput
!*****************************************************************
     SUBROUTINE io_putparameters(p_param)

!---------- local declarations

     IMPLICIT NONE

     TYPE (control_struct), INTENT(in) :: p_param
     INTEGER (KIND = GRID_SI)                         :: i1, i2

!---------- temporary store

     i1 = p_param%phy%i_reflevel
     i2 = p_param%phy%i_crslevel

!---------- write satement

     write(GRID_parameters%ioout,1000) i1, i2
     IF(GRID_parameters%iolog > 0) &
       write(GRID_parameters%iolog,1000) i1, i2

     RETURN

 1000     FORMAT(1x,'***** ***** ***** ***** ***** ***** ***** ***** ***** *****',/ &
            1x,'*****                Global Parameters',16x,'*****',/ &
            1x,'***** ----- ----- ----- ----- ----- ----- ----- ----- *****',/ &
            1x,'***** Finest grid level',22x,i8,' *****',/ &
            1x,'***** Coarsest grid level',20x,i8,' *****',/ &
            1x,'***** ***** ***** ***** ***** ***** ***** ***** ***** *****',/)

     END SUBROUTINE io_putparameters

!*****************************************************************
     SUBROUTINE io_putruntimeinfo(p_ghand)

!---------- local declarations

     IMPLICIT NONE

     TYPE (grid_handle), INTENT(in) :: p_ghand
     INTEGER (KIND = GRID_SI)                        :: i_cnt

!---------- output

     write(GRID_parameters%ioout,1000) p_ghand%i_tnumber, p_ghand%i_tnumfine, &
                         p_ghand%i_enumber, p_ghand%i_enumfine, &
                         p_ghand%i_gnumber, p_ghand%i_gnumfine, &
                         p_ghand%i_nnumber
     IF(GRID_parameters%iolog > 0) &
       write(GRID_parameters%iolog,1000) p_ghand%i_tnumber, p_ghand%i_tnumfine, &
                           p_ghand%i_enumber, p_ghand%i_enumfine, &
                           p_ghand%i_gnumber, p_ghand%i_gnumfine, &
                           p_ghand%i_nnumber
     write(GRID_parameters%ioout,1010)
     IF(GRID_parameters%iolog > 0) &
       write(GRID_parameters%iolog,1010)

     RETURN
     
 1000     FORMAT(1x,'+++++ +++++ +++++ +++++ +++++ +++++ +++++ +++++ +++++ +++++',/ &
            1x,'+++++            Runtime Information Output           +++++',/ &
            1x,'+++++ ----- ----- ----- ----- ----- ----- ----- ----- +++++',/ &
            1x,'+++++ Number of tetrahedra',19x,i8,' +++++',/ &
            1x,'+++++ Number of tetrahedra (fine grid)',7x,i8,' +++++',/ &
            1x,'+++++ Number of elements',21x,i8,' +++++',/ &
            1x,'+++++ Number of elements (fine grid)',9x,i8,' +++++',/ &
            1x,'+++++ Number of edges',24x,i8,' +++++',/ &
            1x,'+++++ Number of edges (fine grid)',12x,i8,' +++++',/ &
            1x,'+++++ Number of nodes',24x,i8,' +++++',/ &
            1x,'+++++ ----- ----- ----- ----- ----- ----- ----- ----- +++++')
 1002     FORMAT(1x,'+++++ ----- ----- ----- ----- ----- ----- ----- ----- +++++')
 1010     FORMAT(1x,'+++++ +++++ +++++ +++++ +++++ +++++ +++++ +++++ +++++ +++++',/)

     END SUBROUTINE io_putruntimeinfo
!*****************************************************************
     SUBROUTINE io_putinputfile(p_param)

!---------- local declarations

     IMPLICIT NONE

     TYPE (control_struct), INTENT(in) :: p_param
     INTEGER (KIND = GRID_SI)                         :: i1, i2, i3, i4
     INTEGER (KIND = GRID_SI)                         :: i_unit=15, i_fst
     CHARACTER (len=32)              :: c_file
     CHARACTER (len=28)              :: c_tmp

!---------- temporary store

     i1 = p_param%phy%i_reflevel
     i2 = p_param%phy%i_crslevel
     i3= 0
     IF(p_param%io%l_matlab) i3= 1
     i4= 0
     IF(p_param%io%l_gmv) i4= 1

!---------- open file

       write(c_tmp,*) trim(GRID_parameters%program_name), '_input.'
       write(c_file,1010) trim(c_tmp), i1
       c_file= adjustl(c_file)
       open(i_unit, file= c_file, action= 'write', form= 'formatted', &
            status='replace', iostat= i_fst)
       not_opened: IF(i_fst /= 0) THEN
         CALL grid_error(c_error='[io_putinputfile] Could not open file for writing parameters')
       END IF not_opened
       IF(GRID_parameters%iolog > 0) &
         write(GRID_parameters%iolog,*) 'INFO: Filename: ', c_file, ' opened on unit: ', i_unit

!---------- write satement

     write(i_unit,1001) c_file, GRID_parameters%program_name
     write(i_unit,1002) i1, i2, i3, i4
     write(i_unit,1003) p_param%io%c_triangfile

!---------- open file

     close(i_unit)
     IF(GRID_parameters%iolog > 0) &
       write(GRID_parameters%iolog,*) 'INFO: Closed file on unit: ', i_unit

     RETURN

 1001     FORMAT('# --- --- --- --- --- --- --- --- --- --- --- --- ---',/ &
            '# Parameter file ',a32,/ &
            '# created automatically by program ',a15,/ &
            '# --- --- --- --- --- --- --- --- --- --- --- --- ---')
 1002     FORMAT('FINE_GRID_LEVEL',/ &
            i8,/ &
            'COARSE_GRID_LEVEL',/ &
            i8,/ &
            'MATLAB_PLOTTING',/ &
            i8,/ &
            'GMV_FILE_PLOTTING',/ &
            i8)
 1003     FORMAT('TRIANG_FILE_NAME',/ &
            a32,/ &
            '# --- ---  End of parameter file  --- --- --- --- ---',/)
 1010     FORMAT(a28,i4.4)

     END SUBROUTINE io_putinputfile
     END MODULE IO_utils
