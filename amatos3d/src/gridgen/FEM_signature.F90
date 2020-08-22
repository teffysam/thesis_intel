!*****************************************************************
!
! MODULE NAME:
!	FEM_signature
! FUNCTION:
!	delivers comfortable FEM support for amatos
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
!
! LIBRARIES:
!
! REFERENCES:
!
! VERSION(S):
!	1. original version	j. behrens	03/2003
!       2. adapted to 3d        o. kunst        04/2009
!
!*****************************************************************
	MODULE FEM_signature
	  USE MISC_globalparam
	  USE MISC_error
	  USE FEM_define
	PRIVATE

	INTEGER (KIND = GRID_SI)                     :: DEF_femtypes= 0_GRID_SI    ! no. of FEM types
	INTEGER (KIND = GRID_SI), PARAMETER          :: DEF_fillen=32_GRID_SI    ! no. of FEM types

!---------- support for FEM by signatures

	TYPE fem_signatur
	  CHARACTER (len=DEF_fillen) :: c_name
	  CHARACTER (len=DEF_fillen) :: c_sigfile
	  INTEGER (KIND = GRID_SI) :: i_order
	  INTEGER (KIND = GRID_SI) :: i_unknowns
	  INTEGER (KIND = GRID_SI) :: i_nodepoints
	  INTEGER (KIND = GRID_SI) :: i_edgepoints
	  INTEGER (KIND = GRID_SI) :: i_elmtpoints
          INTEGER (KIND = GRID_SI) :: i_tetpoints
	  INTEGER (KIND = GRID_SI) :: i_quadpoints
	  INTEGER (KIND = GRID_SI) :: i_fembasentry
	  REAL (KIND = GRID_SR), DIMENSION(:,:), POINTER :: r_gcoo
	  REAL (KIND = GRID_SR), DIMENSION(:,:), POINTER :: r_ecoo
	  REAL (KIND = GRID_SR), DIMENSION(:,:), POINTER :: r_tcoo
	  REAL (KIND = GRID_SR), DIMENSION(:,:), POINTER :: r_qcoo
	  REAL (KIND = GRID_SR), DIMENSION(:),     POINTER :: r_qwei
	  REAL (KIND = GRID_SR), DIMENSION(:,:), POINTER :: r_coef
	  REAL (KIND = GRID_SR), DIMENSION(:,:), POINTER :: r_dfdxi
	  REAL (KIND = GRID_SR), DIMENSION(:,:), POINTER :: r_dfdeta
	  REAL (KIND = GRID_SR), DIMENSION(:,:), POINTER :: r_dfdzeta
	END TYPE fem_signatur

	TYPE fem_sig_arr
	  TYPE(fem_signatur), POINTER :: sig
	END TYPE fem_sig_arr
	
	TYPE grid_femtypearr
	  INTEGER (KIND = GRID_SI)                 :: i_number
	  TYPE(fem_sig_arr), DIMENSION(:), POINTER :: p_type
	END TYPE grid_femtypearr

#ifdef xlf90
	TYPE (grid_femtypearr), SAVE :: GRID_femtypes
#else	
	TYPE (grid_femtypearr) :: GRID_femtypes
#endif

!---------- registration of variables needs storage pattern

	INTEGER (KIND = GRID_SI), PARAMETER                :: DEF_femnodepos=1_GRID_SI
	INTEGER (KIND = GRID_SI), PARAMETER                :: DEF_femedgepos=2_GRID_SI
	INTEGER (KIND = GRID_SI), PARAMETER                :: DEF_femelmtpos=3_GRID_SI
	INTEGER (KIND = GRID_SI), PARAMETER                :: DEF_femtetpos =4_GRID_SI
	INTEGER (KIND = GRID_SI), PARAMETER                :: DEF_itemtypes=DEF_femtetpos
	INTEGER (KIND = GRID_SI), PARAMETER                :: DEF_femtypepos=DEF_itemtypes+1
	INTEGER (KIND = GRID_SI), PARAMETER                :: DEF_varpatsize=16_GRID_SI

	INTEGER (KIND = GRID_SI), DIMENSION(:,:), POINTER  :: i_varpattern
	INTEGER (KIND = GRID_SI)                           :: i_maxfemvars
	INTEGER (KIND = GRID_SI), DIMENSION(DEF_itemtypes) :: i_varmaxpos

!---------- exported data types...

	PUBLIC :: grid_signatureinit,   grid_signaturequit, grid_registerfemvar, &
	          grid_femtypequery,    grid_femvarquery,   grid_registerfemtype, &
		  grid_femvarrecover
	PUBLIC :: DEF_femtypes
	PUBLIC :: fem_signatur, fem_sig_arr, grid_femtypearr
	PUBLIC :: GRID_femtypes
	PUBLIC :: DEF_itemtypes, i_varmaxpos, i_varpattern, i_maxfemvars
	PUBLIC :: DEF_femnodepos, DEF_femedgepos, DEF_femelmtpos, DEF_femtypepos

!--------
        INTERFACE grid_signatureinit
           MODULE PROCEDURE grid_signatureinit
        END INTERFACE ! grid_signatureinit
        INTERFACE grid_signaturequit
           MODULE PROCEDURE grid_signaturequit
        END INTERFACE ! grid_signaturequit
!-------

        CONTAINS
!*****************************************************************
	SUBROUTINE grid_signatureinit

!---------- local declarations

	IMPLICIT NONE
	TYPE (fem_signatur), POINTER                      :: p_sigtmp
	REAL (KIND = GRID_SR), DIMENSION(:,:), POINTER    :: r_tmp1, r_tmp2
	INTEGER (KIND = GRID_SI)                          :: i_cnt, i_alct
	INTEGER (KIND = GRID_SI)                          :: i_totaltypes
	INTEGER (KIND = GRID_SI), DIMENSION(:,:), POINTER :: i_tmp

!---------- check for registered fem types

	IF(DEF_femtypes < 1_GRID_SI) THEN
	  i_cnt= grid_registerfemtype('default.ftf')
	  IF(i_cnt /= 1_GRID_SI) CALL print_error(a_err='[grid_signatureinit] somehow the default fem type got mixed up...')
	END IF

!---------- read signature data files

	DO i_cnt=1,DEF_femtypes
	  CALL read_femsignature(i_cnt)
	END DO
	

!---------- initialize variable registration data

	i_maxfemvars= 0
	i_varmaxpos= 0
	NULLIFY(i_tmp,i_varpattern)
	allocate(i_tmp(DEF_femtypepos,DEF_varpatsize), stat= i_alct)
	IF(i_alct /= 0) CALL print_error(a_err='[grid_signatureinit]: Could not allocate variable pattern array')
	i_varpattern => i_tmp
        i_varpattern = 0
	NULLIFY(i_tmp)

	RETURN
	END SUBROUTINE grid_signatureinit

!*****************************************************************
	SUBROUTINE grid_signaturequit

!---------- local declarations

	IMPLICIT NONE
	TYPE (fem_signatur), POINTER        :: p_sigtmp
	INTEGER (KIND = GRID_SI), PARAMETER :: i_totaltypes= 2
	INTEGER (KIND = GRID_SI)            :: i_cnt

!---------- first deallocate coordinate weight arrays, then structure

	DO i_cnt=1, GRID_femtypes%i_number
	  p_sigtmp=> GRID_femtypes%p_type(i_cnt)%sig
	  IF(p_sigtmp%i_edgepoints /= 0) DEALLOCATE(p_sigtmp%r_gcoo)
	  IF(p_sigtmp%i_elmtpoints /= 0) DEALLOCATE(p_sigtmp%r_ecoo)
          IF(p_sigtmp%i_tetpoints /= 0) DEALLOCATE(p_sigtmp%r_tcoo)
	  IF(p_sigtmp%i_quadpoints /= 0) DEALLOCATE(p_sigtmp%r_qcoo, p_sigtmp%r_qwei)
	  IF(associated(p_sigtmp%r_coef)) THEN
	    DEALLOCATE(p_sigtmp%r_coef)
	    NULLIFY(p_sigtmp%r_coef)
	  END IF
	  IF(associated(p_sigtmp%r_dfdxi)) THEN
	    DEALLOCATE(p_sigtmp%r_dfdxi)
	    NULLIFY(p_sigtmp%r_dfdxi)
	  END IF
	  IF(associated(p_sigtmp%r_dfdeta)) THEN
	    DEALLOCATE(p_sigtmp%r_dfdeta)
	    NULLIFY(p_sigtmp%r_dfdeta)
	  END IF
	  DEALLOCATE(p_sigtmp)
	END DO
	
	DEALLOCATE(GRID_femtypes%p_type)

	DEF_femtypes = 0
	
	DEALLOCATE(i_varpattern)
	NULLIFY(i_varpattern)

        IF(ALLOCATED(i_dsfc_start)) THEN
           DEALLOCATE(i_dsfc_start)
        END IF
        IF(ALLOCATED(i_dsfc_end)) THEN
           DEALLOCATE(i_dsfc_end)
        END IF


	RETURN
	END SUBROUTINE grid_signaturequit

!*****************************************************************
	FUNCTION grid_registerfemvar(i_femtype,i_length) RESULT (i_varindex)

!---------- local declarations

	IMPLICIT NONE
	INTEGER (KIND = GRID_SI)                          :: i_femtype
	INTEGER (KIND = GRID_SI)                          :: i_varindex
	INTEGER (KIND = GRID_SI), OPTIONAL                :: i_length
	INTEGER (KIND = GRID_SI), DIMENSION(:,:), POINTER :: i_tmp
	INTEGER (KIND = GRID_SI)                          :: i_vsiz, i_alct, &
	  i_len, i_return, i_cnt
	TYPE (fem_signatur), POINTER                      :: p_sigtmp

!---------- check for optioinal argument

	IF(present(i_length)) THEN
	  IF(i_length <= 0) CALL print_error(a_err='[grid_registerfemvar]: i_length <= 0 not allowed')
	  i_len= i_length
	ELSE
	  i_len= 1_GRID_SI
	END IF

!---------- calculate index for new variable

	i_varindex= i_maxfemvars+ i_len
	i_return  = i_maxfemvars+ 1_GRID_SI

!---------- check fem type

	IF(i_femtype > DEF_femtypes) CALL print_error(a_err='[grid_registerfemvar]: Illegal FEM type')

!---------- check pattern array size, reallocate if necessary

	i_vsiz= SIZE(i_varpattern,2)
	IF(i_vsiz < i_varindex) THEN
	  i_vsiz= i_vsiz+ DEF_varpatsize
	  allocate(i_tmp(DEF_femtypepos,i_vsiz), stat= i_alct)
	  IF(i_alct /= 0) THEN
	    CALL print_error(a_err='[grid_registerfemvar]: Could not allocate variable pattern array')
	  END IF
	  i_tmp= 0
	  i_tmp(:,1:i_maxfemvars)= i_varpattern(:,1:i_maxfemvars)
          i_tmp(:,i_maxfemvars+1:)= 0
	  deallocate(i_varpattern)
	  i_varpattern => i_tmp
	  NULLIFY(i_tmp)
	END IF

!---------- now calculate offset and number of entries for variable corresponding to FEM type

	p_sigtmp=> GRID_femtypes%p_type(i_femtype)%sig
	DO i_cnt= i_return,i_varindex
	  IF(p_sigtmp%i_nodepoints > 0) THEN
	    i_varpattern(DEF_femnodepos,i_cnt)= i_varmaxpos(DEF_femnodepos)+ 1
	    i_varmaxpos(DEF_femnodepos)= i_varmaxpos(DEF_femnodepos)+ p_sigtmp%i_nodepoints
	  END IF
	  IF(p_sigtmp%i_edgepoints > 0) THEN
	    i_varpattern(DEF_femedgepos,i_cnt)= i_varmaxpos(DEF_femedgepos)+ 1
	    i_varmaxpos(DEF_femedgepos)= i_varmaxpos(DEF_femedgepos)+ p_sigtmp%i_edgepoints
	  END IF
	  IF(p_sigtmp%i_elmtpoints > 0) THEN
	    i_varpattern(DEF_femelmtpos,i_cnt)= i_varmaxpos(DEF_femelmtpos)+ 1
	    i_varmaxpos(DEF_femelmtpos)= i_varmaxpos(DEF_femelmtpos)+ p_sigtmp%i_elmtpoints
	  END IF
	  IF(p_sigtmp%i_tetpoints > 0) THEN
	    i_varpattern(DEF_femtetpos,i_cnt)= i_varmaxpos(DEF_femtetpos)+ 1
	    i_varmaxpos(DEF_femtetpos)= i_varmaxpos(DEF_femtetpos)+ p_sigtmp%i_tetpoints
	  END IF
	  i_varpattern(DEF_femtypepos,i_cnt)= i_femtype
	END DO
	i_maxfemvars= i_varindex

!---------- return value is the first array index

	i_varindex= i_return

	RETURN
	END FUNCTION grid_registerfemvar

!*****************************************************************
	SUBROUTINE grid_femtypequery(i_femtype, i_order, c_description, &
	           i_unknowns, i_nodedofs, i_edgedofs, i_elmtdofs, i_tetradofs, i_fembasis)

!---------- local declarations

	IMPLICIT NONE
	INTEGER (KIND = GRID_SI)           :: i_femtype
	INTEGER (KIND = GRID_SI), OPTIONAL :: i_order
	INTEGER (KIND = GRID_SI), OPTIONAL :: i_unknowns
	INTEGER (KIND = GRID_SI), OPTIONAL :: i_nodedofs
	INTEGER (KIND = GRID_SI), OPTIONAL :: i_edgedofs
	INTEGER (KIND = GRID_SI), OPTIONAL :: i_elmtdofs
        INTEGER (KIND = GRID_SI), OPTIONAL :: i_tetradofs
	INTEGER (KIND = GRID_SI), OPTIONAL :: i_fembasis
	CHARACTER (len=32), OPTIONAL       :: c_description
	TYPE (fem_signatur), POINTER       :: p_sigtmp

!---------- action type: if i_femtype <= 0: give back number of supported FEM types

	act_typ: IF((i_femtype <= 0) .OR. (i_femtype > DEF_femtypes)) THEN
	  i_femtype= DEF_femtypes
	ELSE act_typ
	  p_sigtmp=> GRID_femtypes%p_type(i_femtype)%sig
	  IF(present(i_order)) i_order= p_sigtmp%i_order
	  IF(present(c_description)) c_description= p_sigtmp%c_name
	  IF(present(i_unknowns)) i_unknowns= p_sigtmp%i_unknowns
	  IF(present(i_nodedofs)) i_nodedofs=p_sigtmp%i_nodepoints
	  IF(present(i_edgedofs)) i_edgedofs=p_sigtmp%i_edgepoints
	  IF(present(i_elmtdofs)) i_elmtdofs=p_sigtmp%i_elmtpoints
          IF(present(i_tetradofs)) i_tetradofs=p_sigtmp%i_tetpoints
	  IF(present(i_fembasis)) i_fembasis=p_sigtmp%i_fembasentry
	END IF act_typ

	RETURN
	END SUBROUTINE grid_femtypequery


!*****************************************************************
	SUBROUTINE grid_femvarrecover(i_varnum, i_vars)

!---------- local declarations

	IMPLICIT NONE
	INTEGER (KIND = GRID_SI)                                    :: i_varnum
	INTEGER (KIND = GRID_SI), DIMENSION(:), POINTER             :: i_vars
	
	INTEGER (KIND = GRID_SI), DIMENSION(:), ALLOCATABLE, TARGET :: i_tmp
	INTEGER (KIND = GRID_SI)                                    :: i_alct

!---------- give total number of available fem variables

	i_varnum= i_maxfemvars

!---------- allocate array, if not allocated yet

	IF(.NOT. ASSOCIATED(i_vars)) THEN
	  ALLOCATE(i_tmp(i_varnum), stat= i_alct)
	  IF(i_alct /= 0) CALL print_error(a_err='[grid_femvarrecover]: Could not allocate array i_vars')
	  i_vars=> i_tmp
	ELSE
	  DEALLOCATE(i_vars)
	  NULLIFY(i_vars)
	  ALLOCATE(i_tmp(i_varnum), stat= i_alct)
	  IF(i_alct /= 0) CALL print_error(a_err='[grid_femvarrecover]: Could not allocate array i_vars')
	  i_vars=> i_tmp
	END IF
	
	i_vars= i_varpattern(DEF_femtypepos,1:i_varnum)
	

	RETURN
	END SUBROUTINE grid_femvarrecover

!*****************************************************************
	FUNCTION grid_femvarquery(i_varindex) RESULT (i_femtype)

!---------- local declarations

	IMPLICIT NONE
	INTEGER (KIND = GRID_SI)           :: i_varindex
	INTEGER (KIND = GRID_SI)           :: i_femtype

!---------- action type: if i_femtype <= 0: give back number of supported FEM types

	act_typ: IF((i_varindex <= 0) .OR. (i_varindex > i_maxfemvars)) THEN
	  CALL print_error(a_err='[grid_femvarquery]: Given variable index invalid')
	ELSE act_typ
	  i_femtype= i_varpattern(DEF_femtypepos,i_varindex)
	END IF act_typ

	RETURN
	END FUNCTION grid_femvarquery

!*****************************************************************
	FUNCTION grid_registerfemtype(c_signame) RESULT (i_femtype)

!---------- local declarations

	IMPLICIT NONE
	CHARACTER (LEN=*)                  :: c_signame
	INTEGER (KIND = GRID_SI)           :: i_femtype
	TYPE(fem_sig_arr), DIMENSION(:), POINTER :: p_femsigarr
	TYPE (fem_signatur), POINTER             :: p_sigtmp
	INTEGER (KIND = GRID_SI) :: i_alct, i_tmp

!---------- (re-) allocate main data structure

        DEF_femtypes= DEF_femtypes+ 1_GRID_SI
	IF(DEF_femtypes > DEF_maxfemtypes) &
	  CALL print_error(a_err='[grid_registerfemtype] Max. number of supported FEM types exceeded')
	IF(DEF_femtypes == 1_GRID_SI) THEN
	  NULLIFY(p_femsigarr)
	  ALLOCATE(GRID_femtypes%p_type(DEF_maxfemtypes), stat=i_alct)
	  IF(i_alct /= 0) CALL print_error(a_err='[grid_registerfemtype] Could not allocate p_femsigarr data structure')
	  GRID_femtypes%i_number=  DEF_femtypes
	  NULLIFY(p_sigtmp)
	  ALLOCATE(p_sigtmp, stat=i_alct)
	  IF(i_alct /= 0) CALL print_error(a_err='[grid_registerfemtype] Could not allocate p_sigtmp data structure')
	  GRID_femtypes%p_type(DEF_femtypes)%sig=> p_sigtmp
	ELSE IF(GRID_femtypes%i_number < DEF_femtypes) THEN
	  GRID_femtypes%i_number=  DEF_femtypes
	  NULLIFY(p_sigtmp)
	  ALLOCATE(p_sigtmp, stat=i_alct)
	  IF(i_alct /= 0) CALL print_error(a_err='[grid_registerfemtype] Could not allocate p_sigtmp data structure')
	  GRID_femtypes%p_type(DEF_femtypes)%sig=> p_sigtmp
	END IF

!$OMP PARALLEL
        IF(ALLOCATED(i_dsfc_start))THEN
           DEALLOCATE(i_dsfc_start)
        END IF
        ALLOCATE(i_dsfc_start(DEF_femtypes, DEF_timesteps))
        IF(ALLOCATED(i_dsfc_end))THEN
           DEALLOCATE(i_dsfc_end)
        END IF
        ALLOCATE(i_dsfc_end(DEF_femtypes, DEF_timesteps))
!$OMP END PARALLEL

!---------- set file name

	i_tmp= MIN(DEF_fillen,LEN(c_signame))
	GRID_femtypes%p_type(DEF_femtypes)%sig%c_sigfile= '                                '
	GRID_femtypes%p_type(DEF_femtypes)%sig%c_sigfile(1:i_tmp)= c_signame(1:i_tmp)

!---------- nullify array pointers (this is for the stupic aix compiler)

	NULLIFY(GRID_femtypes%p_type(DEF_femtypes)%sig%r_gcoo)
	NULLIFY(GRID_femtypes%p_type(DEF_femtypes)%sig%r_ecoo)
	NULLIFY(GRID_femtypes%p_type(DEF_femtypes)%sig%r_qcoo)
	NULLIFY(GRID_femtypes%p_type(DEF_femtypes)%sig%r_qwei)
	NULLIFY(GRID_femtypes%p_type(DEF_femtypes)%sig%r_coef)
	NULLIFY(GRID_femtypes%p_type(DEF_femtypes)%sig%r_dfdxi)
	NULLIFY(GRID_femtypes%p_type(DEF_femtypes)%sig%r_dfdeta)

!---------- return femtype index

	i_femtype= DEF_femtypes

	RETURN
	END FUNCTION grid_registerfemtype

!*****************************************************************
	SUBROUTINE read_femsignature(i_femtyp)

!---------- local declarations

	IMPLICIT NONE
	INTEGER (KIND = GRID_SI), INTENT(in) :: i_femtyp
	
	INTEGER (KIND = GRID_SI)             :: i_iofil, i_iost, i_alct, &
	  i_ioend, i_cnt, i_tmp, i_len, i_wid
	CHARACTER (LEN = DEF_fillen)         :: c_nam
	CHARACTER (LEN = 80)                 :: c_filrow
	REAL (KIND = GRID_SR), DIMENSION(:,:), POINTER :: r_tmparr2
	REAL (KIND = GRID_SR), DIMENSION(:), POINTER :: r_tmparr1

!---------- open file

	c_nam= GRID_femtypes%p_type(i_femtyp)%sig%c_sigfile
	i_iofil= 17
	open(unit= i_iofil, file= c_nam, status= 'OLD', action= 'READ', iostat= i_iost)
	file_notopen: IF(i_iost /= 0) THEN
	  IF(GRID_parameters%iolog > 0) &
	    write(GRID_parameters%iolog,*) 'ERROR: Filename: ', c_nam
	  CALL print_error(a_err='[read_femsignature] Could not open signature file')
	END IF file_notopen
	IF(GRID_parameters%iolog > 0) &
	  write(GRID_parameters%iolog,*) 'INFO: [read_femsignature] Opened file ',c_nam,' on unit: ', i_iofil

!---------- initialize

	GRID_femtypes%p_type(i_femtyp)%sig%i_order= 0_GRID_SI
	GRID_femtypes%p_type(i_femtyp)%sig%i_unknowns= 0_GRID_SI
	GRID_femtypes%p_type(i_femtyp)%sig%i_nodepoints= 0_GRID_SI
	GRID_femtypes%p_type(i_femtyp)%sig%i_edgepoints= 0_GRID_SI
	GRID_femtypes%p_type(i_femtyp)%sig%i_elmtpoints= 0_GRID_SI
	GRID_femtypes%p_type(i_femtyp)%sig%i_tetpoints= 0_GRID_SI
	GRID_femtypes%p_type(i_femtyp)%sig%i_quadpoints= 0_GRID_SI
	GRID_femtypes%p_type(i_femtyp)%sig%i_fembasentry= 0_GRID_SI

	NULLIFY(GRID_femtypes%p_type(i_femtyp)%sig%r_gcoo)
	NULLIFY(GRID_femtypes%p_type(i_femtyp)%sig%r_ecoo)
	NULLIFY(GRID_femtypes%p_type(i_femtyp)%sig%r_tcoo)
	NULLIFY(GRID_femtypes%p_type(i_femtyp)%sig%r_qcoo)
	NULLIFY(GRID_femtypes%p_type(i_femtyp)%sig%r_qwei)
	NULLIFY(GRID_femtypes%p_type(i_femtyp)%sig%r_coef)
	NULLIFY(GRID_femtypes%p_type(i_femtyp)%sig%r_dfdxi)
	NULLIFY(GRID_femtypes%p_type(i_femtyp)%sig%r_dfdeta)
	NULLIFY(GRID_femtypes%p_type(i_femtyp)%sig%r_dfdzeta)

!---------- loop to read all the lines of file

	read_loop: DO
	  read(i_iofil,2000,iostat=i_ioend) c_filrow

!---------- if file ended

	  file_end: IF(i_ioend /= 0) THEN
	    close(i_iofil)
	    IF(GRID_parameters%iolog > 0) &
	      write(GRID_parameters%iolog,*) 'INFO: Closed file on unit: ', i_iofil
	    EXIT read_loop
	  ELSE file_end

!---------- decide what to DO with line according to first character

            comment_line: IF(c_filrow(1:1) == '#' .or. c_filrow(1:1) == '!') THEN
               CYCLE read_loop
            endif comment_line

            data_type: select case (c_filrow(1:14))
	    
	    case ('FEM_DESCR_NAME') 
	      read(i_iofil,*) GRID_femtypes%p_type(i_femtyp)%sig%c_name

	    case ('FEM_APPROX_ORD') 
	      read(i_iofil,*) GRID_femtypes%p_type(i_femtyp)%sig%i_order

	    case ('FEM_TOTAL_UNKN') 
	      read(i_iofil,*) GRID_femtypes%p_type(i_femtyp)%sig%i_unknowns

	    case ('FEM_VERTEX_UNK') 
	      read(i_iofil,*) GRID_femtypes%p_type(i_femtyp)%sig%i_nodepoints

	    case ('FEM_EDGE_UNKNO') 
	      read(i_iofil,*) GRID_femtypes%p_type(i_femtyp)%sig%i_edgepoints

            case ('FEM_ELEMENT_UN')
	      read(i_iofil,*) GRID_femtypes%p_type(i_femtyp)%sig%i_elmtpoints

	    case ('FEM_TETRA_UNKN') 
	      read(i_iofil,*) GRID_femtypes%p_type(i_femtyp)%sig%i_tetpoints

	    case ('FEM_QUAD_POINT') 
	      read(i_iofil,*) GRID_femtypes%p_type(i_femtyp)%sig%i_quadpoints

	    case ('FEM_ENTRY_POIN') 
	      read(i_iofil,*) GRID_femtypes%p_type(i_femtyp)%sig%i_fembasentry

	    case ('FEM_EDGE_POSIT') 
	      i_len= GRID_femtypes%p_type(i_femtyp)%sig%i_edgepoints
	      NULLIFY(r_tmparr2)
	      ALLOCATE(r_tmparr2(DEF_tetnodes,i_len), stat=i_alct)
	      IF(i_alct /= 0) CALL print_error(a_err='[read_femsignature] Could not allocate edge unknown positions')
	      DO i_cnt=1,i_len
	        read(i_iofil,*) i_tmp, r_tmparr2(1:DEF_egnodes,i_cnt)
	      END DO
	      GRID_femtypes%p_type(i_femtyp)%sig%r_gcoo=> r_tmparr2
	      NULLIFY(r_tmparr2)

	    case ('FEM_FACE_POSIT') 
	      i_len= GRID_femtypes%p_type(i_femtyp)%sig%i_elmtpoints
	      NULLIFY(r_tmparr2)
	      ALLOCATE(r_tmparr2(DEF_tetnodes,i_len), stat=i_alct)
	      IF(i_alct /= 0) CALL print_error(a_err='[read_femsignature] Could not allocate element unknown positions')
	      DO i_cnt=1,i_len
	        read(i_iofil,*) i_tmp, r_tmparr2(1:DEF_tetnodes,i_cnt)
	      END DO
	      GRID_femtypes%p_type(i_femtyp)%sig%r_ecoo=> r_tmparr2
	      NULLIFY(r_tmparr2)

	    case ('FEM_QUAD_POSIT') 
	      i_len= GRID_femtypes%p_type(i_femtyp)%sig%i_quadpoints
	      NULLIFY(r_tmparr2)
	      ALLOCATE(r_tmparr2(DEF_tetnodes,i_len), stat=i_alct)
	      IF(i_alct /= 0) CALL print_error(a_err='[read_femsignature] Could not allocate quad positions')
	      DO i_cnt=1,i_len
	        read(i_iofil,*) i_tmp, r_tmparr2(1:DEF_elnodes,i_cnt)
	      END DO
	      GRID_femtypes%p_type(i_femtyp)%sig%r_qcoo=> r_tmparr2
	      NULLIFY(r_tmparr2)

	    case ('FEM_QUAD_WEIGH') 
	      i_len= GRID_femtypes%p_type(i_femtyp)%sig%i_quadpoints
	      NULLIFY(r_tmparr1)
	      ALLOCATE(r_tmparr1(i_len), stat=i_alct)
	      IF(i_alct /= 0) CALL print_error(a_err='[read_femsignature] Could not allocate quad weights')
	      DO i_cnt=1,i_len
	        read(i_iofil,*) i_tmp, r_tmparr1(i_cnt)
	      END DO
	      GRID_femtypes%p_type(i_femtyp)%sig%r_qwei=> r_tmparr1
	      NULLIFY(r_tmparr1)

	    case ('FEM_COEF_PLAIN') 
	      i_len= GRID_femtypes%p_type(i_femtyp)%sig%i_quadpoints
	      i_wid= GRID_femtypes%p_type(i_femtyp)%sig%i_unknowns
	      NULLIFY(r_tmparr2)
	      ALLOCATE(r_tmparr2(i_wid,i_len), stat=i_alct)
	      IF(i_alct /= 0) CALL print_error(a_err='[read_femsignature] Could not allocate f coefficients')
	      DO i_cnt=1,i_len
	        read(i_iofil,*) r_tmparr2(1:i_wid,i_cnt)
	      END DO
	      GRID_femtypes%p_type(i_femtyp)%sig%r_coef=> r_tmparr2
	      NULLIFY(r_tmparr2)

	    case ('FEM_COEF_DFDXI') 
	      i_len= GRID_femtypes%p_type(i_femtyp)%sig%i_quadpoints
	      i_wid= GRID_femtypes%p_type(i_femtyp)%sig%i_unknowns
	      NULLIFY(r_tmparr2)
	      ALLOCATE(r_tmparr2(i_wid,i_len), stat=i_alct)
	      IF(i_alct /= 0) CALL print_error(a_err='[read_femsignature] Could not allocate df/dxi coefficients')
	      DO i_cnt=1,i_len
	        read(i_iofil,*) r_tmparr2(1:i_wid,i_cnt)
	      END DO
	      GRID_femtypes%p_type(i_femtyp)%sig%r_dfdxi=> r_tmparr2
	      NULLIFY(r_tmparr2)

	    case ('FEM_COEF_DFDET') 
	      i_len= GRID_femtypes%p_type(i_femtyp)%sig%i_quadpoints
	      i_wid= GRID_femtypes%p_type(i_femtyp)%sig%i_unknowns
	      NULLIFY(r_tmparr2)
	      ALLOCATE(r_tmparr2(i_wid,i_len), stat=i_alct)
	      IF(i_alct /= 0) CALL print_error(a_err='[read_femsignature] Could not allocate df/deta coefficients')
	      DO i_cnt=1,i_len
	        read(i_iofil,*) r_tmparr2(1:i_wid,i_cnt)
	      END DO
	      GRID_femtypes%p_type(i_femtyp)%sig%r_dfdeta=> r_tmparr2
	      NULLIFY(r_tmparr2)

	    case ('FEM_COEF_DFDZE') 
	      i_len= GRID_femtypes%p_type(i_femtyp)%sig%i_quadpoints
	      i_wid= GRID_femtypes%p_type(i_femtyp)%sig%i_unknowns
	      NULLIFY(r_tmparr2)
	      ALLOCATE(r_tmparr2(i_wid,i_len), stat=i_alct)
	      IF(i_alct /= 0) CALL print_error(a_err='[read_femsignature] Could not allocate df/deta coefficients')
	      DO i_cnt=1,i_len
	        read(i_iofil,*) r_tmparr2(1:i_wid,i_cnt)
	      END DO
	      GRID_femtypes%p_type(i_femtyp)%sig%r_dfdeta=> r_tmparr2
	      NULLIFY(r_tmparr2)

	    END select data_type

	  END IF file_end
	END DO read_loop

	RETURN
 2000	FORMAT(a80)
	END SUBROUTINE read_femsignature
!*****************************************************************
!	SUBROUTINE 
!
!---------- local declarations
!
!	IMPLICIT NONE
!
!	RETURN
!	END SUBROUTINE 

	END MODULE FEM_signature
