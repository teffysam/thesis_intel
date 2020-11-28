PROGRAM fl
IMPLICIT NONE
INCLUDE "netcdf.inc"


INTEGER         :: i_fileid i_varid, startB(4),countB(4)
REAL, DIMENSION(100,50,23,1) :: r_flowx
CHARACTER (LEN=48), INTENT(in)  :: c_filename

c_filename='Windparam.dat'

i_ncstat= nf_open(c_filename,NF_NOWRITE,i_fileid)
i_ncstat= nf_inq_varid(i_fileid, 'uvel', i_varid)
startB(1)=1
startB(2)=1
startB(3)=1
startB(4)=1
countB(4)=1
countB(3)=23
countB(2)=50
countB(1)=100
i_ncstat= nf_get_vara_real(i_fileid, i_varid, startB, countB, r_flowx)
write(*,*) r_flowx


END PROGRAM fl
