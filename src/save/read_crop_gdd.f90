LOGICAL FUNCTION READ_CROP_GDD()

  !***************************************************************************
  !
  !   Function        : read_crop_gdd.f
  !   Author          : LA Garcia & Dave Patterson
  !   Date            : dec 2002
  !   Purpose         : This reads the growing degree days crop parameter file.
  !   Calling program : readin.f 
  !   Called programs : none
  !   Input arguments : none   
  !   Output arguments: none 
  !   Assumptions     :
  !   Limitations     :
  !   Notes           :  
  !
  !   History         : (Date, Author, Description)
  !
  !
  !***************************************************************************

  USE Globals

  INTERFACE
     INTEGER FUNCTION CRPTYPE (ndx)
       INTEGER ndx
     END FUNCTION CRPTYPE
  END INTERFACE

  !Locals
  CHARACTER*120 dfile1
  INTEGER IERR, NC, crptyp, I
  INTEGER ID, NM, J, USE
  CHARACTER*80 REMARK

  READ_CROP_GDD = .false.

  dfile1 = dfile
  dfile1(flen:flen+4) = '.gdd'
  OPEN (UNIT=4,FILE=dfile1,STATUS='OLD',IOSTAT=IERR)
  IF (IERR.NE.0) CALL MYEXIT(5)

  READ(4,901,ERR=101) REMARK
  READ(4,*,ERR=101) NC  ! Number of crops

  DO i=1,NC 
     READ(4,*,ERR=101) ID, USE_GDD(ID), USE
     IF (USE_GDD(ID) .EQ. 1) THEN
	READ_CROP_GDD = .TRUE.
     END IF
     IF (USE .EQ. 1) THEN
	! Skip alfalfa info and read grass reference crop info.
	READ(4,*,ERR=101) 
	READ(4,*,ERR=101) 
     END IF
     READ(4,*,ERR=101) GDD_TEMP(ID,1), GDD_TEMP(ID,2), GDD_TEMP(ID,3)
     READ(4,*,ERR=101) GDD_COEFFS(ID,1), GDD_COEFFS(ID,2), GDD_COEFFS(ID,3), GDD_COEFFS(ID,4), GDD_COEFFS(ID,5), GDD_COEFFS(ID,6), GDD_COEFFS(ID,7)
     IF (USE .EQ. 0) THEN
	! Skip grass info and read alfalfa reference crop info.
	READ(4,*,ERR=101) 
	READ(4,*,ERR=101) 
     END IF
  END DO
  CLOSE(4)

  RETURN

901 FORMAT(A80)

101 CALL MYEXIT(12)
END FUNCTION READ_CROP_GDD
