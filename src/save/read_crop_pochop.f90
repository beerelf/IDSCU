SUBROUTINE READ_CROP_POCHOP()

  !***************************************************************************
  !
  !   Function        : read_crop_pochop.f
  !   Author          : LA Garcia 
  !   Date            : July 2003
  !   Purpose         : This reads the Pochop crop coefficient file
  !                     and decides which type of crop coefficient (mean Alfalfa). 
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
  INTEGER ID, NM, J
  CHARACTER*80 REMARK

  dfile1 = dfile
  dfile1(flen:flen+4) = '.kpc'
  OPEN (UNIT=4,FILE=dfile1,STATUS='OLD',IOSTAT=IERR)
  IF (IERR.NE.0) CALL MYEXIT(5)

  READ(4,901,ERR=101) REMARK
  READ(4,*,ERR=101) NC  ! Number of crops
  DO i=1,NC 
     READ(4,900,ERR=101) ID,NM
     crptyp = CRPTYPE(ID)
     IF (crptyp.eq.0 .OR. crptyp.eq.1) THEN  ! alfalfa or perennial
	DO j=1,PKCLEN 
	   READ(4,*,ERR=101) nckcp(ID,j), ckcp(ID,j)
	END DO
     ELSEIF (crptyp.eq.2) THEN
	DO j=1,AKCLEN
	   READ(4,*,ERR=101) nckca(ID,j), ckca(ID,j)
	END DO
     ELSE
	WRITE(2,*) 'NO VALID CROP SELECTED'
     ENDIF
  END DO
  CLOSE(4)

  RETURN

900 FORMAT(I2,A40)
901 FORMAT(A80)

101 CALL MYEXIT(12)
END SUBROUTINE READ_CROP_POCHOP
