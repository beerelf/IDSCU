SUBROUTINE READ_CROP_PM()

  !***************************************************************************
  !
  !   Function        : read_crop_pm.f
  !   Author          : HB Manguerra
  !   Date            : December 1994
  !   Purpose         : This reads the Penman-Monteith crop coefficient file
  !                     and decides which type of crop coefficient (basal Kc
  !                     alfalfa based, basal Kc grass based, mean Kc alfalfa
  !                     based, mean Kc grass based) to use based on availabi- 
  !                     lity and predefined heirarchy. 
  !   Calling program : proto.f 
  !   Called programs : none
  !   Input arguments : none   
  !   Output arguments: none 
  !   Assumptions     :
  !   Limitations     :
  !   Notes           : The model does not use all available type of crop 
  !                     coefficients at the same time. The user choses a 
  !                     particular type to calculate crop CU.
  !
  !   History         : (Date, Author, Description)
  !
  !   11/14/95   HBM  : Added an extra parameter (ITYP) in *.kref file
  !                     which allow the user to specify which type of kc
  !                     parameter to use instead of letting the program to
  !                     decide based on the heirarchy originally specified.
  !                     If ITYP = 1 use Mean Alfalfa
  !                     Otherwise use Mean Grass
  !
  !   11/15/95   HBM  : Basal kc for Penman-Monteith method is not anymore
  !                     supported.
  !
  !   11/20/95   HBM  : Support kc mean format based on ASCE Manual 70.
  !
  !   12/31/02        : Modified by David Patterson to integrate it into
  !                     new 'unified' version.
  !
  !***************************************************************************

  USE Globals

  INTERFACE
     INTEGER FUNCTION CRPTYPE (ndx)
       INTEGER ndx
     END FUNCTION CRPTYPE
  END INTERFACE

  !Local Variable Declaration
  CHARACTER*80 remark
  CHARACTER*120 dfile1
  INTEGER NC, ID, I, LEN, J
  INTEGER IERR, ITYP, CTYPE
  REAL KCDUM(40,48)

  LOGICAL needHargWarning, needP48Warning

  !Read crop coefficient file *.kref
  dfile1 = dfile
  dfile1(flen:flen+4) = '.kref'

  ! Initializing KCDAY to zero so we know when the end of the crop coefficients
  !   have been reached for perennials.
  DO J=1,40
     DO I=1,33
	KCDAY(J,I) = 0.0
	KCB(J,I) = 0.0
     END DO
  END DO

  OPEN(UNIT=1,FILE=dfile1,STATUS='OLD',IOSTAT=IERR)
  IF (IERR.NE.0) CALL MYEXIT(113)
  READ(1,900,ERR=101) REMARK
  READ(1,*,ERR=101) NC
  needHargWarning = .false.
  needP48Warning = .false.
  DO I=1,NC
     LEN = 22
     READ(1,*,ERR=101) ID, CTYPE, ITYP

     !If the crop is perennial (TYP=1) then the length of the season is
     !  divided into 11 segments with days as the units.

     IF (CRPTYPE(ID) .EQ. 1) LEN = 11     ! perennial
     ! CRPTYPE does not handle alfalfa, so this check must occur after.
     IF (CTYPE .EQ. 0) LEN = 33      ! alfalfa
     !         IF (ID.EQ.2) LEN = 11      ! grass pasture

     ETMETH(ID) = ITYP 

     if (ETFLAG(7).EQ.1 .and. ITYP.EQ.1) THEN
	!Hargreaves method only uses grass reference.
	needHargWarning = .true.
     endif
     if (ETFLAG(10).EQ.1 .and. ITYP.EQ.1) THEN
	!Penman 1948 method only uses grass reference.
	needP48Warning = .true.
     endif

     IF (ITYP.EQ.1) THEN     ! mean alfalfa
	DO J=1,LEN
	   READ(1,*,ERR=101) KCDAY(ID,J),KCB(ID,J),KCDUM(ID,J)
	END DO

     ELSE                        ! mean grass
	DO J=1,LEN
	   READ(1,*,ERR=101) KCDAY(ID,J),KCDUM(ID,J),KCB(ID,J)
	END DO
     ENDIF

  END DO

  if (needHargWarning) then
     print *, "------------------------------------------"
     print *, "Warning: you are using the Hargreaves ET "
     print *, "method with alfalfa as the reference crop."
     print *, "This method SHOULD ONLY be used with grass as."
     print *, "the reference crop."
     print *, "------------------------------------------"
  end if

  if (needP48Warning) then
     print *, "------------------------------------------"
     print *, "Warning: you are using the Penman 1948 ET "
     print *, "method with alfalfa as the reference crop."
     print *, "This method CAN ONLY be used with grass as."
     print *, "the reference crop."
     print *, "------------------------------------------"
  end if

900 FORMAT(A80)
  CLOSE(1)

  RETURN

101 CALL MYEXIT(11)

END SUBROUTINE READ_CROP_PM
