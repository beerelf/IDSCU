SUBROUTINE KCPM(CKEY,NLEN,ISTART,DPASS, JSTR, JSTP, &
				!Outputs
     XKCB)

  !***************************************************************************
  !
  !   Function        : kcpm.f 
  !   Author          : HB Manguerra
  !   Date            : November 1995 
  !   Purpose         : This calculates the daily crop coefficient for Penman-
  !                     Monteith method by straight line interpolation using 
  !                     the crop coefficient data set provided in file *.kref. 
  !   Calling program : growth.f
  !   Called programs : none 
  !   Input arguments : cid    - crop index
  !                   : nlen   - length of the period for interpolation
  !                   : istart - starting crop coeff index in *.kref file
  !                   : dpass  - days after the planting (emergence)
  !   Output arguments: none
  !   Assumptions     :  
  !   Limitations     :  
  !   Notes           : The routine is still valid even if nlen is less than
  !                     zero.
  !
  !   History         : (Date, Author, Description)
  !
  !   11/27/95  HBM   : Created this subroutine to accomodate the new format
  !                     of PM coefficients.
  !
  !***************************************************************************


  USE Globals

  INTERFACE
     INTEGER FUNCTION CRPTYPE (ndx)
       INTEGER ndx
     END FUNCTION CRPTYPE
  END INTERFACE


  !Parameters
  INTEGER CKEY, NLEN, ISTART, DPASS, JSTR, JSTP
  !Outputs
  REAL XKCB(366) ! Crop coefficient interpolated

  !Local Variable Declaration
  INTEGER I, II, IDY
  REAL DY1, DY2

  !planting to peak
  II = ISTART
  ! crop_type of 0 is alfalfa, which uses percent.
  IF (crop_type(CKEY) .ne. 0 .AND. CRPTYPE(CKEY) .EQ. 1) THEN  ! perennial
     DY1 = KCDAY(CKEY,II)
     DY2 = KCDAY(CKEY,II+1)
  ELSE 
     DY1 = 0.01*KCDAY(CKEY,II)*NLEN
     DY2 = 0.01*KCDAY(CKEY,II+1)*NLEN
  END IF
  DO I = 1, NLEN
     IDY = JSTR + I - 1 + DPASS
     IF (IDY.GT.JSTP) GOTO 200
     IF (I.GT.DY2 .AND. DY2.GT.0 .AND. II+2 .le. size(KCDAY(CKEY,:))) THEN
	II = II + 1
	DY1 = DY2
	IF (crop_type(CKEY) .ne. 0 .AND. CRPTYPE(CKEY) .EQ. 1) THEN  ! perennial
	   DY2 = KCDAY(CKEY,II+1)
	ELSE
	   DY2 = 0.01*KCDAY(CKEY,II+1)*NLEN
	END IF
     ENDIF

     IF (DY2 .EQ. 0) THEN
	! Assume that we have reached the end of the season, so the last
	!  crop coefficient is repeated.
	XKCB(IDY) = XKCB(IDY-1)
     ELSE
	if ((dy2 - dy1) .eq. 0) then
	   print *, "crop error for crop ", CNAME(CKEY)
	   call myexit(100)
	endif

	XKCB(IDY) = KCB(CKEY,II)-(KCB(CKEY,II)-KCB(CKEY,II+1))* &
	     (DY1-I)/(DY1-DY2)
     END IF
  END DO

200 RETURN
END SUBROUTINE KCPM
