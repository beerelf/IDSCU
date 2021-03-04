SUBROUTINE KCPM2(CID,NLEN,ISTART,DPASS,JSTR,JSTP, &
				!Outputs
     XKCB)

  !***************************************************************************
  !
  !   Function        : kcpm2.f 
  !   Author          : HB Manguerra
  !   Date            : November 1995 
  !   Purpose         : This calculates the daily crop coefficient for Penman-
  !                     Monteith method by straight line interpolation using 
  !                     the crop coefficient data set provided in file *.kref
  !                     for crops other than alfalfa and pasture for the
  !                     period after the effective cover.
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
  !                     of PM coefficients for crops other than alfalfa and
  !                     pasture grass.
  !
  !***************************************************************************

  USE Globals

  !Parameters
  INTEGER CID, NLEN, ISTART, DPASS, JSTR, JSTP
  !Outputs
  REAL XKCB(366)            ! Crop coefficient interpolated

  !Local Variable Declaration
  INTEGER I, II, IDY
  REAL DY1, DY2

  II = ISTART
  DY1 = KCDAY(CID,II)
  DY2 = KCDAY(CID,II+1)

  DO I = 1, NLEN
     IDY = JSTR + I - 1 + DPASS
     IF (IDY.GT.JSTP) GOTO 200
     IF (I.GT.DY2) THEN
	II = II + 1
	DY1 = DY2
	DY2 = KCDAY(CID,II+1)
     ENDIF

     if ((dy2 - dy1) .eq. 0) then
	!print *, "crop error for crop ", CNAME(CID)
	!call myexit(100)
	XKCB(IDY) = 0.0
	exit
     else
	XKCB(IDY) = KCB(CID,II)-(KCB(CID,II)-KCB(CID,II+1))* (DY1-I)/(DY1-DY2)
     endif
  END DO

200 RETURN
END SUBROUTINE KCPM2
