SUBROUTINE KCKP2(CID,NLEN,ISTART,DPASS,JSTR,JSTP, &
     !Outputs
     XKPC)

  !***************************************************************************
  !
  !   Function        : kckp2.f 
  !   Author          : LA Garcia
  !   Date            : October 1998
  !   Purpose         : This calculates the daily crop coefficient for Kimberly-
  !                     Penman method by straight line interpolation using 
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
  !
  !***************************************************************************

  USE Globals

  !Parameters
  INTEGER CID,NLEN,ISTART,DPASS,JSTR,JSTP
  !Outputs
  REAL XKPC(366)

  !Local Variable Declaration
  INTEGER I, II, IDY
  REAL DY1, DY2

  II = ISTART
  DY1 = KCDAY(CID,II)
  DY2 = KCDAY(CID,II+1)
  DO I = 1, NLEN
     IDY = JSTR + I - 1 + DPASS
     IF (IDY.GT.JSTP) GOTO 200
     IF (DY2 .EQ. 0) THEN
	! Assume that we have reached the end of the season, so the last
	!  crop coefficient is repeated.
	XKPC(IDY) = XKPC(IDY-1)
     ELSE
        IF (I.GT.DY2) THEN
	   II = II + 1
           DY1 = DY2
           DY2 = KCDAY(CID,II+1)
	ENDIF
        IF((DY1 - DY2) .EQ. 0.0) then
	   XKPC(IDY) = 0.0
        ELSE
	   XKPC(IDY) = KCB(CID,II)-(KCB(CID,II)-KCB(CID,II+1))*  &
	     (DY1-I)/(DY1-DY2)
        ENDIF
     ENDIF
  END DO

200 RETURN
END SUBROUTINE KCKP2
