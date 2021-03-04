SUBROUTINE KPGROWTH(CKEY, JSTR, JSTP, XKPC)

  !***************************************************************************
  !
  !   Function        : kpgrowth.f 
  !   Author          : LA Garcia
  !   Date            : October 1998
  !   Purpose         : This calculates the daily crop coefficient for Kimberly-
  !                     Penman method by straight line interpolation using 
  !                     the crop coefficient data set provided in 
  !                     file *.kref.
  !   Calling program : proto.f
  !   Called programs : kckp.f, kckp2.f 
  !   Input arguments : cid - crop index
  !   Output arguments: none
  !   Assumptions     : 
  !   Limitations     : The data set of crop coefficients in file *.kref
  !                     should include exactly 33 discrete pairs of data
  !                     for alfalfa, 11 for pasture and 22 for the rest of
  !                     the crops.
  !   Notes           : The interpolation routine is still valid even if nlen
  !                     is less than zero.  If growing season is less than
  !                     peak, the crop coefficient curve is still normalized
  !                     with the peak rather than the shorter growing season.
  !
  !   History         : (Date, Author, Description)
  !
  !
  !***************************************************************************

  USE Globals

  !Parameters
  INTEGER CKEY, JSTR, JSTP
  !Outputs
  REAL XKPC(366)

  INTERFACE
     INTEGER FUNCTION CRPTYPE (ndx)
       INTEGER ndx
     END FUNCTION CRPTYPE

     SUBROUTINE KCKP(CKEY,NLEN,ISTART,DPASS,JSTR,JSTP, &
	  !Outputs
          XKPC)
       INTEGER CKEY,NLEN,ISTART,DPASS, JSTR, JSTP
       !Outputs
       REAL XKPC(366)
     END SUBROUTINE KCKP

     SUBROUTINE KCKP2(CKEY,NLEN,ISTART,DPASS,JSTR,JSTP, &
	  !Outputs
          XKPC)
       INTEGER CKEY,NLEN,ISTART,DPASS, JSTR, JSTP
       !Outputs
       REAL XKPC(366)
     END SUBROUTINE KCKP2

  END INTERFACE

  !Local Variable Declaration
  INTEGER I, NLEN, DPASS

  DO  I = 1,366
     XKPC(I) = 0.0
  END DO

  IF (CRPTYPE(CKEY) .EQ. 1) THEN ! If crop is perrenial use 11 segment with days
     !planting to harvest
     NLEN = JSTP-JSTR+1
     DPASS = 0
     CALL KCKP2(CKEY,NLEN,1,DPASS, JSTR, JSTP, XKPC)

  ELSE                             ! Other crops

     !planting to peak
     NLEN = GDATE5(CKEY)
     DPASS = 0
     CALL KCKP(CKEY,NLEN,1,DPASS, JSTR, JSTP, XKPC)

     !peak to harvest (days after peak)
     NLEN = JSTP-JSTR+1-GDATE5(CKEY)
     DPASS = GDATE5(CKEY)
     CALL KCKP2(CKEY,NLEN,12,DPASS, JSTR, JSTP, XKPC)

  ENDIF

200 RETURN
END SUBROUTINE KPGROWTH
