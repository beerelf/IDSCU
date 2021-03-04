SUBROUTINE GROWTH(CKEY, JSTR, JSTP, &
				!Output
     XKCB)

  !***************************************************************************
  !
  !   Function        : growth.f 
  !   Author          : HB Manguerra
  !   Date            : December 1994
  !   Purpose         : This calculates the daily crop coefficient for Penman-
  !                     Monteith method by straight line interpolation using 
  !                     the crop coefficient data set provided in 
  !                     file *.kref.
  !   Calling program : proto.f
  !   Called programs : kcpm.f, kcpm2.f 
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
  !   11/27/95   HBM  : Changed to accomodate new format of PM coefficients
  !                     given in ASCE Manual 70.  Calls the kcpm.f subroutine
  !                     for the actual day-to-day interpolation.
  !
  !***************************************************************************


  USE Globals

  !Parameters
  INTEGER CKEY, JSTR, JSTP
  !Outputs
  REAL XKCB(366)

  INTERFACE
     SUBROUTINE KCPM(CID,NLEN,ISTART,DPASS, JSTR, JSTP, &
				!Outputs
	  XKCB)
       INTEGER CID, NLEN, ISTART, DPASS, JSTR, JSTP
       REAL XKCB(366)         ! Crop coefficient interpolated
     END SUBROUTINE KCPM

     SUBROUTINE KCPM2(CID,NLEN,ISTART,DPASS,JSTR,JSTP, &
				!Outputs
	  XKCB)
       INTEGER CID, NLEN, ISTART, DPASS, JSTR, JSTP
       REAL XKCB(366)         ! Crop coefficient interpolated
     END SUBROUTINE KCPM2

     INTEGER FUNCTION CRPTYPE (ndx)
       INTEGER ndx
     END FUNCTION CRPTYPE
  END INTERFACE

  !Local Variable Declaration
  INTEGER I,NLEN,DPASS, NDAYS, NCUTS
  REAL fval

  !Calculate daily mean crop coefficients
  IF (crop_type(ckey) .EQ. 0) THEN               ! alfalfa
     !planting to first cut
     NLEN = GDATE5(CKEY)
     DPASS = 0
     CALL KCPM(CKEY,NLEN,1,DPASS,JSTR,JSTP, XKCB)

     !Calculate the number of cuttings.
     ! First calculate number of growing days after first cut.
     ndays = JSTP-JSTR+1-GDATE5(CKEY)
     ! Remove the days for last cut
     ndays = ndays - CUT3(CKEY)
     
     ncuts = 0
     if (ndays .gt. 0) then
	fval = ndays
	ncuts = CEILING(fval / CUT2(CKEY))
     end if

     do icut=0,ncuts
	!intermediate cuts
	!NLEN = MIN(CUT2(CKEY),JSTP-JSTR+1-GDATE5(CKEY))
	! commented out above, should now always be the length of intermediate
	!   cutting.
	NLEN = CUT2(CKEY)
        DPASS = GDATE5(CKEY) + CUT2(CKEY) * icut
        CALL KCPM(CKEY,NLEN,12,DPASS,JSTR,JSTP, XKCB)
     end do

     !last cut occurs after the ncut intermediate cuttings.
     NLEN = MIN(CUT3(CKEY),JSTP-JSTR+1-GDATE5(CKEY)-CUT2(CKEY)*ncuts)
     DPASS = GDATE5(CKEY)+CUT2(CKEY) * ncuts
     CALL KCPM(CKEY,NLEN,23,DPASS,JSTR,JSTP, XKCB)
     ! Make sure that the crop is dead
     if (nlen + dpass .lt. (JSTP-JSTR+1)) then
	print *, "Alfalfa crop growing season should be over but is not."
     end if
     
     !Repeat until the crop dies.
     !do while (nlen + dpass .lt. (JSTP-JSTR+1))
!	!third cut to killing frost - use coeff from second to third cut
!	NLEN = MIN(CUT3(CKEY),JSTP-JSTR+1-GDATE5(CKEY)-CUT2(CKEY)-CUT3(CKEY))
!        DPASS = DPASS + CUT3(CKEY)
!	CALL KCPM(CKEY,NLEN,23,DPASS,JSTR,JSTP, XKCB)
!     end do

  ELSEIF (CRPTYPE(CKEY) .EQ. 1) THEN  ! perennial
     !planting to harvest
     NLEN = JSTP-JSTR+1
     DPASS = 0
     CALL KCPM(CKEY,NLEN,1,DPASS,JSTR,JSTP, XKCB)

  ELSE                             ! annual
     !planting to peak
     NLEN = GDATE5(CKEY)
     DPASS = 0
     CALL KCPM(CKEY,NLEN,1,DPASS,JSTR,JSTP, XKCB)

     !peak to harvest (days after peak)
     NLEN = JSTP-JSTR+1-GDATE5(CKEY)
     DPASS = GDATE5(CKEY)
     CALL KCPM2(CKEY,NLEN,12,DPASS,JSTR,JSTP, XKCB)

  ENDIF

200 RETURN
END SUBROUTINE GROWTH
