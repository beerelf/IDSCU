SUBROUTINE BCWBORIG(IB, nyr, nperiods, actual_nper, iper, farmlos, cuirr, suploss, &
				! Output
     shorts, shorts_from_reduc, shorts_d, wpump, reqt, fsuply, runoff, cussup, sur_sup, wel_sup, cappeff)

  !***************************************************************************
  !
  !   Function        : bc_wborig.f
  !   Author          : Luis A. Garcia
  !   Date            : October 1999 
  !   Purpose         : This calculates the crop consumptive use by Blaney- 
  !                     Criddle ET method.  The original version is based
  !                     on the SCS Modified Blaney-Criddle method.  An 
  !                     enhanced version is also available in which it
  !                     uses a monthly soil moisture budget and the concept
  !                     of winter carry over soil moisture based on total
  !                     precipitation during the preceding off-season.It also
  !                     generates an output file *.obc that contains a 
  !                     detailed results of the consumptive calculation using
  !                     Blaney-Criddle.
  !   Calling program : calcrop.f 
  !   Called programs : clndr.f, perencrp.f, annuacrp.f, xcrain.f 
  !   Input arguments : ib = current basin 
  !   Output arguments: none
  !   Assumptions     :
  !   Limitations     :
  !   Notes           : The routines for the original Blaney-Criddle method 
  !                     are based on USBR XCONS2 program which uses the SCS 
  !                     Modified Blaney-Criddle ET Estimation Method.
  !
  !   History         :(Date, Author, Description)
  !
  !     11/02/95    HBM    Units changed from 1000 acre-ft to acre-ft.
  !
  !***************************************************************************

  USE Globals

  ! Parameters
  INTEGER IB, nyr, nperiods, actual_nper, iper
  REAL farmlos(:), SUPLOSS(:,:)
  ! Output
  REAL wpump(:), reqt(:), shorts(:), shorts_from_reduc(:), shorts_d(:)
  REAL fsuply(:), runoff(:), cussup(:), cuirr(:,:)
  REAL sur_sup(:), wel_sup(:), cappeff(:)
  REAL tot_cuirr, tot_area, rsum

  INTERFACE
     SUBROUTINE WELLSUP(BASIN,WY,M,PUMP,FIELD_EFF,NPERIODS)
       INTEGER BASIN, M, WY, NPERIODS
       REAL PUMP, FIELD_EFF
     END SUBROUTINE WELLSUP

     SUBROUTINE WELLPUMP(BASIN,WY,M,SHORTW,PUMP,WSFLAG)
       INTEGER BASIN, M, WY, WSFLAG
       REAL PUMP, SHORTW
     END SUBROUTINE WELLPUMP

     SUBROUTINE WSDIST(L,IB,nyr,tfsup,Re_Ro, cussup, fsuply, wsuply, &
				! Outputs
	  sur_sup, wel_sup)
       INTEGER L, IB, nyr
       REAL tfsup, Re_Ro, cussup(:), fsuply(:), wsuply(:)
       REAL sur_sup(:), wel_sup(:)
     END SUBROUTINE WSDIST

     REAL FUNCTION AVG_DAILY(jul, nperiods, val)
       !Input args
       INTEGER jul, nperiods
       REAL val
     END FUNCTION AVG_DAILY

  END INTERFACE

  !Local Variable Declaration
  INTEGER IP, mon, day
  REAL wsup, tfsup

  !-----------------------------------------------------------
  !Perform and Write Water Balance for Blaney-Criddle Original
  !-----------------------------------------------------------
  shorts(iper) = 0.0
  shorts_from_reduc(iper) = 0.0
  wpump(iper) = 0.0
  reqt(iper) = 0.0
  cussup(iper) = 0.0
  farmlos(iper) = 0.0
  sur_sup(iper) = 0
  wel_sup(iper) = 0
  cappeff(iper) = 0

  ! Get the month so we know what application eff. to apply.
  if (actual_nper .eq. 12) then
     mon = iper
  else
     CALL CLNDR(iper, m_year, mon, day)
  end if

  ! Calculate composite application efficiency for surface water application.
  tot_cuirr = 0
  tot_area = 0
  DO 201 IP= 1, nparce(IB)
     !----------------------------------------------------------------- 
     !Check if the crop is Alfalfa or Grass.  If so then pro-rate the
     ! crop CU by the decimal that the inputs shows as short.
     !-----------------------------------------------------------------
     IF (crop_type(crop_key(IB,IP)) .EQ. 0 .OR. &
	  sub_crop_type(crop_key(IB,IP)) .EQ. 3 ) THEN
	reqt(iper) = reqt(iper) + cuirr(IP,iper)*AREA(IB,IP,nyr)*CUSHORT(IB)

 ! Composite application efficiency calc
	cappeff(iper) = cappeff(iper) + aeff(IB,IP,mon)*cuirr(IP,iper)*AREA(IB,IP,nyr)*CUSHORT(IB)
	tot_cuirr = tot_cuirr + cuirr(IP,iper)*AREA(IB,IP,nyr)*CUSHORT(IB)
	tot_area = tot_area + AREA(IB,IP,nyr)

	shorts_from_reduc(iper) = shorts_from_reduc(iper) + &
	     cuirr(IP,iper) * AREA(IB,IP,nyr)*(1-CUSHORT(IB))
     ELSE
	reqt(iper) = reqt(iper) + cuirr(IP,iper)*AREA(IB,IP,nyr)

 ! Composite application efficiency calc
	cappeff(iper) = cappeff(iper) + aeff(IB,IP,mon)*cuirr(IP,iper)*AREA(IB,IP,nyr)
	tot_cuirr = tot_cuirr + cuirr(IP,iper)*AREA(IB,IP,nyr)
	tot_area = tot_area + AREA(IB,IP,nyr)
     ENDIF
201 END DO

  if (tot_cuirr .gt. 0) then
     cappeff(iper) = cappeff(iper) / tot_cuirr
  else
     !Patterson:
     !Generate a synthetic application efficiency for when there
     !is field efficiency but no cuirr.  This will allow us to 
     !generate values for farm supply available for CU.
     rsum = 0
     DO IP = 1, nparce(IB)
	rsum = rsum + aeff(IB,IP,mon)*AREA(IB,IP,nyr)
     END DO
     IF (tot_area > 0) THEN
	cappeff(iper) = rsum / tot_area
     END IF
  end if

  reqt(iper) = reqt(iper) / 12.0 ! reqt = 1000 acre-ft
  shorts_from_reduc(iper) = shorts_from_reduc(iper) / 12.0

  IF (ISUPLY.EQ.1) THEN
     !--------------------------------------------------------
     !Multiply the surface supply by the composite application
     !efficiency to give surface supply left.
     !--------------------------------------------------------
     runoff(iper) = SUPLOSS(nyr,iper)*(1.0-cappeff(iper))
     farmlos(iper) = runoff(iper)
     fsuply(iper) = SUPLOSS(nyr,iper)*cappeff(iper)

     !------------------------------------------------
     !Determine the amount of pumping that takes place
     !when the user enters the pumping directly.  Add
     !this pumping as a supply.
     !------------------------------------------------
     CALL WELLSUP(IB,NYR,iper,wsup, cappeff(iper), nperiods)

     tfsup = fsuply(iper) + BWELACU(IB,NYR,iper)

     ! ***What do I do when well pumping is provided????
     if (tfsup .GE. reqt(iper)) THEN
	cussup(iper) = reqt(iper)

 ! If there is additional supply add it as runoff

	runoff(iper) = runoff(iper) + tfsup - reqt(iper) 
	shorts(iper) = 0.0
	shorts_d(iper) = 0
     ELSE
	shorts(iper) = reqt(iper) - tfsup
	shorts_d(iper) = reqt(iper) - tfsup

 !-------------------------------------------------------------
 !Check if the well mode is 0 - Percent of GW Supplied by Well
 !-------------------------------------------------------------
	IF(WMODE(IB) .EQ. 0 ) THEN
	   WellP(IB,NYR,iper) = shorts(iper)
	   CALL WELLPUMP(IB,NYR,iper,SHORTS(iper),wpump(iper),0)
	   WellP(IB,NYR,iper) = wpump(iper)

	ENDIF
	cussup(iper) = tfsup 
	tfsup = tfsup + wpump(iper)
     ENDIF
  ELSE
     cussup(iper) = reqt(iper)
  ENDIF

  CALL WSDIST(iper,IB,nyr,tfsup,-1.0, cussup, fsuply, BWELACU(IB,NYR,:), sur_sup, wel_sup)


  RETURN
END SUBROUTINE BCWBORIG
