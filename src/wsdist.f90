SUBROUTINE WSDIST(L,IB,nyr,tfsup,Re_Ro, cussup, fsuply, wsuply, &
				!Outputs
     sur_sup, wel_sup)

  !***************************************************************************
  !
  !   Function		  : wsdist.f
  !   Author		  : Luis A. Garcia
  !   Date			  : July 2000 
  !   Purpose		  : This calculates the water supply distribution for the
  ! 					surface and groundwater depending on the options the
  ! 					user has selected.
  !   Calling programs: bcwboring.f, bcsmb.f 
  !   Called programs :  
  !   Input arguments : ib = current basin 
  ! 				  : L  = current month/day, depending on mode
  ! 				  : Re_Ro = Effective Rainfall Runoff
  !   Output arguments: none
  !   Assumptions	  :
  !   Limitations	  :
  !
  !   History		  :(Date, Author, Description)
  !
  !
  !***************************************************************************

  USE Globals

  !Parameters
  INTEGER L, IB, nyr
  REAL tfsup, Re_Ro, cussup(:), fsuply(:), wsuply(:)
  !Outputs
  REAL sur_sup(:), wel_sup(:)

  REAL extra
  REAL S_SPLIT ! Surface water portion of the available water supply to meet CU.

  sur_sup(L) = 0.0
  wel_sup(L) = 0.0

  !SUPDIST - Water Supply Distribution
  ! 	  = 1 - Water Supply is Evenly Distributed
  ! 	  = 0 - Surface Water Supply is used first
  ! 	  = 2 - Well Water is used first
  !If the total suply is greater than zero and the distribution
  ! 	is equally distributed between surface and groundwater
  ! 	If well mode is 2 then use gw pumping record

  S_SPLIT = 0.5 ! default is to split evenly
  IF (SUPDIST(IB) .EQ. 3 .AND. (FSUPLY(L) + WSUPLY(L)) .GT. 0) THEN
     S_SPLIT = FSUPLY(L) / (FSUPLY(L) + WSUPLY(L))
  END IF
  IF (SUPDIST(IB) .EQ. 1 .OR. SUPDIST(IB) .EQ. 3) THEN

     ! Split evenly.
     sur_sup(L) = cussup(L) * S_SPLIT
     wel_sup(L) = cussup(L) * (1 - S_SPLIT)

     ! Check if capacities are exceeded.  Start will surface supplies.
     if (sur_sup(L) .gt. fsuply(L)) then
	extra = sur_sup(L) - fsuply(L)
	sur_sup(L) = fsuply(L)
        ! See if we can add excess to well depletion
	wel_sup(L) = wel_sup(L) + extra
	if (wel_sup(L) .gt. wsuply(L)) then
	   extra = wel_sup(L) - wsuply(L)
	   wel_sup(L) = wsuply(L)
	end if
     end if

     ! Check well supply.
     if (wel_sup(L) .gt. wsuply(L)) then
	extra = wel_sup(L) - wsuply(L)
	wel_sup(L) = wsuply(L)
        ! See if we can add excess to surface depletion
	sur_sup(L) = sur_sup(L) + extra

	if (sur_sup(L) .gt. fsuply(L)) then
	   extra = sur_sup(L) - fsuply(L)
	   sur_sup(L) = fsuply(L)
	end if
     end if

     !If the total suply is greater than zero and the distribution
     !  is first surface and any left over groundwater if amount of
     !  water needed is less than what is available
     ! 	
  ELSEIF (SUPDIST(IB) .EQ. 0 ) THEN
     IF (cussup(L).GT.fsuply(L) .AND. cussup(L).LT.tfsup) THEN
	sur_sup(L) = fsuply(L)
	wel_sup(L) = cussup(L)-sur_sup(L)

     !If the total supply is greater than zero and the distribution
     !  is first surface and any left over groundwater if amount of
     !  water needed is more than what is available
     ELSEIF (cussup(L).GT.fsuply(L) .AND. cussup(L).GE.tfsup) THEN
	sur_sup(L) = fsuply(L)
	wel_sup(L) = tfsup-sur_sup(L)
     ELSEIF (cussup(L) .LE. fsuply(L)) THEN
	sur_sup(L) = cussup(L)
	wel_sup(L) = 0
     ENDIF
  !If the total suply is greater than zero and the distribution
  !  is first well and any left over surface if amount of
  !  water needed is less than what is available
  ! 	
  ELSEIF (SUPDIST(IB) .EQ. 2 ) THEN
     wel_sup(L) = cussup(L)

     ! Check well supply.
     if (wel_sup(L) .gt. wsuply(L)) then
	extra = wel_sup(L) - wsuply(L)
	wel_sup(L) = wsuply(L)
        ! See if we can add excess to surface depletion
	sur_sup(L) = sur_sup(L) + extra

	if (sur_sup(L) .gt. fsuply(L)) then
	   extra = sur_sup(L) - fsuply(L)
	   sur_sup(L) = fsuply(L)
	end if
     end if
  ENDIF

  !If the well mode is equal to the option where the well pumping
  !  is used (2) and it is assigned based on the user selection of
  !  surface first of surface and gw in equal amounts.

  IF (WMODE(IB) .EQ. 2) THEN

     !If Re_Ro is greater or equal to 0 then this is the water budget
     !	 for the option with soil moisture.
     !
     IF (Re_Ro .GE. 0.0) THEN
	WellPWS(IB,NYR,L) = wel_sup(L)
     ELSE
	WellP(IB,NYR,L) = wel_sup(L)
     ENDIF

  ENDIF

  RETURN
END SUBROUTINE WSDIST
