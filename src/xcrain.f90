SUBROUTINE XCRAIN(ib,ip,nyr,begmo,begda,endmo,endda,apdep,t_npart,cu, &
				!Outputs
     er)

  !***************************************************************************
  !
  !   Function        : xcrain.f 
  !   Author          : HB Manguerra
  !   Date            : December 1994
  !   Purpose         : This calculates the monthly effective rainfall.
  !   Calling program : calpcrop.f 
  !   Called programs : none 
  !   Input arguments : cu    = monthly potential crop evapotranspiration 
  !                     ip    = current parcel
  !                     npart = number of days in beginning month  in spring
  !                     nbegmo= begin month of the growing season
  !                     nbegda= begin day of the growing season
  !                     nendmo= end month of the growing season
  !                     nendda= end day of the growing season
  !                     apdep = max application depth of crop
  !   Output arguments: er    = monthly effective rainfall
  !   Assumptions     :
  !   Limitations     :
  !   Notes           : The routines are based on USBR XCONS2 program which
  !                     uses the SCS Method based on a Net Application Depth
  !                     (RN_XCO=1); and USBR method (RN_XCO=2). If RN_XCO
  !                     is not equal to 1 or 2, rainfall is not considered and
  !                     effective rainfall is assumed zero.
  !
  !***************************************************************************

  USE Globals

  !Parameters
  INTEGER ib,ip,nyr,begmo,begda,endmo,endda
  REAL apdep
  !Outputs
  INTEGER t_npart
  REAL er(:,:), cu(:,:)

  INTERFACE
     INTEGER FUNCTION DaysInYear(yr)
       INTEGER yr
     END FUNCTION DaysInYear
  END INTERFACE

  !Local Variable Declaration
  INTEGER l
  REAL f, X, val

  !compute effective precipitation for each crop default
  !apdep value: 3 inch net depth of application; f=1.0
  !if apdep is specified, f value is computed based on tr-21.

  IF(apdep.EQ.3.) THEN
     F = 1.0
  ELSEIF(apdep.GT.0.) THEN
     F=0.531747+0.295164*apdep-0.057697*apdep**2 + 0.003804*apdep**3
  ENDIF


  DO L = 1, 12
     er(ip,l) = 0

     ! compute precipitation using the scs method  - monthly
     !  - uses the apdep value

     IF (RN_XCO.EQ.1) THEN
	IF(l.EQ.begmo) cu(ip,l)=cu(ip,l)*month(begmo)/t_npart
	IF(l.EQ.endmo) cu(ip,l)=cu(ip,l)*month(endmo)/endda
	! Weird bug: 0**0.82416 is throwing NaN, so check for this and 
	!   set it to zero.
	val = rntot3(ib,nyr,l)**0.82416
	if (isnan(val)) then
	   val = 0
	end if
	er(ip,l) = (0.70917*(val)-0.11556)*  &
	     (10**(0.02426*cu(ip,l)))*F

	IF(l.EQ.begmo) cu(ip,l)=cu(ip,l)*t_npart/month(begmo)
	IF(l.EQ.endmo) cu(ip,l)=cu(ip,l)*endda/month(endmo)

	!compute precipitation using the usbr method

	if (isnan(er(ip,l))) then
	   er = 0
	end if

     ELSEIF (RN_XCO.eq.2) THEN
	IF(rntot3(ib,nyr,l).LE.1.0) THEN
	   er(ip,l) = rntot3(ib,nyr,l)*0.95
	ELSEIF(rntot3(ib,nyr,l).LE.2.0) THEN
	   er(ip,l) = ((rntot3(ib,nyr,l)-1.0)*0.90) + 0.95
	ELSEIF(rntot3(ib,nyr,l).LE.3.0) THEN
	   er(ip,l) = ((rntot3(ib,nyr,l)-2.0)*0.82) + 1.85
	ELSEIF(rntot3(ib,nyr,l).LE.4.0) THEN
	   er(ip,l) = ((rntot3(ib,nyr,l)-3.0)*0.65) + 2.67
	ELSEIF(rntot3(ib,nyr,l).LE.5.0) THEN
	   er(ip,l) = ((rntot3(ib,nyr,l)-4.0)*0.45) + 3.32
	ELSEIF(rntot3(ib,nyr,l).LE.6.0) THEN
	   er(ip,l) = ((rntot3(ib,nyr,l)-5.0)*0.25) + 3.77
	ELSE
	   er(ip,l) = ((rntot3(ib,nyr,l)-6.0)*0.05) + 4.02
	ENDIF

	!Do not consider rainfall
     ELSE
	er(ip,l) = 0.0
     ENDIF

     !check values
     IF(er(ip,L).LT.0.0) er(ip,L) = 0.0
     IF(er(ip,L).GT.rntot3(ib,nyr,L)) er(ip,L) = rntot3(ib,nyr,L)
     X = month(L)
     if (L .eq. 2) then
	if (DaysInYear(NYR1+nyr-1) .eq. 366) X = 29
     endif

     IF(L.EQ.begmo) er(ip,L) = (( X-begda+1.0)/X)*er(ip,L)
     IF(L.EQ.endmo) er(ip,L) = (endda / X ) * er(ip,L)
     IF(L.eq.begmo.AND.L.eq.endmo) &
	  er(ip,L) = ((endda - begda + 1.0)/X) * er(ip,L)

  END DO

  RETURN
END SUBROUTINE XCRAIN
