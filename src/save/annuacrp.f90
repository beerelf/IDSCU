SUBROUTINE ANNUACRP(ib,ckey,nyr,begs,ends,begmo,begda,endmo, endda, &
				!Outputs
     t_npart, days, temps, dayf, tempf, xf, xkt, xkc)

  !***************************************************************************
  !
  !   Function        : annuacrp.f
  !   Author          : HB Manguerra
  !   Date            : December 1994
  !   Purpose         : This calculates the monthly crop coefficients
  !                     mean temperature and percent daylight hours for
  !                     annual crops.  It includes the calculation required
  !                     for part months (i.e. the beginning and ending months)
  !                     of the growing season.
  !   Calling program : calpcrop.f 
  !   Called programs : myexit.f, intertd.f, interkc.f 
  !   Input arguments : ckey   = crop index
  !                     begs   = start of growing season in julian day 
  !                     ends   = end of growing season in julian day
  !                     begmo  = begin month of growing season
  !                     begda  = begin day of growing season
  !                     endmo  = end month of growing season
  !                     endda  = end day of growing season
  !   Output arguments: t_npart  = number of days in beginning month  in spring 
  !   Assumptions     :
  !   Limitations     : 
  !   Notes           : The main routines were taken from USBR XCONS2 program
  !                     which uses the SCS Modified Blaney-Criddle ET
  !                     Estimation Method.
  !
  !***************************************************************************

  USE Globals

  !Parameters
  INTEGER ib,nyr, ends, ckey, begs
  INTEGER begmo,begda,endmo,endda
  !Outputs
  INTEGER t_npart
  REAL days, temps, dayf, tempf
  REAL xf(12), xkt(12), xkc(12)

  INTERFACE
     INTEGER FUNCTION DaysInYear(yr)
       INTEGER yr
     END FUNCTION DaysInYear

     INTEGER FUNCTION JULIAN (m_mon, m_day, yr)
       INTEGER m_mon, m_day, yr
     END FUNCTION JULIAN

     SUBROUTINE INTERTD(ib,K,N,t_npart,selmon,midpt, tempc, dayC)
       INTEGER ib, t_npart, K, N, selmon, midpt
       !Outputs
       REAL dayC, tempc
     END SUBROUTINE INTERTD

     SUBROUTINE INTERKC(iper, tmps, day, ckey, &
	  !Outputs
          xf, xkt, xkc)
       INTEGER iper, ckey
       REAL tmps, day
       REAL xf(12), xkt(12), xkc(12)
     END SUBROUTINE INTERKC

  END INTERFACE

  INTEGER K, begmon, endmon
  REAL fake3
  INTEGER midpts, midptf
  INTEGER ndays_begmo
  LOGICAL is_leap

  !annual crops
  !winter wheat fall beginning date   
  !must be before December 15        

  ndays_begmo = month(begmo)
  is_leap = .false.
  if (DaysInYear(NYR1+nyr-1) .eq. 366) is_leap = .true.

  if (begmo .eq. 2 .and. is_leap) ndays_begmo = ndays_begmo + 1

  !adjust end of growing season from length of growing season
  IF((ends-begs+1).GT.GDATES(ckey)) ends=begs + GDATES(ckey) - 1

  !Calculate midpoint of beginning month in spring
  midpts=((ndays_begmo-begda+ 1)/2.0) + begda

  !Calculate midpoint of ending month in spring
  midptf= (endda + 1)/2.0

  !Calculate number of days in beginning month of spring
  t_npart= ndays_begmo-begda + 1

  !spring part month computations
  !naccum = accumulative days to midpoint of month
  !nperct = naccum/growing season length

  !  if beginning and ending month are the same,use spring numbers       
  !  for computations                                                    

  IF (begmo.EQ.endmo) midpts = ((endda - begda+1)/2) + begda 
  naccum(begmo)  =  JULIAN(begmo,midpts,nyr) - begs
  fake3 = ends - begs
  nperct(begmo) =(naccum(begmo)/( fake3 ))* 100.0

  !interpolate temperature and dayhours for beginning month in spring
  DO K = 1, 12
     IF (tmean3(ib,nyr,k) .gt. -999) THEN
	IF (JULIAN(begmo,midpts,nyr).LT.middle(K)) then
	   CALL INTERTD(ib,K,nyr,t_npart,begmo, midpts, temps,days)
	   GOTO 109
	ENDIF

	IF (JULIAN(begmo,midpts,nyr).EQ.middle(K)) then
	   temps = tmean3(ib, nyr ,K)
	   days = pclite(K)
	   GOTO 109
	ENDIF
     ENDIF
  END DO
  !call MYEXIT(99)

  !interpolate temperature and dayhours for ending month
109 IF (begmo.NE.endmo) THEN
     DO K = 1,12
	IF (tmean3(ib,nyr,k) .gt. -999) THEN
	   IF (JULIAN(endmo, midptf, nyr).LT. middle(K)) THEN
	      CALL INTERTD(ib,K,nyr,endda, endmo, midptf, tempf,dayf)
	      GO TO 118
	   ENDIF
	   IF( JULIAN(endmo, midptf, nyr) .EQ. middle(K)) THEN 
	      tempf = tmean3(ib, nyr, K)
	      dayf  = pclite(K)
	      GO TO 118
	   ENDIF
	ENDIF
     END DO
     !call MYEXIT(99)
     !! - this isn't fatal.  Assume partial year of weather data.
     tempf = 0
     dayf = 0

118  CONTINUE
  ENDIF

  !interpolate kc for beginning month in spring
  CALL INTERKC(begmo, temps, days, ckey, xf, xkt, xkc)

  !full month kc computations

  IF (begmo .NE. endmo) THEN
     IF (begmo .NE. (endmo-1)) THEN
	begmon = begmo + 1
	endmon =  endmo - 1
	DO K = begmon,endmon
	   naccum(K)=JULIAN(K,15, nyr)-begs
	   nperct(K)=(naccum(K) /( fake3 ))* 100.0
	   if (tmean3(ib, nyr,k) .gt. -999) then
	      CALL INTERKC(k, tmean3(ib, nyr,k), pclite(k), ckey, xf, xkt, xkc)
	   endif
	END DO
     ENDIF

     !interpolate kc for ending of month in fall
     naccum(endmo) = ends - begs- (endda + 1)/2.0
     nperct(endmo)=(naccum(endmo) / (fake3))* 100.0
     CALL INTERKC(endmo, tempf, dayf, ckey, xf, xkt, xkc)
  ENDIF

  RETURN
END SUBROUTINE ANNUACRP
