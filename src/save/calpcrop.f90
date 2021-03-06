SUBROUTINE CALPCROP(IB, et_method, nperiods, suply, suploss, &
     !Outputs
     tot_rain, cuirr, er, cu, sprink_l, &
     storag, exces_er, cussup, shorts, shorts_from_reduc, shorts_stress, &
     runoff, reqt, er_st, carry, &
     wpump, fsuply, sur_sup, wel_sup, avg_rz, maxstore, &
     nbegmo, nbegda, nendmo, nendda)

  !***************************************************************************
  !
  !   Function        : calpcrop.f
  !   Author          : HB Manguerra
  !   Date            : May 1995 
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
  !   Calling program : mainxc.f 
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

  USE globals

  !Parameters
  INTEGER IB, et_method, nperiods
  REAL SUPLY(:,:), SUPLOSS(:,:)
  !Outputs
  REAL tot_rain(:,:), cuirr(:,:), er(:,:), cu(:,:), sprink_l(:,:)
  REAL storag(:), exces_er(:,:)
  REAL cussup(:), shorts(:), shorts_from_reduc(:), shorts_stress(:)
  REAL runoff(:), reqt(:), er_st(:), carry(:)
  REAL wpump(:), fsuply(:), sur_sup(:), wel_sup(:), avg_rz(:), maxstore(:)
  INTEGER nbegmo(:), nbegda(:), nendmo(:), nendda(:)

  INTERFACE
     SUBROUTINE CLNDR (jday, year, &
	  !Outputs
          m_mon, m_day)
       INTEGER jday, year, m_mon, m_day
     END SUBROUTINE CLNDR

     INTEGER FUNCTION DaysInYear(yr)
       INTEGER yr
     END FUNCTION DaysInYear

     SUBROUTINE BCSMB(nyr, IB, nperiods, actual_nper, farmlos, er, exces_er, &
	  cuirr, nendmo, nendda, cappeff, &
	  storag, er_st, carry, &
	  shorts, wpump, reqt, fsuply, runoff, cussup, &
	  sur_sup, wel_sup, avg_rz, maxstore)
       INTEGER nyr, IB, nperiods, actual_nper
       INTEGER nendmo(:), nendda(:)
       REAL farmlos(:), er(:,:), exces_er(:,:), cuirr(:,:)
       REAL storag(:), cappeff(:)
       REAL wpump(:), reqt(:), shorts(:), er_st(:), carry(:)
       REAL fsuply(:), runoff(:), cussup(:)
       REAL sur_sup(:), wel_sup(:), avg_rz(:), maxstore(:)
     END SUBROUTINE BCSMB

     SUBROUTINE BCPET(IB, et_method, nyr, actual_nper, ttemps, ddays, &
          ttempf, ddayf, xxf, xxkt, xxkc, Ret, &
          nbegmo, nbegda, nendmo, nendda, tot_rain, &
	  !Outputs
          cu, sprink_l, cuirr, er, exces_er)
       INTEGER IB, et_method, nyr, actual_nper
       REAL ttemps(:), ddays(:), ttempf(:)
       REAL ddayf(:)
       REAL xxf(:,:),xxkt(:,:),xxkc(:,:)
       REAL Ret(:,:), tot_rain(:,:)
       INTEGER nbegmo(:), nbegda(:), nendmo(:), nendda(:)
       !Output
       REAL cu(:,:), sprink_l(:,:), cuirr(:,:), er(:,:), exces_er(:,:)
     END SUBROUTINE BCPET

     SUBROUTINE BCWBORIG(IB, nyr, nperiods, actual_nper, farmlos, cuirr, suploss, &
				!Output
	  shorts, shorts_from_reduc, shorts_d, wpump, reqt, fsuply, runoff, cussup, &
	  sur_sup, wel_sup, cappeff)
       INTEGER IB, nyr, nperiods, actual_nper
       REAL farmlos(:), SUPLOSS(:,:)
       !Output
       REAL wpump(:), reqt(:), shorts(:), shorts_from_reduc(:), shorts_d(:)
       REAL fsuply(:), runoff(:), cussup(:), cuirr(:,:)
       REAL sur_sup(:), wel_sup(:), cappeff(:)
     END SUBROUTINE BCWBORIG

     SUBROUTINE BCENHANCE(nyr, IB, &
	  !Outputs
	  carry0, storag)
       INTEGER nyr, IB
       REAL carry0
       REAL storag(:)
     END SUBROUTINE BCENHANCE

     SUBROUTINE PROTO(IB, et_method, nyr, actual_nper, use_smb, &
	  !Output
          cu, sprink_l, cuirr, er, exces_er, shorts_d, shorts_stress)
       INTEGER IB, et_method, nyr, actual_nper
       LOGICAL use_smb ! If TRUE, then use a soil moisture budget
       REAL cu(:,:), sprink_l(:,:), cuirr(:,:), er(:,:), exces_er(:,:)
       REAL shorts_d(:), shorts_stress(:)
     END SUBROUTINE PROTO

     INTEGER FUNCTION JULIAN (m_mon, m_day, yr)
       INTEGER m_mon, m_day, yr
     END FUNCTION JULIAN

     SUBROUTINE ReadCU(IB, nyr, &
	  !Outputs
	  cu)
       INTEGER IB, nyr
       REAL cu(:,:)
     END SUBROUTINE ReadCU

     REAL FUNCTION AVG_DAILY(jul, nperiods, val)
       !Input args
       INTEGER jul, nperiods
       REAL val
     END FUNCTION AVG_DAILY

  END INTERFACE

  !Local Variable Declaration
  INTEGER MM, IP, ckey, skey, IDUM, days, NYR, iper, mon, day
  REAL a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,ss,t,r_z,s_l,u
  REAL inctot
  REAL xxf(MAXPARCEL,12),xxkt(MAXPARCEL,12),xxkc(MAXPARCEL,12)
  REAL Ret(MAXPARCEL,12)
  REAL ttemps(MAXPARCEL),ttempf(MAXPARCEL),ddays(MAXPARCEL)
  REAL ddayf(MAXPARCEL)
  REAL farmlos(nperiods)
  REAL cappeff(nperiods) !the composite application efficiency of the farm.
  REAL carry0
  INTEGER nper ! Number of days in each year
  REAL shorts_d(366)
  REAL awcmad
  INTEGER actual_nper ! number of periods in year after taking into
  ! account the incomplete last year
  LOGICAL skip

  !---------------------------------------------------------------------
  ! main loop that computes usage for all crops in the study
  !---------------------------------------------------------------------

  ! Start growing degree day report.
  IF (HAS_GDD) THEN
     WRITE(22,*) "Basin ", BAS_ID(IB)
  END IF

  DO MM = 1, MAXPARCEL
     storag(MM) = 0.0
     ttemps(MM) = 0.0
     npart(MM) = 0.0
  END DO

  !Start detailed water budget report.
  WRITE(23,*) "Basin ", BAS_ID(IB)
  WRITE(23, *) "Data_Item, Year, Interval, Parcel number"

  DO NYR = 1, NYRS
     ! Initialize CU
     DO ip=1, maxparcel
	DO l=1,nperiods
	   cu(ip, l) = -999
	END DO
     END DO

     if (nperiods .gt. 13) then
	! Daily.  Adjust for leap year.
	nper = DaysInYear(NYR + NYR1 -1)
     else
	nper = 12
     endif

     ! Check if this is an incomplete last year.
     if (GNYR2 .eq. NYR1 + (NYR-1)) THEN
	if (nperiods .gt. 13) then
	   ! Daily.  Adjust for leap year.
	   actual_nper = julian(INCOMPLETE_MONTH, INCOMPLETE_DAY, GNYR2)
	else
	   actual_nper = INCOMPLETE_MONTH
	endif
     else
	actual_nper = nper
     endif

     m_year = nyr

     IF (hasCIR) THEN
	! Read the CIR
	CALL ReadCU(IB, nyr, cu)
	!--------Throw error if no CU provided.
 	if (cu(1,1) .lt. 0) then
	   !call myexit(167)
	endif
     END IF

     !Initialize the farm beginning month to 12 for each year
     fbegmo(IB,NYR) = 12
     !Initialize the farm end month to 1 for each year
     fendmo(IB,NYR) = 1

     !Same for daily.
     fbegda(IB, NYR) = 366
     fendda(IB, NYR) = 1

     !Initialize the composite application efficiency to zero
     !DO mon=1,12
!	cappeff(IB,mon) = 0.0
!     END DO
!     totarea = 0.0 

     IF (HAS_GDD) THEN
	WRITE(22,*) "Year", NYR + NYR1 -1
     END IF

     ! Check if there is a problem with the growing season calculations
     ! for this year.  If so, then skip to next year.
     skip = .false.
     if (.not. hasCIR) then ! ignore if CIR is already provided
	do IP = 1, nparce(IB)
	   ! -999 is a flag to indicate spring portion of winter wheat in
	   !   the first year of the simulation
	   if (jend(ip,nyr) .le. jbeg(ip,nyr) .and. jend(ip, nyr) .ne. -999) then
	      skip = .true.
	   end if
	end do
     end if

     if (skip) then
	! Set all growing season dates to invalid states
	do IP = 1, nparce(IB)
	   jend(ip,nyr) = 1
	   jbeg(ip,nyr) = 366
	   nbegmo(ip) = 12
	   nendmo(ip) = 1
	   nbegda(ip) = 31
	   nendda(ip) = 1
	end do
     end if

     !---------------------------------
     !Initialize values to zero
     !---------------------------------
     DO IP = 1, nparce(IB)
	DO iper = 1,nperiods
	   if (.not. hasCIR) cu(IP, iper) = 0
	   cuirr(IP, iper) = 0
	   er(IP, iper) = 0
	   exces_er(IP, iper) = 0
	   sprink_l(IP, iper) = 0
	   shorts_stress(iper) = 0
	END DO

	if (.not. hasCIR) cu(IP, nperiods+1) = 0
     END DO

     !Get Growing Season (Begin and End Dates)
     DO IP = 1, nparce(IB)
	! Additional initialization
	DO iper = 1,nperiods
	   shorts_d(iper) = 0
	END DO
	   
	if (.not. skip) then
	   CALL CLNDR(jbeg(IP,nyr), NYR + NYR1 -1, nbegmo(IP), nbegda(IP))
	   CALL CLNDR(jend(IP,nyr), NYR + NYR1 -1, nendmo(IP), nendda(IP))
	
	   !Setting the initial month for a farm to the earliest month
	   !  from all the fields that are part of that farm
	   IF( nbegmo(IP) .LT. fbegmo(IB,NYR)) THEN
	      fbegmo(ib,nyr) = nbegmo(ip)
	   ENDIF
	   !Setting the final month for a farm to the latest month
	   !  from all the fields that are part of that farm
	   IF( nendmo(IP) .GT. fendmo(IB,NYR)) THEN
	      fendmo(ib,nyr) = nendmo(ip)
	   ENDIF

	   !Find earliest daily planting and harvest.
	   if (jbeg(IP,nyr) .LT. fbegda(IB, NYR)) then
	      fbegda(IB, NYR) = jbeg(IP,nyr)
	   endif
	   if (jend(IP,nyr) .GT. fendda(IB, NYR)) then
	      fendda(IB, NYR) = jend(IP,nyr)
	   endif
	endif
     END DO

     !et_method = 2 - Reference Equation  (Grass and Alfalfa Based)
     if (et_method .EQ. 2 .OR. et_method .EQ. 10 .OR. et_method .EQ. 9) then
	CALL PROTO(IB, et_method, nyr, actual_nper, .false., cu, sprink_l, cuirr, er, exces_er, &
	     shorts_d, shorts_stress)
	!--------et_method = 4 - Kimberly Penman
     elseif (et_method .EQ. 4) then
	CALL PROTO(IB, et_method, nyr, actual_nper, .false., cu, sprink_l, cuirr, er, exces_er, &
	     shorts_d, shorts_stress)
	!--------et_method = 6 ASCE
     elseif (et_method .EQ. 6) then
	CALL PROTO(IB, et_method, nyr, actual_nper, .false., cu, sprink_l, cuirr, er, exces_er, &
	     shorts_d, shorts_stress)
	!-------- User-Supplied ET
     elseif (et_method .EQ. 9) then
	CALL PROTO(IB, et_method, nyr, actual_nper, .false., cu, sprink_l, cuirr, er, exces_er, &
	     shorts_d, shorts_stress)
     else                   ! BC and KP
	CALL BCPET(IB, et_method, nyr, actual_nper, ttemps, ddays, ttempf, ddayf, &
	     xxf, xxkt, xxkc, Ret, &
	     nbegmo, nbegda, nendmo, nendda, tot_rain, &
				!Output
	     cu, sprink_l, cuirr, er, exces_er)
     endif

     CALL BCWBORIG(IB, nyr, nper, actual_nper, farmlos, cuirr, suploss, &
	  !Output
	  shorts, shorts_from_reduc, shorts_d, wpump, reqt, fsuply, runoff, &
          cussup, sur_sup, wel_sup, cappeff)

     !-----------------------------------------------
     ! Create temporary file "tmp1" for Blaney-Criddle
     !-----------------------------------------------
     IF (ISUPLY.EQ.0) THEN
	DO iper = 1, nper
	   !Sum up rain to CU for each parcel.
	   k = 0 ! rain to CU
	   j = 0 ! total rain
	   do ip = 1, nparce(IB)
	      k = k + er(IP,iper) * (AREA(IB,IP,nyr) / 12.0)
	      j = j + RNTOT3(ib,nyr,iper) * (AREA(IB,IP,nyr) / 12.0)
	   end do

	   WRITE(11,*) j, k, reqt(iper), cussup(iper), &
	 shorts_from_reduc(iper)
	 
	END DO
     ELSE
	!-------------------------------------------------
	! If the structure exist then provide water supply
	!   otherwise do not provide water supply information
	!-------------------------------------------------
	DO iper = 1, nper
	   IF(EXIST(nyr) .EQ. 1 .OR. SPFLAG .EQ. 1) THEN

	      ! Get the month so we know what application eff. to apply.
	      if (nper .eq. 12) then
		 mon = iper
	      else
		 CALL CLNDR(iper, NYR + NYR1 -1, mon, day)
	      end if

	      !A River Supply  
	      !  For a given farm (modeling area), sum of river headgate 
	      !  prorata diversions for the various surface water sources.
	      !  River headgate prorata diversion is total river 
	      !  headgate diversion for a given water source multiplied 
	      !  by "farm share allotment divided by total shares"

	      a = suply(nyr,iper)

	      !B Conveyance Loss
	      !  (1-conveyance efficiency) multiplied by A (River Supply)

	      b = suply(nyr,iper) - suploss(nyr, iper)

	      !C Farm Surface Water Supply
	      !  A minus B

	      c = suploss(nyr, iper)

	      !D Surface Water Supply Available for CU
	      !Application Efficiency for surface water multiplied by C

	      d = c * cappeff(iper)

	      !E Surface Water DP and Runoff 
	      !  C minus D

	      e = c - d

	      !F Farm Well Supply
	      !  Sum of discharge measurements in acre-feet for all wells 
	      !  on farm (modeling area)

	      IF( NWELL(IB).GT.0 .AND. WMODE(IB) .EQ. 0) THEN 
		 f = wpump(iper) 
	      ELSE                  
		 f = bwelsup(ib,nyr,iper)
	      ENDIF

	      !G Well Water Available for CU
	      !  Application Efficiency for well water multiplied by F

	      IF( NWELL(IB).GT.0 .AND. WMODE(IB) .EQ. 0) THEN 
		 g = wpump(iper) 
	      ELSE                  
		 g = bwelacu(ib,nyr,iper)
	      ENDIF

	      !H Well Water Loss DP and Runoff
	      !F minus G

	      h = f - g

	      !I Total WATER SUPPLIES (Surface + Well) Available for CU
	      !  D plus G
	      !  If scenario being viewed has no well supply, then I = D and 
	      !  F, G, and H are "greyed-out" (non active) on check box menu 
	      !  at top of Water Budget sheet.

	      i = d + g

	      !J Total Rainfall
	      !  Acre-feet for total farm irrigated acres

	      if (.not. hasCIR) then
		 j = RNTOT3(ib,nyr,iper)*T_AREA(IB,nyr)/12.0
	      else
		 j = 0
	      end if

	      !K Effective Rainfall to CU  See Footnote 1

	      k = 0
	      do ip = 1, nparce(IB)
		 k = k + er(IP,iper) * (AREA(IB,IP,nyr) / 12.0)
	      end do

	      !N Irrigation Water Requirement (IWR)
	      !  Gross CU minus K 
	      !                   Computed by field and then summed for farm (modeling area)

	      n = reqt(iper)

	      !P Shortage
	      !  Shortage is zero for scenario where groundwater assumed to 
	      !  meet all remaining IWR in a month not met by that month's 
	      !  surface water supplies or soil moisture storage.
	      !  For other scenarios (ie, no wells or using well discharge 
	      !  measurements), if O (before setting to zero or PRESET 
	      !  value) is less than zero, then absolute value of O, 
	      !  otherwise zero.

	      p = shorts(iper)

	      !T CU Met by Groundwater
	      !  Only for scenario where groundwater assumed to meet all 
	      !  remaining IWR in a month not met by that month's surface 
	      !  water supplies or soil moisture storage;
	      !  If O (before setting to zero or PRESET value) is less 
	      !  than zero, then absolute value of O, otherwise zero.

	      IF( NWELL(IB).GT.0 .AND. WMODE(IB) .EQ. 0) THEN 
		 t = wpump(iper) 
	      ELSE                 
		 t = wel_sup(iper)
	      ENDIF

	      !Q Depletion of WATER SUPPLIES 
	      !  Positive values of [O minus (Carryover soil moisture from 
	      !  previous month + iper)] plus (N - P) 
	      !  If this value is greater than I, then I, otherwise this 
	      !  value.
	      IF( NWELL(IB).GT.0 .AND. WMODE(IB) .EQ. 0) THEN 
		 q = cussup(iper) + t 
	      ELSE                  
		 q = cussup(iper)
	      ENDIF

	      !R Additional DP and Runoff of WATER SUPPLIES
	      !  I minus Q

	      r = i - q

	      !S Total DP and Runoff of WATER SUPPLIES
	      !  E plus H plus R

	      s = e + h + r

	      !SS CU Met by Surface Water

	      ss = sur_sup(iper)

	      !Shortage due to well discharge

	      ! Sprinkler spray loss
	      s_l = 0
	      do ip = 1, nparce(IB)
		 s_l = s_l + sprink_l(ip, iper) * &
		      AREA(IB,IP,nyr) * CUSHORT(IB) / 12
	      end do

	      ! NWR reduction for grass and alfalfa
	      u = shorts_from_reduc(iper)

	      if (isnan(f)) then
		 print *, "**wpump=", wpump(iper), "iper=", iper, "year=", nyr
	      end if

              ! Add composite application efficiency and shortage due to stress.
	      WRITE(11,*) a, b, c, d, e, f, g, h, i, j, k, &
		   n, p, q, r, s, ss, t, s_l, u, cappeff(iper), shorts_d(iper)

	   ELSE
	      WRITE(11,*) 0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0., &
		   0.,0.,0.,0.,0.,0.,0.
	   ENDIF
198	END DO
     ENDIF
     !-----------------------------------------------
     ! Create temporary file "tmp4" for Blaney-Criddle
     !-----------------------------------------------
     IF (ISUPLY.EQ.0) THEN
	WRITE(14,*) (cussup(MM), MM=1,nper)
     ELSE
	IF(EXIST(nyr) .EQ. 1 .OR. SPFLAG .EQ. 1) THEN
	   IF( NWELL(IB).GT.0 .AND. WMODE(IB) .EQ. 0) THEN 
	      WRITE(14,*) (cussup(MM)+wpump(MM), MM=1,nper)
	   ELSE                  
	      WRITE(14,*) (cussup(MM), MM=1,nper)
	   ENDIF
	ELSE
	   WRITE(14,*) 0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0
	ENDIF
     ENDIF

     if (hasDailyMethod) then
	CALL PROTO(IB, et_method, nyr, actual_nper, .true., cu, sprink_l, cuirr, er, exces_er, &
	     shorts_d, shorts_stress)
	! Recalculate variables
	CALL BCWBORIG(IB, nyr, nper, actual_nper, farmlos, cuirr, suploss, &
	     !Output
	     shorts, shorts_from_reduc, shorts_d, wpump, reqt, fsuply, &
	     runoff, cussup, sur_sup, wel_sup, cappeff)
     endif

     CALL BCENHANCE(nyr, IB, carry0, storag)
     CALL BCSMB(nyr, IB, nper, actual_nper, farmlos, er, exces_er, cuirr, &
	  nendmo, nendda, cappeff, &
				!Output
	  storag, er_st, carry, &
	  shorts, wpump, reqt, fsuply, runoff, cussup, &
	  sur_sup, wel_sup, avg_rz, maxstore)

     !-----------------------------------------------  
     ! Create temporary file "tmp2" for Blaney-Criddle
     !-----------------------------------------------
     IF (ISUPLY.EQ.0) THEN
	DO iper = 1, nper
	   !Sum up rain to CU for each parcel.
	   k = 0 ! rain to CU
	   j = 0 ! total rain
	   do ip = 1, nparce(IB)
	      k = k + er(IP,iper) * (AREA(IB,IP,nyr) / 12.0)
	      j = RNTOT3(ib,nyr,iper) * T_AREA(IB,nyr) / 12.0
	   end do

	   WRITE(12,*)j,k,reqt(iper),carry(iper), cussup(iper), shorts(iper), &
		shorts_from_reduc(iper), cappeff(iper)
	END DO
     ELSE
	DO iper = 1, nper
	   IF(EXIST(nyr) .EQ. 1 .OR. SPFLAG .EQ. 1) THEN

	      ! Get the month so we know what application eff. to apply.
	      if (nper .eq. 12) then
		 mon = iper
	      else
		 CALL CLNDR(iper, NYR + NYR1 -1, mon, day)
	      end if

	      !A River Supply  
	      !  For a given farm (modeling area), sum of river headgate 
	      !  prorata diversions for the various surface water sources.
	      !  River headgate prorata diversion is total river 
	      !  headgate diversion for a given water source multiplied 
	      !  by "farm share allotment divided by total shares"

	      a = suply(nyr,iper)

	      !B Conveyance Loss
	      !  (1-conveyance efficiency) multiplied by A (River Supply)

	      b = suply(nyr, iper) - suploss(nyr, iper)

	      !C Farm Surface Water Supply
	      !  A minus B

	      c = suploss(nyr, iper)

	      !D Surface Water Supply Available for CU
	      !Application Efficiency for surface water multiplied by C

	      d = c * cappeff(iper)

	      !E Surface Water DP and Runoff 
	      !  C minus D

	      e = c - d

	      !F Farm Well Supply
	      !  Sum of discharge measurements in acre-feet for all wells 
	      !  on farm (modeling area)

	      IF( NWELL(IB).GT.0 .AND. WMODE(IB) .EQ. 0) THEN 
		 f = wpump(iper) 
	      ELSE                  
		 f = bwelsup(ib,nyr,iper)
	      ENDIF

	      !G Well Water Available for CU
	      !  Application Efficiency for well water multiplied by F

	      IF( NWELL(IB).GT.0 .AND. WMODE(IB) .EQ. 0) THEN 
		 g = wpump(iper) 
	      ELSE                  
		 g = bwelacu(ib,nyr,iper)
	      ENDIF

	      !H Well Water Loss DP and Runoff
	      !F minus G

	      h = f - g

	      !I Total WATER SUPPLIES (Surface + Well) Available for CU
	      !  D plus G
	      !  If scenario being viewed has no well supply, then I = D and 
	      !  F, G, and H a_stre "greyed-out" (non active) on check box menu 
	      !  at top of Water Budget sheet.

	      i = d + g

	      !J Total Rainfall
	      !  Acre-feet for total farm irrigated acres

	      if (.not. hasCIR) then
		 j = RNTOT3(ib,nyr,iper)*T_AREA(IB,nyr)/12.0
	      else
		 j = 0
	      end if

	      !K Effective Rainfall to CU  See Footnote 1
	      !  Effective Rainfall computed by SCS (limited to CU) or 
	      !  USBR method (J multiplied by factor and then limited to 
	      !  CU) Computed by field and then summed for farm (modeling 
	      !                   area)

	      !  Footnote:
	      !  (1)  As explained in SCS Technical Release 21 (TR-21, 
	      !  September 1970), the SCS effective rainfall formula was 
	      !  " the result of a comprehensive analysis of 50 years of 
	      !  precipitation records at each of 22 Weather Bureau stations 
	      !  so selected that all climatic conditions throughout the 48 
	      !  continental states were represented."  This analysis is 
	      !  described in Herschfield, D.M., Effective Rainfall and 
	      !  Irrigation Water Requirements, Journal of Irrigation and 
	      !  Drainage Division, American Society of Civil Engineers, 
	      !  pp 33-47, 1964.

	      !  The SCS effective rainfall formula was therefore developed 
	      !  for average U.S. conditions.  A review of Herschfield by 
	      !  I.A. Walter (Evaluation of SCS TR-21 Estimates, Colorado 
	      !  ET & Irrigation Efficiency Seminar, 1995) found that only 
	      !  8 of the 22 Weather Bureau stations were located at arid 
	      !  stations similar to Colorado conditions.  Due to this 
	      !  non-similarity with arid Colorado conditions, the SCS 
	      !  effective rainfall procedure should be used with caution 
	      !  and modifications.  In fact TR-21 on page 28 states a 
	      !  caution and suggests that modifications in some areas may 
	      !  be needed in using the SCS effective rainfall procedure 
	      !  because it "necessarily fails to consider two factors" 
	      !  which have a bearing on the effectiveness of rainfall.  
	      !  These two factors are soil intake rates and rainfall 
	      !  intensities. 

	      !  It is typical in the arid conditions of Colorado that when 
	      !  rainfall occurs soil surfaces are much drier (cracked and 
	      !  porous) than average U.S. conditions and compared to an 
	      !  average, the soil profile is drier because of the high 
	      !  arid ET rates.  These conditions contribute to more 
	      !  rainfall being effective in Colorado than for an average 
	      !  mix of arid and humid U.S. conditions.  A reasonable 
	      !  modification to account for these arid factors is to not 
	      !  limit the effective rainfall to the ET for the month but 
	      !  allow the amount of effective rainfall that is greater 
	      !  than the monthly ET to be stored in the root zone profile 
	      !  if storage space exists as indicated by the previous months 
	      !  end-of-month carry-over soil moisture.  The monthly storage 
	      !  of computed effective rainfall is added first to the 
	      !  previous month's carry-over soil moisture before surface 
	      !  and well supplies available for CU are added for the month.

	      k = 0
	      do ip = 1, nparce(IB)
		 k = k + er(IP,iper) * (AREA(IB,IP,nyr) / 12.0)
	      end do

	      !L Effective Rainfall to Soil Storage
	      !  For SCS method, Effective Rainfall computed without CU limit 
	      !  minus K up to available soil storage
	      !  For USBR method, amount of effective rainfall (J multiplied 
	      !  by factor) that is greater than CU up to available soil storage    
	      !  Computed by field and then summed for farm (modeling area).              

	      l = er_st(iper)

	      !M DP and Runoff of Rainfall
	      !  J minus (K + L)

	      m = j - (k + l)

	      !N Irrigation Water Requirement (IWR)
	      !  Gross CU minus K 
	      !  Computed by field and then summed for farm (modeling area)

	      n = reqt(iper)

	      !O Carry-over Soil Moisture (End of Month)
	      !  (Carryover soil moisture from previous month) + L + I - N.  
	      !  If O is less than zero or some PRESET value, then zero or 
	      !  PRESET value, otherwise (if O is greater than MAX, then 
	      !  MAX, otherwise O)
	      !  MAX is sum for all fields on farm of (MAD multiplied by 
	      !  Root Depth multiplied by AWC) 

	      o = carry(iper)

	      !P Shortage
	      !  Shortage is zero for scenario where groundwater assumed to 
	      !  meet all remaining IWR in a month not met by that month's 
	      !  surface water supplies or soil moisture storage.
	      !  For other scenarios (ie, no wells or using well discharge 
	      !  measurements), if O (before setting to zero or PRESET 
	      !  value) is less than zero, then absolute value of O, 
	      !  otherwise zero.

	      p = shorts(iper)

	      !T CU Met by Groundwater
	      !  Only for scenario where groundwater assumed to meet all 
	      !  remaining IWR in a month not met by that month's surface 
	      !  water supplies or soil moisture storage;
	      !  If O (before setting to zero or PRESET value) is less 
	      !  than zero, then absolute value of O, otherwise zero.

	      IF( NWELL(IB).GT.0 .AND. WMODE(IB) .EQ. 0) THEN 
		 t = wpump(iper) 
	      ELSE                  
		 t = wel_sup(iper)
	      ENDIF

	      !Q Depletion of WATER SUPPLIES 
	      !  Positive values of [O minus (Carryover soil moisture from 
	      !  previous month + L)] plus (N - P) 
	      !  If this value is greater than I, then I, otherwise this 
	      !  value.

	      IF( NWELL(IB).GT.0 .AND. WMODE(IB) .EQ. 0) THEN 
		 q = cussup(iper) + t 
	      ELSE                  
		 q = cussup(iper)
	      ENDIF

	      !R Additional DP and Runoff of WATER SUPPLIES
	      !  I minus Q

	      r = i - q

	      !S Total DP and Runoff of WATER SUPPLIES
	      !  E plus H plus R

	      s = e + h + r

	      !SS CU Met by Surface Water

	      ss = sur_sup(iper)

	      ! Average root zone
	      r_z = avg_rz(iper)

	      ! Sprinkler spray loss
	      s_l = 0
	      do ip = 1, nparce(IB)
		 s_l = s_l + sprink_l(ip, iper) * &
		      AREA(IB,IP,nyr) * CUSHORT(IB) / 12
	      end do

	      ! NWR reduction for grass and alfalfa
	      u = shorts_from_reduc(iper)

	      WRITE(12,*) a, b, c, d, e, f, g, h, i, j, k, l, m, &
		   n, o, p, q, r, s, ss, t, r_z, maxstore(iper), &
		   s_l, u, cappeff(iper), shorts_stress(iper)
	   ELSE
	      WRITE(12,*) 0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0, &
	        0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0
	   ENDIF
        END DO
     ENDIF
     !-----------------------------------------------
     ! Create temporary file "tmp5" for Blaney-Criddle
     !-----------------------------------------------
     IF( NWELL(IB).GT.0 .AND. WMODE(IB) .EQ. 0) THEN  
	WRITE(15,*) (cussup(iper)+wpump(iper), iper = 1,nper)
     ELSE
	WRITE(15,*) (cussup(iper), iper = 1,nper)
     ENDIF

     !-------------
     !Print detailed output
     !-------------
     IF (TYPOUT(IB).EQ.3) THEN
	DO IP = 1, nparce(IB) 
	   ckey = crop_key(IB,IP)
	   skey = soil_key(IB,IP)
	   awcmad = mad(ckey) * awc(skey) / 100.0

	   IF(SPFLAG .EQ. 1 .AND. et_method.EQ.7) THEN
	      WRITE(3,918) QUOTE,AREA(IB,IP,nyr),QUOTE,QUOTE, &
		   CNAME(ckey),QUOTE,QUOTE,SNAME(skey), &
		   QUOTE,QUOTE,NYR1+nyr-1,QUOTE 
	      WRITE(3,916) DLINE
	      WRITE(3,917) (QUOTE, IDUM=1,28)
	      WRITE(3,916) SLINE
	   ELSEIF(et_method.EQ.1 .OR. et_method.EQ.8) THEN
	      WRITE(3,908) QUOTE,AREA(IB,IP,nyr),QUOTE,QUOTE, &
		   CNAME(ckey),QUOTE,QUOTE,SNAME(skey), &
		   QUOTE,QUOTE,NYR1+nyr-1,QUOTE 
	      WRITE(3,906) DLINE
	      WRITE(3,907) (QUOTE, IDUM=1,43)
	      WRITE(3,906) SLINE
	   ELSEIF(et_method.EQ.5) THEN
	      WRITE(3,928) QUOTE,AREA(IB,IP,nyr),QUOTE,QUOTE, &
		   CNAME(ckey),QUOTE,QUOTE,SNAME(skey), &
		   QUOTE,QUOTE,NYR1+nyr-1,QUOTE 
	      WRITE(3,906) DLINE
	      WRITE(3,909) (QUOTE, IDUM=1,47)
	      WRITE(3,906) SLINE
	   ELSEIF(et_method.EQ.9) THEN
	      ! Add user-supplied ET
	   ENDIF
	   DO MM = nbegmo(IP), nendmo(IP)

	      !Write the output for the first month of the season

	      days = MONTH(MM)
	      IF( nperiods .EQ. 366 .and. mm .eq. 2) THEN
		 days = days + 1
	      ENDIF

	      IF (MM.EQ.nbegmo(IP)) THEN
		 IF(SPFLAG .EQ. 1 .AND. et_method.EQ.7) THEN
		    WRITE(3,911) QUOTE,AMN(MM),nbegda(IP),QUOTE, &
                         100.*npart(IP)/days,ttemps(IP), &
                         Ret(IP,MM), &
                         xxkc(IP,MM),cu(IP,MM),er(IP,MM),cuirr(IP,MM)
		 ELSEIF(et_method.EQ.1 .OR. et_method.EQ.8) THEN
		    WRITE(3,901) QUOTE,AMN(MM),nbegda(IP),QUOTE, &
                         100.*npart(IP)/days,ttemps(IP),ddays(IP), &
			 xxf(IP,MM),xxkt(IP,MM),xxkc(IP,MM), &
                         xxkt(IP,MM)*xxkc(IP,MM),cu(IP,MM),er(IP,MM), &
                         cuirr(IP,MM)
		 ELSEIF(et_method.EQ.5)THEN
		    WRITE(3,921) QUOTE,AMN(MM),nbegda(IP),QUOTE, &
                         100.*npart(IP)/days,ttemps(IP),ddays(IP), &
                         xxf(IP,MM),xxkt(IP,MM),xxkc(IP,MM), &
                         xxkt(IP,MM)*xxkc(IP,MM),cbc(ckey,MM),cu(IP,MM), &
                         er(IP,MM),cuirr(IP,MM)
		 ELSEIF(et_method.EQ.9) THEN
		    ! Add user-supplied ET
		 ENDIF

		 !Write the output for the last month of the season

	      ELSEIF (MM.EQ.nendmo(IP)) THEN
		 IF(SPFLAG.EQ.1 .AND. et_method.EQ.7) THEN
		    WRITE(3,911) QUOTE,AMN(MM),nendda(IP),QUOTE,100. &
			 *nendda(IP)/days,ttempf(IP),Ret(IP,MM), &
			 xxkc(IP,MM),cu(IP,MM),er(IP,MM),cuirr(IP,MM)
		 ELSEIF(et_method.EQ.1 .OR. et_method.EQ.8)THEN
		    WRITE(3,901) QUOTE,AMN(MM),nendda(IP),QUOTE,100. &
			 *nendda(IP)/days,ttempf(IP),ddayf(IP), &
			 xxf(IP,MM),xxkt(IP,MM),xxkc(IP,MM), &
			 xxkt(IP,MM)*xxkc(IP,MM),cu(IP,MM),er(IP,MM), &
			 cuirr(IP,MM)
		 ELSEIF(et_method.EQ.5)THEN
		    WRITE(3,921) QUOTE,AMN(MM),nendda(IP),QUOTE,100. &
			 *nendda(IP)/days,ttempf(IP),ddayf(IP), &
			 xxf(IP,MM),xxkt(IP,MM),xxkc(IP,MM), &
			 xxkt(IP,MM)*xxkc(IP,MM),cbc(ckey,MM),cu(IP,MM), &
			 er(IP,MM),cuirr(IP,MM)
		 ELSEIF(et_method.EQ.9) THEN
		    ! Add user-supplied ET
		 ENDIF

		 !Write the output for the middle months of the season

	      ELSE
		 IF(SPFLAG.EQ.1 .AND. et_method.EQ.7) THEN
		    WRITE(3,912)QUOTE,AMN(MM),QUOTE, &
			 tmean3(ib,nyr,MM), Ret(IP,MM), &
			 xxkc(IP,MM),cu(IP,MM),er(IP,MM), &
			 cuirr(IP,MM)
		 ELSEIF(et_method.EQ.1 .OR. et_method.EQ.8)THEN
		    WRITE(3,902)QUOTE,AMN(MM),QUOTE, &
			 tmean3(ib,nyr,MM), pclite(MM), &
			 xxf(IP,MM),xxkt(IP,MM),xxkc(IP,MM), &
			 xxkt(IP,MM)* xxkc(IP,MM),cu(IP,MM), &
			 er(IP,MM),cuirr(IP,MM)
		 ELSEIF(et_method.EQ.5)THEN
		    WRITE(3,922)QUOTE,AMN(MM),QUOTE, &
			 tmean3(ib,nyr,MM), pclite(MM), &
			 xxf(IP,MM),xxkt(IP,MM),xxkc(IP,MM), &
			 xxkt(IP,MM)* xxkc(IP,MM),cbc(ckey,MM), &
			 cu(IP,MM),er(IP,MM), cuirr(IP,MM)
		 ELSEIF(et_method.EQ.9) THEN
		    ! Add user-supplied ET
		 ENDIF
	      ENDIF
	   END DO
	   IF (SPFLAG .EQ. 1) THEN
	      inctot = (frz(ckey)-irz(ckey))*awcmad 
	   ELSE
	      inctot = (frz(ckey)-irz(ckey))*12.0*awcmad
	   ENDIF
	   IF(SPFLAG.EQ.1 .AND. et_method.EQ.7) THEN
	      WRITE(3,916) SLINE
	      WRITE(3,914) (QUOTE, IDUM=1,16),cu(IP,13),er(IP,13), cuirr(IP,13)
	   ELSEIF(et_method.EQ.1 .OR. et_method.EQ.8) THEN
	      WRITE(3,906) SLINE
	      WRITE(3,904) (QUOTE, IDUM=1,16),cu(IP,13),er(IP,13), cuirr(IP,13)
	   ELSEIF(et_method.EQ.5) THEN
	      WRITE(3,906) SLINE
	      WRITE(3,924) (QUOTE, IDUM=1,16),cu(IP,13),er(IP,13), cuirr(IP,13)
	   ELSEIF(et_method.EQ.9) THEN
	      ! Add user-supplied ET
	   ENDIF
	   WRITE(3,*)
102	END DO
     ENDIF
  END DO

906 FORMAT(A90)
907 FORMAT(A1,'Month',A1,2x,A1,'Percent',A1,A1,'Temp',A1,A1, &
       'Daylight',A1,2x,A1,'f',A1,4x,A1,'kt',A1,3x,A1,'kc',A1,5x, &
       A1,'k',A1,3x,A1,'Crop',A1,4x,A1,'ER',A1,3x,A1,'IWR',A1/ &
       2x,A1,A1,5x,A1,'Month'A1, &
       A1,1x,A1,'(F)',A1,3x,A1,'(%)',A1,6x,A1,A1,6x, &
       A1,A1,5x,A1,A1,10x,A1,'CU (in)',A1,1x,A1,'(in)',A1, &
       2x,A1,'(in)',A1)
908 FORMAT(1x,A1,'Area (acres) = ',F7.1,A1,A1,A15,A1,2x,A1, &
       'Soil = ',A20,A1,5x,A1,'Year = ',I4,A1)
909 FORMAT(A1,'Month',A1,2x,A1,'Percent',A1,A1,'Temp',A1,A1, &
       'Daylight',A1,2x,A1,'f',A1,4x,A1,'kt',A1,3x,A1,'kc',A1,5x, &
       A1,'k',A1,2x,A1,'Calib.',A1,1x,A1,'Crop',A1,4x,A1,'ER',A1,3x,A1, &
       'IWR',A1/2x,A1,A1,5x,A1,'Month'A1,           &
       A1,1x,A1,'(F)',A1,3x,A1,'(%)',A1,6x,A1,A1,6x, &
       A1,A1,5x,A1,A1,10x,A1,'Coeff.',A1,A1,'CU (in)',A1,1x,A1,'(in)' &
       ,A1,2x,A1,'(in)',A1)
901 FORMAT(A1,A3,1X,I2,A1,F7.1,13F8.2)
902 FORMAT(A1,A3,A1,2x,'   100.0',13F8.2)
904 FORMAT(A1,'Season Total',A1,14A1,35x,3F8.2)
905 FORMAT(87x,F8.2)

917 FORMAT(A1,'Month',A1,2x,A1,'Percent',A1,A1,'Temp',A1, &
       3x,A1,'ETp',A1,A1,'  Kcm',A1,A1,'Crop CU',A1,'   ER',A1,3x,A1, &
       'IWR',A1/2x,A1,A1,5x,A1,'Month',A1, 3x,'(F)',A1,3x,         &
       A1,'(in)',A1,9x,A1,'(in)',A1,2x,A1,'(in)',A1,2x,A1,'(in)',A1)
911 FORMAT(A1,A3,1X,I2,A1,F7.1,6F8.2)
912 FORMAT(A1,A3,A1,2x,'   100.0',6F8.2)
914 FORMAT(A1,'Season Total',A1,14A1,11x,3F8.2)
918 FORMAT(A1,'Area (acres) = ',F7.1,A1,A1,A15,A1,A1, &
       'Soil = ',A10,A1,A1,'Year = ',I4,A1)
916 FORMAT(A72)
921 FORMAT(A1,A3,1X,I2,A1,F7.1,14F8.2)
922 FORMAT(A1,A3,A1,2x,'   100.0',14F8.2)
927 FORMAT(A1,'Month',A1,2x,A1,'Percent',A1,A1,'Temp',A1,A1, &
       'Daylight',A1,2x,A1,'f',A1,4x,A1,'kt',A1,3x,A1,'kc',A1,5x, &
       A1,'k',A1,3x,A1,'Crop',A1,4x,A1,'ER',A1,3x,A1,'IWR',A1/ &
       2x,A1,A1,5x,A1,'Month'A1,           &
       A1,1x,A1,'(F)',A1,3x,A1,'(%)',A1,6x,A1,A1,6x, &
       A1,A1,5x,A1,A1,20x,A1,'CU (in)',A1,1x,A1,'(in)',A1, &
       2x,A1,'(in)',A1)
928 FORMAT(1x,A1,'Area (acres) = ',F7.1,A1,A1,A15,A1,2x,A1, &
       'Soil = ',A30,A1,5x,A1,'Year = ',I4,A1)
924 FORMAT(A1,'Season Total',A1,14A1,43x,3F8.2)

  RETURN
END SUBROUTINE CALPCROP
