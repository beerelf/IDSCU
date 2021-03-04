SUBROUTINE WBUDGET(et_method)

  !***************************************************************************
  !
  !   Function        : wbudget.f
  !   Author          : HB Manguerra
  !   Date            : December 1994
  !   Purpose         : This creates an output file *.owb that contains the
  !                     result of the monthly basin-wide water budget in a
  !                     tabular format when water supply information is
  !                     available.
  !   Calling program : run_cu.f
  !   Called programs : none
  !   Input arguments : et_method = 1 = BC Water Budget
  !                          = 4 = KP Water Budget
  !                          = 5 = Calibrated BC Water Budget
  !   Output arguments: none
  !   Assumptions     :
  !   Limitations     :
  !   Notes           : The value of each parameter of the basin-water budget
  !                     for every basin, year and month are read from *tmp
  !                     files.
  !
  !   History         :(Date, Author, Description)
  !
  !     11/02/95    HBM    Units changed from 1000 acre-ft to acre-ft.
  !     07/01/96    LAG    Changed the soil moisture total for the year
  !                        from a sum of the monthly soil moistures to
  !                        the soil moisture of the last month.
  !     09/16/96    LAG    The requirment is read as an float and then rounded
  !                        to an integer at the as requested by Ray Bennett
  !                        This is done by reading req into ireq and then 
  !                        writing it back to req.
  !
  !***************************************************************************

  USE Globals

  !     Parameters
  INTEGER et_method

  CHARACTER*12 NA, DASH
  INTEGER I,IB,IY, OFLAG, IERR, NPRINT, INDEX, GetDP_ETindex

  REAL conloss, fssup, swacu, swdpro, fwelsup, welcu, weldpro
  REAL totwsup, totrain, effrain, erstor, effrain_dp, iwr, csoilm 
  REAL short, depsw, adddpro, totdpro, cuground
  REAL srsup,sshort, rsup
  REAL sconloss, sfssup, sswacu, sswdpro, sfwelsup, swelcu, sweldpro
  REAL stotwsup, stotrain, seffrain, siwr, scsoilm 
  REAL sdepsw, sadddpro, stotdpro, scuground, serstor, seffrain_dp, sdprorain
  REAL dprorain, cusurf, scusurf
  REAL r_z, sr_z ! root zone
  REAL maxstore ! maximum soil storage
  REAL sprink_l, s_sprink_l ! Sprinkler loss
  REAL short_reduc, s_short_reduc ! grass & alfalfa shortage
  REAL cae, ncae, scae  ! composite app eff, #, and sum for average
  REAL short_stress, s_short_stress
  REAL upflux, s_upflux

  DATA NPRINT/0/

  INTERFACE
     INTEGER FUNCTION DaysInYear(yr)
       INTEGER yr
     END FUNCTION DaysInYear

     SUBROUTINE CLNDR (jday, year, &
	  !     Outputs
	  m_mon, m_day)
       INTEGER jday, year, m_mon, m_day
     END SUBROUTINE CLNDR
  END INTERFACE

  INTEGER nperiods(NYRS) ! Number of days in each year
  INTEGER mon, day, tot_idx
  CHARACTER(6) label

  OFLAG = 0

  NA = '         NA '
  DASH = '       -- '

  DO IY = 1, NYRS
     if (isDailyMethod) then
	!           Daily method
	nperiods(IY) = DaysInYear(IY + NYR1 -1)
     else
	!           Monthly
	nperiods(IY) = 12
     endif
  END DO

  IF( NPRINT .EQ. 0) THEN
     WRITE(7,909)   ! tell GUI water supply is available
909  FORMAT('1')
     NPRINT = NPRINT + 1
  ENDIF

  !------------------------------------------------------------------------
  !
  !     SCS Blaney-Criddle Original
  !
  !------------------------------------------------------------------------

  IF (.true.) THEN

     !-----Read Temporary File
     OPEN (UNIT=11,FILE='tmp1',STATUS='OLD',IOSTAT=IERR)
     IF (IERR.NE.0) CALL MYEXIT(116)

     INDEX = GetDP_ETindex(et_method,0)

     DO IB = 1, NBASIN
	DO IY = 1, NYRS

	   m_year = IY + NYR1 -1
	   tot_idx = nperiods(IY)+1

	   IF (TYPOUT(IB).GE.2) THEN
	      OFLAG = 1
	      WRITE(7,905) QUOTE,QUOTE
	      IF( et_method .EQ. 1 ) THEN
		 WRITE(7,906) QUOTE,QUOTE ! original BC
	      ELSEIF(et_method .EQ. 4) THEN
		 WRITE(7,916) QUOTE,QUOTE ! KP
	      ELSEIF(et_method .EQ. 5) THEN
		 WRITE(7,926) QUOTE,QUOTE ! Calibrated BC
	      ELSEIF( ET_METHOD .EQ. 2) THEN
		 WRITE(7,908) QUOTE, QUOTE ! Penman-Monteith
	      ELSEIF( ET_METHOD .EQ. 10) THEN
		 WRITE(7,978) QUOTE, QUOTE ! Penman 1948
	      ELSEIF(et_method .EQ. 6) THEN
		 WRITE(7,918) QUOTE,QUOTE ! ASCE
	      ELSEIF(et_method .EQ. 7) THEN
		 WRITE(7,928) QUOTE,QUOTE ! Hargreaves
	      ELSEIF(et_method .EQ. 8) THEN
		 WRITE(7,930) QUOTE,QUOTE ! Pochop
	      ELSEIF(et_method .EQ. 9) THEN
		 WRITE(7,932) QUOTE,QUOTE
	      ENDIF
	      WRITE(7,900) QUOTE,BAS_ID(IB),QUOTE,NYR1+IY-1
	      WRITE(7,911) QUOTE,QUOTE,T_AREA(IB,IY)
	      WRITE(7,901) DLLINE
	      WRITE(7,902)

	      WRITE(7,901) SLLINE
	   ENDIF

	   srsup=0.0
	   sconloss=0.0
	   sfssup=0.0
	   sswacu=0.0
	   swdpro=0.0
	   sswdpro=0.0
	   sfwelsup=0.0 
	   swelcu=0.0
	   sweldpro=0.0
	   stotwsup=0.0
	   stotrain=0.0
	   seffrain=0.0 
	   seffrain_dp=0.0 
	   siwr=0.0
	   sshort=0.0
	   sdepsw=0.0
	   sadddpro=0.0
	   stotdpro=0.0
	   scusurf = 0.0
	   scuground=0.0
	   s_sprink_l = 0
	   s_short_reduc = 0
           scae = 0
	   ncae = 0
           s_short_stress = 0
           ! Upflux in non-waterbudet mode will always be zero
           s_upflux = 0

	   DO I = 1, nperiods(IY)
	      if (nperiods(IY) .gt. 12) then
		 CALL CLNDR(I, m_year, mon, day)
		 write(label, FMT="(I2,A3)") day, amn(mon)
	      else
		 mon = I
		 label = amn(I)
	      endif

	      READ(11,*,ERR=500) rsup,conloss,fssup,swacu,swdpro, &
                   fwelsup,  &
                   welcu, weldpro, totwsup, totrain, effrain,  &
                   iwr, short, depsw,  &
                   adddpro,totdpro, cusurf,cuground, sprink_l, short_reduc, &
		   cae, short_stress, upflux

              effrain_dp = totrain - effrain

	      IF (TYPOUT(IB).GE.2) THEN
		 WRITE(7,913) QUOTE,label,QUOTE,rsup,conloss,fssup, &
                      swacu,swdpro,fwelsup,  &
		      welcu, weldpro, totwsup, totrain, effrain, &
                      QUOTE,dash,QUOTE,effrain_dp,iwr, &
                      QUOTE,dash,QUOTE,short, depsw, &
                      adddpro, totdpro, cusurf,cuground, &
		      QUOTE, dash,QUOTE, QUOTE, dash,QUOTE, sprink_l, &
		      short_reduc, cae, short_stress, upflux
	      ENDIF

	      srsup = rsup + srsup
	      sconloss = sconloss + conloss
	      sfssup = sfssup + fssup
	      sswacu = sswacu + swacu
	      sswdpro = sswdpro + swdpro
	      sfwelsup = sfwelsup + fwelsup 
	      swelcu = swelcu + welcu
	      sweldpro = sweldpro + weldpro
	      stotwsup = stotwsup + totwsup
	      stotrain = stotrain + totrain
	      seffrain = seffrain + effrain 
	      seffrain_dp = seffrain_dp + effrain_dp 
	      siwr = siwr + iwr
              if (cae .gt. 0) then
		 scae = scae + cae
                 ncae = ncae + 1
	      end if
              s_short_stress = s_short_stress + short_stress
              s_upflux = s_upflux + upflux

	      !LAG           The carry over soil moisture should not be summed. It
	      !LAG           should just be the carry over of the last month

	      sshort = sshort + short
	      sdepsw = sdepsw + depsw
	      sadddpro = sadddpro + adddpro
	      stotdpro = stotdpro + totdpro
	      scusurf = scusurf + cusurf
	      scuground = scuground + cuground
	      s_sprink_l = s_sprink_l + sprink_l
	      s_short_reduc = s_short_reduc + short_reduc

	      ! Populate arrays for total DP & RO and Conveyance loss
	      ! This arrays are used in spwellw to write to the owl
	      !   file the field DP & RO as well as DP from Conveyance loss

	      FARM_DP(IB,IY,I, INDEX) = totdpro * DP_ROPerc(mon)
	      FARM_RO(IB,IY,I, INDEX) = totdpro * (1.0 - DP_ROPerc(mon))
	      DITCH_DP(IB,IY,I, INDEX) = conloss

	   END DO

	   ! Calculate yearly totals
	   DO I = 1,nperiods(IY)
	      FARM_DP(IB,IY,tot_idx, INDEX) = &
                   FARM_DP(IB,IY,tot_idx, INDEX) +  &
                   FARM_DP(IB,IY,I, INDEX)
	      FARM_RO(IB,IY,tot_idx, INDEX) = &
                   FARM_RO(IB,IY,tot_idx, INDEX) +  &
                   FARM_RO(IB,IY,I, INDEX)
	      DITCH_DP(IB,IY,tot_idx, INDEX) = &
                   DITCH_DP(IB,IY,tot_idx, INDEX) + &
                   DITCH_DP(IB,IY,I, INDEX)
	   END DO

           ! Average composite application efficiency
           if (ncae .gt. 0) then
	      scae = scae / ncae
           end if
	   IF (TYPOUT(IB).GE.2) THEN
	      WRITE(7,901) SLLINE
	      WRITE(7,914) QUOTE,QUOTE,srsup,sconloss,sfssup, &
                   sswacu,sswdpro,sfwelsup,swelcu,  &
                   sweldpro, stotwsup, stotrain, seffrain, &
                   QUOTE,dash,QUOTE,seffrain_dp,siwr, &
                   QUOTE,dash,QUOTE,sshort, &
                   sdepsw, sadddpro, stotdpro, scusurf, scuground, &
		   QUOTE, dash,QUOTE, QUOTE, dash,QUOTE, s_sprink_l, &
	           s_short_reduc, scae, s_short_stress, s_upflux
	      WRITE(7,*)
	   ENDIF

	END DO
     END DO

     CLOSE(11,STATUS='delete')

     !------------------------------------------------------------------------
     !
     !     SCS Blaney-Criddle Enhanced
     !
     !------------------------------------------------------------------------

     OPEN (UNIT=12,FILE='tmp2',STATUS='OLD')

     INDEX = GetDP_ETindex(et_method,1)

     DO IB = 1, NBASIN
	DO IY = 1, NYRS

	   m_year = IY + NYR1 -1
	   tot_idx = nperiods(IY)+1

	   IF (TYPOUT(IB).GE.2) THEN
	      OFLAG = 1 
	      WRITE(7,905) QUOTE, QUOTE
	      IF(et_method .EQ. 1) THEN
		 WRITE(7,907) QUOTE, QUOTE                 ! enhanced BC
	      ELSEIF(et_method .EQ. 4) THEN
		 WRITE(7,917) QUOTE, QUOTE                 ! KP
	      ELSEIF(et_method .EQ. 5) THEN
		 WRITE(7,927) QUOTE, QUOTE                  ! Calibrated BC
	      ELSEIF( ET_METHOD .EQ. 2) THEN
		 WRITE(7,910) QUOTE, QUOTE ! Penman-Monteith
	      ELSEIF( ET_METHOD .EQ. 10) THEN
		 WRITE(7,980) QUOTE, QUOTE ! Penman 1948
	      ELSEIF(et_method .EQ. 6) THEN
		 WRITE(7,919) QUOTE,QUOTE  ! ASCE
	      ELSEIF(et_method .EQ. 7) THEN
		 WRITE(7,929) QUOTE,QUOTE ! Hargreaves
	      ELSEIF(et_method .EQ. 8) THEN
		 WRITE(7,931) QUOTE,QUOTE ! Pochop
	      ELSEIF(et_method .EQ. 9) THEN
		 WRITE(7,933) QUOTE,QUOTE
	      ENDIF
	      WRITE(7,900) QUOTE,BAS_ID(IB),QUOTE,NYR1+IY-1
	      WRITE(7,911) QUOTE,QUOTE,T_AREA(IB,IY)
	      WRITE(7,901) DLLINE
	      WRITE(7,902)
	      WRITE(7,901) SLLINE
	   ENDIF

	   srsup=0.0
	   sconloss=0.0
	   sfssup=0.0
	   sswacu=0.0
	   swdpro=0.0
	   sswdpro=0.0
	   sfwelsup=0.0 
	   swelcu=0.0
	   sweldpro=0.0
	   stotwsup=0.0
	   stotrain=0.0
	   seffrain=0.0 
	   serstor=0.0
	   siwr=0.0
	   sdprorain=0.0
	   scsoilm=0.0
	   sshort=0.0
	   sdepsw=0.0
	   sadddpro=0.0
	   stotdpro=0.0
	   scuground=0.0
	   scusurf = 0.0
	   sr_z = 0.0
	   s_sprink_l = 0
	   s_short_reduc = 0
           scae = 0
	   ncae = 0
           s_short_stress = 0
           s_upflux = 0

	   DO I = 1, nperiods(IY)

	      if (nperiods(IY) .gt. 12) then
		 CALL CLNDR(I, m_year, mon, day)
		 write(label, FMT="(I2,A3)") day, amn(mon)
	      else
		 mon = I
		 label = amn(I)
	      endif

	      READ(12,*) rsup,conloss,fssup,swacu,swdpro,fwelsup, &
		   welcu, weldpro, totwsup, totrain, effrain, &
		   erstor, dprorain,iwr, csoilm, short, depsw, &
		   adddpro,totdpro,cusurf,cuground, r_z, maxstore, &
		   sprink_l, short_reduc, cae, short_stress, upflux

	      IF (TYPOUT(IB).GE.2) THEN

		 WRITE(7,903) QUOTE,label,QUOTE,rsup,conloss,fssup, &
		      swacu,swdpro,fwelsup,  &
		      welcu, weldpro, totwsup, totrain, effrain,  &
		      erstor, dprorain, iwr, csoilm, short, depsw,  &
		      adddpro, totdpro, cusurf,cuground, r_z, maxstore, &
		      sprink_l, short_reduc, cae, short_stress, upflux

	      ENDIF

	      srsup = rsup + srsup
	      sconloss = sconloss + conloss
	      sfssup = sfssup + fssup
	      sswacu = sswacu + swacu
	      sswdpro = sswdpro + swdpro
	      sfwelsup = sfwelsup + fwelsup 
	      swelcu = swelcu + welcu
	      sweldpro = sweldpro + weldpro
	      stotwsup = stotwsup + totwsup
	      stotrain = stotrain + totrain
	      seffrain = seffrain + effrain 
	      serstor = serstor + erstor
	      sdprorain = sdprorain + dprorain
	      siwr = siwr + iwr

!LAG           The carry over soil moisture should not be summed. It
!LAG           should just be the carry over of the last month

	      scsoilm = csoilm
	      sshort = sshort + short
	      sdepsw = sdepsw + depsw
	      sadddpro = sadddpro + adddpro
	      stotdpro = stotdpro + totdpro
	      scuground = scuground + cuground
	      scusurf = scusurf + cusurf
	      sr_z = sr_z + r_z
	      s_sprink_l = s_sprink_l + sprink_l
	      s_short_reduc = s_short_reduc + short_reduc
              if (cae .gt. 0) then
		 scae = scae + cae
                 ncae = ncae + 1
	      end if
              s_short_stress = s_short_stress + short_stress
              s_upflux = s_upflux + upflux

	      ! Populate arrays for total DP & RO and Conveyance loss
	      ! This arrays are used in spwellw to write to the owl
	      !    file the field DP & RO as well as DP from Conveyance loss

	      FARM_DP(IB,IY,I, INDEX) = totdpro * DP_ROPerc(mon)
	      FARM_RO(IB,IY,I, INDEX) = totdpro * (1.0 - DP_ROPerc(mon))
	      DITCH_DP(IB,IY,I, INDEX) = conloss

	   END DO

	   ! Calculate yearly totals
	   DO I = 1,12
	      FARM_DP(IB,IY,tot_idx, INDEX) = &
                   FARM_DP(IB,IY,tot_idx, INDEX) +  &
                   FARM_DP(IB,IY,I, INDEX)
	      FARM_RO(IB,IY,tot_idx, INDEX) = &
                   FARM_RO(IB,IY,tot_idx, INDEX) +  &
		   FARM_RO(IB,IY,I, INDEX)
	      DITCH_DP(IB,IY,tot_idx, INDEX) = &
                   DITCH_DP(IB,IY,tot_idx, INDEX) + &
                   DITCH_DP(IB,IY,I, INDEX)
	   END DO

           ! Average composite application efficiency
           if (ncae .gt. 0) then
	      scae = scae / ncae
           end if

	   IF (TYPOUT(IB).GE.2) THEN
	      WRITE(7,901) SLLINE
	      WRITE(7,904) QUOTE,QUOTE,srsup,sconloss,sfssup, &
		   sswacu,sswdpro,sfwelsup,swelcu,  &
		   sweldpro, stotwsup, stotrain, seffrain,  &
		   serstor, sdprorain, siwr, scsoilm, sshort,  &
		   sdepsw, sadddpro, stotdpro, scusurf, scuground, &
		   sr_z / nperiods(IY), maxstore, s_sprink_l, s_short_reduc, &
		   scae, s_short_stress, s_upflux

	      WRITE(7,*)
	   ENDIF

	END DO
     END DO

     CLOSE(12,STATUS='delete')

  ENDIF         ! end of blaney-criddle

  !
  ! IF NONE OF THE BASINS REQUIRE OUTPUT PRINT THE FOLLOWING
  !
  IF ( OFLAG .EQ. 0) THEN
     WRITE(7,921)
     WRITE(7,922)
     WRITE(7,923)
     WRITE(7,924)
  ENDIF

900 FORMAT(A1,A60,A1,50x,I4)  
901 FORMAT(A270)

902 FORMAT(    1x,"Month    River      Conveyance    Farm Surface Surf&
       &. Water  Surf. Water      Farm Well   Well Water   Well Water   To&
       &t. Water        Total      Eff. Rain    Eff. Rain   DP & Runoff  I&
       &rr. Water   Carry-Over             Depletion of  Additional   Tota&
       &l DP &    CU Met by    CU Met by    Rootzone    Max Storage   Sprinkler Loss    NWR Short    Crop Stress  GW Upflux"/ &
       "          Supply        Loss       Water Supply   Available  DP &&
       & Runoff        Supply     Available    DP & Runoff  Supplies      &
       &  Rainfall      to CU       to Soil    of Rainfall  Requirements  &
       &   Soil    Shortage       Water    DP & Runoff   Runoff of      Su&
       &rface     Groundwater"/&
       "                                                        &         
       &                                                            &      
       &                            Storage                          &     
       &Moisture                Supplies   of Surf. Wat  Surf. Water  Root Zone&
       &  Soil Storage")

903 FORMAT(1x,A1,A6,A1,1x,2(F12.4,1x),2x,26(F12.4,1x))
904 FORMAT(A1,'Total',A1,2(F12.4,1x),2x,26(F12.4,1x))
905 FORMAT(86x,A1,'Basin-Wide Irrigation Water Budget (acre-ft)',A1)
906 FORMAT(89x,A1,'SCS Modified Blaney-Criddle (USBR Original)',A1)
907 FORMAT(89x,A1,'  SCS Modified Blaney-Criddle (Enhanced)',A1)
908 FORMAT(103x,A1,'Penman-Monteith ',A1)
910 FORMAT(103x,A1,'Penman-Monteith (Water Budget)',A1)
978 FORMAT(103x,A1,'Penman 1948 ',A1)
980 FORMAT(103x,A1,'Penman 1948 (Water Budget)',A1)

911 FORMAT(A1,'Acreage = ',A1,F10.0)

913 FORMAT(1x,A1,A6,A1,1x,2(F12.4,1x),2x,9(F12.4,1x), &
	 1(A1,1x,A10,1x,A1),2(F12.4,1x),A1,1x,A10,1x,A1,6(F12.4,1x), &
	 2(A1,1x,A10,1x,A1),5(F12.4,1x))
914 FORMAT(A1,'Total',A1,2(F12.4,1x),2x,9(F12.4,1x), &
	 1(A1,1x,A10,1x,A1),2(F12.4,1x),A1,1x,A10,1x,A1,6(F12.4,1x), &
	 2(A1,1x,A10,1x,A1), 5(F12.4,1x))
916 FORMAT(103x,A1,'Kimberly-Penman',A1)
917 FORMAT(96x,A1,'Kimberly-Penman (Water Budget)',A1)
918 FORMAT(98x,A1,'ASCE Standarized Ref. ET ',A1)
919 FORMAT(89x,A1,'ASCE Standarized Ref. ET (Water Budget)',A1)

921 FORMAT(1X,'=====================================================')
922 FORMAT(1X,'THIS RUN DID NOT CONTAIN A WATER BUDGET OUTPUT OPTION')
923 FORMAT(1X,'       FOR ANY OF THE SUB-AREAS BEING MODELED')
924 FORMAT(1X,'=====================================================')

926 FORMAT(98x,A1,'Calibrated Blaney-Criddle',A1)
927 FORMAT(91x,A1,'Calibrated Blaney-Criddle (Water Budget)',A1)
928 FORMAT(103x,A1,'Hargreaves',A1)
929 FORMAT(98x,A1,'Hargreaves (Water Budget)',A1)
930 FORMAT(103x,A1,'Pochop',A1)
931 FORMAT(98x,A1,'Pochop (Water Budget)',A1)
932 FORMAT(103x,A1,'User-Supplied ET',A1)
933 FORMAT(98x,A1,'User-Supplied ET (Water Budget)',A1)

  RETURN

500 CALL MYEXIT(115)

END SUBROUTINE WBUDGET
