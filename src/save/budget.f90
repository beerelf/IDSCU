SUBROUTINE BUDGET(et_method)

  !***************************************************************************
  !
  !   Function        : budget.f
  !   Author          : HB Manguerra
  !   Date            : December 1994
  !   Purpose         : This creates an output file *.owb that contains the
  !                     monthly basin-wide water budget in a tabular format
  !                     when information about water supply is not available.  
  !   Calling program : run_cu.f
  !   Called programs : none
  !   Input arguments : et_method = 1 = BC Water Budget
  !                          = 4 = KP Water Budget
  !                          = 5 = Calibrated BC Water Budget
  !   Output arguments: none
  !   Assumptions     :
  !   Limitations     :
  !   Notes           : The value of each parameter of the basin-water budget
  !                     for every basin, year and month are read from tmp*
  !                     files that were created by subroutine calpcrop.
  !
  !   History         :(Date, Author, Description)
  !
  !     11/02/95    HBM    Units changed from 1000 acre-ft to acre-ft.
  !
  !***************************************************************************

  USE Globals

  !     Parameters
  INTEGER et_method

  CHARACTER*10 NA
  CHARACTER*10 DASH
  INTEGER I, IB, IY, OFLAG, NPRINT
  REAL sm, dep, req, rsup, rtot, shrt, cumet, dcumet, shorts_from_reduc
  REAL srsup, srtot, ssm, sdep, sreq, sshrt, sshorts_from_reduc
  DATA NPRINT/0/

  INTERFACE
     INTEGER FUNCTION DaysInYear(yr)
       INTEGER yr
     END FUNCTION DaysInYear

     SUBROUTINE CLNDR (jday, year, &
				! Outputs
	  m_mon, m_day)
       INTEGER jday, year, m_mon, m_day
     END SUBROUTINE CLNDR
  END INTERFACE

  INTEGER nperiods(NYRS) ! Number of days in each year
  INTEGER mon, day
  CHARACTER(6) label

  OFLAG = 0

  NA = '        NA'
  DASH = '       -- '

  DO IY = 1, NYRS
     if (hasDailyMethod)  then
	! Daily method
	nperiods(IY) = DaysInYear(IY + NYR1 -1)
     else
	! Monthly
	nperiods(IY) = 12
     endif
  END DO

  IF( NPRINT .EQ. 0) THEN
     WRITE(7,909)           ! flag for GUI indicating water supply not available
909  FORMAT('0')
     NPRINT = NPRINT + 1
  ENDIF

  !-----Write Blaney-Criddle Results if Activated
  IF (.true.) THEN

     !------------------------------------------------------------------------
     !
     !     SCS Blaney-Criddle Original
     !
     !------------------------------------------------------------------------

     !-----Read Temporary File
     OPEN(UNIT=11,FILE='tmp1',STATUS='OLD')

     DO IB = 1, NBASIN
	DO IY = 1, NYRS
	   m_year = IY + NYR1 -1

	   IF (TYPOUT(IB).GE.2) THEN
	      OFLAG = 1
	      WRITE(7,905) QUOTE,QUOTE
	      IF( ET_METHOD .EQ. 1) THEN
		 WRITE(7,906) QUOTE, QUOTE ! original BC
	      ELSEIF( ET_METHOD .EQ. 4) THEN
		 WRITE(7,916) QUOTE, QUOTE ! KP
	      ELSEIF( ET_METHOD .EQ. 5) THEN
		 WRITE(7,926) QUOTE, QUOTE ! Calibrated BC
	      ELSEIF( ET_METHOD .EQ. 2) THEN
		 WRITE(7,908) QUOTE, QUOTE ! Penman-Monteith
	      ELSEIF( ET_METHOD .EQ. 10) THEN
		 WRITE(7,968) QUOTE, QUOTE ! Penman 1948
	      ELSEIF( ET_METHOD .EQ. 6) THEN
		 WRITE(7,928) QUOTE, QUOTE ! ASCE
	      ELSEIF( ET_METHOD .EQ. 7) THEN
		 WRITE(7,931) QUOTE, QUOTE ! Hargreaves
	      ELSEIF( ET_METHOD .EQ. 8) THEN
		 WRITE(7,935) QUOTE, QUOTE ! Pochop
	      ELSEIF( ET_METHOD .EQ. 9) THEN
		 WRITE(7,937) QUOTE, QUOTE
	      ENDIF
	      WRITE(7,900) QUOTE,BAS_ID(IB),QUOTE,NYR1+IY-1
	      WRITE(7,911) QUOTE,QUOTE,T_AREA(IB,IY)
	      WRITE(7,901) DLLINE
	      WRITE(7,902)
	      WRITE(7,901) SLLINE
	   ENDIF

	   sreq = 0.0
	   dcumet = 0.0
	   sshorts_from_reduc = 0.0
	   srsup = 0.0
	   srtot = 0.0
	   DO I = 1, nperiods(IY)
	      if (nperiods(IY) .gt. 12) then
		 CALL CLNDR(I, m_year, mon, day)
		 write(label, FMT="(I2, A3)") day, amn(mon)
	      else
		 label = amn(I)
	      endif


	      READ(11,*) rtot,rsup,req,cumet,shorts_from_reduc
	      IF (TYPOUT(IB).GE.2) THEN
		 WRITE(7,913) QUOTE,label,QUOTE,QUOTE,na,QUOTE, &
		      QUOTE, na,QUOTE,QUOTE,na,QUOTE,QUOTE,na, &
		      QUOTE,QUOTE,na, QUOTE,QUOTE,na,QUOTE,QUOTE, &
		      na,QUOTE,QUOTE,na,QUOTE, QUOTE,na,QUOTE, &
		      rtot, rsup, QUOTE,na,QUOTE,QUOTE, &
		      na,QUOTE,req,QUOTE, na,QUOTE, &
		      QUOTE,na,QUOTE,cumet,QUOTE,na,QUOTE,QUOTE, na, &
		      QUOTE,QUOTE,na,QUOTE,QUOTE,na,QUOTE, &
		      QUOTE,na,QUOTE, QUOTE,na,QUOTE, QUOTE,na,QUOTE, &
		      shorts_from_reduc
		      
	      ENDIF
	      srsup = srsup + rsup
	      srtot = srtot + rtot
	      sreq = sreq + req
	      dcumet = dcumet + cumet
	      sshorts_from_reduc = sshorts_from_reduc + shorts_from_reduc
	   END DO

	   IF (TYPOUT(IB).GE.2) THEN
	      WRITE(7,901) SLLINE
	      WRITE(7,914) QUOTE,QUOTE,QUOTE,na,QUOTE,QUOTE,na, &
		   QUOTE, QUOTE,na,QUOTE,QUOTE,na,QUOTE,QUOTE,na, &
		   QUOTE,QUOTE, na,QUOTE,QUOTE,na,QUOTE,QUOTE,na, &
		   QUOTE,QUOTE, na,QUOTE,srtot, srsup, &
		   QUOTE, na,QUOTE,QUOTE,na, &
		   QUOTE,sreq,QUOTE, na,QUOTE,QUOTE,na,QUOTE, &
		   dcumet,QUOTE,na,QUOTE,QUOTE, na,QUOTE,QUOTE,na, &
		   QUOTE,QUOTE,na,QUOTE, &
		   QUOTE,na,QUOTE, QUOTE,na,QUOTE, QUOTE,na, QUOTE,&
		   sshorts_from_reduc

	      WRITE(7,*)
	      !
	      ! Check for mass balance if it does not match then 
	      !   stop the program.
	      !
	      !LAG           IF( sreq .GT. 0.99*sdep .AND. sreq .LT. 1.01*sdep) THEN
	      !LAG              CALL myexit(80)
	      !LAG           ENDIF
	   ENDIF

	END DO
     END DO
     CLOSE(11,STATUS='delete')

     !------------------------------------------------------------------------
     !
     !     SCS Blaney-Criddle Enhanced 
     !
     !------------------------------------------------------------------------

     OPEN(UNIT=12,FILE='tmp2',STATUS='OLD')

     DO IB = 1, NBASIN
	DO IY = 1, NYRS

	   m_year = IY + NYR1 -1

	   IF (TYPOUT(IB).GE.2) THEN
	      OFLAG = 1
	      WRITE(7,905) QUOTE,QUOTE

	      IF( ET_METHOD .EQ. 1) THEN
		 WRITE(7,907) QUOTE, QUOTE ! Enhanced  BC
	      ELSEIF( ET_METHOD .EQ. 4) THEN
		 WRITE(7,917) QUOTE, QUOTE ! KP
	      ELSEIF( ET_METHOD .EQ. 5) THEN
		 WRITE(7,927) QUOTE, QUOTE ! Calibrated BC
	      ELSEIF( ET_METHOD .EQ. 2) THEN
		 WRITE(7,910) QUOTE, QUOTE ! Penman-Monteith
	      ELSEIF( ET_METHOD .EQ. 10) THEN
		 WRITE(7,970) QUOTE, QUOTE ! Penman 1948
	      ELSEIF( ET_METHOD .EQ. 6) THEN
		 WRITE(7,929) QUOTE, QUOTE ! ASCE
	      ELSEIF( ET_METHOD .EQ. 7) THEN
		 WRITE(7,932) QUOTE, QUOTE ! Hargreaves
	      ELSEIF( ET_METHOD .EQ. 8) THEN
		 WRITE(7,936) QUOTE, QUOTE ! Pochop
	      ELSEIF( ET_METHOD .EQ. 9) THEN
		 WRITE(7,938) QUOTE, QUOTE
	      ENDIF

	      WRITE(7,900) QUOTE,BAS_ID(IB),QUOTE,NYR1+IY-1
	      WRITE(7,911) QUOTE,QUOTE,T_AREA(IB,IY)
	      WRITE(7,901) DLLINE
	      WRITE(7,902)
	      !               WRITE(7,902) (QUOTE, IDUM=1,60)
	      WRITE(7,901) SLLINE
	   ENDIF

	   srsup = 0.0
	   srtot = 0.0
	   sreq = 0.0
	   ssm = 0.0
	   sdep = 0.0
	   sshrt = 0.0
	   sshorts_from_reduc = 0.0
	   DO 110 I = 1, nperiods(IY)
	      if (nperiods(IY) .gt. 12) then
		 CALL CLNDR(I, m_year, mon, day)
		 write(label,FMT="(I2, A3)") day, amn(mon)
	      else
		 label = amn(I)
	      endif

	      IF (SPFLAG .EQ. 1) THEN
		 READ(12,*) rtot, rsup, req, sm, dep, shrt, shorts_from_reduc
	      ELSE
		 READ(12,*) rsup,req,sm,dep, shorts_from_reduc
	      ENDIF
	      IF (TYPOUT(IB).GE.2 .AND. SPFLAG.EQ.0) &	
		   WRITE(7,903) QUOTE,label,QUOTE,QUOTE,na,QUOTE,QUOTE, &
                   na,QUOTE,QUOTE,na,QUOTE,QUOTE,na,QUOTE,QUOTE, &
                   na,QUOTE,QUOTE,na,QUOTE,QUOTE,na,QUOTE,QUOTE, &
                   na,QUOTE,QUOTE,na,QUOTE, &
                   rtot,rsup,QUOTE,na,QUOTE,QUOTE,na,QUOTE,req,sm, &
                   QUOTE,na,QUOTE,dep,QUOTE,na,QUOTE,QUOTE,na,QUOTE, &
                   QUOTE,na,QUOTE,QUOTE,na,QUOTE, &
		   QUOTE,na,QUOTE, QUOTE,na,QUOTE, QUOTE,na,QUOTE, &
		   shorts_from_reduc, QUOTE,na,QUOTE


	      IF (TYPOUT(IB).GE.2 .AND. SPFLAG.EQ.1) &	
		   WRITE(7,933) QUOTE,label,QUOTE,QUOTE,na,QUOTE,QUOTE, &
                   na,QUOTE,QUOTE,na,QUOTE,QUOTE,na,QUOTE,QUOTE, &
                   na,QUOTE,QUOTE,na,QUOTE,QUOTE,na,QUOTE,QUOTE, &
                   na,QUOTE,QUOTE,na,QUOTE, &
                   rtot,rsup,QUOTE,na,QUOTE,QUOTE,na,QUOTE,req,sm, &
                   shrt,dep,QUOTE,na,QUOTE,QUOTE,na,QUOTE,QUOTE,na, &
                   QUOTE,QUOTE,na,QUOTE, &
		   QUOTE,na,QUOTE, QUOTE,na,QUOTE, QUOTE,na,QUOTE, &
		   shorts_from_reduc, QUOTE,na,QUOTE

	      srsup = srsup + rsup
	      srtot = srtot + rtot
	      sreq = sreq + req
	      sshrt = sshrt + shrt

	      !LAG           The carry over soil moisture should not be summed. It
	      !LAG           should just be the carry over of the last month
	      !LAG           ssm = ssm + sm
	      ssm = sm

	      sdep = sdep + dep
110        END DO

	   IF (TYPOUT(IB).GE.2) THEN
	      WRITE(7,901) SLLINE
	      IF(SPFLAG .EQ. 0) &
		   WRITE(7,904) QUOTE,QUOTE,QUOTE,na,QUOTE,QUOTE, &
                   na,QUOTE,QUOTE,na,QUOTE,QUOTE,na,QUOTE,QUOTE, &
                   na,QUOTE,QUOTE,na,QUOTE,QUOTE,na,QUOTE,QUOTE, &
                   na,QUOTE,QUOTE,na,QUOTE,QUOTE, &
                   srtot,QUOTE,srsup,QUOTE,na,QUOTE,QUOTE,na,QUOTE, &
                   sreq,ssm,QUOTE,na, &
                   QUOTE,sdep,QUOTE,na,QUOTE,QUOTE,na,QUOTE,QUOTE, &
                   na,QUOTE,QUOTE,na,QUOTE, &
		   QUOTE,na,QUOTE, QUOTE,na,QUOTE, QUOTE,na,QUOTE, &
		   sshorts_from_reduc, QUOTE,na,QUOTE


	      IF(SPFLAG .EQ. 1)  &
		   WRITE(7,934) QUOTE,QUOTE,QUOTE,na,QUOTE,QUOTE, &
                   na,QUOTE,QUOTE,na,QUOTE,QUOTE,na,QUOTE,QUOTE, &
                   na,QUOTE,QUOTE,na,QUOTE,QUOTE,na,QUOTE,QUOTE, &
                   na,QUOTE,QUOTE,na,QUOTE, &
                   srtot,srsup,QUOTE,na,QUOTE,QUOTE,na,QUOTE, &
                   sreq,ssm,sshrt,sdep,QUOTE,na,QUOTE,QUOTE,na,QUOTE, &
                   QUOTE,na,QUOTE,QUOTE,na,QUOTE, &
		   QUOTE,na,QUOTE, QUOTE,na,QUOTE, QUOTE,na,QUOTE, &
		   sshorts_from_reduc, QUOTE,na,QUOTE

	      WRITE(7,*)

	   ENDIF

	END DO
     END DO
     CLOSE(12,STATUS='delete')
  ENDIF         ! end of blaney-criddle

  !
  !     IF NONE OF THE BASINS REQUIRE OUTPUT PRINT THE FOLLOWING
  !
  IF( OFLAG .EQ. 0) THEN
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
       &l DP &    CU Met by    CU Met by    Rootzone    Max Storage   Sprinkler Loss   Crop Stress"/ &

       "          Supply        Loss       Water Supply   Available  DP &&
       & Runoff        Supply     Available    DP & Runoff  Supplies      &
       &  Rainfall      to CU       to Soil    of Rainfall  Requirements  &
       &   Soil    Shortage       Water    DP & Runoff   Runoff of      Su&
       &rface     Groundwater"/&
       "                                   &                              
       &                                          &                        
       &                            Storage           &                    
       &Moisture                Supplies   of Surf. Wat  Surf. Water")

903 FORMAT(1x,A1,A6,A1,1x,2(A1,A10,A1),2x,7(A1,A10,A1), &
       1x,2(F12.3),1x,2(A1,1x,A10,1x,A1),2(F12.3),A1,A10,A1,F12.3, &
       7(A1,1x,A10,1x,A1), F12.3,(A1,1x,A10,1x,A1))
904 FORMAT(A1,'Total',A1,2(A1,A10,A1),2x,8(A1,A10,A1), &
       1x,F12.3,1x,A1,2(1x,A10,1x,A1),2(F12.3),A1,A10,A1,F12.3, &
       7(A1,1x,A10,1x,A1), F12.3,(A1,1x,A10,1x,A1))
933 FORMAT(1x,A1,A6,A1,1x,2(A1,A10,A1),2x,7(A1,A10,A1), &
       1x,2(F12.3),2(A1,A10,A1),4(F12.3),7(A1,1x,A10,1x,A1), F12.3,(A1,1x,A10,1x,A1))
934 FORMAT(A1,'Total',A1,1x,2(A1,A10,A1),2x,7(A1,A10,A1), &
       1x,2(F12.3),2(A1,A10,A1),4(F12.3),7(A1,1x,A10,1x,A1), F12.3,(A1,1x,A10,1x,A1))

905 FORMAT(36x,A1,'Basin-Wide Irrigation Water Budget (acre-ft)',A1)
906 FORMAT(39x,A1,'SCS Modified Blaney-Criddle (USBR Original)',A1)
907 FORMAT(39x,A1,'  SCS Modified Blaney-Criddle (Enhanced)',A1)
908 FORMAT(53x,A1,'Penman-Monteith',A1) 
910 FORMAT(53x,A1,'Penman-Monteith (Water Budget)',A1) 
968 FORMAT(53x,A1,'Penman 1948',A1) 
970 FORMAT(53x,A1,'Penman 1948 (Water Budget)',A1) 
911 FORMAT(A1,'Acreage = ',A1,F10.0)

913 FORMAT(1x,A1,A6,A1,1x,2(A1,A10,A1),2x,7(A1,A10,A1), 2(F12.3), &
       2(A1,1x,A10,1x,A1),F12.3,2(A1,A10,A1),F12.3,7(A1,1x,A10,1x,A1), &
       F12.3)
914 FORMAT(A1,'Total',A1,2(A1,A10,A1),2x,7(A1,A10,A1), 2(F12.3), &
       2(A1,1x,A10,1x,A1),F12.3,2(A1,A10,A1),F12.3,7(A1,1x,A10,1x,A1), &
       F12.3)

916 FORMAT(53x,A1,'Kimberly-Penman',A1) 
917 FORMAT(46x,A1,'Kimberly-Penman (Water Budget)',A1) 
921 FORMAT(1X,'=====================================================')
922 FORMAT(1X,'THIS RUN DID NOT CONTAIN A WATER BUDGET OUTPUT OPTION')
923 FORMAT(1X,'       FOR ANY OF THE SUB-AREAS BEING MODELED')
924 FORMAT(1X,'=====================================================')

926 FORMAT(48x,A1,'Calibrated Blaney-Criddle',A1)
927 FORMAT(41x,A1,'Calibrated Blaney-Criddle (Water Budget)',A1)

928 FORMAT(48x,A1,'ASCE Standarized Ref. ET ',A1)
929 FORMAT(41x,A1,'ASCE Standarized Ref. ET (Water Budget)',A1)

931 FORMAT(55x,A1,'Hargreaves',A1)
932 FORMAT(52x,A1,'Hargreaves (Water Budget)',A1)

935 FORMAT(55x,A1,'Pochop',A1)
936 FORMAT(52x,A1,'Pochop (Water Budget)',A1)
937 FORMAT(54x,A1,'User-Supplied ET',A1)
938 FORMAT(52x,A1,'User-Supplied ET (Water Budget)',A1)

  RETURN
END SUBROUTINE BUDGET
