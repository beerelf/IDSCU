SUBROUTINE OWEIGH(WT,TM,RN)

  !***************************************************************************
  !
  !   Function        : oweigh.f
  !   Author          : HB Manguerra
  !   Date            : December 1994
  !   Purpose         : This calculates the monthly mean temperature and rain- 
  !                     fall representative of the given stockpond or reservoir
  !                     by weighted average of sorrounding weather stations. 
  !                     The weather parameters to be weighted are read from 
  !                     either *.wm file or *.wd file.
  !   Calling program : stockp.f, reserv.f 
  !   Called programs : none 
  !   Input arguments : WT = weather station weights
  !   Output arguments: TM = monthly mean temperature (degree F)
  !                   : RN = monthly total rainfall (in)
  !   Assumptions     :
  !   Limitations     :
  !   Notes           : The file *.wm or *.wd is repeatedly read and close 
  !                     for every stockpond and reservoir.
  !
  !                     If both *.wm and *.wd files are available (for the 
  !                     case where Penman-Monteith and Blaney-Criddle
  !                     ET estimation methods are activated), the *.wm file
  !                     is used.  When no ET estimation method is active, the
  !                     *.wm file has to be provided. The *.wd file is 
  !                     going to be used only when *.wm file is not available
  !                     (for the case where Penman-Monteith is activated and
  !                     Blaney-Criddle ET method is not).
  !
  !***************************************************************************

  USE Globals

  !Local variable declaration
  INTEGER i, ii, id, im, iy, ib
  INTEGER nsta, yr1, yr2, IYEAR, NBAS
  INTEGER MON, endyr, iday, ilast
  INTEGER IYY, XTRADY
  INTEGER IERR
  REAL X1,X2,TM(nyrs,12),RN(nyrs,12)
  REAL dummy, WT(13)
  REAL sum1, sum2, tmn, tmx, rf
  REAL temp(nyrs,366), rain(nyrs,366)
  CHARACTER*80 NME, REMARK
  CHARACTER*120 dfile1

  INTERFACE
     SUBROUTINE CLNDR (jday, year, &
	  !Outputs
          m_mon, m_day)
       INTEGER jday, year, m_mon, m_day
     END SUBROUTINE CLNDR
  END INTERFACE

  !Read weather parameters from *.wpm

  IF (ETFLAG(1) .EQ. 0 .AND. ETFLAG(2) .EQ. 1) THEN
     dfile1 = dfile
     dfile1(flen:flen+3) = '.wd'

     OPEN(UNIT=18,FILE=dfile1,STATUS='OLD',IOSTAT=IERR)
     IF (IERR.NE.0) CALL MYEXIT(3)
     READ(18,*,ERR=102) nbas,nsta,yr1,yr2
     IF ((YR1.NE.GNYR1).OR.(YR2.NE.GNYR2)) CALL MYEXIT(99)
     IF (NBAS.NE.NBASIN) CALL MYEXIT(99)
     READ(18,900,ERR=102) REMARK
     !Read matrix of weights for non-precipitation parameters
     DO IB = 1, NBAS
	READ(18,*,ERR=102) 
     END DO
     READ(18,900,ERR=102) REMARK
     !Read matrix of weights for non-precipitation parameters
     DO IB = 1, NBAS
	READ(18,*,ERR=102) 
     END DO

     !Initialize
     do iy = 1,nyrs
	do id = 1,366
	   temp(iy,id) = 0.0
	   rain(iy,id) = 0.0
	END DO
     END DO

     do i = 1,nsta
	READ(18,900,ERR=102) NME
	READ(18,900,ERR=102) REMARK
	READ(18,*,ERR=102) dummy,dummy,dummy,dummy,dummy

	XTRADY = 0
	DO IYY = GNYR1,NYR1-1
	   IF ((MOD(IYY,4)).EQ. 0) XTRADY = XTRADY + 1
	END DO

	CALL SKIPLN(18,367*(NYR1-GNYR1)+XTRADY)
	do iy = 1,nyrs
	   READ(18,*,ERR=102) IYEAR
	   READ(18,900,ERR=102) REMARK
	   endyr = 365
	   IF ((MOD(IYEAR,4)).EQ. 0)  endyr = 366
	   do ii= 1,endyr
	      READ(18,*,ERR=102) iday,tmn,tmx,dummy,dummy,dummy,rf
	      temp(iy,ii) = temp(iy,ii) + WT(i) * (tmn+tmx) * 0.5
	      rain(iy,ii) = rain(iy,ii) + WT(i) * rf
           END DO
        END DO
	XTRADY = 0
	DO IYY = NYR2+1,GNYR2
	   IF ((MOD(IYY,4)).EQ. 0) XTRADY = XTRADY + 1
	END DO
	CALL SKIPLN(18,367*(GNYR2-NYR2)+XTRADY)

     END DO

     do iy = 1, nyrs
	m_year = IY + NYR1 -1

	ilast = 1
	sum1 = 0.0
	sum2 = 0.0
	endyr = 365
	month(2) = 28
	iyear = nyr1 + iy -1
	IF ((MOD(IYEAR,4)).EQ. 0) endyr = 366
	if (endyr.eq.366) month(2) = 29
	do ii = 1, endyr
	   call clndr(ii,m_year,im,id)
	   if (im.ne.ilast) then   ! next month
	      TM(IY,ILAST) = sum1/month(ilast)
	      RN(IY,ILAST) = sum2
	      ILAST = im
	      sum1 = 0.0
	      sum2 = 0.0
	   endif
	   sum1 = sum1 + temp(iy,ii)
	   sum2 = sum2 + rain(iy,ii)
	   if (ii.eq.endyr) then
	      TM(IY,ILAST) = sum1/month(ilast)
	      RN(IY,ILAST) = sum2
	   endif
	END DO
     END DO

     CLOSE(18)

     !Read weather parameters from *.wm

  ELSE

     dfile1 = dfile
     dfile1(flen:flen+3) = '.wm'

     OPEN(UNIT=18,FILE=dfile1,STATUS='OLD',IOSTAT=IERR)
     IF (IERR.NE.0) CALL MYEXIT(2)
     CALL SKIPN(18)
     READ(18,*,ERR=101) nbas,nsta,yr1,yr2
     IF ((YR1.NE.GNYR1).OR.(YR2.NE.GNYR2)) CALL MYEXIT(99)
     IF (NBAS.NE.NBASIN) CALL MYEXIT(99)

     READ(18,900,ERR=101) REMARK
     !Read matrix of weights for non-precipitation parameters
     DO IB = 1, NBAS
	READ(18,*,ERR=101) 
     END DO

     READ(18,900,ERR=101) REMARK
     !Read matrix of weights for non-precipitation parameters
     DO IB = 1, NBAS
	READ(18,*,ERR=101) 
     END DO

     !Initialize
     do iy = 1,nyrs
	do im = 1,12
	   TM(iy,im) = 0.0
	   RN(iy,im) = 0.0
	END DO
     END DO

     do i = 1,nsta
	READ(18,900,ERR=101) NME 
	READ(18,900,ERR=101) REMARK
	READ(18,*,ERR=101) dummy,dummy,dummy
	CALL SKIPLN(18,14*(NYR1-GNYR1))
	do iy = 1,nyrs
	   READ(18,*,ERR=101) IYEAR
	   READ(18,900,ERR=101) REMARK
	   do im= 1,12
	      READ(18,*,ERR=101) MON,X1,X2
	      TM(iy,im) = TM(iy,im) + WT(i) * X1
	      RN(iy,im) = RN(iy,im) + WT(i) * X2
           END DO
        END DO
	CALL SKIPLN(18,14*(GNYR2-NYR2))
     END DO
     CLOSE(18)
  ENDIF

900 FORMAT(A80)

  RETURN

101 CALL MYEXIT(9)
102 CALL MYEXIT(10)

end SUBROUTINE OWEIGH
