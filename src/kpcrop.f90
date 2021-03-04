      SUBROUTINE KPCROP(icrop,t_npart,begs,ends,begmo,begda,
     :     endmo, endda, key)

C***************************************************************************
C
C   Function        : kpcrop.f
C   Author          : LA Garcia
C   Date            : October 1998
C   Purpose         : This reads the Kimberly-Penman crop coefficient file
C                     and decides which type of crop coefficient (mean Alfalfa). 
C   Calling program : readin.f 
C   Called programs : none
C   Input arguments : nend  = end of growing season in julian day
C   Input arguments : icrop = crop index
C                     nbeg  = start of growing season in julian day
C                     nend  = end of growing season in julian day
C                     nbegmo = begin month of growing season
C                     nbegda = begin day of growing season
C                     nendmo = end month of growing season
C                     nendda = end day of growing season
C                     key    = crop/soil combination
C   Output arguments: t_npart = number of days in beginning month  in spring
C   Assumptions     :
C   Limitations     :
C   Notes           :  
C
C   History         : (Date, Author, Description)
C
C
C***************************************************************************

      USE Globals


C-----Local Variable Declaration
      INTEGER  OMON, NDAYS, ends, t_npart, key, K, JULIAN
      INTEGER  begs, icrop, begmo, begda, endmo, endda

      INTERFACE
         INTEGER FUNCTION DaysInYear(yr)
         INTEGER yr
         END FUNCTION

         SUBROUTINE INTERTD(ib,K,N,t_npart,selmon,midpt,
     :     tempc, dayC)
         INTEGER ib, t_npart, K, N, selmon, midpt
C     Outputs
         REAL dayC, tempc
         END SUBROUTINE

         SUBROUTINE CLNDR (jday, year,
C     Outputs
     $     m_mon, m_day)
         INTEGER jday, year, m_mon, m_day
         END SUBROUTINE
      END INTERFACE

      REAL SXKCB
      INTEGER ndays_begmo

      ndays_begmo = month(begmo)
      is_leap = .false.
      if (DaysInYear(NYR1+nyr-1) .eq. 366) is_leap = .true.

      if (begmo .eq. 2 .and. is_leap) ndays_begmo = ndays_begmo + 1

C-----Calculate midpoint of beginning month in spring
      midpts=((ndays_begmo-begda + 1 )/2) + begda

C-----Calculate midpoint of ending month in spring
      midptf= (endda + 1)/2

C-----Calculate number of days in beginning month of spring
      t_npart=(ndays_begmo-begda + 1 )

C-----interpolate temperature and dayhours for beginning month in spring
      DO  10 K = 1, 8
         IF(JULIAN(begmo, midpts) .LT. middle(K)) THEN
            CALL INTERTD(ib,temps,days,K,nyr,t_npart,begmo,
     :         midpts)
            GO TO 20
         ENDIF
         IF(JULIAN(begmo, midpts) .EQ. middle(k)) THEN
            temps = tmean(nyr,k)
CLAG            days = pclite(k)
            GO TO 20
         ENDIF
   10 CONTINUE
      CALL MYEXIT(117)

C-----interpolate temperature and dayhours for ending month in spring
   20 DO  30 k = 1,12
         IF( JULIAN(endmo, midptf).LT. middle(K)) THEN
            CALL INTERTD(ib,tempf,dayf,K,nyr,endda,
     :         endmo, midptf)
            GO TO 40
         ENDIF
         IF( JULIAN(endmo, midptf) .EQ. middle(K)) THEN
            tempf = tmean(nyr, K)
CLAG            dayf  = pclite(K)
            GO TO 40
         ENDIF
   30 CONTINUE
      CALL MYEXIT(118)

   40 CONTINUE

      JSTR = begs
      JSTP = ends

C-----Calculate Crop Coefficients 
      CALL KPGROWTH(ICROP,KEY)

C---------------------------------------------------------------
C      Begin Daily Loop
C---------------------------------------------------------------
      SXKCB = 0.0
      NDAYS = 0
      DO 50 DOY = BEGS,ENDS 

C-----Calculate Gregorian Day and Month from Julian Day
         CALL CLNDR(DOY,IMON,IDAY)
         IF(DOY .EQ. BEGS) THEN
            OMON = IMON
         ENDIF 

         IF(OMON .EQ. IMON) THEN
            SXKCB = SXKCB + XKPC(DOY)
            NDAYS = NDAYS + 1
         ELSE
            XKC(OMON) = SXKCB/NDAYS
            OMON = IMON
            NDAYS = 0
            SXKCB = 0
         ENDIF

C-----End of daily Calculations
 50   CONTINUE
      XKC(OMON) = SXKCB/NDAYS
      NDAYS = 0
      SXKCB = 0

      RETURN
      END
