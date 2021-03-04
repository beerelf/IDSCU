***************************************************************************
C
C   Include File    : pmcommon.inc
C   Author          : HB Manguerra
C   Date            : December 1994
C   Purpose         : This includes declaration of all variables used
C                     in the Penman-Monteith calculation of evapotranspi-
C                     ration outside the calulation of reference 
C                     evapotranspiration.
C   Calling program : acount.f, evap.f, finput.f, foutput.f, growth.f,
C                     irgate.f, kbasal.f, proto.f 
C   Assumptions     :
C   Limitations     :
C   Notes           :
C
C***************************************************************************
      MODULE PMCOMMON
      SAVE


C-----Dimension
      INTEGER PM_NY, KP_CI
      PARAMETER (PM_NY = 150)
      PARAMETER (KP_CI = 21)

C-----Variable Declaration
      CHARACTER*20 CROP

      INTEGER JSTR, JSTP, IYEAR, DOY, IMON, IDAY
      INTEGER ILAST
      INTEGER NDYR
      INTEGER JSTRYR, JENDYR
      INTEGER IRN, ETMETH(40)
      INTEGER DYBEGM

      REAL SMBEG, SMBEGM
      REAL SMSTGD, SMSTG0
      REAL CAPVAP, RZONE, XKA, XKS, EVAPDT
      REAL REFET(366),TRANS(366),QIRR(366),SMCAP,SMCAP0
      REAL DEFIT(12)
      REAL PCP(366), EPCP(366), CN(3)
      REAL SMSTG(0:366), XKCB(366), WSEVAP(366)
      REAL DPERC(366)
      REAL ET(366)
      REAL RNFAC
      REAL, ALLOCATABLE :: REFETR(:,:), REFETO(:,:) ! NYEARS, 366
      REAL KCDAY(40,33) 
      REAL KCB(40,33)
      REAL pnm_er(12),pnm_dp(12),pnm_et(12), pnm_qr(12)

C     KIMBERLY PENMAN VALUES
      REAL XKPC(366)

      END MODULE
