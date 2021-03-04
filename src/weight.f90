      SUBROUTINE WEIGHT(N,NSTA,IB,WW,WP,
C     Outputs
     $     ETR,ETO,RF)

C***************************************************************************
C
C   Function        : etoref.f
C   Author          : HB Manguerra
C   Date            : December 1994
C   Purpose         : This calculates the reference evapotranspiration and
C                     precipitation representative of the given subarea
C                     by weighted average of sorrounding weather stations.
C   Calling program : proto.f
C   Called programs : none
C   Input arguments : N          = number of calculation years
C                     NSTA       = number of weather stations
C                     WW()       = weights of weather parameters other than 
C                                  precipitation
C                     WP()       = weights of precipitation parameters
C                     ib         = current sub-basin
C   Output arguments: minim(366) = weighted minimum daily temperature
C                     etr(366)   = weighted reference et alfalfa-based
C                     eto(366)   = weighted reference et grass-based 
C                     rf(366)    = weighted precipitation 
C   Assumptions     :
C   Limitations     :
C   Notes           : The reference evapotranspiration and precipitation
C                     are read from files that
C                     are created by previous calculations.
C
C***************************************************************************


      USE Globals

C     Paramemeters
      INTEGER N, NSTA, IB
      REAL WW(nbasin,13), WP(nbasin,13)
C     Outputs
      REAL, DIMENSION(N,366) :: ETR, ETO, RF

      INTERFACE
         INTEGER FUNCTION DaysInYear(yr)
         INTEGER yr
         END FUNCTION
      END INTERFACE

C-----Local Variable Declaration
      INTEGER I, IY, J, NT, NDYR, YEAR
      REAL X2,X3,X4 
      CHARACTER*80 F_IN1
      CHARACTER*120 dfile1
      CHARACTER*80 NAME(13) 

      dfile1 = dfile

C-----Open input files
      DO 5 I = 1, NSTA
         IF (I.LT.9) THEN
            WRITE(F_IN1,100) I
         ELSE
            WRITE(F_IN1,101) I
         ENDIF

         dfile1(flen:flen+4) = F_IN1

         OPEN(100+I,FILE=dfile1)
 5    CONTINUE

C-----Initialize variables to zero
      DO 7 IY = 1,N
         DO 6 J  = 1,366
            ETR(IY,J) = 0.0
            ETO(IY,J) = 0.0
            RF(IY,J) = 0.0
 6       CONTINUE
 7    CONTINUE

C-----Read and Weigh
      DO 10 I = 1,NSTA
         DO 15 IY = 1,N 
            READ(100+I,900) NAME(I)
            READ(100+I,*) YEAR
            NDYR = DaysInYear(YEAR)
            DO 20 J = 1, NDYR
               READ(100+I,*) NT, X2, X3, X4 
               ETR(IY,J) = ETR(IY,J) + WW(IB,I) * X2
               ETO(IY,J) = ETO(IY,J) + WW(IB,I) * X3 ! ww=should be in fraction
C            Patterson: Ignore rainfall because it is already in the .wd file.
C               RF(IY,J) = RF(IY,J) + WP(IB,I) * X4
 20         CONTINUE
 15      CONTINUE
 10   CONTINUE

C-----Close Files
      DO 30 I = 1, NSTA
         CLOSE(100+I)
 30   CONTINUE

 100  FORMAT ('.w',I1)
 101  FORMAT ('.w',I2)
 900  FORMAT (A80)

      RETURN
      END
