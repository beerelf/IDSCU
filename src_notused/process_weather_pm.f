      SUBROUTINE PROCESS_WEATHER_PM(is, ndyr, X1, X2, X3, X4, X5, X6,
     :     slat, elev, ZM, ZH)

C***************************************************************************
C
C   Function        : process_weather_pm.f
C   Author          : LA Garcia & Dave Patterson
C   Date            : dec 2002
C   Purpose         : This reads the Penman-Monteith crop coefficient file
C                     and decides which type of crop coefficient (mean Alfalfa). 
C   Calling program : readin.f 
C   Called programs : none
C   Input arguments : ndyr - number of days in the year
C                     X1,X6 - values in columns 1-6 of weather data.
C                     slat,elev - weather station latitude and elevation
C   Output arguments: maxT, minT - maximum and minimum temp in centigrade.
C                     dpt - dew point
C                     RS - solar radiation
C                     WD - wind speed
C                     RFall - precipitation
C   Assumptions     :
C   Limitations     :
C   Notes           :  
C
C   History         : (Date, Author, Description)
C
C
C***************************************************************************

      INTERFACE
         SUBROUTINE ETREF(is,ndyr,etr,eto,minT,maxT,dpt,wd,rs,ZM,ZH,slat,
     :        elev)
C-----Parameters
         INTEGER ndyr, is
         REAL etr(366), eto(366)
         REAL maxT(366), minT(366)
         REAL wd(366),rs(366), ZM, ZH, slat, elev
         END SUBROUTINE

         SUBROUTINE AVGMON(ndyr,mn,mx,rf,dewpt,solar,wind,tavg,rdata,
     :     dpdata,srdata,wddata)
C     Parameters
         INTEGER ndyr
         REAL mn(366),mx(366),rf(366)
         REAL dewpt(366),solar(366),wind(366)
         REAL tavg(12), rdata(12)
         REAL dpdata(12),srdata(12),wddata(12)
         END SUBROUTINE

      END INTERFACE

C     Parameter declarations
      INTEGER is, DOY, NDYR              ! Number of days in the year
      REAL X1(366), X2(366), X3(366), X4(366), X5(366), X6(366)
      REAL slat, elev, ZM, ZH

C     Locals
      REAL maxT(366), minT(366), DPT(366), RS(366), WD(366)
      REAL RFall(366)
      REAL eto(366), etr(366)
      REAL tempAvg(12), rainf12(12), dew12(12), solar12(12), wind12(12)

      DO 10 DOY=1,NDYR
         ! Calculate Penman-Monteith ET parameters.
         maxT(DOY)  = .5556 * (X1(DOY) - 32) ! farenheit to centigrade
         minT(DOY)  = .5556 * (X2(DOY) - 32)
         DPT(DOY)  = .5556 * (X3(DOY) - 32) ! X3 = Dew Pt
         RS(DOY) = X4(DOY) / 23.892       ! langley to MJ / m2
         WD(DOY) = X5(DOY) * 1.609        ! mi/day to km/day 
         RFall(DOY) = X6(DOY)
 10   CONTINUE

      CALL ETREF(is, NDYR,ETR,ETO,minT,maxT,dpt,wd,rs,ZM,ZH,SLAT,ELEV)
      CALL AVGMON(NDYR,minT,maxT,RFall,DPT,RS,WD,
     :     TEMPAVG,RAINF12,DEW12,SOLAR12,WIND12)

      RETURN
      END
