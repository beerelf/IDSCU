SUBROUTINE LSTOCK(IB,CU_LS)

  !***************************************************************************
  !
  !   Function        : lstock.f
  !   Author          : HB Manguerra
  !   Date            : December 1994
  !   Purpose         : This calculates the consumptive use by livestock.
  !   Calling program : other.f
  !   Called programs : otable.f 
  !   Input arguments : ib    = current sub-basin
  !   Output arguments: cu_ls = monthly consumptive use of livestock in acre-
  !                             ft for all years (acre-ft).
  !   Assumptions     : The use of an average daily consumption value is 
  !                     adequate to estimate the long-term (annual) total
  !                     consumption of the livestocks.
  !   Limitations     : Variations in monthly consumption is due mainly on the
  !                     variation in monthly head counts and not on the 
  !                     changing water consumption pattern caused by
  !                     temperature etc.  Moreover, since livestock surveys
  !                     are usually reported by years and not by months, the
  !                     monthly value may as well be taken equal to the yearly
  !                     value which makes the monthly consumption constant 
  !                     each year. It can be made to vary however, by 
  !                     arbitrarily selecting monthly head counts that
  !                     deviates from the annual value, the deviation being
  !                     a measure of the effect of temperature, etc.  
  !   Notes           : The consumptive use is calculated as parameters are
  !                     read from the *.oth file.  The folowing parameters
  !                     are required:
  !
  !                     nytpe = number of types of livestock
  !                     C     = livestock daily water consumption
  !                           = gallon/head/day
  !                     N_LS  = number of heads of a particular livestock 
  !
  !***************************************************************************

  USE Globals

  INTEGER I,J,K,IYR,IB
  INTEGER NTYPE, N_LS(12)
  REAL C(10),CU_LS(nyrs,12)
  CHARACTER*80 OTITLE, REMARK

  !Initialize
  DO I = 1,NYRS
     DO J = 1,12
	CU_LS(I,J) = 0.0
     END DO
  END DO

  READ(2,900) OTITLE
  READ(2,900) REMARK
  !Read Livestock Constants
  READ(2,*) ntype
  READ(2,900) REMARK

  !Read Number of Heads (Livestock, Month and Year)
  DO K = 1, ntype
     READ(2,902) C(K), REMARK
     READ(2,900) REMARK
     CALL SKIPLN(2,NYR1-GNYR1)
     DO I = 1, NYRS 
	READ(2,*) IYR, (N_LS(J), J=1,12)

	!Calculate consumptive use
	DO J = 1, 12
	   CU_LS(I,J) = CU_LS(I,J) + C(K)*N_LS(J) ! gall/day 
	END DO
     END DO
     CALL SKIPLN(2,GNYR2-NYR2)
  END DO

  !Convert gallons/day to acre-ft/month
  DO I = 1, NYRS 
     DO J = 1, 12
	CU_LS(I,J) = CU_LS(I,J) * MONTH(J) * 3.068883e-6
     END DO
  END DO

  !Write Tabular Summary
  WRITE(20,903) QUOTE,BAS_ID(IB),QUOTE
  WRITE(20,901) QUOTE,QUOTE
  CALL OTABLE(CU_LS)

900 FORMAT(A80)
901 FORMAT(A1,'Livestock Consumptive Use(acre-ft)',A1)
902 FORMAT(F4.1,A80)
903 FORMAT(A1,A60,A1)

  RETURN
END SUBROUTINE LSTOCK
