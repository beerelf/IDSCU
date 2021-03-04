SUBROUTINE MUNIC(IB,CU_MC)

  !***************************************************************************
  !
  !   Function        : munic.f
  !   Author          : HB Manguerra
  !   Date            : December 1994
  !   Purpose         : This calculates and summarizes in tabular form the 
  !                     CU from municipal category as read from *.oth file.
  !                     This category is broken down into 4 subcategories:
  !                     urban, rural, commercial and public use.  The per
  !                     capita usage and actual usage ratio is required for
  !                     each category.
  !   Calling program : other.f 
  !   Called programs : otable.f 
  !   Input arguments : ib    = current basin
  !   Output arguments: cu_mc = monthly consumptive value from municipal areas
  !                             for all years (acre-ft).
  !   Assumptions     :
  !   Limitations     :
  !   Notes           : The consumptive use is calculated as parameters are
  !                     read from the *.oth file.  The folowing parameters
  !                     are required:
  !
  !                     c1  = per capita usage in urban areas (gal/head/day)
  !                     c2  = per capita usage in rural areas (gal/head/day)
  !                     c3  = per capita usage for commercial use (gal/head/day)
  !                     c4  = per capita usage for public use (gal/head/day)
  !                     r1  = actual usage ratio in urban areas
  !                     r2  = actual usage ratio in rural areas
  !                     r3  = actual usage ratio for commercial use
  !                     r4  = actual usage ratio for public use
  !                     npop= annual population
  !                     perc= percent/100 of annual population classified as
  !                           urban
  !                     fct = monthly coefficients
  !
  !                     Monthly coefficients can be used to account for monthly 
  !                     variations in population as well as drinking patterns.
  !
  !***************************************************************************

  USE Globals

  !Local variable declaration
  INTEGER I,J,IYR,IB 
  REAL PERC, NPOP, C1, C2, C3, C4, R1, R2, R3, R4
  REAL FCT(12), CU_MC(nyrs,12)
  CHARACTER*80 OTITLE, REMARK

  !Initialize variables to zero
  DO I = 1,NYRS
     DO J = 1,12
	CU_MC(I,J) = 0.0
     END DO
  END DO

  READ(2,900) OTITLE
  READ(2,900) REMARK 

  !Read Municipal Data - Per Capita Usage, Actual Consumption Ratio
  READ(2,*) C1,C2,C3,C4     ! urban, rural, commercial, public
  READ(2,900) REMARK
  READ(2,*) R1,R2,R3,R4 

  !Read Population and Calculate Consumptive Use
  READ(2,900) REMARK
  CALL SKIPLN(2,NYR1-GNYR1)
  DO I = 1, NYRS
     READ(2,*) IYR, NPOP, PERC, (FCT(J), J=1,12)
     DO J = 1,12
	CU_MC(I,J) = CU_MC(I,J) + FCT(J)*NPOP*(PERC*R1*C1 + &
	     (1.0-PERC)*R2*C2 + R3*C3 + R4*C4) ! gall / day  
     END DO
  END DO
  CALL SKIPLN(2,GNYR2-NYR2)
  !Convert gallons/day to acre-ft/month
  DO I = 1, NYRS
     DO J = 1, 12
	CU_MC(I,J) = CU_MC(I,J) * MONTH(J) * 3.068883e-6
     END DO
  END DO

  !-----Save results of each subarea to *.net
  WRITE(20,902) QUOTE,BAS_ID(IB),QUOTE
  WRITE(20,901) QUOTE,QUOTE
  CALL OTABLE(CU_MC)

900 FORMAT(A80)
901 FORMAT(A1,'Municipal Consumptive Use(acre-ft)',A1)
902 FORMAT(A1,A60,A1)

  RETURN
END SUBROUTINE MUNIC
