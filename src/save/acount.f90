SUBROUTINE ACOUNT (DOY, SMCAP, TRANS_D, WSEVAP_D, ER_D, &
     XKS, DEFIT, use_smb, &
				!     Outputs
     QIRR_D, ET_D, DPERC_D, SMSTG_D)

  !***************************************************************************
  !
  !   Function        : acount.f 
  !   Author          : HB Manguerra
  !   Date            : December 1994
  !   Purpose         : This implements the daily soil moisture budget during
  !                     the growing season for the Penman-Monteith method.  
  !   Calling program : proto.f
  !   Called programs : irgate.f, evap.f
  !   Input arguments : cid - crop index
  !   Output arguments: none
  !   Assumptions     :
  !   Limitations     :
  !   Notes           : This is a condensed and modified version of the acount
  !                     subroutine of program SMB which is developed by
  !                     Wheeler and Associates.
  !
  !                   : The _D suffix is to indicate that inputs are daily
  !                     versions of the corresponding arrays in order to avoid
  !                     confusion.
  !
  !   History         :(Date, Author, Description)
  !
  !   11/15/95   HBM  : Subroutine Evap is not needed because the 
  !                     basal kc type is not supported anymore.
  !
  !***************************************************************************


  USE Globals

  !Parameters
  INTEGER DOY
  REAL SMCAP, TRANS_D, WSEVAP_D, ER_D, QIRR_D
  REAL XKS, DEFIT(12)
  LOGICAL use_smb
  !Outputs
  REAL ET_D, DPERC_D, SMSTG_D

  INTERFACE
     SUBROUTINE IRGATE(SMCAP, SMSTGD, DEFIT_M, TRANS_D, use_smb, &
				!Outputs
	  QIRR_D)
       REAL SMCAP, SMSTGD, DEFIT_M, TRANS_D
       LOGICAL use_smb
       !Outputs
       REAL QIRR_D
     END SUBROUTINE IRGATE

     SUBROUTINE CLNDR (jday, year, &
				!Outputs
          m_mon, m_day)
       INTEGER jday, year, m_mon, m_day
     END SUBROUTINE CLNDR
  END INTERFACE

  !Local Variable Declarations
  INTEGER IMON, IDAY

  !Calculation During Growing Season-----------------!
  !Add retained precipitation during growing season
  SMSTG_D = SMSTG_D + ER_D                          

  CALL CLNDR(DOY,m_year,IMON,IDAY)                  

  !Irrigate if Necessary                             
  CALL IRGATE(SMCAP, SMSTG_D, DEFIT(IMON), TRANS_D, use_smb,  &
       !Outputs                                           
       QIRR_D)                                      

  SMSTG_D = SMSTG_D + QIRR_D                        

  !Calculate wet soil evaporation                    
  XKS = 0                                           
  ET_D = TRANS_D + WSEVAP_D                         

  !Take ET out of soil moisture storage              
  if (USE_WATER_STRESS) then
     ! Now we let soil moisture go negative if we are modeling crop stress
     SMSTG_D = SMSTG_D-ET_D
  else
     SMSTG_D = MAX( SMSTG_D-ET_D, 0.0 )
  end if

  !Calculate deep percolation                        
  DPERC_D = MAX(SMSTG_D-SMCAP, 0.0)                 
  SMSTG_D = SMSTG_D - DPERC_D                       
  !End of calculation during growing season----------!

300 FORMAT ('**** SOIL MOISTURE HAS DRIED UP ****  DATE: ',2I4)

  RETURN
END SUBROUTINE ACOUNT
