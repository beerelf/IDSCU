SUBROUTINE OTHER 

  !***************************************************************************
  !
  !   Function        : other.f 
  !   Author          : HB Manguerra
  !   Date            : April 1995 
  !   Purpose         : This is the main calling program for calculating
  !                     consumptive use for other (non-et) uses. The
  !                     results are summarized in tabular forms (month and
  !                     year) by category for every subarea; and by 
  !                     category for the total project (sum of all subareas). 
  !   Calling program : run_cu.f 
  !   Called programs : lstock.f, stockp.f, reserv.f, munic.f, minera.f
  !                     therma.f, export.f, fswild.f, recrea.f, fswild.f
  !                     recrea.f
  !   Input arguments : none 
  !   Output arguments: none 
  !   Assumptions     :
  !   Limitations     :
  !   Notes           : The CU are expressed in acre-ft.
  !
  !***************************************************************************


  USE Globals

  !-----Local variable declaration
  INTEGER I, IY, IM
  CHARACTER*120 dfile1, ofile1
  REAL CU_LS(nyrs,12),CU_SP(nyrs,12)
  REAL CU_RS(nyrs,12),CU_MC(nyrs,12)
  REAL CU_MN(nyrs,12), CU_TH(nyrs,12), CU_EX(nyrs,12)
  REAL CU_FW(nyrs,12), CU_RC(nyrs,12)
  REAL TCU_LS(nyrs,12),TCU_SP(nyrs,12),TCU_RS(nyrs,12)
  REAl TCU_MC(nyrs,12)
  REAL TCU_MN(nyrs,12),TCU_TH(nyrs,12),TCU_EX(nyrs,12)
  REAL TCU_FW(nyrs,12), TCU_RC(nyrs,12)
  REAL DLITE(12)

  !-----specify file input and output file extensions
  dfile1 = dfile
  ofile1 = dfile
  dfile1(flen:flen+4) = '.oth'
  ofile1(flen:flen+4) = '.net'

  !-----Open input and output files
  OPEN(UNIT=2,FILE=dfile1,STATUS='OLD')
  OPEN(UNIT=20,FILE=ofile1)

  !-----Initialize total project variables to zero
  DO IY = 1,NYRS
     DO IM = 1,12
	TCU_LS(IY,IM) = 0.0
	TCU_SP(IY,IM) = 0.0
	TCU_RS(IY,IM) = 0.0
	TCU_MC(IY,IM) = 0.0
	TCU_MN(IY,IM) = 0.0
	TCU_TH(IY,IM) = 0.0
	TCU_EX(IY,IM) = 0.0
	TCU_FW(IY,IM) = 0.0
	TCU_RC(IY,IM) = 0.0
     END DO
  END DO

  !-----Compute non-et CU for each subarea
  DO I = 1, NBASIN
     READ(2,900) BAS_ID(I)

     !-----Compute daylight hours
     CALL DAYHRS(BLAT(I),DLITE)

     CALL LSTOCK(I,CU_LS)
     CALL STOCKP(I,DLITE,CU_SP)
     CALL RESERV(I,DLITE,CU_RS)
     CALL MUNIC(I,CU_MC)
     CALL MINERA(I,CU_MN)
     CALL THERMA(I,CU_TH)
     CALL EXPORT(I,CU_EX)
     CALL FSWILD(I,CU_FW)
     CALL RECREA(I,CU_RC)

     !-----Update Total Consumptive Use
     DO IY = 1,NYRS
	DO IM = 1,12
	   TCU_LS(IY,IM) = TCU_LS(IY,IM) + CU_LS(IY,IM)
	   TCU_SP(IY,IM) = TCU_SP(IY,IM) + CU_SP(IY,IM)
	   TCU_RS(IY,IM) = TCU_RS(IY,IM) + CU_RS(IY,IM)
	   TCU_MC(IY,IM) = TCU_MC(IY,IM) + CU_MC(IY,IM)
	   TCU_MN(IY,IM) = TCU_MN(IY,IM) + CU_MN(IY,IM)
	   TCU_TH(IY,IM) = TCU_TH(IY,IM) + CU_TH(IY,IM)
	   TCU_EX(IY,IM) = TCU_EX(IY,IM) + CU_EX(IY,IM)
	   TCU_FW(IY,IM) = TCU_FW(IY,IM) + CU_FW(IY,IM)
	   TCU_RC(IY,IM) = TCU_RC(IY,IM) + CU_RC(IY,IM)
	END DO
     END DO
  END DO
  CLOSE(2)

  WRITE(20,901) QUOTE,QUOTE,QUOTE,QUOTE
  CALL OTABLE(TCU_LS)

  WRITE(20,902) QUOTE,QUOTE,QUOTE,QUOTE
  CALL OTABLE(TCU_SP)

  WRITE(20,903) QUOTE,QUOTE,QUOTE,QUOTE
  CALL OTABLE(TCU_RS)

  WRITE(20,904) QUOTE,QUOTE,QUOTE,QUOTE
  CALL OTABLE(TCU_MC)

  WRITE(20,905) QUOTE,QUOTE,QUOTE,QUOTE
  CALL OTABLE(TCU_MN)

  WRITE(20,906) QUOTE,QUOTE,QUOTE,QUOTE
  CALL OTABLE(TCU_TH)

  WRITE(20,907) QUOTE,QUOTE,QUOTE,QUOTE
  CALL OTABLE(TCU_EX)

  WRITE(20,908) QUOTE,QUOTE,QUOTE,QUOTE
  CALL OTABLE(TCU_FW)

  WRITE(20,909) QUOTE,QUOTE,QUOTE,QUOTE
  CALL OTABLE(TCU_RC)
  CLOSE(20)

900 FORMAT(A60)
901 FORMAT(A1,'Project Total',A1/ &
       A1,'Livestock Consumptive Use(acre-ft)',A1)
902 FORMAT(A1,'Project Total',A1/ &
       A1,'Stockpond Consumptive Use(acre-ft)',A1)
903 FORMAT(A1,'Project Total',A1/ &
       A1,'Reservoir Consumptive Use(acre-ft)',A1)
904 FORMAT(A1,'Project Total',A1/ &
       A1,'Municipal Consumptive Use(acre-ft)',A1)
905 FORMAT(A1,'Project Total',A1/ &
       A1,'Mineral Resource Consumptive Use(acre-ft)',A1)
906 FORMAT(A1,'Project Total',A1/ &
       A1,'Thermal Electric Consumptive Use(acre-ft)',A1)
907 FORMAT(A1,'Project Total',A1/ &
       A1,'Export Consumptive Use(acre-ft)',A1)
908 FORMAT(A1,'Project Total',A1/ &
       A1,'Fish and Wildlife Consumptive Use(acre-ft)',A1)
909 FORMAT(A1,'Project Total',A1/ &
       A1,'Recreation Consumptive Use(acre-ft)',A1)

  RETURN
END SUBROUTINE OTHER
