SUBROUTINE FOUTPUT(IP,NYR,IB, DOY, IYEAR, &
     IMON, IDAY, JSTR, JSTP, SMSTG_D, SMSTG_PREV, &
     REFET_D, XKS, XKA, XKCB_D, ET_D, ER_D, DPERC_D, QIRR_D, NDYR)

  !***************************************************************************
  !
  !   Function        : foutput.f
  !   Author          : HB Manguerra
  !   Date            : December 1994
  !   Purpose         : This generates a detailed tabular ouput of the 
  !                     consumptive use computation based on Penman-Monteith
  !                     method.  Each subarea is assigned with a separate
  !                     flag (TYPOUT) indicating whether a detailed output
  !                     should be generated or not. 
  !   Calling program : proto.f
  !   Called programs : none
  !   Input arguments : ip    = current parcel 
  !                     ckey  = crop index
  !                     skey  = soil index
  !                     NYR   = current year (0 based)
  !                     incst = daily increase in storage due to root growth 
  !                     IB    = current sub-basin
  !                     IYEAR = printable year
  !   Output arguments: none
  !   Assumptions     :
  !   Limitations     : 
  !   Notes           : The tabular output that are designed 120 chars
  !                     long.  They should be printed in landscape format to
  !                     preserve readability of the results.
  !
  !   History         :(Date, Author, Description)
  !
  !   11/15/95   HBM  : Basal kc type for Penman-Monteith is not supported 
  !                     anymore.
  !                      
  !***************************************************************************

  USE Globals

  !Parameters
  INTEGER IP, DOY, NDYR ! Day of year
  INTEGER IB, NYR
  INTEGER IYEAR, IMON, IDAY ! Date of output
  INTEGER JSTR, JSTP ! Crop start and end (planting and harvest)
  REAL SMSTG_D, SMSTG_PREV! Soil moisture storage for today and yesterday
  REAL REFET_D ! Daily soil moisture storage, alfalfa or grass based depending on mode.
  REAL XKS, XKA, XKCB_D
  REAL ET_D, ER_D, DPERC_D, QIRR_D

  INTERFACE
     INTEGER FUNCTION DaysInYear(yr)
       INTEGER yr
     END FUNCTION DaysInYear
  END INTERFACE

  !Local Variable Declaration
  CHARACTER*21 STRG
  INTEGER IDUM 
  INTEGER CKEY, SKEY, II
  REAL SUM1, SUM2, SUM3, SUM4, SUM5, inctot
  REAL pnm_er(12),pnm_dp(12),pnm_et(12), pnm_qr(12), pnm_st(12)
  REAL SMBEG
  INTEGER days_offset

  if (DaysInYear(IYEAR) .eq. 366 .and. imon .eq. 2)  then
     days_offset = 1
  else
     days_offset = 0
  endif

  !Get Crop Scenario for Parcel
  CKEY = CROP_KEY(IB,IP)
  SKEY = SOIL_KEY(IB,IP)

  IF (TYPOUT(IB).EQ.3) THEN
     IF (IDAY.EQ.1) THEN

	IF (ETMETH(CKEY).EQ.1) THEN
	   STRG = 'Mean KC Alfalfa Based'
	ELSE 
	   STRG = 'Mean KC Grass Based  '
	ENDIF

	WRITE(3,905) QUOTE,AREA(IB,IP,NYR),QUOTE,QUOTE,CNAME(ckey), &
	     QUOTE,QUOTE,STRG,QUOTE,QUOTE,SNAME(skey),QUOTE, &
	     QUOTE,IYEAR,QUOTE
	WRITE(3,906) DLINE
	WRITE(3,903) (QUOTE, IDUM=1,44)
	WRITE(3,906) SLINE
     ENDIF

     !     ---Soil moisture storage difference is SMSTG_D-SMSTG_PREV
     WRITE(3,904) DOY,QUOTE,AMN(IMON),IDAY,QUOTE,REFET_D,XKA, &
	  XKCB_D,XKCB_D*XKA+XKS, &
	  ET_D,SMSTG_D,ER_D, &
	  DPERC_D, ET_D-ER_D
	  !DPERC_D, QIRR_D -- replaced qirr with NWR
	  

  ENDIF

  !Initialize Tabular Monthly Values First Day of Each Year
  IF (DOY.EQ.1) THEN
     DO II=1,12
	PNM_ET(II) = 0.0
	PNM_QR(II) = 0.0
	pnm_er(II) = 0.0
	pnm_dp(II) = 0.0
	pnm_st(II) = 0.0
     END DO
  ENDIF

  PNM_ET(IMON) = PNM_ET(IMON) + ET_D 
  PNM_QR(IMON) = PNM_QR(IMON) + (ET_D-ER_D) !QIRR_D
  PNM_ER(IMON) = PNM_ER(IMON) + ER_D
  pnm_dp(imon) = pnm_dp(imon) + DPERC_D
  pnm_st(imon) = SMSTG_D

  !Write Monthly Total 
  IF (TYPOUT(IB).EQ.3) THEN
     IF (IDAY.EQ. (MONTH(IMON) + days_offset)) THEN
	WRITE(3,906) SLINE
	WRITE(3,907) QUOTE,QUOTE,QUOTE,QUOTE,QUOTE,QUOTE,QUOTE, &
	     QUOTE,QUOTE,QUOTE,QUOTE,QUOTE,PNM_ET(IMON), &
	     pnm_st(IMON), PNM_ER(IMON),pnm_dp(imon), PNM_QR(IMON)
     ENDIF
  ENDIF

  IF (IDAY.EQ.1) THEN
     IF (JSTR.EQ.1) THEN
	SMBEG = SMSTG_D
     ELSE
	SMBEG = SMSTG_PREV
     ENDIF
  ENDIF

  !Write Season Total
  IF (TYPOUT(IB).EQ.3) THEN
     IF (DOY.EQ.NDYR) THEN
	SUM1 = 0.0
	SUM2 = 0.0
	SUM3 = 0.0
	SUM4 = 0.0
	SUM5 = 0.0
	DO II=1,12
	   SUM1 = SUM1 + PNM_ET(II)
	   SUM2 = SUM2 + PNM_ER(II)
	   SUM3 = SUM3 + pnm_dp(II)
	   SUM4 = SUM4 + PNM_QR(II)
	   SUM5 = SUM5 + PNM_ST(II)
	END DO

	WRITE(3,906) DLINE
	IF (SPFLAG .EQ. 1) THEN
	   inctot = (frz(ckey)-irz(ckey))*awc(skey)
	ELSE
	   inctot = (frz(ckey)-irz(ckey))*12.0*awc(skey)
	ENDIF

	WRITE(3,908) QUOTE,QUOTE,QUOTE,QUOTE,QUOTE,QUOTE, &
	     QUOTE,QUOTE, QUOTE,QUOTE,QUOTE,QUOTE, SUM1, &
	     SUM5, SUM2, SUM3, SUM4
	WRITE(3,906) DLINE
     ENDIF
  ENDIF


903 FORMAT (A1,'DOY',A1,1x,A1,'Date',A1,2x,A1,'Ref ET',A1,1x,A1,'Ka', &
       A1,2x,A1,'Kcm',A1,3x,A1,'Kc',A1,4x,A1,'ET',A1,3x,A1,'Storage', &
       A1,2x,A1,'Re',A1,4x,A1,'Perc',A1,3x,A1,'IWR',A1/ &
       1x,2A1,6x,2A1,4x,A1,'(in)',A1,3x,2A1,4x,2A1,6x,2A1,4x,A1,'(in)', &
       A1,3x,A1,'(in)',A1,3x,A1,'(in)',A1,3x,A1,'(in)',A1,3x,A1,'(in)', &
       A1)  
904 FORMAT(1x,I3,1x,A1,A3,I3,A1,F7.3,3(1x,F6.3),5(1x,F8.3))
905 FORMAT(//1x,A1,'Area (acres) = ',F7.1,A1,A1,A20,A1,A1,A21,A1,4x, &
       A1,'Soil = ',A15,A1,3x,A1,'Year = ',I4,A1)
906 FORMAT(A90)
907 FORMAT(A1,'Monthly Total',A1,A1,A1,A1,A1,A1,A1,A1,A1,A1,A1,16x,5F9.3)
908 FORMAT(A1,'Season Total',A1,A1,A1,A1,A1,A1,A1,A1,A1,A1,A1,17x,5F9.3) 

  RETURN
END SUBROUTINE FOUTPUT
