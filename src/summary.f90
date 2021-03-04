SUBROUTINE SUMMARY()

!***************************************************************************
!
!   Function        : summary.f
!   Author          : HB Manguerra
!   Date            : May 1995, 
!   Purpose         : This summarizes input information and save them in
!                     *.sum output file.  Depending on a user-specified 
!                     flag, the summary can either be basic or detailed.
!   Calling program : run_cu.f
!   Called programs :
!   Input arguments : none
!   Output arguments: none
!   Assumptions     :
!   Limitations     :
!   Notes           : Flag:
!                         S_OUT = 0 - basic summary
!                         S_OUT = 1 - detailed summary
!   Luis Garcia
!   Oct. 2003 - Update the number of crops that the model can handle.  
!               Added Sorghum, Sunflowers, and Soybeans.
!
!***************************************************************************

  USE globals

  INTERFACE
     INTEGER FUNCTION DaysInYear(yr)
       INTEGER yr
     END FUNCTION DaysInYear

     SUBROUTINE CLNDR (jday, year, &
       !  Outputs
	  m_mon, m_day)
       INTEGER jday, year, m_mon, m_day
     END SUBROUTINE CLNDR

     INTEGER FUNCTION JULIAN (m_mon, m_day, yr)
       !     Parameters
       INTEGER m_mon, m_day, yr
     END FUNCTION JULIAN

     INTEGER FUNCTION Crptype(ndx)
       integer ndx
     END FUNCTION Crptype

     REAL FUNCTION parse_num(instr)
       CHARACTER*120 instr
     END FUNCTION parse_num
  END INTERFACE

!-----Local variable declaration
  CHARACTER*8 OPTSTR
  CHARACTER*120 ofile1
  CHARACTER*120 TITL1, TITL2, TITL3
  CHARACTER*120 TITL4, TITL5, TITL6
  INTEGER I,J,K,Y,IDUM,tlen
  REAL SUM
  REAL SUM_C(N_CROPS), SUM_Y(NYRS)
  INTEGER EDAT12(N_CROPS), FDAT12(N_CROPS)
  INTEGER EDAT34(N_CROPS), FDAT34(N_CROPS)
  REAL ST1(N_CROPS), ST2(N_CROPS)
  REAL SMAD(N_CROPS), SRZ(N_CROPS), SAWC(N_SOILS)
  INTEGER NUMC(N_CROPS)
  INTEGER JDAT12, JDAT34, NN, K_SOIL
  INTEGER EP1, EP2, FP1, FP2, EH1, EH2, FH1, FH2 
  CHARACTER*20 NU_NME(N_CROPS+1)
  INTEGER NU_K, NU_DIM, IYY, XTRADY
  REAL NU_SUM(NYRS,N_CROPS)
  REAL SMLT
  INTEGER YR1,YR2
  REAL TWWS(NBASIN),TWRS(NBASIN) ! NBASIN
  INTEGER IERR

  CHARACTER*360 REMARK, LINE
  CHARACTER*240 DFILE1, TITL7,TITL7a,SLLINa,DLLINa
  INTEGER IB,NBAS,IY,NDYR,IYEARS,ID,IM,IS
  REAL TDATA(12),RDATA(12),DUMMY,LAT
  REAL SSUMT,SSUMR,SSUMD,SSUMS,SSUMW,SSUM,SSUMDL
  REAL DPDATA(12),SRDATA(12),WDDATA(12)
  REAL SUMT(12),SUMR(12),SUMD(12),SUMS(12),SUMW(12),SUMDL(12)
  REAL TMX(366),TMN(366),EDPT(366),RS(366),WD(366),RFALL(366)
  REAL TMP_T(NYRS,12),TMP_R(NYRS,12),TMP_D(NYRS,12)
  REAL X(6,366)
  REAL TMP_W(NYRS,12),TMP_S(NYRS,12)
  INTEGER yr_offset

  ! Weather stuff
  CHARACTER*10 substr
  INTEGER s
  REAL f

  PARAMETER (TITL1=' Year      Crop            Soil Type         Are&
  &a  Planting  Harvest  Earliest    Latest    Root    MAD    AWC   A&
  &pplic')
  PARAMETER (TITL2='                                                &
  &                      Moisture   Moisture   Depth                D &
  &epth')         
  PARAMETER (TITL3='                                            (acr&
  &e)                      (F)        (F)      (ft)    (%)  (ft/ft) &
  &(in)')


  PARAMETER (TITL4='"Year"    "Crop            Soil Type"       "Are&
  &a""Planting""Harvest""Earliest"  "Latest"  "Root"  "MAD"  "AWC" "A&
  &pplic"')
  PARAMETER (TITL5='  ""                    ""                    ""&
  &        ""      ""   "Moisture" "Moisture" "Depth"  ""      ""  "D&
  &epth"')         
  PARAMETER (TITL6='  ""                    ""                 "(acr&
  &e)"     ""      ""     "(F)"      "(F)"    "(ft)"  "(%)""(ft/ft)"&
  &(in)"')

  TITL7=' Subarea\Station   St1  St2  St3  St4  St5  St6  St7  St8  &
  &St9 St10 St11 St12 St13 St14 St15 St16 St17 St18 ST19 St20    '

  ofile1 = dfile
  ofile1(flen:flen+4) = '.sum'

  OPEN(UNIT=8,FILE=ofile1)

!-----Initialize
  DO  I = 1, N_CROPS
     SUM_C(I) = 0.0
  END DO

  !-----Write Title strings
  WRITE(8,899) 
  WRITE(8,900) (QUOTE,TITLE(I), QUOTE, I=1,3)
  WRITE(8,*)

  !-----Write begin and end years
  WRITE(8,901) QUOTE,QUOTE,NYR1,QUOTE,QUOTE,NYR2

  !-----Show projection options.
  IF (HISTYR2 < NYR2) THEN
     !        Determine how data was synthesized.
     WRITE(8,*) "Last year of historical data is ", HISTYR2
     WRITE(8,*) "Synthesized data is ", PROJ_DETAILS
  ENDIF

  !-----Write program main options
  IF (ETFLAG(1).EQ.1) THEN
     OPTSTR = 'Enabled '
  ELSE
     OPTSTR = 'Disabled'
  ENDIF
  WRITE(8,902) QUOTE,OPTSTR,QUOTE

  IF (ETFLAG(2).EQ.1) THEN
     OPTSTR = 'Enabled '
  ELSE
     OPTSTR = 'Disabled'
  ENDIF
  WRITE(8,903) QUOTE,OPTSTR,QUOTE

  IF (ETFLAG(3).EQ.1) THEN
     OPTSTR = 'Enabled '
  ELSE
     OPTSTR = 'Disabled'
  ENDIF
  WRITE(8,904) QUOTE,OPTSTR,QUOTE

  IF (ETFLAG(4).EQ.1) THEN
     OPTSTR = 'Enabled '
  ELSE
     OPTSTR = 'Disabled'
  ENDIF
  WRITE(8,9041) QUOTE,OPTSTR,QUOTE

  IF (ETFLAG(5).EQ.1) THEN
     OPTSTR = 'Enabled '
  ELSE
     OPTSTR = 'Disabled'
  ENDIF
  WRITE(8,9042) QUOTE,OPTSTR,QUOTE

  IF (ETFLAG(6).EQ.1) THEN
     OPTSTR = 'Enabled '
  ELSE
     OPTSTR = 'Disabled'
  ENDIF
  WRITE(8,9043) QUOTE,OPTSTR,QUOTE

  IF (ETFLAG(7).EQ.1) THEN
     OPTSTR = 'Enabled '
  ELSE
     OPTSTR = 'Disabled'
  ENDIF
  WRITE(8,9044) QUOTE,OPTSTR,QUOTE

  IF (ETFLAG(8).EQ.1) THEN
     OPTSTR = 'Enabled '
  ELSE
     OPTSTR = 'Disabled'
  ENDIF
  WRITE(8,9045) QUOTE,OPTSTR,QUOTE

  IF (ETFLAG(9).EQ.1) THEN
     OPTSTR = 'Enabled '
  ELSE
     OPTSTR = 'Disabled'
  ENDIF
  WRITE(8,9046) QUOTE,OPTSTR,QUOTE


  WRITE(8,*)      

  !-----Write option for monthly effective rainfall
  IF (ETFLAG(1).EQ.1 .OR. ETFLAG(4) .EQ. 1) THEN 
     IF (RN_XCO.EQ.1) THEN
	WRITE(8,905) QUOTE,QUOTE
     ELSEIF (RN_XCO.EQ.2) THEN
	WRITE(8,906) QUOTE,QUOTE
     ELSE
	WRITE(8,907) QUOTE,QUOTE
     ENDIF
  ELSE
     WRITE(8,930) QUOTE,QUOTE
  ENDIF

  WRITE(8,*)

  !-----Write option for daily effective rainfall
  IF (ETFLAG(2).EQ.1) THEN
     IF (RN_REF.EQ.1) THEN
	WRITE(8,908) QUOTE,QUOTE,QUOTE,QUOTE
     ELSEIF (RN_REF.EQ.2) THEN
	WRITE(8,909) QUOTE,QUOTE,QUOTE,RPARA(1),QUOTE
     ELSEIF (RN_REF.EQ.3) THEN
	WRITE(8,910) QUOTE,QUOTE,QUOTE,RPARA(1),QUOTE,QUOTE, &
	     RPARA(2),QUOTE,QUOTE,RPARA(3),QUOTE
     ELSE
	WRITE(8,911) QUOTE,QUOTE
     ENDIF
  ELSE
     WRITE(8,931) QUOTE,QUOTE
  ENDIF

  WRITE(8,*)

  !-----Write water supply availability
  IF (ISUPLY.EQ.1) THEN
     WRITE(8,912) QUOTE,QUOTE
  ELSE
     WRITE(8,913) QUOTE,QUOTE
  ENDIF

  WRITE(8,*)

  !-----Write annual carry over soil moisture coeffs
  WRITE(8,914) QUOTE,QUOTE
  SMLT = SMEF(1)
  YR1 = NYR1
  DO I = NYR1,NYR2
     IF (SMEF(I-NYR1+1).NE.SMLT) THEN
	WRITE(8,934) YR1,I-1, SMEF(I-NYR1)
	SMLT = SMEF(I-NYR1+1)
	YR1 = I
     ENDIF
  END DO
  WRITE(8,934) YR1,NYR2, SMEF(I-NYR1)
  WRITE(8,*)

  !-----Write total project area by crop
  DO J = 1, NYRS
     SUM = 0.0
     DO K = 1, N_CROPS
	IF (C_AREA(J,K).GT.0) THEN
	   SUM = SUM + C_AREA(J,K) 
	   SUM_C(K) = SUM_C(K) + C_AREA(J,K)
	ENDIF
     END DO

     SUM_Y(J) = SUM

  END DO


  !-----Renumber indeces
  NU_K = 0
  DO K = 1,N_CROPS
     IF (SUM_C(K).GT.0) THEN
	NU_K = NU_K +1
	IF (IQUOTE.EQ.1) THEN
	   NU_NME(NU_K) = CNAME(K)
	ELSE
	   NU_NME(NU_K) = CNAME(K)
	ENDIF
	DO J=1,NYRS
	   NU_SUM(J,NU_K) = C_AREA(J,K)
	END DO
     ENDIF
  END DO

  NU_DIM = NU_K
  IF (IQUOTE.EQ.1) THEN
     NU_NME(NU_DIM+1) = '   "Total"'
  ELSE
     NU_NME(NU_DIM+1) = '     Total'
  ENDIF

  WRITE(8,921) QUOTE,QUOTE
  WRITE(8,922) QUOTE,QUOTE,(NU_NME(K), K=1,NU_DIM+1) 
  DO J=1,NYRS
     WRITE(8,923) NYR1+J-1,(NU_SUM(J,K), K=1,NU_DIM), SUM_Y(J)
  END DO
  
  WRITE(8,*)

  IF (S_OUT.EQ.0) THEN
     !-----Write basic crop information
     DO K = 1,N_CROPS
	EDAT12(K) = 365
	FDAT12(K) = 0
	EDAT34(K) = 365
	FDAT34(K) = 0
	ST1(K) = 0
	ST2(K) = 0
	SMAD(K) = 0
	SRZ(K) = 0
	NUMC(K) = 0
     END DO

     DO K = 1,N_CROPS
	JDAT12 = JULIAN(GDATE1(K),GDATE2(K), 1990)
	JDAT34 = JULIAN(GDATE3(K),GDATE4(K), 1990)
	IF (JDAT12.LT.EDAT12(K)) EDAT12(K) = JDAT12
	IF (JDAT12.GT.FDAT12(K)) FDAT12(K) = JDAT12
	IF (JDAT34.LT.EDAT34(K)) EDAT34(K) = JDAT34
	IF (JDAT34.GT.FDAT34(K)) FDAT34(K) = JDAT34
	ST1(K) = ST1(K) + TMOIS1(K)
	ST2(K) = ST2(K) + TMOIS2(K)
	SMAD(K) = SMAD(K) + MAD(K)
	SRZ(K) = SRZ(K) + FRZ(K)
	NUMC(K) = NUMC(K) + 1
     END DO

     WRITE(8,999) DLLINE
     WRITE(8,926) (QUOTE, IDUM=1,48)
     WRITE(8,999) SLLINE
     DO K = 1,N_CROPS
	NN = NUMC(K)
	! Use non-leap year for calendar date.
	CALL CLNDR(EDAT12(K),1990,EP1,EP2)
	CALL CLNDR(FDAT12(K),1990,FP1,FP2)
	CALL CLNDR(EDAT34(K),1990,EH1,EH2)
	CALL CLNDR(FDAT34(K),1990,FH1,FH2) 
	IF (SUM_C(K).GT.0) THEN
	   IF (CRPTYPE(K).EQ.1) THEN !perennial
	      WRITE(8,927) QUOTE,CNAME(K),QUOTE,QUOTE,QUOTE,QUOTE, &
		   QUOTE,QUOTE,QUOTE,QUOTE,QUOTE,ST1(K)/NN, &
		   ST2(K)/NN, SRZ(K)/NN,SMAD(K)/NN
	   ELSE
	      WRITE(8,925) QUOTE,CNAME(K),QUOTE,QUOTE,EP1,EP2,QUOTE, &
		   QUOTE,FP1,FP2,QUOTE,QUOTE,EH1,EH2,QUOTE,QUOTE, &
		   FH1, FH2,QUOTE,ST1(K)/NN,ST2(K)/NN,SRZ(K)/NN, &
		   SMAD(K)/NN
	   ENDIF
	ENDIF
     END DO
     WRITE(8,999) SLLINE
     WRITE(8,*)

  ELSE
     !-----Write crop information if output <> 0
     WRITE(8,916) QUOTE,QUOTE
     DO I = 1, NBASIN
	WRITE(8,917) QUOTE,BAS_ID(I),QUOTE
	WRITE(8,999) DLLINE
	IF (IQUOTE.EQ.0) THEN
	   WRITE(8,999) TITL1
	   WRITE(8,999) TITL2
	   WRITE(8,999) TITL3
	ELSE
	   WRITE(8,999) TITL4
	   WRITE(8,999) TITL5
	   WRITE(8,999) TITL6
	ENDIF
	WRITE(8,999) SLLINE
	DO Y = 1,NYRS 
	   DO J = 1, NPARCE(I)
	      K = CROP_KEY(I,J)
	      K_SOIL = SOIL_KEY(I, J)
	      IF (CRPTYPE(K).EQ.2) THEN ! perennial
		 IF (J.EQ.1) THEN
		    WRITE(8,935) NYR1+Y-1,QUOTE,CNAME(K),SNAME(K_SOIL), &
			 QUOTE, AREA(I,J,Y),QUOTE,QUOTE,QUOTE,QUOTE, &
			 TMOIS1(K),TMOIS2(K),IRZ(K),MAD(K),AWC(K_SOIL)
		 ELSE
		    WRITE(8,936) QUOTE,QUOTE,QUOTE,CNAME(K), &
			 SNAME(K_SOIL),QUOTE, AREA(I,J,Y),QUOTE,QUOTE, &
			 QUOTE,QUOTE, TMOIS1(K),TMOIS2(K),IRZ(K), &
			 MAD(K),AWC(K_SOIL)
		 ENDIF

	      ELSE
		 IF (J.EQ.1) THEN
		    WRITE(8,918) NYR1+Y-1,QUOTE,CNAME(K),SNAME(K_SOIL), &
			 QUOTE, AREA(I,J,Y),QUOTE,GDATE1(K), &
			 GDATE2(K),QUOTE, QUOTE,GDATE3(K),GDATE4(K), &
			 QUOTE, TMOIS1(K),TMOIS2(K),IRZ(K),MAD(K), &
			 AWC(K_SOIL)
		 ELSE
		    WRITE(8,919) QUOTE,QUOTE,QUOTE,CNAME(K), &
			 SNAME(K_SOIL),QUOTE, AREA(I,J,Y),QUOTE,GDATE1(K), &
			 GDATE2(K),QUOTE,QUOTE, GDATE3(K),GDATE4(K), &
			 QUOTE, TMOIS1(K),TMOIS2(K),IRZ(K),MAD(K), &
			 AWC(K_SOIL)
		 ENDIF
	      ENDIF
	   END DO
	   WRITE(8,*)
	   WRITE(8,920) QUOTE,QUOTE,QUOTE,QUOTE,T_AREA(I,Y)
	   WRITE(8,*)
	END DO
	WRITE(8,999) SLLINE
	WRITE(8,*)
	WRITE(8,*)
     END DO
  ENDIF

!-----------------------------------------------------------------------------
!     Summary of Weather Parameters
!-----------------------------------------------------------------------------
  
  IF (ETFLAG(1) .EQ. 1 .OR. ETFLAG(4) .EQ. 1 ) THEN
     dfile1 = dfile
     dfile1(flen:flen+3) = '.wm'
  ENDIF

  IF (ETFLAG(1).EQ.1 .OR. ETFLAG(4) .EQ. 1) THEN

     !-----open and read input file
     OPEN(UNIT=18,FILE=dfile1,STATUS='OLD',IOSTAT=IERR)
     IF (IERR.NE.0) CALL MYEXIT(2)
     CALL SKIPN(18)
     READ(18,*,ERR=503) NBAS,N_STA,YR1,YR2

     IF ((YR1.NE.GNYR1).OR.(YR2.NE.GNYR2)) CALL MYEXIT(36)
     IF (NBAS.NE.NBASIN) CALL MYEXIT(37)
     READ(18,950,ERR=504) REMARK
     !-----Read matrix of weights for non-precipitation parameters
     ALLOCATE(WWS(NBASIN, N_STA))
     ALLOCATE(WRS(NBASIN, N_STA))
     ALLOCATE(STNAME(N_STA))

     DO IB = 1, NBAS
	DO I = 1, N_STA
	   WWS(IB, I) = 0
	   WRS(IB, I) = 0
	END DO
     END DO

     DO IB = 1, NBAS
	READ(18,fmt='(A360)',ERR=505) LINE               
	! Skip basin name
	LINE = LINE(28:360)
	do while (LEN(LINE) .gt. 0)
	   ! Weather station is first token
	   I = index(LINE, ":")
	   if (I .eq. 0) exit
	   substr = LINE(1:I-1)
	   s = parse_num(substr)
	   ! Skip : and read weight
	   LINE = LINE(I+1:LEN(LINE))
	   f = parse_num(LINE)
	   WWS(IB, s) = f
	END do
     END DO

31   FORMAT(A27,20(F6.2))

     READ(18,950,ERR=506) REMARK
     !-----Read matrix of weights for non-precipitation parameters
     DO  IB = 1, NBAS
	READ(18,fmt='(A360)',ERR=507) LINE               
	! Skip basin name
	LINE = LINE(28:360)
	do while (LEN(LINE) .gt. 0)
	   ! Weather station is first token
	   I = index(LINE, ":")
	   if (I .eq. 0) exit
	   substr = LINE(1:I-1)
	   s = parse_num(substr)
	   ! Skip : and read weight
	   LINE = LINE(I+1:LEN(LINE))
	   f = parse_num(LINE)
	   WRS(IB, s) = f
	END do
     END DO

     !-----Sum Weights
     DO IB = 1, NBAS
	TWWS(IB) = 0.0
	TWRS(IB) = 0.0
     END DO

     DO IB = 1, NBAS
	DO I = 1, N_STA
	   TWWS(IB) = TWWS(IB) + WWS(IB,I)
	   TWRS(IB) = TWRS(IB) + WRS(IB,I)
	END DO
     END DO


     DO I = 1,N_STA
	READ(18,950,ERR=508) STNAME(I) 
	READ(18,950,ERR=509) REMARK
	READ(18,*,ERR=510) LAT,DUMMY,DUMMY

	! Skip weather station URL
	call skipln(18, 1)

	DO IY = 1,gnyrs
	   READ(18,*,ERR=511) IYEARS
	   READ(18,950,ERR=512) REMARK
	   DO IM= 1,12
	      READ(18,*,ERR=513) DUMMY,X(1,IM), X(2,IM)
	   END DO

	   !     ---------Check if this year is skipped.
	   if (iyears .lt. NYR1 .or. iyears .gt. NYR2) cycle

	   yr_offset = NYR1 - GNYR1

	   DO IM= 1,12
	      TMP_T(IY-yr_offset,IM) = X(1,IM)
	      TMP_R(IY-yr_offset,IM) = X(2,IM)
	   END DO
	END DO

	!-----Calculate Daylight from Latitude and Tabular values
	CALL DAYHRS(LAT,SUMDL)
	SSUMDL = 0.0
	DO IM = 1,12
	   SSUMDL = SSUMDL + SUMDL(IM)
        END DO

	!-----Calculate average values for the duration

	DO IM = 1,12
	   SUMT(IM) = 0.0
	   SUMR(IM) = 0.0
	END DO

	DO IY = 1,NYRS
	   DO IM = 1,12
	      SUMT(IM) = SUMT(IM) + TMP_T(IY,IM)
	      SUMR(IM) = SUMR(IM) + TMP_R(IY,IM)
	   END DO

	   SSUMT = 0.0
	   SSUMR = 0.0
	   DO IM = 1,12
	      SSUMT = SSUMT + SUMT(IM)
	      SSUMR = SSUMR + SUMR(IM)
           END DO
	END DO

	!-----Write

	WRITE(8,*)

	!-----Write Weather Summary Tables
	IF (ETFLAG(1).EQ.1 .AND. I.LT.10) THEN
	   WRITE(8,964) QUOTE,I,STNAME(I),QUOTE
	ELSEIF (ETFLAG(4).EQ.1 .AND. I.LT.10) THEN
	   WRITE(8,974) QUOTE,I,STNAME(I),QUOTE
	ELSEIF( ETFLAG(4).EQ.1 .AND. I.GE.10) THEN 
	   WRITE(8,975) QUOTE,I,STNAME(I),QUOTE
	ELSE
	   WRITE(8,971) QUOTE,I,STNAME(I),QUOTE
	ENDIF

	WRITE(8,999) DLLINE
	WRITE(8,998) (QUOTE, IDUM = 1,28)
	WRITE(8,999) SLLINE

	IF (S_OUT.EQ.0) THEN
	   WRITE(8,952) QUOTE,QUOTE,(SUMT(IM)/NYRS, &
		IM=1,12),SSUMT/12/NYRS
	   WRITE(8,956) QUOTE,QUOTE,(SUMR(IM)/NYRS, &
		IM=1,12),SSUMR/NYRS
	   IF( ETFLAG(1) .EQ. 1) THEN
	      WRITE(8,973) QUOTE,QUOTE,(SUMDL(IM),IM=1,12),SSUMDL/12
	   ENDIF
	   WRITE(8,999) SLLINE
	   WRITE(8,*)

	ELSE

	   !-----Mean Temperature
	   WRITE(8,957) QUOTE,QUOTE
	   DO IY = 1,NYRS
	      SSUM = 0.0
	      DO IM=1,12
		 SSUM = SSUM + TMP_T(IY,IM)
	      END DO
	      WRITE(8,962) NYR1+IY-1,(TMP_T(IY,IM),IM=1,12),SSUM/12
	   END DO
	   WRITE(8,963) QUOTE,QUOTE,(SUMT(IM)/NYRS, &
		IM=1,12),SSUMT/12/NYRS
	   WRITE(8,999) SLLINE

	   !------Rainfall
	   WRITE(8,961) QUOTE,QUOTE
	   DO IY = 1,NYRS
	      SSUM = 0.0
	      DO IM=1,12
		 SSUM = SSUM + TMP_R(IY,IM)
              END DO
	      WRITE(8,962) NYR1+IY-1,(TMP_R(IY,IM),IM=1,12),SSUM
           END DO
	   WRITE(8,963) QUOTE,QUOTE,(SUMR(IM)/NYRS, &
		IM=1,12),SSUMR/NYRS
	   WRITE(8,999) SLLINE

	   !------Daylight
	   WRITE(8,972) QUOTE,QUOTE              
	   WRITE(8,963) QUOTE,QUOTE,(SUMDL(IM),IM=1,12),SSUMDL/12 
	   WRITE(8,999) SLLINE
	   WRITE(8,*)
	ENDIF


205  END DO


     !-----Write Weight Matrix for Blaney-Criddle
     tlen = 18+5*N_STA+1
     if (tlen .gt. 230) then
	tlen = 230
     endif
     TITL7a = TITL7(:tlen)
     TITL7a(tlen:tlen+7) = ' Total '

     tlen = 20+5*N_STA+6
     if (20+5*N_STA+6 .gt. 230) then
	tlen = 230
     end if
     SLLINa = SLLINE(:tlen)
     DLLINa = DLLINE(:tlen)

     IF(ETFLAG(1) .EQ. 1) THEN
	WRITE(8,965) QUOTE,QUOTE
     ELSEIF( ETFLAG(4) .EQ. 1) THEN
	WRITE(8,976) QUOTE,QUOTE
     ENDIF
     WRITE(8,999) DLLINa
     WRITE(8,997) QUOTE,TITL7a,QUOTE
     WRITE(8,999) SLLINa
     DO IB = 1, NBAS
	WRITE(8,966) QUOTE,BAS_ID(IB),QUOTE, &
	     (WWS(IB,IS), IS=1,N_STA),TWWS(IB)
     END DO
     WRITE(8,999) SLLINa
     WRITE(8,*)

     IF(ETFLAG(1) .EQ. 1) THEN
	WRITE(8,967) QUOTE,QUOTE
     ELSEIF( ETFLAG(4) .EQ. 1) THEN
	WRITE(8,977) QUOTE,QUOTE
     ENDIF
     WRITE(8,999) DLLINa
     WRITE(8,997) QUOTE,TITL7a,QUOTE
     WRITE(8,999) SLLINa
     DO IB = 1, NBAS
	WRITE(8,966) QUOTE,BAS_ID(IB),QUOTE, &
	     (WRS(IB,IS), IS=1,N_STA),TWRS(IB)
     END DO
     WRITE(8,999) SLLINa
     WRITE(8,*)

     CLOSE(18)

  ENDIF

  IF (ETFLAG(2) .EQ. 1 .or. ETFLAG(4) .EQ. 1 .or. ETFLAG(6) .EQ. 1 .or. ETFLAG(9) .EQ. 1) THEN
     !-----READ *.wd FILE
     dfile1 = dfile
     dfile1(flen:flen+3) = '.wd'

     !-----Open input file *.wd
     OPEN(UNIT=98,FILE=dfile1,STATUS='OLD',IOSTAT=IERR)
     IF (IERR.NE.0) CALL MYEXIT(3)
     READ(98,*,ERR=514) NBAS, N_STA, YR1, YR2
     IF ((YR1.NE.GNYR1).OR.(YR2.NE.GNYR2)) CALL MYEXIT(50)
     IF (NBAS.NE.NBASIN) CALL MYEXIT(51)
     READ(98,950,ERR=515) REMARK

     !-----Read matrix of weights for non-precipitation parameters
     if (.not. ALLOCATED(WWS)) then
	ALLOCATE(WWS(NBASIN, N_STA))
	ALLOCATE(WRS(NBASIN, N_STA))
	ALLOCATE(STNAME(N_STA))

	DO IB = 1, NBAS
	   DO I = 1, N_STA
	      WWS(IB, I) = 0
	      WRS(IB, I) = 0
	   END DO
	END DO

     endif

     DO IB = 1, NBAS
	READ(98,fmt='(A360)',ERR=516) LINE
	! Skip basin name
	LINE = LINE(28:360)
	do while (LEN(LINE) .gt. 0)
	   ! Weather station is first token
	   I = index(LINE, ":")
	   if (I .eq. 0) exit
	   substr = LINE(1:I-1)
	   s = parse_num(substr)
	   ! Skip : and read weight
	   LINE = LINE(I+1:LEN(LINE))
	   f = parse_num(LINE)
	   WWS(IB, s) = f
	END do
     END DO

    !-----Read matrix of weights for precipitation parameters
     READ(98,950,ERR=517) REMARK
     DO IB = 1, NBAS
	READ(98,fmt='(A360)',ERR=518) LINE               
	! Skip basin name
	LINE = LINE(28:360)
	do while (LEN(LINE) .gt. 0)
	   ! Weather station is first token
	   I = index(LINE, ":")
	   if (I .eq. 0) exit
	   substr = LINE(1:I-1)
	   s = parse_num(substr)
	   ! Skip : and read weight
	   LINE = LINE(I+1:LEN(LINE))
	   f = parse_num(LINE)
	   WRS(IB, s) = f
	END do
     END DO

     !-----Sum Weights
     DO IB = 1, NBAS
	TWWS(IB) = 0.0
	TWRS(IB) = 0.0
     END DO

     DO IB = 1, NBAS
	DO I = 1, N_STA
	   TWWS(IB) = TWWS(IB) + WWS(IB,I)
	   TWRS(IB) = TWRS(IB) + WRS(IB,I)
        END DO
     END DO

     DO I = 1, N_STA
	READ(98,950,ERR=519) STNAME(I)
	READ(98,950,ERR=520) REMARK
	READ(98,*,ERR=521) DUMMY,DUMMY,DUMMY,DUMMY,DUMMY 

	! Skip weather station URL
	call skipln(98, 1)

	XTRADY = 0
	DO IYY = GNYR1,NYR1-1
	   IF (DaysInYear(IYY).EQ. 366) XTRADY = XTRADY + 1
	END DO
	CALL SKIPLN(98,367*(NYR1-GNYR1)+XTRADY)
	DO IY = 1,NYRS
	   m_year = iy + nyr1 - 1
	   READ(98,*,ERR=502) IYEARS
	   READ(98,950,ERR=502) REMARK
	   NDYR = DaysInYear(IYEARS)
	   DO ID = 1, NDYR
	      READ (98,*,ERR=502) DUMMY, X(1,ID),X(2,ID),X(3,ID), &
		   X(4,ID),X(5,ID),X(6,ID)
	   END DO

	   if (iyears .lt. NYR1 .or. iyears .gt. NYR2) cycle

	   DO ID = 1, NDYR
	      TMX(ID) = X(1,ID)
	      TMN(ID) = X(2,ID)
	      EDPT(ID) = X(3,ID)
	      RS(ID) = X(4,ID)
	      WD(ID) = X(5,ID)
	      RFALL(ID) = X(6,ID)
	   END DO

	   CALL AVGMON(NDYR,TMN,TMX,RFALL,EDPT,RS,WD, &
		TDATA,RDATA,DPDATA,SRDATA,WDDATA)

	   DO IM = 1,12
	      TMP_T(IY,IM) = TDATA(IM)
	      TMP_R(IY,IM) = RDATA(IM)
	      TMP_D(IY,IM) = DPDATA(IM)
	      TMP_S(IY,IM) = SRDATA(IM)
	      TMP_W(IY,IM) = WDDATA(IM)
	   END DO

	END DO
	XTRADY = 0
	DO IYY = NYR2+1,GNYR2
	   IF (DaysInYear(IYY).EQ. 366) XTRADY = XTRADY + 1
	END DO
	CALL SKIPLN(98,367*(GNYR2-NYR2)+XTRADY)

	!-----Calculate average values for the duration

	DO IM = 1,12
	   SUMT(IM) = 0.0
	   SUMR(IM) = 0.0
	   SUMD(IM) = 0.0
	   SUMS(IM) = 0.0
	   SUMW(IM) = 0.0
	END DO

	DO IY = 1,NYRS
	   DO IM = 1,12
	      SUMT(IM) = SUMT(IM) + TMP_T(IY,IM)
	      SUMR(IM) = SUMR(IM) + TMP_R(IY,IM)
	      SUMD(IM) = SUMD(IM) + TMP_D(IY,IM)
	      SUMS(IM) = SUMS(IM) + TMP_S(IY,IM)
	      SUMW(IM) = SUMW(IM) + TMP_W(IY,IM)
           END DO

	   SSUMT = 0.0
	   SSUMR = 0.0
	   SSUMD = 0.0
	   SSUMS = 0.0
	   SSUMW = 0.0
	   DO  IM = 1,12
	      SSUMT = SSUMT + SUMT(IM)
	      SSUMR = SSUMR + SUMR(IM)
	      SSUMD = SSUMD + SUMD(IM)
	      SSUMS = SSUMS + SUMS(IM)
	      SSUMW = SSUMW + SUMW(IM)
	   END DO

	END DO

	!-----Write
	WRITE(8,*)
	IF (I.LT.10) THEN
	   WRITE(8,951) QUOTE,I,STNAME(I),QUOTE
	ELSE
	   WRITE(8,970) QUOTE,I,STNAME(I),QUOTE
	ENDIF

	WRITE(8,999) DLLINE
	WRITE(8,998) (QUOTE, IDUM = 1,28)
	WRITE(8,999) SLLINE

	IF (S_OUT.EQ.0) THEN
	   WRITE(8,952) QUOTE,QUOTE,(SUMT(IM)/NYRS, &
		IM=1,12),SSUMT/12/NYRS
	   WRITE(8,953) QUOTE,QUOTE,(SUMD(IM)/NYRS, &
		IM=1,12),SSUMD/12/NYRS
	   WRITE(8,954) QUOTE,QUOTE,(SUMS(IM)/NYRS, &
		IM=1,12),SSUMS/12/NYRS
	   WRITE(8,955) QUOTE,QUOTE,(SUMW(IM)/NYRS, &
		IM=1,12),SSUMW/12/NYRS
	   WRITE(8,956) QUOTE,QUOTE,(SUMR(IM)/NYRS, &
		IM=1,12),SSUMR/NYRS
	   WRITE(8,999) SLLINE

	ELSE

	   !-----Mean Temperature
	   WRITE(8,957) QUOTE,QUOTE
	   DO  IY = 1,NYRS
	      SSUM = 0.0
	      DO IM=1,12
		 SSUM = SSUM + TMP_T(IY,IM) 
	      END DO
	      WRITE(8,962) NYR1+IY-1,(TMP_T(IY,IM),IM=1,12),SSUM/12 
	   END DO
	   WRITE(8,963) QUOTE,QUOTE,(SUMT(IM)/NYRS, &
		IM=1,12),SSUMT/12/NYRS
	   WRITE(8,999) SLLINE

	   !-----Actual Vapor Pressure
	   WRITE(8,958) QUOTE,QUOTE
	   DO IY = 1,NYRS
	      SSUM = 0.0
	      DO IM=1,12
		 SSUM = SSUM + TMP_D(IY,IM)
	      END DO
	      WRITE(8,962) NYR1+IY-1,(TMP_D(IY,IM),IM=1,12),SSUM/12
	   END DO
	   WRITE(8,963) QUOTE,QUOTE,(SUMD(IM)/NYRS, &
		IM=1,12),SSUMD/12/NYRS
	   WRITE(8,999) SLLINE

	   !------Solar Radiation
	   WRITE(8,959) QUOTE,QUOTE
	   DO IY = 1,NYRS
	      SSUM = 0.0
	      DO IM=1,12
		 SSUM = SSUM + TMP_S(IY,IM)
	      END DO
	      WRITE(8,962) NYR1+IY-1,(TMP_S(IY,IM),IM=1,12),SSUM/12
	   END DO
	   WRITE(8,963) QUOTE,QUOTE,(SUMS(IM)/NYRS, &
		IM=1,12),SSUMS/12/NYRS
	   WRITE(8,999) SLLINE

	   !------Wind Speed
	   WRITE(8,960) QUOTE,QUOTE
	   DO IY = 1,NYRS
	      SSUM = 0.0
	      DO IM=1,12
		 SSUM = SSUM + TMP_W(IY,IM)
	      END DO
	      WRITE(8,962) NYR1+IY-1,(TMP_W(IY,IM),IM=1,12),SSUM/12
	   END DO
	   WRITE(8,963) QUOTE,QUOTE,(SUMW(IM)/NYRS, &
		IM=1,12),SSUMW/12/NYRS
	   WRITE(8,999) SLLINE

	   !------Rainfall
	   WRITE(8,961) QUOTE,QUOTE
	   DO IY = 1,NYRS
	      SSUM = 0.0
	      DO IM=1,12
		 SSUM = SSUM + TMP_R(IY,IM)
	      END DO
	      WRITE(8,962) NYR1+IY-1,(TMP_R(IY,IM),IM=1,12),SSUM
	   END DO
	   WRITE(8,963) QUOTE,QUOTE,(SUMR(IM)/NYRS, &
		IM=1,12),SSUMR/NYRS
	   WRITE(8,999) SLLINE
	   WRITE(8,*)

	ENDIF

     END DO

     !-----Write Weight Matrix for Penman-Monteith 
     tlen = 18+5*N_STA+1
     if (tlen .gt. 230) then
	tlen = 230
     endif
     TITL7a = TITL7(:tlen)
     TITL7a(tlen:tlen+7) = ' Total '
     tlen = 20+5*N_STA+6
     if (tlen .gt. 230) then
	tlen = 230
     endif
     SLLINa = SLLINE(:tlen)
     DLLINa = DLLINE(:tlen)
     WRITE(8,968) QUOTE,QUOTE
     WRITE(8,999) DLLINa
     WRITE(8,997) QUOTE,TITL7a,QUOTE
     WRITE(8,999) SLLINa
     DO IB = 1, NBAS
	WRITE(8,966) QUOTE,BAS_ID(IB),QUOTE, &
	     (WWS(IB,IS), IS=1,N_STA)
     END DO
     WRITE(8,999) SLLINa
     WRITE(8,*)

     WRITE(8,969) QUOTE,QUOTE
     WRITE(8,999) DLLINa
     WRITE(8,997) QUOTE,TITL7a,QUOTE
     WRITE(8,999) SLLINa
     DO IB = 1, NBAS
	WRITE(8,966) QUOTE,BAS_ID(IB),QUOTE, &
	     (WRS(IB,IS), IS=1,N_STA)
     END DO
     WRITE(8,999) SLLINa
     WRITE(8,*)
     CLOSE(98)
  ENDIF                     ! end of (if flag2.eq.1)

  if (allocated(WWS)) then
     DEALLOCATE(WWS)
     DEALLOCATE(WRS)
     DEALLOCATE(STNAME)
  endif
  
  
899 FORMAT(1x,'Input Summary File Generated by South Platte CU Model')
900 FORMAT(A1,A120,A1)
901 FORMAT(A1,'Begin Year             :    ',A1,I4,/ &
	 A1,'End Year               :    ',A1,I4/)
902 FORMAT(A1,'Blaney-Criddle Method            :    ',A8,A1)
903 FORMAT(A1,'Penman-Monteith Method           :    ',A8,A1)
904 FORMAT(A1,'Other uses calculation           :    ',A8,A1)
9041 FORMAT(A1,'Kimberly-Penman Method           :    ',A8,A1)
9042 FORMAT(A1,'Calibrated Blaney-Criddle Method :    ',A8,A1) 
9043 FORMAT(A1,'ASCE Method                      :    ',A8,A1) 
9044 FORMAT(A1,'Hargreaves Method                :    ',A8,A1) 
9045 FORMAT(A1,'Pochop Method                    :    ',A8,A1) 
9046 FORMAT(A1,'User-Supplied ET Method          :    ',A8,A1) 
905 FORMAT(A1,'Monthly Precip Method            :    SCS',A1)
906 FORMAT(A1,'Monthly Precip Method            :    USBR',A1)
907 FORMAT(A1,'Monthly Precip Method            :    None',A1)
908 FORMAT(A1,'Daily Precip Method - All daily total rainfall below 1.&
	 &0 inch are effective.',A1/&
	 A1,'                       :    Maximum effective rainfall = 1.0 i&
	 &nch',A1)
909 FORMAT(A1,'Daily Precip Method    :    Effective rainfall = factor &
	 & x total rainfall',A1/&
	 A1,'                       :    Factor = ',F4.2,A1)              
910 FORMAT(A1,'Daily Precip Method    :    Curve Number method from NE&
	 &H SEC 4 METHOD',A1/&
	 A1,'                       :    Curve number 1 =',F3.1,A1/&
	 A1,'                       :    Curve number 2 =',F3.1,A1/&
	 A1,'                       :    Curve number 2 =',F3.1,A1)
911 FORMAT(A1,'Daily Precip Method    :    None',A1)
912 FORMAT(A1,'Water Supply Data      :    Available',A1)
913 FORMAT(A1,'Water Supply Data      :    None',A1)
914 FORMAT(A1,'Annual Carry-Over Soil Moisture Coefficient',A1)
915 FORMAT(14x, I4,'     :    ',F4.2)
916 FORMAT(A1,'Crop Information',A1/'----------------'/)
917 FORMAT(A1,A40,A1)
918 FORMAT(1x,I4,1x,A1,A20,A15,A1,F7.0,1x,A1,I2,'/',I2,A1,3x,A1,I2,&
	 '/',I2,A1,3x,F4.1,7x,F4.1,1x,2F8.1,F7.1)
919 FORMAT(3x,2A1,1x,A1,A20,A15,A1,F7.0,1x,A1,I2,'/',I2,A1,3x,A1, &
	 I2,'/',I2,A1,3x,F4.1,7x,F4.1,1x,2F8.1,F7.1)
920 FORMAT(A1,A1,4x,A1,'TOTAL',A1,30x,F7.0)
921 FORMAT(A1,'Total Project Area by Crop (acres)',A1 &
	 /'-----------------------------------')
922 FORMAT(A1,'Year',A1,4x,<nu_dim+1>A20)
923 FORMAT(1x,I4,5x,<nu_dim+1>F20.0)
924 FORMAT(A1,'Total               ',A1,4x,100F10.0)
925 FORMAT(A1,A20,A1,A1,I2,'/',I2,A1,4x,A1,I2,'/',I2,A1,4x,A1, &
	 I2,'/',I2,A1,4x,A1,I2,'/',I2,A1,F9.1,3F10.1)
926 FORMAT(A1,'Crop',A1,16x,A1,'Earliest',A1,1x,A1,'Latest',A1,2x, &
	 A1,'Earliest',A1,1x,A1,'Latest',A1,2x,A1,'Earliest',A1,1x,A1, &
	 'Latest',A1,3x,A1,'Root',A1,3x,A1,'MAD',A1,3x,A1,'AWC',A1/ &
	 22x,A1,'Planting',A1,A1,'Planting',A1,1x,A1,'Harvest',A1,2x, &
	 A1,'Harvest',A1,1x,A1,'Moisture',A1,1x,A1,'Moisture',A1/ &
	 66x,A1,'(F)',A1,6x,A1,'(F)',A1,4x,A1,'(ft)',A1,3x,A1,'(%)',A1, &
	 2x,A1,'(ft/ft)',A1,2x,A1,'(in)',A1)
927 FORMAT(A1,A20,A1,A1,'--','/','--',A1,4x,A1,'--','/','--',A1,4x,A1, &
	 '--','/','--',A1,4x,A1,'--','/','--',A1,F9.1,3F10.1)
930 FORMAT(A1,'Monthly Precip Method  :    Not Applicable',A1)
931 FORMAT(A1,'Daily Precip Method    :    Not Applicable',A1)
934 FORMAT(7x,I4,' - 'I4,'      :    ',F4.2)
935 FORMAT(1x,I4,1x,A1,A20,A15,A1,F7.0,1x,A1,'--/--',A1,3x,A1, &
	 '--/--',A1,3x,F4.1,7x,F4.1,1x,2F8.1,F7.1)
936 FORMAT(3x,2A1,1x,A1,A20,A15,A1,F7.0,1x,A1,'--/--',A1,3x,A1, &
	 '--/--',A1,3x,F4.1,7x,F4.1,1x,2F8.1,F7.1)
950 FORMAT(A160)
951 FORMAT(A1,'Daily Weather Parameters = (St',I1,')', &
	 A70,A1)
952 FORMAT(A1,'Mean Temp (F) ',A1,13F8.2)
953 FORMAT(A1,'Dew Temp (F) ',A1,13F8.2)
954 FORMAT(A1,'Solar (lang/d)',A1,13F8.2)
955 FORMAT(A1,'Wind  (mi/day) ',A1,13F8.2)
956 FORMAT(A1,'Rainfall (in) ',A1,13F8.2)
957 FORMAT(A1,'Mean Temperature (F)',A1)
958 FORMAT(A1,'Dew Temp (F)',A1)
959 FORMAT(A1,'Solar Radiation (langley/day)',A1)
960 FORMAT(A1,'Wind Speed (mi/day)',A1)
961 FORMAT(A1,'Monthly Total Rainfall (in)',A1) 
962 FORMAT(3x,I4,9x,13F8.2)
963 FORMAT(A1,2x,'Mean',A1,8x,13F8.2)
964 FORMAT(A1,'Monthly Weather Parameters = (St',I1,')',A70,A1)
965 FORMAT(A1,'Matrix of Weights for Non-Precipitation Parameters (inc&
	 &. Temperature)',A1)
966 FORMAT(A1,A16,A1,21F5.1)
967 FORMAT(A1,'Matrix of Weights for Precipitation Parameter',A1)
968 FORMAT(A1,'Matrix of Weights for Non-Precipitation Parameters (inc&
	 &. Temperature)',A1)
969 FORMAT(A1,'Matrix of Weights for Precipitation Parameters',A1)
970 FORMAT(A1,'Daily Weather Parameters = (St',I2,')', &
	 A70,A1)
971 FORMAT(A1,'Monthly Weather Parameters = (St',I2,')',A70,A1)
972 FORMAT(A1,'Percent Daylight Hours',A1)
973 FORMAT(A1,'Daylight (%)  ',A1,13F8.2)
974 FORMAT(A1,'Monthly Weather Parameters = (St',I1,')',A70, &
	 A1)
975 FORMAT(A1,'Monthly Weather Parameters = (St',I2,')',A70, &
	 A1)
976 FORMAT(A1,'Matrix of Weights for Non-Precipitation Parameters (inc&
	 &. Temperature)',A1)
977 FORMAT(A1,'Matrix of Weights for Precipitation Parameter',A1)

997 FORMAT(A1,A120,A1)
998 FORMAT (1x,A1,'Parameter',A1,7x,A1,'Jan',A1,3x,A1,'Feb',A1,3x,A1, &
	 'Mar',A1,3x,A1,'Apr',A1,3x,A1,'May',A1,3x,A1,'Jun',A1,3x,A1, &
	 'Jul',A1,3x,A1,'Aug',A1,3x,A1,'Sep',A1,3x,A1,'Oct',A1,3x,A1, &
	 'Nov',A1,3x,A1,'Dec',A1,1x,A1,'Annual',A1)

999 FORMAT(A120)

  !Close the summary file
  CLOSE(8)

  RETURN

501 CALL MYEXIT(9)
502 CALL MYEXIT(10)

503 CALL MYEXIT(38)
504 CALL MYEXIT(39)
505 CALL MYEXIT(40)
506 CALL MYEXIT(41)
507 CALL MYEXIT(42)
508 CALL MYEXIT(43)
509 CALL MYEXIT(44)
510 CALL MYEXIT(45)
511 CALL MYEXIT(46)
512 CALL MYEXIT(47)
513 CALL MYEXIT(48)
514 CALL MYEXIT(49)
515 CALL MYEXIT(52)
516 CALL MYEXIT(53)
517 CALL MYEXIT(54)
518 CALL MYEXIT(55)
519 CALL MYEXIT(56)
520 CALL MYEXIT(57)
521 CALL MYEXIT(58)

END SUBROUTINE SUMMARY
