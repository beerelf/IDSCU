      SUBROUTINE MAINXC(FLAG)

C***************************************************************************
C
C   Function        : mainxc.f
C   Author          : HB Manguerra
C   Date            : May 1995 
C   Purpose         : This is the main calling subroutine for calculating
C                     crop consumptive use by Blaney-Criddle estimation 
C                     method.
C   Calling program : run_cu.f 
C   Called programs : dayhrs.f, readin.f, calpcrop.f, monthly.f, supply.f
C                     frost.f, wbuild.f 
C   Input arguments : none 
C   Output arguments: none
C   Assumptions     :
C   Limitations     :
C   Version 1.1     : This version contains the OBCFLAG that checks if the
C                   : detailed output for blaney-criddle was selected if
C                   : it was not it puts a message in the *.obc file.
C
C***************************************************************************

      USE Globals

C     Parameters
      INTEGER et_method

C-----Local variable declaration
      INTEGER IY, IM, OFLAG, NUM
      INTEGER nbegyr, nendyr
      INTEGER IB
      CHARACTER*120 ofile1
     
C-----specify input file extension
C      flen = index(dfile,' ')

      ofile1 = dfile
      IF( et_method .EQ. 4) THEN
         ofile1(flen:flen+4) = '.okp'
      ELSEIF( et_method .EQ. 1) THEN
         ofile1(flen:flen+4) = '.obc'
      ELSEIF( et_method .EQ. 5) THEN
         ofile1(flen:flen+5) = '.ocbc'
      ELSEIF( et_method .EQ. 7) THEN
         ofile1(flen:flen+4) = '.ohg'
      ENDIF

C-----set obcflag to zero
      oflag = 0

C-----open output file
      OPEN (UNIT=3,FILE=ofile1,STATUS='UNKNOWN')

C-----temporary files for water budget
      OPEN (UNIT=11,FILE='tmp1',STATUS='UNKNOWN')
      OPEN (UNIT=12,FILE='tmp2',STATUS='UNKNOWN')

C-----Write Headings
      IF( et_method .EQ. 4) THEN
         WRITE(3,910) QUOTE,QUOTE,QUOTE,QUOTE
      ENDIF
      IF( et_method .EQ. 1) THEN
         WRITE(3,900) QUOTE,QUOTE,QUOTE,QUOTE
      ENDIF
      IF( et_method .EQ. 5) THEN
         WRITE(3,930) QUOTE,QUOTE,QUOTE,QUOTE
      ENDIF
      IF( et_method .EQ. 7) THEN
         WRITE(3,920) QUOTE,QUOTE,QUOTE,QUOTE
      ENDIF

C-----reinitialize month(2)
      MONTH(2) = 28

C-----assigns global variables to variables specific to Blaney-
C-----Criddle.  Also reads the *.kbc file
      CALL READIN(nbegyr, nendyr, flag)

      CALL MONTHLY(flag)

C-----Start Sub-Basin Calculations
      DO 10 IB = 1, NBASIN
         IF (TYPOUT(IB).EQ.3) THEN
            WRITE(3,901) QUOTE,BAS_ID(IB),QUOTE
            OFLAG = 1
         ENDIF

C-----Read Water Supply
         IF (ISUPLY.EQ.1 .AND. SPFLAG.EQ.0) CALL SUPPLY(IB)
         IF (ISUPLY.EQ.1 .AND. SPFLAG.EQ.1) CALL SPSUPPLY(IB)
         
C-----calculate daylight hours from basin latitude and year
         CALL DAYHRS(BLAT(IB),PCLITE)

C-----determine growing season from frost dates
         CALL FROST(IB)

C-----calculate carry-over soil moisture
CLAG         CALL WBUILD(2,IB)

C-----main blaney-criddle or kimberly-penman calculations
         CALL CALPCROP(IB,et_method)

 10   CONTINUE

      IF( OFLAG .EQ. 0 .AND. et_method .EQ. 1) THEN	   
         WRITE(3, 921)
         WRITE(3, 922)
         WRITE(3, 923)
         WRITE(3, 924)
      ELSEIF( OFLAG .EQ. 0 .AND. et_method .EQ. 4) THEN
         WRITE(3, 925)
         WRITE(3, 926)
         WRITE(3, 927)
         WRITE(3, 928)
      ELSEIF( OFLAG .EQ. 0 .AND. et_method .EQ. 5) THEN
         WRITE(3, 935)
         WRITE(3, 936)
         WRITE(3, 937)
      ELSEIF( OFLAG .EQ. 0 .AND. et_method .EQ. 7) THEN
         WRITE(3, 945)
         WRITE(3, 946)
         WRITE(3, 947)
         WRITE(3, 948)
      ENDIF 

      CLOSE(3)
      CLOSE(11)
      CLOSE(12)
      
 900  FORMAT(15x,A1,'Detailed Results of the Consumptive Use Calculation
     :',A1/A1,30x,'Blaney-Criddle Method',A1/)
 901  FORMAT(A1,A40,A1/)
 903  FORMAT(A1,'Blaney-Criddle Weather Parameters for SubArea = ',
     :A40,A1)
 904  FORMAT(A1,'Mean Monthly Temperature (Farenheit)',A1)
 905  FORMAT(A1,'Total Monthly Rainfall (Inches)',A1)

 910  FORMAT(15x,A1,'Detailed Results of the Consumptive Use Calculation
     :',A1/A1,30x,'Kimberly-Penman Method',A1/)

 920  FORMAT(15x,A1,'Detailed Results of the Consumptive Use Calculation
     :',A1/A1,32x,'Hargreaves Method',A1/)

 930  FORMAT(15x,A1,'Detailed Results of the Consumptive Use Calculation
     :',A1/A1,24x,'Calibrated Blaney-Criddle Method',A1/)

 914  FORMAT (1x,A1,'Subarea',A1,9x,A1,'Jan',A1,3x,A1,'Feb',A1,3x,A1,
     :'Mar',A1,3x,A1,'Apr',A1,3x,A1,'May',A1,3x,A1,'Jun',A1,3x,A1,
     :'Jul',A1,3x,A1,'Aug',A1,3x,A1,'Sep',A1,3x,A1,'Oct',A1,3x,A1,
     :'Nov',A1,3x,A1,'Dec',A1,2x,A1,'Mean',A1)
 916  FORMAT (1x,A1,'Subarea',A1,9x,A1,'Jan',A1,3x,A1,'Feb',A1,3x,A1,
     :'Mar',A1,3x,A1,'Apr',A1,3x,A1,'May',A1,3x,A1,'Jun',A1,3x,A1,
     :'Jul',A1,3x,A1,'Aug',A1,3x,A1,'Sep',A1,3x,A1,'Oct',A1,3x,A1,
     :'Nov',A1,3x,A1,'Dec',A1,2x,A1,'Total',A1)
 915  FORMAT(A120)
 920  FORMAT(A1,A14,A1,13F8.2)
 921  FORMAT(1X,'=====================================================')
 922  FORMAT(1X,'THIS RUN DID NOT CONTAIN A DETAILED B-C OUTPUT OPTION')
 923  FORMAT(1X,'       FOR ANY OF THE SUB-AREAS BEING MODELED')
 924  FORMAT(1X,'=====================================================')
 
 925  FORMAT(1X,'=====================================================')
 926  FORMAT(1X,'THIS RUN DID NOT CONTAIN A DETAILED K-P OUTPUT OPTION')
 927  FORMAT(1X,'       FOR ANY OF THE SUB-AREAS BEING MODELED')
 928  FORMAT(1X,'=====================================================')

 935  FORMAT(1X,'=====================================================')
 936  FORMAT(1X,'  THIS RUN DID NOT CONTAIN A DETAILED CALIBRATED BC')
 937  FORMAT(1X,'OUTPUT OPTION FOR ANY OF THE SUB-AREAS BEING MODELED')
 938  FORMAT(1X,'=====================================================')

 945  FORMAT(1X,'=====================================================')
 946  FORMAT(1X,'   THIS RUN DID NOT CONTAIN A DETAILED HARGREAVES')
 947  FORMAT(1X,'OUTPUT OPTION FOR ANY OF THE SUB-AREAS BEING MODELED')
 948  FORMAT(1X,'=====================================================')

      RETURN 
      END

