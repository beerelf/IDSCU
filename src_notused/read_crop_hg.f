      SUBROUTINE READ_CROP_HG()

C***************************************************************************
C
C   Function        : read_crop_hg.f
C   Author          : L.A. Garcia
C   Date            : July 2003
C   Purpose         : This reads the Hargreaves crop coefficient file
C                     and decides which type of crop coefficient (basal Kc
C                     grass based, mean Kc grass based) to use based on availabi- 
C                     lity and predefined heirarchy. 
C   Calling program : proto.f 
C   Called programs : none
C   Input arguments : none   
C   Output arguments: none 
C   Assumptions     :
C   Limitations     :
C   Notes           : The model does not use all available type of crop 
C                     coefficients at the same time. The user choses a 
C                     particular type to calculate crop CU.
C
C   History         : (Date, Author, Description)
C
C
C***************************************************************************

      USE Globals

C-----Local Variable Declaration
      CHARACTER*80 remark
      CHARACTER*120 dfile1
      INTEGER NC, ID, I, LEN, J
      INTEGER IERR, ITYP, CRPTYPE
      REAL KCDUM(40,48)

C-----Read crop coefficient file *.kref
      dfile1 = dfile
      dfile1(flen:flen+4) = '.kref' ! Assume Kimberly-Penman coefficients

      OPEN(UNIT=1,FILE=dfile1,STATUS='OLD',IOSTAT=IERR)
      IF (IERR.NE.0) CALL MYEXIT(113)
      READ(1,900,ERR=101) REMARK
      READ(1,*,ERR=101) NC
      DO 10 I=1,NC
         LEN = 22
         READ(1,*,ERR=101) ID, ITYP
C
C         If the crop is perennial (TYP=1) then the length of the season is
C         divided into 11 segments with days as the units.
C
         IF( CRPTYPE(ID) .EQ. 1) LEN = 11
         IF (ID.EQ.1) LEN = 33      ! alfalfa
C         IF (ID.EQ.2) LEN = 11      ! grass pasture

         ETMETH(ID) = ITYP 
         IF (ITYP.EQ.1) THEN     ! mean alfalfa
         DO 22 J=1,LEN
            READ(1,*,ERR=101) KCDAY(ID,J),KCB(ID,J),KCDUM(ID,J)
 22      CONTINUE

	 ELSE                        ! mean grass
         DO 23 J=1,LEN
	    READ(1,*,ERR=101) KCDAY(ID,J),KCDUM(ID,J),KCB(ID,J)
 23      CONTINUE
         ENDIF

 10   CONTINUE


 900  FORMAT(A80)

      RETURN

 101  CALL MYEXIT(11)

      END
