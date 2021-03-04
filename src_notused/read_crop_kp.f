      SUBROUTINE READ_CROP_KP()

C***************************************************************************
C
C   Function        : read_crop_kp.f
C   Author          : LA Garcia
C   Date            : October 1998
C   Purpose         : This reads the Kimberly-Penman crop coefficient file
C                     and decides which type of crop coefficient (mean Alfalfa). 
C   Calling program : readin.f 
C   Called programs : none
C   Input arguments : none   
C   Output arguments: none 
C   Assumptions     :
C   Limitations     :
C   Notes           :  
C
C   History         : (Date, Author, Description)
C
C                   : Modified by Dave Patterson to integrate subroutine
C                   :   into unified version.
C
C***************************************************************************

      USE Globals

C-----Local Variable Declaration
      CHARACTER*80 remark
      CHARACTER*120 dfile1
      INTEGER NC, ID, I, LEN, J, CRPTYPE
      INTEGER IERR

C-----Read crop coefficient file *.kref
      dfile1 = dfile
      dfile1(flen:flen+4) = '.kref'

      OPEN(UNIT=1,FILE=dfile1,STATUS='OLD',IOSTAT=IERR)
      IF (IERR.NE.0) CALL MYEXIT(113)
      READ(1,900,ERR=101) REMARK
      READ(1,*,ERR=101) NC
      DO 10 I=1,NC
         LEN = 22
         READ(1,*,ERR=101) ID
         IF(CRPTYPE(ID) .EQ. 1 ) LEN = 11          ! If perrenial use 11 periods 
C                                                   w/ days as the units
         IF (ID.EQ.1) LEN = 33                     ! alfalfa set period to 33

         DO 20 J=1,LEN
            READ(1,*,ERR=101) KPDAY(ID,J),KPC(ID,J)
 20      CONTINUE

 10   CONTINUE

 900  FORMAT(A80)
      CLOSE(1)

      RETURN

 101  CALL MYEXIT(114)

      END
