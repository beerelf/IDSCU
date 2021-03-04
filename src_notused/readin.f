      SUBROUTINE READIN(nbegyr, nendyr, flag)

C***************************************************************************
C
C   Function        : readin.f
C   Author          : HB Manguerra
C   Date            : December 1994
C   Purpose         : This assigns the global variables to local variables
C                     specific to Blaney-Criddle method.  It also reads
C                     the Blaney-Criddle  crop coefficient data set from
C                     *.kbc input file.
C   Calling program : mainxc.f 
C   Called programs : none
C   Input argument  : none
C   Output arguments: nbegyr = beginning year
C                     nendyr = ending year
C   Assumptions     :
C   Limitations     : 
C   Notes           :
C
C***************************************************************************

C     Parameters
      INTEGER nendyr, nbegyr, flag

      INTERFACE
         SUBROUTINE READ_CROP_BC(dfile,
C     Output
     :        nckca, nckcp, ckca, ckcp)
C     Parameters
         CHARACTER*120 dfile
C     Output
         INTEGER, POINTER :: nckca(:,:), nckcp(:,:)
         REAL, POINTER :: ckca(:,:), ckcp(:,:)
         END SUBROUTINE

         SUBROUTINE READ_CROP_KP(dfile, KPDAY, KPC)
         CHARACTER*120 dfile
C     Output
         REAL KPDAY(40,33), KPC(40,33)
         END SUBROUTINE
      END INTERFACE


C-----Local variable declaration


      IF( FLAG .EQ. 4) THEN
         CALL READ_CROP_KP(dfile, KPDAY, KPC)
      ELSEIF( FLAG .EQ. 1 .OR. FLAG .EQ. 5) THEN
         CALL READ_CROP_BC()
      ELSEIF (FLAG .EQ. 2) THEN
         CALL READ_CROP_PM
      ENDIF


      RETURN

      END
