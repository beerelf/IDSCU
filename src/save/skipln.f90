      SUBROUTINE SKIPLN(NF,NLINE)
!***************************************************************************
!
!   Function        : myskip.f
!   Author          : Henry B. Manguerra 
!   Date            : September 1995
!   Purpose         : skips data line entries when reading input data file.
!   Calling program :
!   Called programs : none
!   Input arguments : none
!   Output arguments: none
!   Assumptions     :
!   Limitations     :
!   Notes           :
!
!***************************************************************************


      INTEGER I, NF, NLINE
      CHARACTER*120 ALINE

      DO 10 I = 1, NLINE
         READ(NF,100) ALINE
 10   CONTINUE
 
 100  FORMAT(A120)

      RETURN

      END
