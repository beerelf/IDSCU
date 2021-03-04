SUBROUTINE SKIPN(NF)

  !***************************************************************************
  !
  !   Function        : skipn.f
  !   Author          : Ray Bennett with minor modifications by Luis Garcia
  !   Date            : May 1995
  !   Purpose         : skips any number of comment cards identified
  !                     as a '*'  from a data file
  !   Calling program :
  !   Called programs : none
  !   Input arguments : none
  !   Output arguments: none
  !   Assumptions     :
  !   Limitations     :
  !   Notes           :
  !
  !***************************************************************************

  INTEGER nf
  CHARACTER REC1*1

100 READ(NF,500,END=999) REC1

  IF(REC1.EQ.'c' .OR. REC1.EQ.'C' .OR. REC1.EQ.'#') THEN
     GOTO 100
  ELSE
     BACKSPACE(NF)
  ENDIF

500 FORMAT(A1)
999 RETURN
END SUBROUTINE SKIPN

