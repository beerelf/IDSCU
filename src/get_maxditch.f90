      INTEGER FUNCTION GET_MAXDITCH()

!***************************************************************************
!
!   Function        : get_maxditch.f
!   Author          : Dave Patterson
!   Date            : Jan 2003
!   Purpose         : Read the .sup and return the maximum number of ditches
!                     that appear in any one farm.
!   Calling program : -
!   Called programs : none
!   Input arguments : none
!   Output arguments: none
!   Assumptions     :
!   Limitations     : 
!   Notes           :
!***************************************************************************

      USE Globals

!     Locals
      INTEGER IB,D,NBAS,YR1,YR2
      INTEGER IERR, NDITCH, FHEADG, skip_mult

      CHARACTER*30 BNAME
      CHARACTER*120 dfile1

      skip_mult = 1 ! Number of lines to skip per year (1 month's worth)
      if (hasDailyMethod) then
          skip_mult = 12        ! Skip 12 lines per year
      endif

!-----Check if this is the first basin/farm/subarea.  If so then open the supply file.
      dfile1 = dfile
      dfile1(flen:flen+4) = '.sup'
      OPEN(UNIT=19,FILE=dfile1,STATUS='OLD',IOSTAT=IERR)
      IF (IERR.NE.0) CALL MYEXIT(101)
      READ(19,*,ERR=107) NBAS, YR1, YR2
      CALL SKIPLN(19,1)     ! Skip vary annual flag.

      GET_MAXDITCH = 0

      DO 20 ib=1,NBAS
         READ(19,900,ERR=105) BNAME, NDITCH, FHEADG

!        Add the farm headgate.
         NDITCH = NDITCH + 1

         IF (NDITCH > GET_MAXDITCH) GET_MAXDITCH = NDITCH

         DO 10 D=1,NDITCH
            CALL SKIPLN(19,1) ! Ditch name
            CALL SKIPLN(19,1) ! Ditch shares
	    CALL SKIPLN(19,1) ! irrigated acres of ditch
	    CALL SKIPLN(19,1) ! total irrigated acres of ditch

            CALL SKIPLN(19,(GNYR2-GNYR1+1)*skip_mult) ! Ditch supply
 10      CONTINUE
 20   CONTINUE

      CLOSE(19)

 900  FORMAT(A27,I5,I5)     
      RETURN

 105  CALL MYEXIT(105)
 107  CALL MYEXIT(107)

      END

