INTEGER FUNCTION GET_MAXWELL()

!***************************************************************************
!
!   Function        : get_maxwell.f
!   Author          : Dave Patterson
!   Date            : Jan 2003
!   Purpose         : Read the .wel and return the maximum number of wells
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

  INTEGER I, IB
  INTEGER WNBAS, NW, WYR1, WYR2
  INTEGER IERR
      
  CHARACTER*60 BASNAME
  CHARACTER*120 dfile1
  LOGICAL WFILEXST

  dfile1 = dfile
  dfile1(flen:flen+4) = '.wel'
  WFILEXST = .FALSE.
  INQUIRE(FILE=dfile1, EXIST=WFILEXST)
  IF(WFILEXST) THEN 
     OPEN(UNIT=21,FILE=dfile1,STATUS='OLD',IOSTAT=IERR)
     IF (IERR.NE.0) CALL MYEXIT(102)
  ENDIF

  IF (WFILEXST) THEN
     READ(21,*,ERR=108) WNBAS, WYR1, WYR2
     read(21,*) IERR !dummy

     GET_MAXWELL = 0

     DO IB= 1, WNBAS
	READ(21,800,ERR=111) BASNAME
	READ(21, *) NW, IERR
800	FORMAT(A60)

	IF (NW .GT. GET_MAXWELL) GET_MAXWELL = NW
	DO I = 1, NW
	   CALL SKIPLN(21, 3) ! skip well name, sdf line, well mode

	   CALL SKIPLN(21, 1) !  percent irrigated by year

	   ! Skip application efficiency
	   CALL SKIPLN(21,(NYR1-GNYR1))
	   CALL SKIPLN(21, NYRS)
	   CALL SKIPLN(21,(GNYR2-NYR2))

	   ! Skip pumping data
	   CALL SKIPLN(21,(NYR1-GNYR1))
	   CALL SKIPLN(21, NYRS)
	   CALL SKIPLN(21,(GNYR2-NYR2))
	end do
     end do

     CLOSE(21)
  ENDIF
      
  RETURN

108 CALL MYEXIT(108)
111 CALL MYEXIT(111)

END FUNCTION GET_MAXWELL
