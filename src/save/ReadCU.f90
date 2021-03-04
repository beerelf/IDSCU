SUBROUTINE ReadCU(IB, nyr, &
     !Outputs
     cu)
 
!***************************************************************************
  !
  !   Function        : ReadCU.f
  !   Author          : David Patterson
  !   Date            : Aug 2007
  !   Purpose         : Reads user-supplied ET for the given basin.
  !   Calling program : 
  !   Called programs : 
  !   Input arguments : ib = current basin 
  !   Output arguments: cu = consumptive use
  !   Assumptions     :
  !   Limitations     :
  !   Notes           :
  !
  !   History         :(Date, Author, Description)
  !
  !***************************************************************************

  USE globals

  !Parameters
  INTEGER IB, nyr
  REAL cu(:,:)

  !Locals
  INTEGER ibasin, nvalues, v, iyear, IERR, nyr_POR
  CHARACTER*120 dfile1, line

  dfile1 = dfile
  dfile1(flen:flen+4) = '.et'

  ! Read to the selected modeling area.
  OPEN (UNIT=2 ,FILE=dfile1,STATUS='OLD',IOSTAT=IERR)
  IF (IERR.NE.0) CALL MYEXIT(166)

  ! Convert the year after the start of simulation to the year
  !   after the start of the period of record.
  nyr_POR = nyr + (NYR1-GNYR1)

  DO ibasin=1,IB
     ! First line is modeling area name.
     READ(2, *) line
     ! Next line is the number of data items (this will always be one)
     READ(2, *) line
     ! Next is the period of record.
     READ(2, *) line
     ! Next is the number of values.
     READ(2, *) nvalues
     ! Next are the values for the year.  This is the average record, so we can skip it.
     READ(2, *) line

     ! Next line is number of years
     READ(2, *) line

     ! Load the data for the particular basin and year.
     DO iyear=1,GNYRS
	! Next is the number of values.
	READ(2, *) nvalues
	IF (ibasin .EQ. IB .AND. iyear .EQ. nyr_POR) THEN
	   READ(2, *) (cu(1, v), v=1,nvalues)

	   ! Remove illegal values.
	   DO v=1,nvalues
	      if (cu(1,v) .lt. 0) cu(1,v) = 0
	   end do
	ELSE
	   ! Not the right basin and year
	   READ(2, *) line
	END IF
     END DO
  END DO

  CLOSE(2)

  RETURN

END SUBROUTINE ReadCU
