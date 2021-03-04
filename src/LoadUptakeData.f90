SUBROUTINE LoadUptakeData(IB, LAI, Dwt, WCwp, soilClass)

  !***************************************************************************
  !
  !   Function        : LoadUptakeData.f
  !   Author          : David Patterson
  !   Date            : Dec 2009
  !   Purpose         : Reads user-supplied CU for the given basin.
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

  INTEGER IB, soilClass(:)
  REAL LAI(:, :, :), Dwt(:, :, :)
  REAL WCwp(:)
  REAL data_nyrs

  !Locals
  INTEGER i, iyear_sim, ibasin, nvalues, v, iyear, IERR, ip, iocheck, iparam
  CHARACTER*120 dfile1, line, basin_name

  dfile1 = dfile
  dfile1(flen:flen+4) = '.wup'

  ! Read to the selected modeling area.
  OPEN (UNIT=2 ,FILE=dfile1,STATUS='OLD',IOSTAT=IERR)
  IF (IERR .EQ. 0) THEN
     DO ibasin=1,IB 
        ! First line is modeling area name.
        READ(2, *) basin_name

        DO IP=1,NPARCE(ibasin)
           ! Skip spring portion of winter wheat because the parcel is duplicated.
           IF (SUB_CROP_TYPE(CROP_KEY(ibasin, IP)) .EQ. 1) THEN
              CYCLE
           END IF

           ! Read field number
           READ(2, '(A)') line
           ! Check if this field has water upflux data by checking for an extra value in line
           READ(line, *, IOSTAT=iocheck) i, WCwp(IP), soilClass(IP)
           ! Convert meters to mm
           WCwp(IP) = WCwp(IP) * 1000

           ! The above line will be fully read if the user has provided uptake data.
           IF (iocheck .EQ. 0) THEN
              ! Next line is the number of data items (this will always be three)
              READ(2, *) line
              DO IPARAM=1,2
                 ! Next is the period of record.
                 READ(2, *) line
                 ! Next is the number of values.
                 READ(2, *) nvalues
                 ! Next are the values for the year.  This is the average record, so we can skip it.
                 READ(2, *) line

                 ! Next line is number of years
                 READ(2, *) data_nyrs
                 if (data_nyrs .eq. 365 .or. data_nyrs .lt. 0) then
                    print *, "Error in LoadUptake."
                 end if

                 ! Load the data for the particular basin and year.
                 DO iyear=1,data_nyrs
                    ! Next is the number of values.
                    READ(2, *) nvalues
                    if (nvalues .lt. 0) then
                       print *, "Error in LoadUptake."
                    end if
                    IF (ibasin .EQ. IB) THEN
                       iyear_sim = iyear - (NYR1 - GNYR1)
                       IF (iyear_sim .gt. 0 .and. iyear_sim .le. NYRS .and. nvalues .gt. 0) THEN
                          IF (IPARAM .EQ. 1) THEN
                             READ(2, *) (LAI(IP, iyear_sim, v), v=1,nvalues)
                          ELSEIF (IPARAM .EQ. 2) THEN
                             READ(2, *) (Dwt(IP, iyear_sim, v), v=1,nvalues)
                          END IF
                       ELSE
                          ! Don't process this line, it is out of the simulation year range.
                          READ(2, *) v
                       END IF
                    ELSE
                       ! Don't process this line, it is for the wrong basin.
                       READ(2, *) v
                    END IF

                    ! Skip blank line after data.
                    READ(2, '(A)') line
                 END DO
              END DO
           ELSE
              ! Set soilclass to -1 to indicate no data
              soilClass(IP) = -1
           END IF
        END DO
     END DO
  END IF
  CLOSE(2)
  RETURN

END SUBROUTINE LoadUptakeData
