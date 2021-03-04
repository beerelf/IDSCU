SUBROUTINE FROST(ib, daily)

  !***************************************************************************
  !
  !   Function        : frost.f
  !   Author          : HB Manguerra
  !   Date            : June 1995 
  !   Purpose         : This determines the start and end of the growing season
  !                     based on the user specified dates, published 28 and
  !                     32 degree F frost dates and calculated frost dates based
  !                     on monthly mean temperature. 
  !   Calling program : mainxc.f, proto.f 
  !   Called programs : spring.f, fall.f, myexit.f 
  !   Input arguments : ib   = current sub-basin
  !   Output arguments: none 
  !   Assumptions     :
  !   Limitations     :
  !   Notes           : tflg1 = flag for frost method in spring
  !                     tflg2 = flag for frost method in fall
  !
  !                     if flag is:
  !                            = 0, frost date is based on monthly mean temp
  !                            = 1, frost date is based on published 28 
  !                                 degree F frost date
  !                            = 2, frost date is based on published 32 
  !                                 degree F frost date
  !
  !***************************************************************************

  USE Globals

  !Parameters
  INTEGER ib
  LOGICAL daily

  INTERFACE
     SUBROUTINE SPRING(ib, yr, tfrost, daily, &
				!Outputs
	  Jdays)
       INTEGER ib, yr
       REAL tfrost
       LOGICAL daily
       INTEGER Jdays
     END SUBROUTINE SPRING

     SUBROUTINE FALL(ib, yr, tfrost, daily, &
				!Outputs
	  Jdays)
       INTEGER ib, yr
       REAL tfrost
       LOGICAL daily
       INTEGER Jdays
     END SUBROUTINE FALL

     INTEGER FUNCTION JULIAN (m_mon, m_day, yr)
       !Parameters
       INTEGER m_mon, m_day, yr
     END FUNCTION JULIAN
  END INTERFACE

  !Locals
  INTEGER i,j,jfrost,key
  ! DO not use n_wheat_parcel because it is easier to just recalculate it here.
  INTEGER i_wheat_parcel, nfalldays

  !When calculating the harvest date of winter wheat, we need
  !  to subtract the amount of growth in the winter from the number
  !  of days in the spring growing season.
  INTEGER fall_field_idx(nparce_ori(ib)) ! Max number of winter wheat
  !   fields is nparce(ib).
  !When we start processing spring period winter wheat, set this to true
  !  so we can reset the spring period counter.  This will always be true 
  !  because the only spring period winter wheat fields occur at the end 
  !  of the field list.
  LOGICAL doneWithWinterPeriod

  !Use mean monthly temperature for computing frost dates

  i_wheat_parcel = 0
  doneWithWinterPeriod = .false.

  !Use mean monthly temperature for computing frost dates

  do i=1, nparce(ib) 
     key = crop_key(ib,i)

     if (sub_crop_type(key) .eq. 0) then ! winter wheat begin
	! Store the index to this field so spring wheat can access it.
	i_wheat_parcel = i_wheat_parcel + 1
	fall_field_idx(i_wheat_parcel) = i

     elseif (sub_crop_type(key) .eq. 1) then ! winter wheat spring period
	if (.not. doneWithWinterPeriod) then
	   doneWithWinterPeriod = .true.
	   i_wheat_parcel = 0
	endif
	i_wheat_parcel = i_wheat_parcel + 1
     endif

     do j=1, nyrs 
	if (sub_crop_type(key) .eq. 0) then ! winter wheat begin
	   ! date is user specified
	   jbeg(i,j) = JULIAN(gdate1(key),gdate2(key),j) 
	   if (tflg2(key).eq.0) then
	      call fall(ib,j,tmois2(key),daily,jfrost)
	   elseif (tflg2(key).eq.1) then
	      jfrost = t28(ib,j,2)
	   elseif (tflg2(key).eq.2) then
	      jfrost = t32(ib,j,2)
	   else 
	      call MYEXIT(32)   
	   endif
	   jend(i,j) = MIN(jfrost,JULIAN(gdate3(key),gdate4(key),j))
	   !check for length of growing season
	   if ((jend(i,j)-jbeg(i,j)+1).gt.gdates(key)) then
	      jend(i,j) = jbeg(i,j) + gdates(key) - 1
	   end if

	elseif (sub_crop_type(key) .eq. 1) then ! spring wheat end 
	   ! date is user specified
	   jend(i,j) = JULIAN(gdate3(key),gdate4(key),j) 

	   if (tflg1(key).eq.0) then
	      call spring(ib,j,tmois1(key),daily,jfrost)
	      if (.not. IGNORE_FROST_DATES .and. jfrost .lt. t28(ib,j,1)) then
		 jfrost = t28(ib,j, 1)
	      end if
	   elseif (tflg1(key).eq.1) then
	      jfrost = t28(ib,j,1)
	   elseif (tflg1(key).eq.2) then
	      jfrost = t32(ib,j,1)
	   else
	      call MYEXIT(33)
	   endif
	   jbeg(i,j) = MAX(jfrost,JULIAN(gdate1(key),gdate2(key),j))

	   !-----check for length of growing season
	   if (j .ne. 1) then
	      ! Adding fall growth to total length of season
	      nfalldays = (jend(fall_field_idx(i_wheat_parcel),j-1) &
		   - jbeg(fall_field_idx(i_wheat_parcel),j-1))
	      if (nfalldays + &
		   (jend(i,j)-jbeg(i,j)).gt.gdates(key)) then
		 jend(i,j) = jbeg(i,j) + (gdates(key) - nfalldays)-1
	      endif
	   else
	      ! No winter wheat in first year
	      jend(i, j) = -999
	   endif
	else
	   if (tflg1(key).eq.0) then
	      call spring(ib,j,tmois1(key),daily,jfrost)
	      if (.not. IGNORE_FROST_DATES .and. jfrost .lt. t28(ib,j,1)) then
		 jfrost = t28(ib,j, 1)
	      end if
	   elseif (tflg1(key).eq.1) then
	      jfrost = t28(ib,j,1)
	   elseif (tflg1(key).eq.2) then
	      jfrost = t32(ib,j,1)
	   else
	      call MYEXIT(34)
	   endif

	   jbeg(i,j) = MAX(jfrost,JULIAN(gdate1(key),gdate2(key),j))
	   !if (crop_type(key) .eq. 0 .and. USE_GDD(key) .and. daily) then
	   !   ! Alfalfa starts growing on Jan 1 when using growing degree days.
	   !   jbeg(i, j) = 1
	   !end if

	   if (tflg2(key).eq.0) then
	      call fall(ib,j,tmois2(key),daily,jfrost)
	   elseif (tflg2(key).eq.1) then
	      jfrost = t28(ib,j,2)
	   elseif (tflg2(key).eq.2) then
	      jfrost = t32(ib,j,2)
	   else 
	      call MYEXIT(35)   
	   endif
	   jend(i,j) = MIN(jfrost,JULIAN(gdate3(key),gdate4(key),j))
	   !check for length of growing season
	   if ((jend(i,j)-jbeg(i,j)+1).gt.gdates(key)) then
	      jend(i,j) = jbeg(i,j) + gdates(key) - 1
	   endif
	endif

	! Error check for no growing season.
	if (jend(i,j) .le. jbeg(i, j) .and. jend(i,j) .gt. -999) then
	   print *, "Could not compute a growing season for parcel ", i, "in year ", NYR1+j-1, " for modeling area ", BAS_ID(ib), &
		". Please check weather data for this year for the weather stations that this modeling area uses."
	   !call MYEXIT(48)
	endif
     end do
  end do

  RETURN
END SUBROUTINE FROST
