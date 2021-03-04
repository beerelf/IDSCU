INTEGER FUNCTION GetSpringWheatKey()

!***************************************************************************
!
!   Function        : GetSpringWheatKey.f
!   Author          : Dave Patterson
!   Date            : December 2004
!   Purpose         : winter wheat crops must have a corresponding spring wheat crop type.  This funtion will search the list of crops and return the crop ID of the spring wheat.
!   Calling program : 
!   Called programs : none 
!   Input arguments : 
!   Output arguments: crop index of the spring wheat crop type.
!   Assumptions     :
!   Limitations     :
!   Notes           : 
!
  use globals

  integer k

  GetSpringWheatKey = 0

  do k = 1, n_crops
     if (sub_crop_type(k) .eq. 1) then
	GetSpringWheatKey = k
	exit
     end if
  end do

  if (GetSpringWheatKey .eq. 0) then
     print *, "Error in get_spring_wheat_key: no spring period for winter wheat found."
     call myexit(100)
  end if
  return
end function GetSpringWheatKey

