LOGICAL FUNCTION CheckCropCoeffs (et_method, ckey)

  !***************************************************************************
  !
  !   Function        : CheckCropCoeffs
  !   Author          : LA Garcia & D Patterson
  !   Date            : Sep 2007
  !   Purpose         : Determines if the selected crop has crop coefficients.
  !   Calling programs: 
  !   Called programs : 
  !   Input arguments : et_method - type of ET method (1 - BC, 2 - PM, 4 - KP, 5 BC Calibrated)
  !                     ckey - crop index
  !   Output arguments: 
  !   Assumptions     :
  !   Limitations     :
  !   Notes           :
  !
  !***************************************************************************

  USE Globals

  INTERFACE
     INTEGER FUNCTION CRPTYPE (ndx)
       INTEGER ndx
     END FUNCTION CRPTYPE
  END INTERFACE

  INTEGER et_method, ckey
  INTEGER i

  CheckCropCoeffs = .false.

  if (et_method .eq. 1 .or. et_method .eq. 5 .or. et_method .eq. 8) then
     ! BC coefficients
     IF (crptype(ckey).eq.1) then
	do i=1,pkclen
	   if (ckcp(ckey,i) .gt. 0) then
	      CheckCropCoeffs = .true.
	      exit
	   endif
	end do
     elseif (crptype(ckey).eq.2) then
	do i=1,akclen
	   if (ckca(ckey,i) .gt. 0) then
	      CheckCropCoeffs = .true.
	      exit
	   endif
	end do
     endif
  else
     ! Assume reference crop
     do i=1,33
	if (KCB(ckey,i) .gt. 0) then
	   CheckCropCoeffs = .true.
	   exit
	endif
     end do
  endif
  RETURN 
end FUNCTION CheckCropCoeffs

