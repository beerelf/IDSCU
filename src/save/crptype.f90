INTEGER FUNCTION CRPTYPE (ndx)

!***************************************************************************
!
!   Function        : crptype.f
!   Author          : HB Manguerra
!   Date            : December 1994
!   Purpose         : This determines whether the crop is perrenial or
!                     annual.
!   Calling program : calpcrop.f 
!   Called programs : none 
!   Input arguments : ndx     = crop index
!   Output arguments: crptype = 1 (perrenial) or 2 (annual)
!   Assumptions     :
!   Limitations     :
!   Notes           :
!***************************************************************************


  USE globals

  integer ndx

  if (crop_type(ndx) .eq. 0 .OR. crop_type(ndx) .eq. 1  .OR. crop_type(ndx) .eq. 3) then ! Phreatophytes are treated as perennial
      CRPTYPE = 1 ! Perennial
   else
      CRPTYPE = 2
   END IF
   RETURN

 END FUNCTION CRPTYPE
