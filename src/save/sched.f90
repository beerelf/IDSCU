SUBROUTINE SCHED(IB,IY,L,er2_s,cuirr, &
				!Outputs
     stor,erain,dep,stg,shrt)

  !***************************************************************************
  !
  !   Function        : sched.f
  !   Author          : HB Manguerra
  !   Date            : December 1994
  !   Purpose         : This performs the basin-wide water budget when for
  !                     Blaney-Criddle method when water supply information
  !                     is not available.
  !   Calling program : calpcrop.f
  !   Called programs : none
  !   Input arguments : IB   = current sub-basin
  !                   : IY   = current year
  !                   : L    = current month
  !                   : root = root depth
  !                   : er2_s= rainfall contribution to soil storage at each
  !                   :        parcel
  !   Output arguments: stor = soil moisture storage at each parcel
  !                   : er   = rainfall contribution to soil storage at each
  !                   :         sub-basin
  !                   : ro   = runoff at each sub-basin
  !                   : dep  = total depletion at each sub-basin
  !                   : stg  = total soil moisture storage at each sub-basin
  !                   : shrt = irrigation water demand shortage
  !   Assumptions     :
  !   Limitations     :
  !   Notes           : Only used by the Blaney-Criddle Estimation method.
  !
  !   History         :(Date, Author, Description)
  !
  !     11/02/95    HBM    Units changed from 1000 acre-ft to acre-ft.
  !
  !***************************************************************************

  USE Globals

  !Parameters
  INTEGER IB, IY, L
  REAL er2_s(:,:), cuirr(:,:)
  !Outputs
  REAL stor(*)
  REAL dep, erain, stg, shrt

  !Locals
  INTEGER IP
  INTEGER ckey, skey
  REAL scap(MAXPARCEL)
  REAL maxstor, awcmad, cuirr_adj

  !maxstor is the maximum storage for one farm
  maxstor = 0.0

  DO IP = 1, nparce(IB)
     ckey = crop_key(IB,IP)
     skey = soil_key(IB,IP)
     awcmad = mad(ckey) * awc(skey) / 100.0

     IF (SPFLAG .EQ. 1) THEN
	scap(IP) = RZ(ckey)*awcmad
     ELSE
	scap(IP) = 12.0*RZ(ckey)*awcmad
     ENDIF
     !MAD         madlev(IP) = 0.01 * (100.0 - mad(ckey)) * scap(IP)

     maxstor = maxstor + irz(ckey)*awcmad*profssea(iy)* area(IB,IP,IY)
  END DO
  maxstor = maxstor/12.0

  !supply requirements if moisture storag < mad
  dep = 0.0
  shrt = 0.0
  DO IP = 1, nparce(IB)
     ! Adjust cuirr to account for alfalfa/grass shortage
     cuirr_adj = cuirr(IP,L)
     IF( CROP_TYPE(crop_key(IB,IP)) .EQ. 0 .OR. SUB_CROP_TYPE(crop_key(IB,IP)) .EQ. 3 ) THEN
	Cuirr_adj = cuirr(IP,L) * cushort(ib)
     END IF

     !MAD	   IF (stor(IP).lt.madlev(IP)) THEN
     IF(stor(IP) .LT. 0.0) THEN

	!Since stor(IP) is negative we sum it to DCUirr because
	!it represents a deficit.  The result is how much of the
	!DCUirr demand was met.

	dep = dep + (cuirr_adj+stor(IP))*AREA(IB,IP,IY)

	!The shortage is the negative value of the storage.
	!Which means what water was not supplied.

	shrt = shrt + (ABS(stor(IP))*AREA(IB,IP,IY))
	stor(IP) = 0.0
     ELSEIF( stor(IP) .GE. 0.0) THEN
	dep = dep + cuirr_adj*AREA(IB,IP,IY)
     ENDIF
  END DO
  shrt = shrt/12.0
  dep = dep/12.0 

  !Calculate storage level
  stg = 0.0
  DO IP = 1, nparce(IB)
     !If outside of season and user specifies the beginning
     !of the season carryover, then do not do any addition 
     !to the soil moisture balance.  
     !PROFLAG = 1 MEANS USER SET THE SOIL MOISTURE AT THE BEGINNING
     !            OF EACH SEASON TO BE A SPECIFIED NUMBER (PROFSSEA).

     IF (PROFLAG(IY).NE.1) THEN

	IF (stor(IP)+er2_s(IP,L).GE.Scap(IP)) THEN

	   !If effective rainfall cannot be used then set it to zero

	   !              re_ro = er2_s(IP,L) - (Scap(IP) - storag(IP))
	   !              re_ro = re_ro * AREA(IB,IP,IY)/12.0
	   !              ro = ro + re_ro  

	   er2_s(IP,L) = Scap(IP) - stor(IP)
	   stor(IP) = Scap(IP)

	ELSE
	   stor(IP) = stor(IP) + er2_s(IP,L)
	ENDIF
	!The use sets the storage at the beginning of the season so 
	!set the rainfall storage to zero.
     ELSE
	er2_s(IP,L) = 0.0
     ENDIF
     !MAD            stg = stg + (storag(IP)-MADLev(IP))*AREA(IB,IP,IY)
     stg = stg + stor(IP)*AREA(IB,IP,IY)
     !OIRG         stg = stg + (stor(IP)-madlev(IP))*AREA(IB,IP,IY)
  END DO
  stg = stg / 12.0

  !Calculate carry-over from rainfall
  !      er = 0.0 ! Don't know what this does
  erain = 0.0
  DO IP = 1, nparce(IB)
     erain = erain + er2_s(IP,L)*AREA(IB,IP,IY)
  END DO
  erain = erain / 12.0

  IF(stg .GT. maxstor) stg = maxstor

  RETURN
END SUBROUTINE SCHED
