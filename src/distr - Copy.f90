SUBROUTINE DISTR(IB,IY,L,nperiods,exces_er,cuirr,Qs,Qr,pc_rz,Qu,nendmo, &
				!Outputs
     er_stor, er2_s, storag, ro, sh, acu, stg, re_ro, SMBSeason)

  !************************************************************************
  !
  !   Function        : distr.f
  !   Author          : HB Manguerra
  !   Date            : December 1994
  !   Purpose         : This performs the basin-wide water budget when water
  !                     supply information is available. The water budget 
  !                     parameters are computed by subarea (i.e. combining the
  !                     water requirements of all crops in a given subarea).
  !                     Actual CU, water shortage, deep percolation and runoff
  !                     are computed by subarea. The soil moisture storage in 
  !                     every parcel is computed. Areas under alfalfa and 
  !                     pasture are shorted first before other crops.
  !   Calling program : calpcrop.f
  !   Called programs : myexit.f 
  !   Input arguments : IB   = current sub-basin
  !                   : IY   = current year
  !                   : L    = current month
  !                   : Root = Root depth
  !                   : f_iwr= irrigation water requirement of each parcel
  !                   : exces_er= rainfall contribution to soil storage at each
  !                   :        parcel
  !                   : Qs   = water supply at each sub-basin
  !                   : Qr   = water requirement of each sub-basin
  !                   : Qu   = groundwater requirement of each sub-basin
  !   Output arguments: storage = soil moisture storage at each parcel
  !                   : er_stor = rainfall contribution to soil storage at each
  !                   :         sub-basin
  !                   : ro   = runoff at each sub-basin
  !                   : sh   = amount short at each sub-basin
  !                   : acu  = total actual CU at each sub-basin
  !                   : stg  = total soil moisture storage at each sub-basin
  !   Assumptions     :
  !   Limitations     :
  !   Notes           : Only used by the Blaney-Criddle Estimation method.
  !
  !   History         :(Date, Author, Description)
  !
  !     11/02/95    HBM    Units changed from 1000 acre-ft to acre-ft.
  !
  !
  !***************************************************************************

  USE Globals

  !Parameters
  INTEGER IB, IY, L, nendmo(:), nperiods
  REAL exces_er(:,:), cuirr(:,:), er2_s(:,:)
  REAL storag(:)
  REAL Qs, Qr, pc_rz(:,:),Qu(:,:)
  !Outputs
  REAL er_stor, ro, sh, acu, stg, re_ro, t_cuirr
  INTEGER SMBSeason, mon, day

  !Locals
  INTEGER IP
  REAL volume, ALTScap
  REAL Scap(MAXPARCEL)
  REAL deficit(MAXPARCEL)
  REAL Qs_UD_p(MAXPARCEL), Qs_UD_Res, Qs_Residual
  LOGICAL DONE
  REAL Xvol, Qs_UD, ShtgEs, ShAddQs, Qreq, Tdeficit, cuirr_sh
  REAL total_depth, awcmad
  REAL prev_storag(MAXPARCEL) ! used for shortage calculation
  REAL diff

  ro = 0.0
  sh = 0.0
  acu = 0.0
  stg = 0.0
  er_stor = 0.0
  volume = 0.0
  re_ro = 0.0
  t_cuirr = 0.0

  !calculate common parameters

  SMBSeason = 0

  ! Patterson: Add Qu to the total water supply

  DO IP = 1, nparce(IB)
     ! Initialize er2_s
     er2_s(IP, L) = 0

     prev_storag(IP) = storag(IP)

     awcmad = mad(crop_key(IB,IP)) * awc(soil_key(IB,IP)) / 100.0

     IF (SPFLAG .EQ. 1) THEN
	! Patterson: changed to use growth increment.
	!Scap(IP) = RZ(crop_key(IB,IP))*AWCMAD
	Scap(IP) = pc_rz(IP,L) * AWCMAD
     ELSE
	Scap(IP) = 12.0*pc_rz(IP,L)*AWCMAD
     ENDIF

     !If outside of season and user specifies the beginning
     !of the season carryover, then do not do any addition 
     !to the soil moisture balance.  Therefore set the variable
     !SMBSeason (Soil Moisture Begining of Season) to zero. 

     if (nperiods .eq. 12) then ! monthly
	IF (((L.LT.fbegmo(IB,IY)).OR.(L.GT.fendmo(IB,IY))) .AND. &
	     (PROFLAG(IY).EQ.1)) THEN
	   SMBSeason = 1
	ENDIF
     else
	IF (((L.LT.fbegda(IB,IY)).OR.(L.GT.fendda(IB,IY))) .AND. &
	     (PROFLAG(IY).EQ.1)) THEN
	   SMBSeason = 1
	ENDIF
     ENDIF
  END DO

  !Calculate storage level plus carry-over rainfall

  IF (SMBSeason .EQ. 0) THEN
     DO IP = 1, nparce(IB)
	IF ((storag(IP)+exces_er(IP,L).GE.Scap(IP)) .AND. &
	     (exces_er(IP,L) .GT. 0.0)) THEN

    !Calculate carry-over from rainfall

	   er_stor = er_stor + (Scap(IP)-storag(IP))*AREA(IB,IP,IY) 
	   er2_s(IP, L) = er2_s(IP, L) + (Scap(IP)-storag(IP))

	   re_ro = exces_er(IP,L) - (Scap(IP) - storag(IP))
	   re_ro = re_ro * AREA(IB,IP,IY)/12.0
	   ro = ro + re_ro  

	   storag(IP) = Scap(IP)

	ELSE
	   storag(IP) = storag(IP) + exces_er(IP,L)

    !Calculate carry-over from rainfall

	   er_stor = er_stor + exces_er(IP,L)*AREA(IB,IP,IY) 
	   er2_s(IP, L) = er2_s(IP, L) + exces_er(IP,L)
	ENDIF

     END DO
  ENDIF

  ! Patterson: include groundwater uptake here?
  DO IP = 1, nparce(IB)
     ! Patterson: I'm removing this so that uptake always gets added.  I'll check for storag > max at the end and add it to the runoff
     IF (.FALSE. .AND. (storag(IP)+Qu(IP,L).GT.Scap(IP))) THEN
        ! This should be an error?  Can groundwater uptake ever exceed
        !  soil capacity?
        CALL CLNDR(L,IY,mon,day)
        print *, "warning: ", mon, "/", day, "/", IY + nyr1 -1, ": groundwater uptake is causing current soil moisture storage to exceed total storage."
        storag(IP) = Scap(IP)
     ELSE
        storag(IP) = storag(IP) + Qu(IP,L)
     ENDIF
  END DO

  IF (Qs .GE. Qr .AND. SMBSeason .EQ. 0) THEN

     !distribute requirements first for all crops before putting extra
     Qreq = 0.0

     DO IP = 1, nparce(IB)

	IF (CROP_TYPE(crop_key(IB,IP)) .EQ. 0 .OR. SUB_CROP_TYPE(crop_key(IB,IP)) .EQ. 3) THEN
	   storag(IP) = storag(IP) - cuirr(IP,L) * cushort(ib)
	else
	   storag(IP) = storag(IP) - cuirr(IP,L)
	endif

 !Qreq is a variable that tracks the amount of deficit that 
 !water storage has (below zero).

        IF (USE_WATER_STRESS) THEN
            IF (storag(IP) .LT. 0.0) THEN
               ! Patterson: Don't set Qreq in this situation because we don't want to do anything
               !   special when storage goes negative.
	       ! Qreq = (storag(IP)/12)*area(IB,IP,IY) + Qreq
	    END IF
	ELSE
            IF (storag(IP) .LT. 0.0) THEN
	       Qreq = (storag(IP)/12)*area(IB,IP,IY) + Qreq
	       storag(IP) = 0.0
	    END IF
	ENDIF

     END DO

     !Qreq is added because it should be a negative number that
     !we are removing from the volume.

     volume = Qs + Qreq  

     !initially distribute for crops other than alfalfa and pasture
     !calculate total area with soil moisture < Scap
     !ALTScap is the area that is not at storage capacity and therefore
     !the additional volume can be used in these areas.

     ALTScap = 0.0
     DO IP = 1, nparce(IB)
	IF(storag(IP).LT.Scap(IP)) ALTScap = ALTScap+area(IB,IP,IY)
     END DO

     !convert excess amount in depth and adjust storage

     DO WHILE ((ALTScap.GT.0.0).AND.(volume.GT.0.0))

	volume = 12 * volume / ALTScap    ! volume = inches
	DO IP = 1, nparce(IB)

	   IF (storag(IP).LT.Scap(IP)) storag(IP)=storag(IP)+volume

	END DO

 !calculate remaining excess amount in acre-in due to areas with
 !storage that exceeds Scap; and also recalculate total area 
 !with soil moisture storage still < Scap

	ALTScap = 0.0
	volume = 0.0

	DO IP = 1, nparce(IB)
	   IF (storag(IP) .GE. Scap(IP)) THEN
	      volume = volume + (storag(IP)-Scap(IP))* area(IB,IP,IY) 
	      storag(IP) = Scap(IP)
	   ELSE
	      ALTScap = ALTScap + area(IB,IP,IY)
	   ENDIF
	END DO

	IF( volume .GT. 0.0) THEN
	   volume = volume / 12.0 
	ELSE
	   volume = 0.0
	ENDIF
     END DO

     !Calculate water short
     sh = 0.0

  ELSEIF( Qs.GE. 0.0 .AND. SMBSeason .EQ. 0) THEN            ! IF Qs < Qr

     ALTScap = 0.0
     DO IP = 1, nparce(IB)            
	cuirr_sh = cuirr(IP,L)
	IF( CROP_TYPE(crop_key(IB,IP)) .EQ. 0 .OR. SUB_CROP_TYPE(crop_key(IB,IP)) .EQ. 3 ) THEN
	   cuirr_sh = cuirr(IP,L) * cushort(ib)
	ENDIF
	IF( cuirr_sh .GT. 0.0001 ) THEN
           ! This allows us to only allocate surface water to fields that have a demand.
	   ALTScap = ALTScap+area(IB,IP,IY)
	ENDIF
     END DO

     !convert supply amount in depth and adjust storage

     Qs_UD = (12.0*Qs/ALTScap)    ! volume = inches

     DONE = .FALSE.

     DO IP = 1, nparce(IB)
	cuirr_sh = cuirr(IP,L)
	IF( CROP_TYPE(crop_key(IB,IP)) .EQ. 0 .OR. SUB_CROP_TYPE(crop_key(IB,IP)) .EQ. 3 ) THEN
	   cuirr_sh = cuirr(IP,L) * cushort(ib)
	ENDIF
	IF( cuirr_sh .GT. 0.0001 ) THEN
	   Qs_UD_p(IP) = Qs_UD
	ELSE
	   Qs_UD_p(IP) = 0.0
	ENDIF
     END DO

     Qs_UD_Res = 0.0

     DO WHILE ( .NOT.DONE )
	Qs_Residual = 0.0
	ALTScap = 0.0
	DO IP = 1, nparce(IB)

	   cuirr_sh = cuirr(IP,L)
	   IF( CROP_TYPE(crop_key(IB,IP)) .EQ. 0 .OR. SUB_CROP_TYPE(crop_key(IB,IP)) .EQ. 3 ) THEN
	      cuirr_sh = cuirr(IP,L) * cushort(ib)
	   ENDIF

           ! calculate depth of surface water applied as the per field value
           !  plus the amount left over from the previous loop
	   total_depth = Qs_UD_p(IP) + Qs_UD_Res
	   IF( (total_depth .GT. cuirr_sh) .AND. (cuirr_sh .GT. 0.0001) ) THEN  
	      IF( Qs_UD_p(IP) .NE. cuirr_sh ) THEN
	         ! There is more supply than required, so add this as residual
		 Qs_residual=Qs_residual + ((total_depth - cuirr_sh) &
	             * area(IB,IP,IY))
                 ! Assign to this field the amount actually used of supply.
		 Qs_UD_p(IP) = cuirr_sh
	      ENDIF
	   ELSEIF (total_depth .LT. cuirr_sh .AND.  cuirr_sh .GT. 0.0001 ) THEN
              ! Assign to this field the amount actually used of supply.
	      Qs_UD_p(IP) = Qs_UD_p(IP) + Qs_UD_Res
	      ALTScap = ALTScap + area(IB,IP,IY) 
	   ENDIF
	END DO

	IF( ALTScap .LT. 0.0001) THEN
	   DONE = .TRUE.
	ELSE
	   Qs_UD_Res = (Qs_residual/ALTScap)
	   DONE = (Qs_UD_Res .LT. 0.00001)
	ENDIF
     END DO

     volume = 0.0
     Tdeficit = 0.0

     DO IP = 1, nparce(IB)
	cuirr_sh = cuirr(IP,L)
	IF( CROP_TYPE(crop_key(IB,IP)) .EQ. 0 .OR. SUB_CROP_TYPE(crop_key(IB,IP)) .EQ. 3 ) THEN
	   cuirr_sh = cuirr(IP,L) * cushort(ib)
	ENDIF
	IF (cuirr_sh .GT. 0.0001 ) THEN
	   IF ((storag(IP)+Qs_UD_p(IP)-cuirr_sh).LT.Scap(IP)) THEN
	      IF(( storag(IP) + Qs_UD_p(IP)-cuirr_sh ) .LT. 0.0 ) THEN
		 deficit(IP) = ((storag(IP)+Qs_UD_p(IP)-cuirr_sh)/12.) &
		      * area(IB,IP,IY) * -1.0
	      ELSE
		 deficit(IP) = 0.0
	      ENDIF

              storag(IP)=storag(IP)+Qs_UD_p(IP)-cuirr_sh

       !              deficit(IP) = ((Scap(IP) - storag(IP))/12.)
       !     :              * area(IB,IP,IY)
	      Tdeficit = Tdeficit + deficit(IP)
	   ELSE 
	      storag(IP) = Scap(IP)
	      IF (area(IB,IP,IY) .GT. 0.000001) THEN
		 volume = volume + (((storag(IP)+Qs_UD_p(IP)-cuirr_sh)- &
                      Scap(IP))*12./area(IB,IP,IY))
	      ENDIF
	      deficit(IP) = 0.0
	   ENDIF
	ENDIF

     END DO

     !update storage

     !Use all the water from soil moisture first
     !if after this, water is left then put it back
     !into the soil (Loop 270).

     DO IP = 1, nparce(IB)

	cuirr_sh = cuirr(IP,L)
	IF( CROP_TYPE(crop_key(IB,IP)) .EQ. 0 .OR. SUB_CROP_TYPE(crop_key(IB,IP)) .EQ. 3 ) THEN
	   cuirr_sh = cuirr(IP,L) * cushort(ib)
	ENDIF

	IF (cuirr_sh .GT. 0.0001 ) THEN

    !Check if the area is zero 

	   IF( area(IB,IP,IY).LT.0.001 .OR. Tdeficit.LT.0.001) THEN
	      Xvol = 0.0
	   ELSE
	      Xvol=(((deficit(IP)/Tdeficit)*volume)*12.) /area(IB,IP,IY)
	   ENDIF

    !Prorate shortages only if
    !you have used all the root zone storage
    !(storag(IP) is less than zero).

	   IF(.NOT. USE_WATER_STRESS .and. storag(IP) .LT. 0.0) THEN
	      IF(storag(IP)+Xvol .GT. 0.0) THEN
		 Tdeficit = Tdeficit - deficit(IP)
		 volume = volume + ((storag(IP)+Xvol) *area(IB,IP,IY)/12.0)
                 storag(IP) = 0.0
	      ELSE
                 IF (.NOT. USE_WATER_STRESS) THEN
		    storag(IP) = storag(IP) + Xvol
                 end if
		 volume = volume - ((Xvol/12.0)*area(IB,IP,IY))
	      ENDIF
	   ENDIF
	ENDIF
     END DO

     !Check if water is left to put back into the soil.

     DO WHILE ( volume .GT. 0.01 .AND. Tdeficit .GT. 0.001) 

	DO IP = 1, nparce(IB)

	   cuirr_sh = cuirr(IP,L)
	   IF( CROP_TYPE(crop_key(IB,IP)) .EQ. 0 .OR. SUB_CROP_TYPE(crop_key(IB,IP)) .EQ. 3 ) THEN
	      cuirr_sh = cuirr(IP,L) * cushort(ib)
	   ENDIF

	   IF (cuirr_sh .GT. 0.0001 ) THEN

       !check if the the available water is larger than the
       !amount the soil can store and the plant needs.  If this
       !is the case then keep it for other crops.

       !Check if the area is zero 

	      IF( area(IB,IP,IY) .LT. 0.001 .OR. Tdeficit .LT. 0.001) THEN
		 Xvol = 0.0
	      ELSE
		 Xvol=(((deficit(IP)/Tdeficit)*volume)*12.) /area(IB,IP,IY)
	      ENDIF

	      IF(storag(IP)+Xvol .GT. Scap(IP)) THEN
		 Tdeficit = Tdeficit - deficit(IP)
		 volume = volume + ((storag(IP)+Xvol-Scap(IP)) *area(IB,IP,IY)/12.0)
		 storag(IP) = Scap(IP)
	      ELSE
		 storag(IP) = storag(IP) + Xvol
		 volume = volume - (Xvol/12.0)*area(IB,IP,IY)
	      ENDIF
	   ENDIF
	END DO
     END DO

     !Calculate water short

     sh = 0.0
     DO IP = 1, nparce(IB)

	cuirr_sh = cuirr(IP,L)
	IF( CROP_TYPE(crop_key(IB,IP)) .EQ. 0 .OR. SUB_CROP_TYPE(crop_key(IB,IP)) .EQ. 3 ) THEN
	   cuirr_sh = cuirr(IP,L) * cushort(ib)
	ENDIF

	IF (cuirr_sh .GT. 0.0001 ) THEN
	   IF (storag(IP).LT.0.0) THEN
              IF (USE_WATER_STRESS) THEN
		 ! be sure that the shortage doesn't include any remaining storage.
		 sh = (-1.0*(storag(IP) - amin1(prev_storag(IP), 0.0)))*AREA(IB,IP,IY)
	      ELSE
		 sh = sh + (-1.0*storag(IP))*AREA(IB,IP,IY)
	         storag(IP) = 0.0
	      END IF
	   ENDIF
	ENDIF

     END DO
     sh = sh / 12.0      ! volume = acre-ft

     !specify runoff = volume = 0
     volume = 0.0
  ELSE                  ! end of Qs < Qr
     volume = Qs
  ENDIF

  !Calculate runoff
  ro = volume

  DO IP = 1, nparce(IB)

     !If the user sets a target for the end of season water amount then
     !check if it is less that available and that well mode is such 
     !that it will pump to desired amount.
     !-------------------------------------------------------------
     !Check if the well mode is 0 - Percent of GW Supplied by Well
     !PROFLAG = 1 means the user set a target storage at the end of 
     !          the season
     !-------------------------------------------------------------

     IF( WMODE(IB) .EQ. 0 .AND. L.EQ.nendmo(IP) .AND. PROFLAG(IY) .EQ. 1) THEN
	IF(storag(IP) .LT. PROFEND(IY)*Scap(IP)) THEN
	   ShtgEs = PROFEND(IY)*Scap(IP) - storag(IP)
           IF (USE_WATER_STRESS) THEN
              sh = ShtgEs * AREA(IB,IP,IY)/12.         ! volume = acre-ft
	      storag(IP) = PROFEND(IY)*Scap(IP)
	   ELSE
              sh = sh + ShtgEs * AREA(IB,IP,IY)/12.         ! volume = acre-ft
	      storag(IP) = PROFEND(IY)*Scap(IP)
           END IF
	ENDIF

 !If the Water Supply is > Water Req and shortage is > 0
 !compute the additional amount of water supply available

	IF( Qs .GT. Qr .AND. sh .GT. 0.0) THEN
	   ShAddQs = Qs - Qr 
	   IF( (ShAddQs-sh) .LT. 0.0 ) THEN
	      sh = sh - ShAddQs
	      ro = 0.0
	   ELSE
	      ro = Qs - (Qr + sh)
	      sh = 0.0
	   ENDIF
	ENDIF

     ENDIF
  END DO

  IF(ro .LT. 0.0) ro = 0.0 

  !Only if you do not have shortages otherwise the
  !carryover is zero

  stg = 0.0
  DO IP = 1, nparce(IB)
     stg = stg + storag(IP)*AREA(IB,IP,IY)
  END DO
  stg = stg / 12.0

  IF (SMBSeason .EQ. 0) THEN
     er_stor = er_stor / 12.0
  ELSEIF (SMBSeason .EQ. 1) THEN
     er_stor = 0.0
  ENDIF

  !DAP -- had trouble with this block of code because of problems when proflag = 1
  !       stg = 0.0
  !       IF (SMBSeason .EQ. 0) THEN
  !          DO 101 IP = 1, nparce(IB)
  !             stg = stg + storag(IP)*AREA(IB,IP,IY)
  !  101     END DO
  !          stg = stg / 12.0
  !          er_stor = er_stor / 12.0
  !       ELSEIF (SMBSeason .EQ. 1) THEN
  !          DO 102 IP = 1, nparce(IB)
  !             stg = stg + storag(IP)*T_AREA(IB,IY)/12.0
  !  102     END DO
  !          er_stor = 0.0
  !       ENDIF

  !This equation assumes that if water is available from surface
  !supplies (Qs) and it does not return as runoff (ro) then it
  !must be a depletion met by surface supply.  NOTE: Only when there is
  !consumptive use

  DO IP = 1, nparce(IB)
     cuirr_sh = cuirr(IP,L)
     IF( CROP_TYPE(crop_key(IB,IP)) .EQ. 0 .OR. SUB_CROP_TYPE(crop_key(IB,IP)) .EQ. 3 ) THEN
	cuirr_sh = cuirr(IP,L) * cushort(ib)
     endif

     t_cuirr = t_cuirr + cuirr_sh*T_AREA(IB,IY)/12.0
  END DO

  !Calculate the amount of depletion of surface water supplies ??

  acu = Qs - ro

  !      IF ( t_cuirr .gt. 0) then
  !         acu = Qs - ro
  !      ENDIF

  !This equation if un commented will cap the cu met by water supplies to the
  !Crop Consumptive Use.  This is a problem for the water budget since water 
  !suplies can go into soil moisture.

  !      IF(acu .GT. t_cuirr) acu = t_cuirr 

  IF(acu .LT. 0.0) acu = 0.0

  RETURN
END SUBROUTINE DISTR
