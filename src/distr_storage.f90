      SUBROUTINE DISTR(IB,IY,L,nperiods,exces_er,cuirr,Qs,Qr,nendmo,
C     Outputs
     $     er_stor, storag, ro, sh, acu, stg, re_ro)

************************************************************************
C
C   Function        : distr.f
C   Author          : HB Manguerra
C   Date            : December 1994
C   Purpose         : This performs the basin-wide water budget when water
C                     supply information is available. The water budget 
C                     parameters are computed by subarea (i.e. combining the
C                     water requirements of all crops in a given subarea).
C                     Actual CU, water shortage, deep percolation and runoff
C                     are computed by subarea. The soil moisture storage in 
C                     every parcel is computed. Areas under alfalfa and 
C                     pasture are shorted first before other crops.
C   Calling program : calpcrop.f
C   Called programs : myexit.f 
C   Input arguments : IB   = current sub-basin
C                   : IY   = current year
C                   : L    = current month
C                   : Root = Root depth
C                   : f_iwr= irrigation water requirement of each parcel
C                   : exces_er= rainfall contribution to soil storage at each
C                   :        parcel
C                   : Qs   = water supply at each sub-basin
C                   : Qr   = water requirement of each sub-basin
C   Output arguments: storage = soil moisture storage at each parcel
C                   : er_stor = rainfall contribution to soil storage at each
C                   :         sub-basin
C                   : ro   = runoff at each sub-basin
C                   : sh   = amount short at each sub-basin
C                   : acu  = total actual CU at each sub-basin
C                   : stg  = total soil moisture storage at each sub-basin
C   Assumptions     :
C   Limitations     :
C   Notes           : Only used by the Blaney-Criddle Estimation method.
C
C   History         :(Date, Author, Description)
C
C     11/02/95    HBM    Units changed from 1000 acre-ft to acre-ft.
C
C
C***************************************************************************

      USE Globals

C     Parameters
      INTEGER IB, IY, L, nendmo(:), nperiods
      REAL exces_er(:,:), cuirr(:,:)
      REAL storag(:)
      REAL Qs, Qr
C     Outputs
      REAL er_stor, ro, sh, acu, stg, re_ro, t_cuirr

C     Locals
      INTEGER SMBSeason, IP
      REAL volume, ALTScap
      REAL Scap(nparce(IB))
      REAL deficit(nparce(IB))
      REAL Qs_UD_p(nparce(IB)), Qs_UD_Res, Qs_Residual
      LOGICAL DONE
      REAL Xvol, QS_UD, ShtgEs, ShAddQs, Qreq, Tdeficit, cuirr_sh

      REAL adj_iwr(nparce(IB)) ! IWR after water storage deducted.
      REAL parcel_s(nparce(IB)) ! fraction full of parcel
      INTEGER parcel_idx(nparce(IB)) ! sorted indices of parcels by fraction full
      INTEGER t_ip, i_ins
      REAL diff

      ro = 0.0
      sh = 0.0
      acu = 0.0
      stg = 0.0
      er_stor = 0.0
      volume = 0.0
      re_ro = 0.0
	t_cuirr = 0.0

C-----calculate common parameters

      SMBSeason = 0

      DO 4 IP = 1, nparce(IB)
         IF (SPFLAG .EQ. 1) THEN
            Scap(IP) = RZ(bkey(IB,IP))*AWCMAD(bkey(IB,IP))
         ELSE
            Scap(IP) = 12.0*RZ(bkey(IB,IP))*AWCMAD(bkey(IB,IP))
         ENDIF

C     If outside of season and user specifies the beginning
C     of the season carryover, then do not do any addition 
C     to the soil moisture balance.  Therefore set the variable
C     SMBSeason (Soil Moisture Begining of Season) to zero. 

         if (nperiods .eq. 12) then ! monthly
            IF (((L.LT.fbegmo(IB,IY)).OR.(L.GT.fendmo(IB,IY))) .AND. 
     :           (PROFLAG(IY).EQ.1)) THEN
               SMBSeason = 1
            ENDIF
         else
            IF (((L.LT.fbegda(IB,IY)).OR.(L.GT.fendda(IB,IY))) .AND. 
     :           (PROFLAG(IY).EQ.1)) THEN
               SMBSeason = 1
            ENDIF
         ENDIF
 4    CONTINUE

C-----Calculate storage level plus carry-over rainfall

      IF (SMBSeason .EQ. 0) THEN
         DO 100 IP = 1, nparce(IB)
            IF ((storag(IP)+exces_er(IP,L).GE.Scap(IP)) .AND. 
     :          (exces_er(IP,L) .GT. 0.0)) THEN

C-----Calculate carry-over from rainfall

               er_stor = er_stor + (Scap(IP)-storag(IP))*AREA(IB,IP,IY) 

               re_ro = exces_er(IP,L) - (Scap(IP) - storag(IP))
               re_ro = re_ro * AREA(IB,IP,IY)/12.0
               ro = ro + re_ro  
     
               storag(IP) = Scap(IP)
               
            ELSE
               storag(IP) = storag(IP) + exces_er(IP,L)

C-----Calculate carry-over from rainfall

               er_stor = er_stor + exces_er(IP,L)*AREA(IB,IP,IY) 
            ENDIF

 100     CONTINUE
      ENDIF


      IF (Qs .GE. Qr .AND. SMBSeason .EQ. 0) THEN

C-----distribute requirements first for all crops before putting extra
         Qreq = 0.0

         DO 5 IP = 1, nparce(IB)

            IF( N_CRP(bkey(IB,IP)) .EQ. 1 .OR. 
     :           N_CRP(bkey(IB,IP)) .EQ. 2 ) THEN
               storag(IP) = storag(IP) - cuirr(IP,L) * cushort(ib)
            else
               storag(IP) = storag(IP) - cuirr(IP,L)
            endif

C-----------Qreq is a variable that tracks the amount of deficit that 
C-----------water storage has (below zero).

            IF( storag(IP) .LT. 0.0) THEN
               Qreq = (storag(IP)/12)*area(IB,IP,IY) + Qreq
               storag(IP) = 0.0
            ENDIF

 5       CONTINUE

C--------Qreq is added because it should be a negative number that
C--------we are removing from the volume.

         volume = Qs + Qreq  

C-----initially distribute for crops other than alfalfa and pasture
C-----calculate total area with soil moisture < Scap
C-----ALTScap is the area that is not at storage capacity and therefore
C-----the additional volume can be used in these areas.

         ALTScap = 0.0
         DO 10 IP = 1, nparce(IB)
            IF(storag(IP).LT.Scap(IP)) ALTScap = ALTScap+area(IB,IP,IY)
 10      CONTINUE

C-----convert excess amount in depth and adjust storage

         DO WHILE ((ALTScap.GT.0.0).AND.(volume.GT.0.0))

            volume = 12 * volume / ALTScap    ! volume = inches
            DO 30 IP = 1, nparce(IB)

               IF (storag(IP).LT.Scap(IP)) storag(IP)=storag(IP)+volume
               
 30         CONTINUE
      
C-----calculate remaining excess amount in acre-in due to areas with
C-----storage that exceeds Scap; and also recalculate total area 
C-----with soil moisture storage still < Scap

            ALTScap = 0.0
            volume = 0.0

            DO 40 IP = 1, nparce(IB)
               IF (storag(IP) .GE. Scap(IP)) THEN
                  volume = volume + (storag(IP)-Scap(IP))*
     :                 area(IB,IP,IY) 
                  storag(IP) = Scap(IP)
               ELSE
                  ALTScap = ALTScap + area(IB,IP,IY)
               ENDIF 
 40         CONTINUE

            IF( volume .GT. 0.0) THEN
               volume = volume / 12.0 
            ELSE
               volume = 0.0
            ENDIF     
         END DO

C-----Calculate water short
         sh = 0.0

      ELSEIF( Qs.GE. 0.0 .AND. SMBSeason .EQ. 0) THEN            ! IF Qs < Qr

C    
         do ip = 1, nparce(IB)
            cuirr_sh = cuirr(IP,L)
            IF( N_CRP(bkey(IB,IP)) .EQ. 1 .OR. 
     :           N_CRP(bkey(IB,IP)) .EQ. 2 ) THEN
               cuirr_sh = cuirr(IP,L) * cushort(ib)
            ENDIF
            if (cuirr_sh .gt. storag(ip)) then
C    ----------Need more water than is in storage.
               storag(ip) = 0
               adj_iwr(ip) = cuirr_sh - storag(ip)
            else
C    ----------Have enough in storage.
               adj_iwr(ip) = 0
               storag(ip) = storag(ip) - cuirr_sh
            endif
               
         end do

         ALTScap = 0.0
         DO 11 IP = 1, nparce(IB)            
            IF (adj_iwr(ip) .GT. 0.0001 ) THEN
               ALTScap = ALTScap+area(IB,IP,IY)
            ENDIF
 11      CONTINUE

C-----convert supply amount in depth and adjust storage

         Qs_UD = (12.0*Qs/ALTScap)    ! volume = inches

         DONE = .FALSE.

         DO IP = 1, nparce(IB)
            IF( adj_iwr(ip) .GT. 0.0001 ) THEN
               Qs_UD_p(IP) = Qs_UD
            ELSE
               Qs_UD_p(IP) = 0.0
            ENDIF
         END DO

         Qs_UD_Res = 0.0  ! Extra volume of surface water.

         DO WHILE ( .NOT.DONE )
            Qs_Residual = 0.0
            DO IP = 1, nparce(IB)
               IF( Qs_UD_p(IP) .GT. adj_iwr(ip) .AND. 
     :              adj_iwr(ip) .GT. 0.0001 ) THEN  
C    -------------Case where we have more water than we need.  Residual
C    -------------is the extra volume.
                  Qs_residual = Qs_residual + 
     :                 ((Qs_UD_p(IP) - adj_iwr(ip)) * area(IB,IP,IY))
                  Qs_UD_p(IP) = adj_iwr(ip)

C    -------------Remove this parcel from future consideration
                  ALTScap = ALTScap - area(IB,IP,IY)

               ELSEIF (Qs_UD_p(IP) .LT. adj_iwr(ip) .AND.
     :                 adj_iwr(ip) .GT. 0.0001 ) THEN

C    -------------Add residual water to this field since there is
C    -------------currently not enough surface water here to meet demand.
                  Qs_UD_p(IP) = Qs_UD_p(IP) + Qs_UD_Res 

               ENDIF
            END DO
	       
            IF( ALTScap .LT. 0.0001) THEN
               DONE = .TRUE.
            ELSE
               Qs_UD_Res = (Qs_residual/ALTScap)
               DONE = (Qs_UD_Res .LT. 0.00001)
            ENDIF
         END DO

C    ----Check if we still have surface water.  If so, then fill
C    ----up the soil moisture profile.
         if (Qs_residual .gt. 0) then
C    -------sort the parcels in order of most dry.
            parcel_s(1) = storag(1) / scap(1)
            parcel_idx(1) = 1
            DO IP = 2, nparce(IB)
               parcel_idx(ip) = ip
               parcel_s(ip) = storag(ip) / scap(ip)

               do t_ip = 1, ip-1
                  if (parcel_s(ip) .gt. parcel_s(t_ip)) then
C    ----------------insert into list.  Move elements up.
                     do i_ins = t_ip, ip-1
                        parcel_s(t_ip+1) = parcel_s(t_ip)
                        parcel_idx(t_ip+1) = parcel_idx(t_ip)
                     end do
                     parcel_s(t_ip) = storag(ip) / scap(ip)
                     parcel_idx(t_ip) = ip
                     exit       ! t_ip
                  endif
               end do
            END DO
                 
C    -------Now fill up storage.
            DO IP = 1, nparce(IB)
               diff = (Scap(IP) - storag(IP)) * area(IB,IP,IY)
               if (diff .gt. Qs_residual) then
C    -------------More storage is available than surface supply.
                  storag(IP) = (Qs_residual/area(IB,IP,IY))
                  exit
               else
                  storag(IP) = scap(ip)
                  Qs_residual = Qs_residual - diff
               endif
            end do
         endif
 
         volume = 0.0
         Tdeficit = 0.0

         DO 31 IP = 1, nparce(IB)
            IF ((storag(IP)+Qs_UD-adj_iwr(ip)).LT.Scap(IP)) THEN
               IF (( storag(IP) + Qs_UD_p(IP) - adj_iwr(ip))
     $              .LT. 0.0 ) THEN
                  deficit(IP) =
     $                 ((storag(IP)+Qs_UD_p(IP)-adj_iwr(ip))/12.)
     :                 * area(IB,IP,IY) * -1.0
               ELSE
                  deficit(IP) = 0.0
               ENDIF

               storag(IP)=storag(IP)+Qs_UD_p(IP)-adj_iwr(ip)
C              deficit(IP) = ((Scap(IP) - storag(IP))/12.)
C     :              * area(IB,IP,IY)
               Tdeficit = Tdeficit + deficit(IP)
            ELSE 
               storag(IP) = Scap(IP)
               IF (area(IB,IP,IY) .GT. 0.000001) THEN
                  volume = volume +
     $                 (((storag(IP)+Qs_UD_p(IP)-adj_iwr(ip))-
     :                 Scap(IP))*12./area(IB,IP,IY))
               ENDIF
               deficit(IP) = 0.0
            ENDIF

 31      CONTINUE

C-----update storage
C
C           Use all the water from soil moisture first
C           if after this, water is left then put it back
C           into the soil (Loop 270).
C
         DO 260 IP = 1, nparce(IB)
C    
C                 Check if the area is zero 
C
            IF( area(IB,IP,IY).LT.0.001 .OR. Tdeficit.LT.0.001) THEN
               Xvol = 0.0
            ELSE
               Xvol=(((deficit(IP)/Tdeficit)*volume)*12.)
     :              /area(IB,IP,IY)
            ENDIF
C
C           Prorate shortages only if
C           you have used all the root zone storage
C           (storag(IP) is less than zero).
C
            IF(storag(IP) .LT. 0.0) THEN
               IF(storag(IP)+Xvol .GT. 0.0) THEN
                  Tdeficit = Tdeficit - deficit(IP)
                  volume = volume + ((storag(IP)+Xvol)
     :                 *area(IB,IP,IY)/12.0)
                  storag(IP) = 0.0
               ELSE
                  storag(IP) = storag(IP) + Xvol
                  volume = volume - ((Xvol/12.0)*area(IB,IP,IY))
               ENDIF
            ENDIF
260      CONTINUE

C           Check if water is left to put back into
C           the soil.
C
         DO WHILE ( volume .GT. 0.01 .AND. Tdeficit .GT. 0.001) 
C
            DO 270 IP = 1, nparce(IB)

C     check if the the available water is larger than the
C     amount the soil can store and the plant needs.  If this
C     is the case then keep it for other crops.
C
C              Check if the area is zero 
C
               IF( area(IB,IP,IY) .LT .0.001 .OR. 
     :              Tdeficit.LT.0.001) THEN
                  Xvol = 0.0
               ELSE
                  Xvol=(((deficit(IP)/Tdeficit)*volume)*12.)
     :                 /area(IB,IP,IY)
               ENDIF
 
               IF(storag(IP)+Xvol .GT. Scap(IP)) THEN
                  Tdeficit = Tdeficit - deficit(IP)
                  volume = volume + ((storag(IP)+Xvol-Scap(IP))
     :                 *area(IB,IP,IY)/12.0)
                  storag(IP) = Scap(IP)
               ELSE
                  storag(IP) = storag(IP) + Xvol
                  volume = volume - (Xvol/12.0)*area(IB,IP,IY)
               ENDIF
 270        CONTINUE
         END DO

C-----Calculate water short

         sh = 0.0
         DO 90 IP = 1, nparce(IB)

            cuirr_sh = cuirr(IP,L)
            IF( N_CRP(bkey(IB,IP)) .EQ. 1 .OR. 
     :           N_CRP(bkey(IB,IP)) .EQ. 2 ) THEN
               cuirr_sh = cuirr(IP,L) * cushort(ib)
            ENDIF

            IF (cuirr_sh .GT. 0.0001 ) THEN
               IF (storag(IP).LT.0.0) THEN
                  sh = sh + (-1.0*storag(IP))*AREA(IB,IP,IY)
                  storag(IP) = 0.0
               ENDIF
            ENDIF

 90      CONTINUE
         sh = sh / 12.0         ! volume = acre-ft

C-----specify runoff = volume = 0
         volume = 0.0
      ELSE                      ! end of Qs < Qr
         volume = Qs
      ENDIF

C-----Calculate runoff
      ro = volume

      DO 91 IP = 1, nparce(IB)

C        If the user sets a target for the end of season water amount then
C        check if it is less that available and that well mode is such 
C        that it will pump to desired amount.
C        -------------------------------------------------------------
C        Check if the well mode is 0 - Percent of GW Supplied by Well
C        PROFLAG = 1 means the user set a target storage at the end of 
C                  the season
C        -------------------------------------------------------------

         IF( WMODE(IB) .EQ. 0 .AND. L.EQ.nendmo(IP) 
     :       .AND. PROFLAG(IY) .EQ. 1) THEN
            IF(storag(IP) .LT. PROFEND(IY)*Scap(IP)) THEN
               ShtgEs = PROFEND(IY)*Scap(IP) - storag(IP)
               sh = sh + ShtgEs * AREA(IB,IP,IY)/12.         ! volume = acre-ft
               storag(IP) = PROFEND(IY)*Scap(IP)
            ENDIF
C
C           If the Water Supply is > Water Req and shortage is > 0
C           compute the additional amount of water supply available
C
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

  91  CONTINUE

      IF(ro .LT. 0.0) ro = 0.0 

C-----Only if you do not have shortages otherwise the
C-----carryover is zero

      stg = 0.0
      DO 101 IP = 1, nparce(IB)
         stg = stg + storag(IP)*AREA(IB,IP,IY)
 101  CONTINUE
      stg = stg / 12.0
      IF (SMBSeason .EQ. 0) THEN
         er_stor = er_stor / 12.0
      ELSEIF (SMBSeason .EQ. 1) THEN
         er_stor = 0.0
      ENDIF

CDAP -- had trouble with this block of code because of problems when proflag = 1
C       stg = 0.0
C       IF (SMBSeason .EQ. 0) THEN
C          DO 101 IP = 1, nparce(IB)
C             stg = stg + storag(IP)*AREA(IB,IP,IY)
C  101     CONTINUE
C          stg = stg / 12.0
C          er_stor = er_stor / 12.0
C       ELSEIF (SMBSeason .EQ. 1) THEN
C          DO 102 IP = 1, nparce(IB)
C             stg = stg + storag(IP)*T_AREA(IB,IY)/12.0
C  102     CONTINUE
C          er_stor = 0.0
C       ENDIF

C     This equation assumes that if water is available from surface
C     supplies (Qs) and it does not return as runoff (ro) then it
C     must be a depletion met by surface supply.  NOTE: Only when there is
C     consumptive use

      DO 110 IP = 1, nparce(IB)
         cuirr_sh = cuirr(IP,L)
         IF( N_CRP(bkey(IB,IP)) .EQ. 1 .OR. 
     :        N_CRP(bkey(IB,IP)) .EQ. 2 ) THEN
            cuirr_sh = cuirr(IP,L) * cushort(ib)
         endif

         t_cuirr = t_cuirr + cuirr_sh*T_AREA(IB,IY)/12.0
 110  CONTINUE

C     Calculate the amount of depletion of surface water supplies ??

      acu = Qs - ro

C      IF ( t_cuirr .gt. 0) then
C         acu = Qs - ro
C      ENDIF
C
C     This equation if un commented will cap the cu met by water supplies to the
C     Crop Consumptive Use.  This is a problem for the water budget since water 
C     suplies can go into soil moisture.
C
C      IF(acu .GT. t_cuirr) acu = t_cuirr 

      IF(acu .LT. 0.0) acu = 0.0

      RETURN
      END
