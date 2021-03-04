SUBROUTINE BCENHANCE(nyr, IB, &
  ! Outputs
  carry0, storag)

!***************************************************************************
!
!   Function        : calpcrop.f
!   Author          : HB Manguerra
!   Date            : May 1995 
!   Purpose         : This calculates the crop consumptive use by Blaney- 
!                     Criddle ET method.  The original version is based
!                     on the SCS Modified Blaney-Criddle method.  An 
!                     enhanced version is also available in which it
!                     uses a monthly soil moisture budget and the concept
!                     of winter carry over soil moisture based on total
!                     precipitation during the preceding off-season.It also
!                     generates an output file *.obc that contains a 
!                     detailed results of the consumptive calculation using
!                     Blaney-Criddle.
!   Calling program : mainxc.f 
!   Called programs : clndr.f, perencrp.f, annuacrp.f, xcrain.f 
!   Input arguments : ib = current basin 
!   Output arguments: none
!   Assumptions     :
!   Limitations     :
!   Notes           : The routines for the original Blaney-Criddle method 
!                     are based on USBR XCONS2 program which uses the SCS 
!                     Modified Blaney-Criddle ET Estimation Method.
!
!   History         :(Date, Author, Description)
!
!     11/02/95    HBM    Units changed from 1000 acre-ft to acre-ft.
!
!***************************************************************************

      USE Globals

      ! Parameters
      INTEGER nyr, IB
      ! Outputs
      REAL carry0
      REAL storag(:)

      ! Local Variable Declaration
      INTEGER CKEY, SKEY, IP
      REAL istor, awcmad
      REAL scap(MAXPARCEL)


      !----------------------
      !Enhanced Blaney-Criddle
      !----------------------
      carry0 = 0.0
      DO IP = 1, nparce(IB)
         ckey = crop_key(IB,IP)
         skey = soil_key(IB,IP)
	 awcmad = mad(ckey) * awc(skey) / 100.0

         !irz(ckey) = frz(ckey)   ! WARNING - this nullifies the fact that
                                 ! root depth increases during the growing
                                 ! season.  This capability is only used
                                 ! in the Penman-Monteith method
         rz(ckey) = irz(ckey)
         maxcov(IP) = jbeg(IP,nyr) + GDATE5(ckey) - 1
         incrz(IP) = (frz(ckey)-irz(ckey))/GDATE5(ckey)
         IF (SPFLAG .EQ. 1) THEN
            incst(IP) = incrz(IP) * awcmad
         ELSE
            incst(IP) = 12.0*incrz(IP) * awcmad
         ENDIF

         IF (nyr.eq.1) THEN     ! initial storage
            IF (SPFLAG .EQ. 1) THEN
	       !IDS istor = irz(ckey)*awc(ckey)
	       !  THIS CHANGE ALLOWS THE INITIAL SOIL STORAGE TO BE
	       !  LESS THAN FULL AT THE BEGINING OF THE SIMULATION
	       !  PROFSSIM IS THE PORTION OF PROFILE FULL AT BEGINNING
	       !  OF SIMULATION.

	       !  ADDED THE ABILITY TO RESET THE INITIAL SOIL MOISTURE TO A USER
	       !      DEFINED PERCENT OF TOTAL AVAILABLE SOIL MOISTURE.
	       !  PROFLAG = 1 MEANS USER SET THE SOIL MOISTURE AT THE BEGINNING
	       !      OF EACH SEASON TO BE A SPECIFIED NUMBER (PROFSSEA).

               IF( PROFLAG(nyr).EQ.1 .OR. PROFLAG(nyr).EQ.3) THEN
                  istor = irz(ckey)*awcmad * profssea(nyr)
               ELSE  
                  istor = irz(ckey)*awcmad * profssim
               ENDIF
            ELSE
               istor = 12.0*irz(ckey)*awcmad
            ENDIF
            storag(IP) = istor  ! storage at the start
	    ! of simulation based on
	    ! initial root depth
         ELSE                   ! adjust rootzone and add wbu
            IF (SPFLAG .EQ. 1) THEN
               istor = irz(ckey)*awcmad
            ELSE
               istor = 12.0*irz(ckey)*awcmad
            ENDIF
            IF( frz(ckey) .EQ. 0.0 ) THEN
               storag(IP) = 0.0
            ELSE
               storag(IP) = storag(IP)*irz(ckey)/frz(ckey)
            ENDIF

	    !IDS ADDED THE ABILITY TO RESET THE INITIAL SOIL MOISTURE TO A USER
	    !   DEFINED PERCENT OF TOTAL AVAILABLE SOIL MOISTURE.
	    !	 PROFLAG = 1 MEANS USER SET THE SOIL MOISTURE AT THE BEGINNING
	    !         OF EACH SEASON TO BE A SPECIFIED NUMBER (PROFSSEA).
	    !
	    !    IF( PROFLAG(nyr) .EQ. 1) THEN
	    !      wbu(IP,nyr) = irz(ckey)*awc(ckey)*profssea(nyr)
	    !    ENDIF                  

	    !Patterson: Water buildup was not allocated, therefore we commented
	    !  it out:
!            storag(IP) = amin1(istor,storag(IP)+wbu(IP,nyr))
!     New:
            storag(IP) = amin1(istor,storag(IP))
         ENDIF
!           ----------------------------------------------
!           Calculate total initial storage of all parcels
!   		  ----------------------------------------------
         IF (SPFLAG .EQ. 1) THEN
            scap(IP) = rz(ckey)*awcmad
         ELSE
            scap(IP) = 12.0*rz(ckey)*awcmad
         ENDIF  
!
!           THIS FIXES THE PROBLEM THAT THE MAD SHOULD ONLY BE OF THE AVAILABLE
!           WATER AND NOT THE WHOLE PROFILE.  THE PROBLEM IS THAT IF YOU ASSUME
!           THE WHOLE PROFILE (SCAP) YOU CAN RUN INTO NEGATIVE STORAGE.
!
!IDS           madlev(IP) = 0.01 * (100.0 - mad(ckey)) * scap(IP)

!           CALCULATE THE MAD ONLY FOR THE FIRST YEAR OTHERWISE THE
!           CARRYOVER BETWEEN YEARS DOES NOT WORK

!MAD            IF ( nyr .EQ. 1) THEN 
!MAD               madlev(IP) = 0.01 * (100.0 - mad(ckey)) * storag(IP)
!MAD            ENDIF
!MAD            carry0 = carry0 + (storag(IP)-madlev(IP))*AREA(IB,IP,nyr)
         carry0 = carry0 + storag(IP)*AREA(IB,IP,nyr)
      END DO
      carry0 = carry0 / 12.0

      RETURN
      END
