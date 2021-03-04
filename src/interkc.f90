SUBROUTINE INTERKC(iper, tmps, day, ckey, &
     !     Outputs
     xf, xkt, xkc)

  !***************************************************************************
  !
  !   Function        : interkc.f 
  !   Author          : HB Manguerra
  !   Date            : December 1994
  !   Purpose         : This calculates the crop coefficient kc, climatic 
  !                   : coefficient kt, and (t x d) / 100 of the current
  !                     month for the annual crops only. 
  !   Calling program : annuacrp.f 
  !   Called programs : none 
  !   Input arguments : iper = current month
  !                     tmps = mean monthly temperature for the month (already
  !                            interpolated for part months).
  !                     day  = daylight hours for the month (already interpo-
  !                            lated for part months).
  !                     ckey = index of crop
  !   Output arguments: none 
  !   Assumptions     :
  !   Limitations     :
  !   Notes           : The routines are based on USBR XCONS2 program which
  !                     uses the SCS Modified Blaney-Criddle ET Estimation
  !                     Method.
  !
  !***************************************************************************

  USE Globals

  !Parameters
  INTEGER iper, ckey
  REAL tmps, day
  !Outputs
  REAL xf(12), xkt(12), xkc(12)

  INTEGER k

  !interporlate monthly kc (15th day of the month)
  DO k=1,AKCLEN
     IF(nckca(ckey,k).GT.nperct(iper)) GO TO 11
     IF(nckca(ckey,k).EQ.nperct(iper)) xkc(iper) = ckca(ckey,k)
     IF(nckca(ckey,k).EQ.nperct(iper)) GO TO 12
  END DO

  !No value match so do not interpolate
  ! set xkc to zero and continue

  xkc(iper) = 0.0
  GOTO 12

11 xkc(iper)=ckca(ckey,k-1)+(ckca(ckey,k)-  &
	ckca(ckey,k-1))* (nperct(iper)         &
	-nckca(ckey,k-1))/(nckca(ckey,k)-nckca(ckey,k-1))

  !calculate f value of the current month.
12 xf(iper)=(tmps*day)/100.0

  !calculate kt value of the current month.
  IF(tmps.LT.36.0) THEN
     xkt(iper)=0.3
  ELSE
     xkt(iper)=0.0173*tmps-0.314
  ENDIF

  RETURN
END SUBROUTINE INTERKC

