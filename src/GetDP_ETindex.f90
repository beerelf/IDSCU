INTEGER FUNCTION GetDP_ETindex (flag, index)

  !***************************************************************************
  !
  !   Function        : GetDP_ETindex
  !   Author          : LA Garcia & D Patterson
  !   Date            : June 2001
  !   Purpose         : This calculates an index that is the combination
  !                     of the ET type and the flag for BC original or 
  !                     enhanced.
  !   Calling programs: wbudget.f, spwellw.f
  !   Called programs : myexit.f 
  !   Input arguments : flag - type of ET method (1 - BC, 2 - PM, 4 - KP, 5 BC Calibrated)
  !                     index - Water budget (Yes - 1, NO - 0)
  !   Output arguments: 
  !   Assumptions     :
  !   Limitations     :
  !   Notes           :
  !
  !***************************************************************************

  !Local variable declaration
  INTEGER flag, index 

  IF( flag .EQ. 1 .AND. index .EQ. 0) THEN
     GetDP_ETindex = 1
  ELSEIF( flag .EQ. 4 .AND. index .EQ. 0) THEN
     GetDP_ETindex = 2
  ELSEIF( flag .EQ. 5 .AND. index .EQ. 0) THEN
     GetDP_ETindex = 3
  ELSEIF( flag .EQ. 2 .AND. index .EQ. 0) THEN
     GetDP_ETindex = 4
  ELSEIF( flag .EQ. 6 .AND. index .EQ. 0) THEN
     GetDP_ETindex = 5
  ELSEIF( flag .EQ. 7 .AND. index .EQ. 0) THEN
     GetDP_ETindex = 6
  ELSEIF( flag .EQ. 8 .AND. index .EQ. 0) THEN
     GetDP_ETindex = 7
  ELSEIF( flag .EQ. 9 .AND. index .EQ. 0) THEN
     GetDP_ETindex = 8
  ELSEIF( flag .EQ. 10 .AND. index .EQ. 0) THEN
     GetDP_ETindex = 9
  ELSEIF( flag .EQ. 1 .AND. index .EQ. 1) THEN
     GetDP_ETindex = 10
  ELSEIF( flag .EQ. 4 .AND. index .EQ. 1) THEN
     GetDP_ETindex = 11
  ELSE IF( flag .EQ. 5 .AND. index .EQ. 1) THEN
     GetDP_ETindex = 12
  ELSE IF( flag .EQ. 2 .AND. index .EQ. 1) THEN
     GetDP_ETindex = 13
  ELSE IF( flag .EQ. 6 .AND. index .EQ. 1) THEN
     GetDP_ETindex = 14
  ELSE IF( flag .EQ. 7 .AND. index .EQ. 1) THEN
     GetDP_ETindex = 15
  ELSE IF( flag .EQ. 8 .AND. index .EQ. 1) THEN
     GetDP_ETindex = 16
  ELSE IF( flag .EQ. 9 .AND. index .EQ. 1) THEN
     GetDP_ETindex = 17
  ELSE IF( flag .EQ. 10 .AND. index .EQ. 1) THEN
     GetDP_ETindex = 18
  ENDIF

  RETURN
END FUNCTION GetDP_ETindex
