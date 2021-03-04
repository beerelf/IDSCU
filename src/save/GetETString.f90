CHARACTER*20 FUNCTION GetETString(ET)
  INTEGER ET

  if (ET .eq. 1) THEN
     GetETString = "Blaney-Criddle"
  ELSEIF (ET .eq. 2) THEN
     GetETString = "Penman-Monteith"
  ELSEIF (ET .eq. 3) THEN
     GetETString = "Other Uses"
  ELSEIF (ET .eq. 4) THEN
     GetETString = "Kimbery-Penman"
  ELSEIF (ET .eq. 5) THEN
     GetETString = "Calibrated BC"
  ELSEIF (ET .eq. 6) THEN
     GetETString = "ASCE"
  ELSEIF (ET .eq. 7) THEN
     GetETString = "Hargreaves"
  ELSEIF (ET .eq. 8) THEN
     GetETString = "Pochop"
  ELSEIF (ET .eq. 9) THEN
     GetETString = "User-Supplied ET"
  ELSEIF (ET .eq. 10) THEN
     GetETString = "Penman 1948"
  END IF

  RETURN
END FUNCTION GetETString
