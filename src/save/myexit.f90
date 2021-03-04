      SUBROUTINE MYEXIT(errndx)

!***************************************************************************
!
!   Function        : myexit.f
!   Author          : HB Manguerra
!   Date            : December 1994
!   Purpose         : This implements the exit routines when the program is 
!                     about to stop.  The history of execution is saved
!                     in *.log file.
!   Calling program : annuacrp.f, frost.f, kbasal.f, monthly.f, perencrp.f,
!                     proto.f, rain.f, distr.f, indece.f, supply.f, julian.f
!   Called programs : none 
!   Input arguments : errndx = error code 
!   Output arguments: none
!   Assumptions     :
!   Limitations     :
!   Notes           : Currently, error code is limited to two: run is
!                     successful; and errors are detected.  Eventually, the
!                     program should be able to specifically identify what
!                     kind of error was detected.
!
!   History         : (Date, Author, Description)
!      11/01/95   HBM      Error messages 1-13 incorporated.     
!
!***************************************************************************

      USE Globals

      INTEGER errndx
      CHARACTER*120 dfile1
      CHARACTER*140 errmsg(202)

      errmsg(1) = 'File *.cmn was not found!'
      errmsg(2) = 'File *.wm was not found!'
      errmsg(3) = 'File *.wd was not found!'
      errmsg(4) = 'File *.kpm was not found!'
      errmsg(5) = 'File *.kbc was not found!'
      errmsg(6) = 'File *.sup was not found!'
      errmsg(7) = 'File *.cmn was empty!'
      errmsg(8) = 'File *.cmn does not have correct data format!'
      errmsg(9) = 'File *.wm does not have correct data format!'
      errmsg(10) = 'File *.wd does not have correct data format!'
      errmsg(11) = 'File *.kpm does not have correct data format!'
      errmsg(12) = 'File *.kbc does not have correct data format!'
      errmsg(13) = 'File *.sup does not have correct data format!'

      errmsg(14) = 'File *.cmn problem with scenario years'
      errmsg(15) = 'File *.cmn problem with simulation years'
      errmsg(16) = 'File *.cmn problem with number of basins'
      errmsg(17) = 'File *.cmn problem with ET methods flags'
      errmsg(18) = 'File *.cmn problem with effective rainfall flag'
      errmsg(19) = 'File *.cmn problem with effective rainfall param.'
      errmsg(20) = 'File *.cmn problem with supply flag'
      errmsg(21) = 'File *.cmn problem with output options'
      errmsg(22) = 'File *.cmn problem with snow melt efficiency'
      errmsg(23) = 'File *.cmn problem with crop name title'
      errmsg(24) = 'File *.cmn problem with number of crops'
      errmsg(25) = 'File *.cmn problem with crop name or soil name'
      errmsg(26) = 'File *.cmn problem with crop data title'
      errmsg(27) = 'File *.cmn problem with alfalfa crop data'
      errmsg(28) = 'File *.cmn problem with crop data not alfalfa'
      errmsg(29) = 'File *.cmn problem with basin data title'
      errmsg(30) = 'File *.cmn problem with basin id data'
      errmsg(31) = 'File *.cmn problem with basin data'

      errmsg(32) = 'File *.cmn wrong fall frost method for winter wheat'
      errmsg(33) = 'File *.cmn wrong spring frost method for spg wheat'
      errmsg(34) = 'File *.cmn wrong spring frost method'
      errmsg(35) = 'File *.cmn wrong fall frost method'

!23456789012345678901234567890123456789012345678901234567890123456789012

      errmsg(36) = 'File *.wm simulation years exceed .cmn years'
      errmsg(37) = 'File *.wm # of basins does not match .cmn # basins'
      errmsg(38) = 'File *.wm problem with sim. years or # basins'
      errmsg(39) = 'File *.wm problem remarks for temperature weights'
      errmsg(40) = 'File *.wm problem temperature weights' 
      errmsg(41) = 'File *.wm problem remarks for precipiation weights'
      errmsg(42) = 'File *.wm problem precipitation weights' 
      errmsg(43) = 'File *.wm problem temperature station name' 
      errmsg(44) = 'File *.wm problem temperature station remark' 
      errmsg(45) = 'File *.wm problem temperature stat. lat,long,elev' 
      errmsg(46) = 'File *.wm problem weather data year' 
      errmsg(47) = 'File *.wm problem weather data comment' 
      errmsg(48) = 'File *.wm problem weather data' 
	  
      errmsg(49) = 'File *.wd problem with sim. years or # basins'
      errmsg(50) = 'File *.wd simulation years exceed .cmn years'
      errmsg(51) = 'File *.wd # of basins does not match .cmn # basins'
      errmsg(52) = 'File *.wd problem remarks for temperature weights'
      errmsg(53) = 'File *.wd problem temperature weights' 
      errmsg(54) = 'File *.wd problem remarks for precipiation weights'
      errmsg(55) = 'File *.wd problem precipitation weights' 
      errmsg(56) = 'File *.wd problem temperature station name' 
      errmsg(57) = 'File *.wd problem temperature station remark' 
      errmsg(58) = 'File *.wd problem temperature stat. lat,long,elev' 

      errmsg(59) = 'Latitude of a sub-basin is not valid '

      errmsg(60) = 'File *.sup does not match .cmn # sub-basin '
      errmsg(61) = 'File *.sup problem with simulation years'

      errmsg(70) = 'Error with weather data. No temperature for a month needed to compute the fall cutoff'
      errmsg(71) = 'Error with converting a day to julian date'
      errmsg(72) = 'Error with converting a month to julian date'
      errmsg(73) = 'Error with weather data. No temperature for a month needed to compute the spring cutoff'
      errmsg(74) = 'Error with weather data. No frost date for a month used in the computations'  

      errmsg(80) = 'Error with water balance in original BC'

      errmsg(90) = 'File *.wkp is not found!'
      errmsg(91) = 'File debug1 could not be opened'
      errmsg(92) = 'Perenial crop date does not match middle of month'
      errmsg(93) = 'Crop name not recognized'
      errmsg(94) = 'No crop name provided for this parcel'
      errmsg(95) = 'Illegal crop type.'

      errmsg(99) = 'Type of error not identified!'
      errmsg(100) = ''

      errmsg(101) = 'File *.sup not found'
      errmsg(102) = 'File *.wel was not found'
      errmsg(103) = 'File *.sup problem with simulation years'
      errmsg(104) = 'File *.sup # basins does not match .cmn # basins'
      errmsg(105) = 'File *.sup error with basin name or # of ditches'
      errmsg(106) = 'File *.sup error with water supply values'
      errmsg(107) = 'File *.sup error # basins or simulation years' 
      errmsg(108) = 'File *.wel error # basins or simulation years' 
      errmsg(109) = 'File *.wel problem with simulation years'
      errmsg(110) = 'File *.wel # basins does not match .cmn # basins'
      errmsg(111) = 'File *.wel error with basin name or # of wells'
      errmsg(112) = 'File *.wel error with water supply values'

      errmsg(113) = 'File *.kref was not found!'
      errmsg(114) = 'File *.kref does not have correct data format!'

      errmsg(115) = 'End of file tmp1 was found'
      errmsg(116) = 'Error opening tmp1 file'

      errmsg(117) = 'Error in KP determining first month temperature'
      errmsg(118) = 'Error in KP determining last month temperature'

      errmsg(119) = 'Error in BC determining first month temperature'
      errmsg(120) = 'Error in BC determining last month temperature'

      errmsg(121) = 'Error in KP determining first month temperature'
      errmsg(122) = 'Error in KP determining last month temperature'
      errmsg(123) = 'Error in KP determining middle month temperature'

      errmsg(124) = 'The application efficiency needs to be greater than 0.01 (1%)'
      errmsg(125) = 'The application efficiency cannot be greater than 1 (100%)' 
      
      errmsg(126) = 'The average temperature for at least one month in the Blaney-Criddle data is zero, this is not allowed'
      errmsg(127) = 'The average temperature for at least one month in the Kimberly Penman data is zero, this is not allowed'

      errmsg(128) = 'The sum of the weights of the weather stations do not add to 1'

      errmsg(129) = 'User supplied well pumping with zero efficiency'

      errmsg(130) = 'You cannot mix user supplied well pumping and computed well pumping'

      errmsg(131) = 'Error with crop coefficients. The crop coefficient for the beginning of the season could not be determined'

      errmsg(132) = 'Error reading the version number line in the common file'

      errmsg(133) = 'Simulation years are not within the dataset years'
      errmsg(140) = 'Planting month is 0 or greater than 12'
      errmsg(141) = 'Planting day is 0 or greater than 31'
      errmsg(142) = 'Harvest month is 0 or greater than 12'
      errmsg(143) = 'Harvest day is 0 or greater than 31'
      errmsg(144) = 'Days to full covers are either 0 or greater than 365' 
      errmsg(145) = 'Length of season is either 0 or greater than 365'
      errmsg(146) = 'Crop ID error.'
      errmsg(147) = 'Soil ID error.'

      errmsg(150) = 'BC Popchop can only be run for Grass or Alfalfa'
      errmsg(151) = 'BC Popchop has an elevation adjustment and you must provide an elevation for the basin.'

      errmsg(160) = 'Error w/ format of Leaf Area Index file (crop.lai)'
      errmsg(161) = 'Leaf Area Index file (crop.lai) does not exist'

      errmsg(162) = 'Stomata Resistance file (crop.sr) does not exit'
      errmsg(163) = 'Error w/ format of Stomata Res. file (crop.sr)'

      errmsg(164) = 'Alfalfa Constant LAI file (crop.alf) does not exit'
      errmsg(165) = 'Error w/ format of Alfalfa LAI file (crop.alf)'
      errmsg(166) = 'Failed to open User ET file'
      errmsg(167) = 'Illegal user-supplied ET provided'
      errmsg(168) = 'No fields provided for any modeling area.'
      errmsg(169) = 'Unable to read frost date ignore flag.'
      errmsg(170) = 'Unable to read Kc overrides Kt for BC flag.'
      errmsg(201) = 'File *.cmn problem with DP & RP monthly % values'

!     flen = index(dfile,' ')
      dfile1 = dfile
      dfile1(flen:flen+4) = '.log'

      OPEN(UNIT=999,FILE=dfile1)
      IF (errndx.eq.0) then
	 IF (useCropMode) THEN
	    PRINT *, "**Used Crop Mode**"
	 END IF
         WRITE(999,900)
         WRITE(*,900)
      elseIF (errndx.eq.-1) then
         WRITE(999,901)
         WRITE(*,901)
      else
         WRITE(999,901)
         WRITE(*,901)
	 WRITE(999,902) errndx,errmsg(errndx)
	 WRITE(*,902) errndx,errmsg(errndx)

	 stop '1'
      endif

 900  FORMAT(1x,'run successful - program ended normally')
 901  FORMAT(1x,'error encountered - program aborted')
 902  FORMAT(1x,'Error ',I2,'. ->'2x,A100)
      STOP
      END
