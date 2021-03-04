C
C
C   Include File    : xccommon.inc
C   Author          : HB Manguerra
C   Date            : December 1994
C   Purpose         : This includes declaration of all variables used
C                     in the Blaney-Criddle calculation of evapotranspi-
C                     ration.
C   Calling program : annuacrp.f, calpcrop.f, interkc.f, intertd.f,
C                     mainxc.f, perencrp.f, readin.f, xcrain.f, crptyp.f
C   Assumptions     :
C   Limitations     :
C   Notes           :
C
C***************************************************************************
      MODULE XCCOMMON
      SAVE

C-----Dimension (not dependent on global dimension)
      INTEGER AKCLEN    ! annual crops
      PARAMETER (AKCLEN = 21)
      INTEGER PKCLEN    ! perrenial crops
      PARAMETER (PKCLEN = 25)
      INTEGER XC_CRP    ! number of crops considered in xcons2
      PARAMETER (XC_CRP = 23)
      INTEGER DIM_NC1   ! number of crop-soil-date combinations 
      PARAMETER (DIM_NC1 = 999)


      REAL pclite(12), xf(12), xkt(12), xkc(12)
      REAL ckca(xc_crp,pkclen), ckcp(xc_crp,akclen)
      REAL days, dayf, temps, tempf

      INTEGER midpts, midptf
      INTEGER nyr, typepre, numyrs, nucrps
      INTEGER nckca(xc_crp,pkclen), nckcp(xc_crp,akclen)
      INTEGER, ALLOCATABLE :: ncrop(:), ngrows(:) ! DIM_NC1
      INTEGER naccum(12), nperct(12)
      CHARACTER*40, ALLOCATABLE :: CROP1(:) ! DIM_NC1

      END MODULE
