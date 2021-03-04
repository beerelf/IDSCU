# Microsoft Developer Studio Project File - Name="spcu_daily" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=spcu_daily - Win32 Release
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "spcu_daily.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "spcu_daily.mak" CFG="spcu_daily - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "spcu_daily - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "spcu_daily - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "spcu_daily - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir ".\Release"
# PROP BASE Intermediate_Dir ".\Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir ".\Release"
# PROP Intermediate_Dir ".\Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /nologo /I "Release/"
# ADD F90 /check:bounds /check:noflawed_pentium /check:power /check:overflow /compile_only /include:"Release/" /math_library:fast /nologo
# SUBTRACT F90 /check:format /check:output_conversion /check:underflow
# ADD CPP /FD
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib /nologo /stack:0x10000000 /subsystem:console /machine:I386 /out:".\Release/run_cu.exe"
# SUBTRACT LINK32 /pdb:none /debug

!ELSEIF  "$(CFG)" == "spcu_daily - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir ".\spcu_dai"
# PROP BASE Intermediate_Dir ".\spcu_dai"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir ".\spcu_dai"
# PROP Intermediate_Dir ".\spcu_dai"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /debug:full /nologo /I "spcu_dai/"
# ADD F90 /browser /check:bounds /check:noflawed_pentium /check:power /check:overflow /check:underflow /compile_only /debug:full /include:"spcu_dai/" /nologo /traceback /warn:argument_checking /warn:declarations /warn:unused
# SUBTRACT F90 /check:format /check:output_conversion /nopdbfile
# ADD CPP /FR /FD
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386
# ADD LINK32 kernel32.lib /nologo /stack:0x10000000 /subsystem:console /incremental:no /debug /machine:I386 /out:".\debug/run_cu.exe"
# SUBTRACT LINK32 /pdb:none

!ENDIF 

# Begin Target

# Name "spcu_daily - Win32 Release"
# Name "spcu_daily - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;for;f90"
# Begin Source File

SOURCE=.\src\acount.f90
DEP_F90_ACOUN=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\annuacrp.f90
DEP_F90_ANNUA=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\avg_daily.f90
DEP_F90_AVG_D=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\avgmon.f90
DEP_F90_AVGMO=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\bcenhance.f90
DEP_F90_BCENH=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\bcpet.f90
DEP_F90_BCPET=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\bcsmb.f90
DEP_F90_BCSMB=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\bcwborig.f90
DEP_F90_BCWBO=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\budget.f90
DEP_F90_BUDGE=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\calc_cu.f90
DEP_F90_CALC_=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\calc_storage.f90
DEP_F90_CALC_S=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\calpcrop.f90
DEP_F90_CALPC=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\CheckCropCoeffs.f90
DEP_F90_CHECK=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\clndr.f90
DEP_F90_CLNDR=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\crptype.f90
DEP_F90_CRPTY=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\dayhrs.f90
DEP_F90_DAYHR=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\DaysInYear.f90
DEP_F90_DAYSI=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\distr.f90
DEP_F90_DISTR=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\ET_Parameters.f90
# End Source File
# Begin Source File

SOURCE=.\src\etasce.f90
DEP_F90_ETASC=\
	".\spcu_dai\ET_Parameters.mod"\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\etchref.f90
DEP_F90_ETCHR=\
	".\spcu_dai\ET_Parameters.mod"\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\etoref.f90
DEP_F90_ETORE=\
	".\spcu_dai\ET_Parameters.mod"\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\etref.f90
DEP_F90_ETREF=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\etrref.f90
DEP_F90_ETRRE=\
	".\spcu_dai\ET_Parameters.mod"\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\export.f90
DEP_F90_EXPOR=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\fall.f90
DEP_F90_FALL_=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\finput.f90
DEP_F90_FINPU=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\foutput.f90
DEP_F90_FOUTP=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\frost.f90
DEP_F90_FROST=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\fswild.f90
DEP_F90_FSWIL=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\get_maxditch.f90
DEP_F90_GET_M=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\get_maxwell.f90
DEP_F90_GET_MA=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\get_spring_wheat_key.f90
DEP_F90_GET_S=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\GetDP_ETindex.f90
# End Source File
# Begin Source File

SOURCE=.\src\GetETString.f90
# End Source File
# Begin Source File

SOURCE=.\src\globals.f90
# End Source File
# Begin Source File

SOURCE=.\src\growth.f90
DEP_F90_GROWT=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\growth_gdd.f90
DEP_F90_GROWTH=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\interkc.f90
DEP_F90_INTER=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\intertd.f90
DEP_F90_INTERT=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\irgate.f90
DEP_F90_IRGAT=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\IsWSUsed.f90
DEP_F90_ISWSU=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\julian.f90
DEP_F90_JULIA=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\kckp.f90
DEP_F90_KCKP_=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\kckp2.f90
DEP_F90_KCKP2=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\kcpm.f90
DEP_F90_KCPM_=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\kcpm2.f90
DEP_F90_KCPM2=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\kpannual.f90
DEP_F90_KPANN=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\kpgrowth.f90
DEP_F90_KPGRO=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\kpperen.f90
DEP_F90_KPPER=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\LoadUptakeData.f90
DEP_F90_LOADU=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\lstock.f90
DEP_F90_LSTOC=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\minera.f90
DEP_F90_MINER=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\munic.f90
DEP_F90_MUNIC=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\myexit.f90
DEP_F90_MYEXI=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\otable.f90
DEP_F90_OTABL=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\other.f90
DEP_F90_OTHER=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\oweigh.f90
DEP_F90_OWEIG=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\parse_num.f90
# End Source File
# Begin Source File

SOURCE=.\src\perencrp.f90
DEP_F90_PEREN=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\proj.f90
DEP_F90_PROJ_=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\proto.f90
DEP_F90_PROTO=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\rain.f90
DEP_F90_RAIN_=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\read_crop_bc.f90
DEP_F90_READ_=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\read_crop_gdd.f90
DEP_F90_READ_C=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\read_crop_pm.f90
DEP_F90_READ_CR=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\read_crop_pochop.f90
DEP_F90_READ_CRO=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\read_weather.f90
DEP_F90_READ_W=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\ReadCU.f90
DEP_F90_READC=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\recrea.f90
DEP_F90_RECRE=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\reserv.f90
DEP_F90_RESER=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\run_cu.f90
DEP_F90_RUN_C=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\sched.f90
DEP_F90_SCHED=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\skipln.f90
# End Source File
# Begin Source File

SOURCE=.\src\skipn.f90
# End Source File
# Begin Source File

SOURCE=.\src\spdsupply.f90
DEP_F90_SPDSU=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\spring.f90
DEP_F90_SPRIN=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\spsupply.f90
DEP_F90_SPSUP=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\spwellr.f90
DEP_F90_SPWEL=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\spwellw.f90
DEP_F90_SPWELL=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\spwsupply.f90
DEP_F90_SPWSU=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\stable.f90
DEP_F90_STABL=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\stockp.f90
DEP_F90_STOCK=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\summary.f90
DEP_F90_SUMMA=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\supply.f90
DEP_F90_SUPPL=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\table.f90
DEP_F90_TABLE=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\therma.f90
DEP_F90_THERM=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\wbudget.f90
DEP_F90_WBUDG=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\wellpump.f90
DEP_F90_WELLP=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\wellsup.f90
DEP_F90_WELLS=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\wsdist.f90
DEP_F90_WSDIS=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\wsupply.f90
DEP_F90_WSUPP=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\src\xcrain.f90
DEP_F90_XCRAI=\
	".\spcu_dai\Globals.mod"\
	
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;cnt;rtf;gif;jpg;jpeg;jpe"
# End Group
# End Target
# End Project
