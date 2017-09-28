# Microsoft Developer Studio Project File - Name="a1" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 5.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=a1 - Win32 PVM Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "a1.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "a1.mak" CFG="a1 - Win32 PVM Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "a1 - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "a1 - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE "a1 - Win32 PVM Debug" (based on "Win32 (x86) Console Application")
!MESSAGE "a1 - Win32 PVM Release" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "a1 - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /Ob2 /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D "_MBCS" /D "TIMING" /FR /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386 /out:"Release/Subdue.exe"
# SUBTRACT LINK32 /profile

!ELSEIF  "$(CFG)" == "a1 - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /Gm /GX /Zi /Od /D "_DEBUG" /D "WIN32" /D "_CONSOLE" /D "_MBCS" /FR /YX /FD /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /out:"Debug/Subdue.exe"
# SUBTRACT LINK32 /profile

!ELSEIF  "$(CFG)" == "a1 - Win32 PVM Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "PVM Debug"
# PROP BASE Intermediate_Dir "PVM Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "PVM_Debug"
# PROP Intermediate_Dir "PVM_Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /Gm /GX /Zi /Od /I "C:\Program Files\pvm3.4\include" /I "C:\Program Files\pvm3.4\tracer" /I "C:\Program Files\pvm3.4\src" /D "_DEBUG" /D "_PVM_SUBDUE_" /D "WIN32" /D "_CONSOLE" /D "_MBCS" /FR /YX /FD /c
# SUBTRACT CPP /u
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 oldnames.lib kernel32.lib libcpmtd.lib wsock32.lib libc.lib gdi32.lib user32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib libpvm3.lib libgpvm3.lib /nologo /version:4.2 /subsystem:console /debug /machine:I386 /nodefaultlib /out:"PVM_Debug/PVMSubdue.exe" /pdbtype:sept
# Begin Special Build Tool
SOURCE=$(InputPath)
PostBuild_Cmds=copy                    PVM_Release\PVMSubdue.exe \
                                 %PVM_ROOT%\bin\%PVM_ARCH%\ 
# End Special Build Tool

!ELSEIF  "$(CFG)" == "a1 - Win32 PVM Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "PVM Release"
# PROP BASE Intermediate_Dir "PVM Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "PVM_Release"
# PROP Intermediate_Dir "PVM_Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /I "C:\Program Files\pvm3.4\include" /I "C:\Program Files\pvm3.4\tracer" /I "C:\Program Files\pvm3.4\src" /D "NDEBUG" /D "_PVM_SUBDUE_" /D "WIN32" /D "_CONSOLE" /D "_MBCS" /FR /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 oldnames.lib kernel32.lib libcpmtd.lib wsock32.lib libc.lib gdi32.lib user32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib libpvm3.lib libgpvm3.lib /nologo /subsystem:console /machine:I386 /nodefaultlib /out:"PVM_Release/PVMSubdue.exe"
# SUBTRACT LINK32 /profile
# Begin Special Build Tool
SOURCE=$(InputPath)
PostBuild_Cmds=copy                    PVM_Release\PVMSubdue.exe \
                                 %PVM_ROOT%\bin\%PVM_ARCH%\ 
# End Special Build Tool

!ENDIF 

# Begin Target

# Name "a1 - Win32 Release"
# Name "a1 - Win32 Debug"
# Name "a1 - Win32 PVM Debug"
# Name "a1 - Win32 PVM Release"
# Begin Group "Source files"

# PROP Default_Filter "c"
# Begin Source File

SOURCE=.\abstract.c
# End Source File
# Begin Source File

SOURCE=.\cluster.c
# End Source File
# Begin Source File

SOURCE=.\compress.c
# End Source File
# Begin Source File

SOURCE=.\concept.c
# End Source File
# Begin Source File

SOURCE=.\dl.c
# End Source File
# Begin Source File

SOURCE=.\eval.c
# End Source File
# Begin Source File

SOURCE=.\extemp.c
# End Source File
# Begin Source File

SOURCE=.\extend.c
# End Source File
# Begin Source File

SOURCE=.\fuzzymat.c
# End Source File
# Begin Source File

SOURCE=.\graphop.c
# End Source File
# Begin Source File

SOURCE=.\labels.c
# End Source File
# Begin Source File

SOURCE=.\main.c
# End Source File
# Begin Source File

SOURCE=.\matchq.c
# End Source File
# Begin Source File

SOURCE=.\maths.c
# End Source File
# Begin Source File

SOURCE=.\prntstct.c
# End Source File
# Begin Source File

SOURCE=.\pvm.c
# End Source File
# Begin Source File

SOURCE=.\readgrph.c
# End Source File
# Begin Source File

SOURCE=.\rvrsesub.c
# End Source File
# Begin Source File

SOURCE=.\subdue.c
# End Source File
# Begin Source File

SOURCE=.\subgphop.c
# End Source File
# Begin Source File

SOURCE=.\subsop.c
# End Source File
# Begin Source File

SOURCE=.\tempop.c
# End Source File
# End Group
# Begin Group "Header files"

# PROP Default_Filter "h"
# Begin Source File

SOURCE=.\cluster.h
# End Source File
# Begin Source File

SOURCE=.\subdue.h
# End Source File
# End Group
# Begin Source File

SOURCE=.\Readme
# End Source File
# End Target
# End Project
