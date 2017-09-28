# Microsoft Developer Studio Generated NMAKE File, Based on d4R.dsp
!IF "$(CFG)" == ""
CFG=d4R - Win32 Release
!MESSAGE No configuration specified. Defaulting to d4R - Win32 PVM Debug.
!ENDIF 

!IF "$(CFG)" != "d4R - Win32 Release" && "$(CFG)" != "d4R - Win32 Debug" &&\
 "$(CFG)" != "d4R - Win32 PVM Debug" && "$(CFG)" != "d4R - Win32 PVM Release"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "d4R.mak" CFG="d4R - Win32 PVM Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "d4R - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "d4R - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE "d4R - Win32 PVM Debug" (based on "Win32 (x86) Console Application")
!MESSAGE "d4R - Win32 PVM Release" (based on "Win32 (x86) Console Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

!IF  "$(CFG)" == "d4R - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\Subdue.exe" "$(OUTDIR)\d4R.bsc"

!ELSE 

ALL : "$(OUTDIR)\Subdue.exe" "$(OUTDIR)\d4R.bsc"

!ENDIF 

CLEAN :
	-@erase "$(INTDIR)\abstract.obj"
	-@erase "$(INTDIR)\abstract.sbr"
	-@erase "$(INTDIR)\cluster.obj"
	-@erase "$(INTDIR)\cluster.sbr"
	-@erase "$(INTDIR)\compress.obj"
	-@erase "$(INTDIR)\compress.sbr"
	-@erase "$(INTDIR)\concept.obj"
	-@erase "$(INTDIR)\concept.sbr"
	-@erase "$(INTDIR)\dl.obj"
	-@erase "$(INTDIR)\dl.sbr"
	-@erase "$(INTDIR)\eval.obj"
	-@erase "$(INTDIR)\eval.sbr"
	-@erase "$(INTDIR)\extemp.obj"
	-@erase "$(INTDIR)\extemp.sbr"
	-@erase "$(INTDIR)\extend.obj"
	-@erase "$(INTDIR)\extend.sbr"
	-@erase "$(INTDIR)\fuzzymat.obj"
	-@erase "$(INTDIR)\fuzzymat.sbr"
	-@erase "$(INTDIR)\graphop.obj"
	-@erase "$(INTDIR)\graphop.sbr"
	-@erase "$(INTDIR)\labels.obj"
	-@erase "$(INTDIR)\labels.sbr"
	-@erase "$(INTDIR)\main.obj"
	-@erase "$(INTDIR)\main.sbr"
	-@erase "$(INTDIR)\matchq.obj"
	-@erase "$(INTDIR)\matchq.sbr"
	-@erase "$(INTDIR)\maths.obj"
	-@erase "$(INTDIR)\maths.sbr"
	-@erase "$(INTDIR)\prntstct.obj"
	-@erase "$(INTDIR)\prntstct.sbr"
	-@erase "$(INTDIR)\pvm.obj"
	-@erase "$(INTDIR)\pvm.sbr"
	-@erase "$(INTDIR)\readgrph.obj"
	-@erase "$(INTDIR)\readgrph.sbr"
	-@erase "$(INTDIR)\rvrsesub.obj"
	-@erase "$(INTDIR)\rvrsesub.sbr"
	-@erase "$(INTDIR)\subdue.obj"
	-@erase "$(INTDIR)\subdue.sbr"
	-@erase "$(INTDIR)\subgphop.obj"
	-@erase "$(INTDIR)\subgphop.sbr"
	-@erase "$(INTDIR)\subsop.obj"
	-@erase "$(INTDIR)\subsop.sbr"
	-@erase "$(INTDIR)\tempop.obj"
	-@erase "$(INTDIR)\tempop.sbr"
	-@erase "$(INTDIR)\vc50.idb"
	-@erase "$(OUTDIR)\d4R.bsc"
	-@erase "$(OUTDIR)\Subdue.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /ML /W3 /GX /O2 /Ob2 /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D\
 "_MBCS" /FR"$(INTDIR)\\" /Fp"$(INTDIR)\d4R.pch" /YX /Fo"$(INTDIR)\\"\
 /Fd"$(INTDIR)\\" /FD /c 
CPP_OBJS=.\Release/
CPP_SBRS=.\Release/

.c{$(CPP_OBJS)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(CPP_OBJS)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(CPP_OBJS)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(CPP_SBRS)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(CPP_SBRS)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(CPP_SBRS)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

RSC=rc.exe
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\d4R.bsc" 
BSC32_SBRS= \
	"$(INTDIR)\abstract.sbr" \
	"$(INTDIR)\cluster.sbr" \
	"$(INTDIR)\compress.sbr" \
	"$(INTDIR)\concept.sbr" \
	"$(INTDIR)\dl.sbr" \
	"$(INTDIR)\eval.sbr" \
	"$(INTDIR)\extemp.sbr" \
	"$(INTDIR)\extend.sbr" \
	"$(INTDIR)\fuzzymat.sbr" \
	"$(INTDIR)\graphop.sbr" \
	"$(INTDIR)\labels.sbr" \
	"$(INTDIR)\main.sbr" \
	"$(INTDIR)\matchq.sbr" \
	"$(INTDIR)\maths.sbr" \
	"$(INTDIR)\prntstct.sbr" \
	"$(INTDIR)\pvm.sbr" \
	"$(INTDIR)\readgrph.sbr" \
	"$(INTDIR)\rvrsesub.sbr" \
	"$(INTDIR)\subdue.sbr" \
	"$(INTDIR)\subgphop.sbr" \
	"$(INTDIR)\subsop.sbr" \
	"$(INTDIR)\tempop.sbr"

"$(OUTDIR)\d4R.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib /nologo /subsystem:console /incremental:no\
 /pdb:"$(OUTDIR)\Subdue.pdb" /machine:I386 /out:"$(OUTDIR)\Subdue.exe" 
LINK32_OBJS= \
	"$(INTDIR)\abstract.obj" \
	"$(INTDIR)\cluster.obj" \
	"$(INTDIR)\compress.obj" \
	"$(INTDIR)\concept.obj" \
	"$(INTDIR)\dl.obj" \
	"$(INTDIR)\eval.obj" \
	"$(INTDIR)\extemp.obj" \
	"$(INTDIR)\extend.obj" \
	"$(INTDIR)\fuzzymat.obj" \
	"$(INTDIR)\graphop.obj" \
	"$(INTDIR)\labels.obj" \
	"$(INTDIR)\main.obj" \
	"$(INTDIR)\matchq.obj" \
	"$(INTDIR)\maths.obj" \
	"$(INTDIR)\prntstct.obj" \
	"$(INTDIR)\pvm.obj" \
	"$(INTDIR)\readgrph.obj" \
	"$(INTDIR)\rvrsesub.obj" \
	"$(INTDIR)\subdue.obj" \
	"$(INTDIR)\subgphop.obj" \
	"$(INTDIR)\subsop.obj" \
	"$(INTDIR)\tempop.obj"

"$(OUTDIR)\Subdue.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "d4R - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\Subdue.exe" "$(OUTDIR)\d4R.bsc"

!ELSE 

ALL : "$(OUTDIR)\Subdue.exe" "$(OUTDIR)\d4R.bsc"

!ENDIF 

CLEAN :
	-@erase "$(INTDIR)\abstract.obj"
	-@erase "$(INTDIR)\abstract.sbr"
	-@erase "$(INTDIR)\cluster.obj"
	-@erase "$(INTDIR)\cluster.sbr"
	-@erase "$(INTDIR)\compress.obj"
	-@erase "$(INTDIR)\compress.sbr"
	-@erase "$(INTDIR)\concept.obj"
	-@erase "$(INTDIR)\concept.sbr"
	-@erase "$(INTDIR)\dl.obj"
	-@erase "$(INTDIR)\dl.sbr"
	-@erase "$(INTDIR)\eval.obj"
	-@erase "$(INTDIR)\eval.sbr"
	-@erase "$(INTDIR)\extemp.obj"
	-@erase "$(INTDIR)\extemp.sbr"
	-@erase "$(INTDIR)\extend.obj"
	-@erase "$(INTDIR)\extend.sbr"
	-@erase "$(INTDIR)\fuzzymat.obj"
	-@erase "$(INTDIR)\fuzzymat.sbr"
	-@erase "$(INTDIR)\graphop.obj"
	-@erase "$(INTDIR)\graphop.sbr"
	-@erase "$(INTDIR)\labels.obj"
	-@erase "$(INTDIR)\labels.sbr"
	-@erase "$(INTDIR)\main.obj"
	-@erase "$(INTDIR)\main.sbr"
	-@erase "$(INTDIR)\matchq.obj"
	-@erase "$(INTDIR)\matchq.sbr"
	-@erase "$(INTDIR)\maths.obj"
	-@erase "$(INTDIR)\maths.sbr"
	-@erase "$(INTDIR)\prntstct.obj"
	-@erase "$(INTDIR)\prntstct.sbr"
	-@erase "$(INTDIR)\pvm.obj"
	-@erase "$(INTDIR)\pvm.sbr"
	-@erase "$(INTDIR)\readgrph.obj"
	-@erase "$(INTDIR)\readgrph.sbr"
	-@erase "$(INTDIR)\rvrsesub.obj"
	-@erase "$(INTDIR)\rvrsesub.sbr"
	-@erase "$(INTDIR)\subdue.obj"
	-@erase "$(INTDIR)\subdue.sbr"
	-@erase "$(INTDIR)\subgphop.obj"
	-@erase "$(INTDIR)\subgphop.sbr"
	-@erase "$(INTDIR)\subsop.obj"
	-@erase "$(INTDIR)\subsop.sbr"
	-@erase "$(INTDIR)\tempop.obj"
	-@erase "$(INTDIR)\tempop.sbr"
	-@erase "$(INTDIR)\vc50.idb"
	-@erase "$(INTDIR)\vc50.pdb"
	-@erase "$(OUTDIR)\d4R.bsc"
	-@erase "$(OUTDIR)\Subdue.exe"
	-@erase "$(OUTDIR)\Subdue.ilk"
	-@erase "$(OUTDIR)\Subdue.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MLd /W3 /Gm /GX /Zi /Od /D "_DEBUG" /D "WIN32" /D "_CONSOLE"\
 /D "_MBCS" /FR"$(INTDIR)\\" /Fp"$(INTDIR)\d4R.pch" /YX /Fo"$(INTDIR)\\"\
 /Fd"$(INTDIR)\\" /FD /c 
CPP_OBJS=.\Debug/
CPP_SBRS=.\Debug/

.c{$(CPP_OBJS)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(CPP_OBJS)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(CPP_OBJS)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(CPP_SBRS)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(CPP_SBRS)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(CPP_SBRS)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

RSC=rc.exe
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\d4R.bsc" 
BSC32_SBRS= \
	"$(INTDIR)\abstract.sbr" \
	"$(INTDIR)\cluster.sbr" \
	"$(INTDIR)\compress.sbr" \
	"$(INTDIR)\concept.sbr" \
	"$(INTDIR)\dl.sbr" \
	"$(INTDIR)\eval.sbr" \
	"$(INTDIR)\extemp.sbr" \
	"$(INTDIR)\extend.sbr" \
	"$(INTDIR)\fuzzymat.sbr" \
	"$(INTDIR)\graphop.sbr" \
	"$(INTDIR)\labels.sbr" \
	"$(INTDIR)\main.sbr" \
	"$(INTDIR)\matchq.sbr" \
	"$(INTDIR)\maths.sbr" \
	"$(INTDIR)\prntstct.sbr" \
	"$(INTDIR)\pvm.sbr" \
	"$(INTDIR)\readgrph.sbr" \
	"$(INTDIR)\rvrsesub.sbr" \
	"$(INTDIR)\subdue.sbr" \
	"$(INTDIR)\subgphop.sbr" \
	"$(INTDIR)\subsop.sbr" \
	"$(INTDIR)\tempop.sbr"

"$(OUTDIR)\d4R.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib /nologo /subsystem:console /incremental:yes\
 /pdb:"$(OUTDIR)\Subdue.pdb" /debug /machine:I386 /out:"$(OUTDIR)\Subdue.exe" 
LINK32_OBJS= \
	"$(INTDIR)\abstract.obj" \
	"$(INTDIR)\cluster.obj" \
	"$(INTDIR)\compress.obj" \
	"$(INTDIR)\concept.obj" \
	"$(INTDIR)\dl.obj" \
	"$(INTDIR)\eval.obj" \
	"$(INTDIR)\extemp.obj" \
	"$(INTDIR)\extend.obj" \
	"$(INTDIR)\fuzzymat.obj" \
	"$(INTDIR)\graphop.obj" \
	"$(INTDIR)\labels.obj" \
	"$(INTDIR)\main.obj" \
	"$(INTDIR)\matchq.obj" \
	"$(INTDIR)\maths.obj" \
	"$(INTDIR)\prntstct.obj" \
	"$(INTDIR)\pvm.obj" \
	"$(INTDIR)\readgrph.obj" \
	"$(INTDIR)\rvrsesub.obj" \
	"$(INTDIR)\subdue.obj" \
	"$(INTDIR)\subgphop.obj" \
	"$(INTDIR)\subsop.obj" \
	"$(INTDIR)\tempop.obj"

"$(OUTDIR)\Subdue.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "d4R - Win32 PVM Debug"

OUTDIR=.\PVM_Debug
INTDIR=.\PVM_Debug
# Begin Custom Macros
OutDir=.\PVM_Debug
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\PVMSubdue.exe" "$(OUTDIR)\d4R.bsc"

!ELSE 

ALL : "$(OUTDIR)\PVMSubdue.exe" "$(OUTDIR)\d4R.bsc"

!ENDIF 

CLEAN :
	-@erase "$(INTDIR)\abstract.obj"
	-@erase "$(INTDIR)\abstract.sbr"
	-@erase "$(INTDIR)\cluster.obj"
	-@erase "$(INTDIR)\cluster.sbr"
	-@erase "$(INTDIR)\compress.obj"
	-@erase "$(INTDIR)\compress.sbr"
	-@erase "$(INTDIR)\concept.obj"
	-@erase "$(INTDIR)\concept.sbr"
	-@erase "$(INTDIR)\dl.obj"
	-@erase "$(INTDIR)\dl.sbr"
	-@erase "$(INTDIR)\eval.obj"
	-@erase "$(INTDIR)\eval.sbr"
	-@erase "$(INTDIR)\extemp.obj"
	-@erase "$(INTDIR)\extemp.sbr"
	-@erase "$(INTDIR)\extend.obj"
	-@erase "$(INTDIR)\extend.sbr"
	-@erase "$(INTDIR)\fuzzymat.obj"
	-@erase "$(INTDIR)\fuzzymat.sbr"
	-@erase "$(INTDIR)\graphop.obj"
	-@erase "$(INTDIR)\graphop.sbr"
	-@erase "$(INTDIR)\labels.obj"
	-@erase "$(INTDIR)\labels.sbr"
	-@erase "$(INTDIR)\main.obj"
	-@erase "$(INTDIR)\main.sbr"
	-@erase "$(INTDIR)\matchq.obj"
	-@erase "$(INTDIR)\matchq.sbr"
	-@erase "$(INTDIR)\maths.obj"
	-@erase "$(INTDIR)\maths.sbr"
	-@erase "$(INTDIR)\prntstct.obj"
	-@erase "$(INTDIR)\prntstct.sbr"
	-@erase "$(INTDIR)\pvm.obj"
	-@erase "$(INTDIR)\pvm.sbr"
	-@erase "$(INTDIR)\readgrph.obj"
	-@erase "$(INTDIR)\readgrph.sbr"
	-@erase "$(INTDIR)\rvrsesub.obj"
	-@erase "$(INTDIR)\rvrsesub.sbr"
	-@erase "$(INTDIR)\subdue.obj"
	-@erase "$(INTDIR)\subdue.sbr"
	-@erase "$(INTDIR)\subgphop.obj"
	-@erase "$(INTDIR)\subgphop.sbr"
	-@erase "$(INTDIR)\subsop.obj"
	-@erase "$(INTDIR)\subsop.sbr"
	-@erase "$(INTDIR)\tempop.obj"
	-@erase "$(INTDIR)\tempop.sbr"
	-@erase "$(INTDIR)\vc50.idb"
	-@erase "$(INTDIR)\vc50.pdb"
	-@erase "$(OUTDIR)\d4R.bsc"
	-@erase "$(OUTDIR)\PVMSubdue.exe"
	-@erase "$(OUTDIR)\PVMSubdue.ilk"
	-@erase "$(OUTDIR)\PVMSubdue.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MLd /W3 /Gm /GX /Zi /Od /I "C:\Program Files\pvm3.4\include"\
 /I "C:\Program Files\pvm3.4\tracer" /I "C:\Program Files\pvm3.4\src" /D\
 "_DEBUG" /D "_PVM_SUBDUE_" /D "WIN32" /D "_CONSOLE" /D "_MBCS" /FR"$(INTDIR)\\"\
 /Fp"$(INTDIR)\d4R.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
CPP_OBJS=.\PVM_Debug/
CPP_SBRS=.\PVM_Debug/

.c{$(CPP_OBJS)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(CPP_OBJS)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(CPP_OBJS)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(CPP_SBRS)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(CPP_SBRS)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(CPP_SBRS)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

RSC=rc.exe
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\d4R.bsc" 
BSC32_SBRS= \
	"$(INTDIR)\abstract.sbr" \
	"$(INTDIR)\cluster.sbr" \
	"$(INTDIR)\compress.sbr" \
	"$(INTDIR)\concept.sbr" \
	"$(INTDIR)\dl.sbr" \
	"$(INTDIR)\eval.sbr" \
	"$(INTDIR)\extemp.sbr" \
	"$(INTDIR)\extend.sbr" \
	"$(INTDIR)\fuzzymat.sbr" \
	"$(INTDIR)\graphop.sbr" \
	"$(INTDIR)\labels.sbr" \
	"$(INTDIR)\main.sbr" \
	"$(INTDIR)\matchq.sbr" \
	"$(INTDIR)\maths.sbr" \
	"$(INTDIR)\prntstct.sbr" \
	"$(INTDIR)\pvm.sbr" \
	"$(INTDIR)\readgrph.sbr" \
	"$(INTDIR)\rvrsesub.sbr" \
	"$(INTDIR)\subdue.sbr" \
	"$(INTDIR)\subgphop.sbr" \
	"$(INTDIR)\subsop.sbr" \
	"$(INTDIR)\tempop.sbr"

"$(OUTDIR)\d4R.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
LINK32_FLAGS=oldnames.lib kernel32.lib libcpmtd.lib wsock32.lib libc.lib\
 gdi32.lib user32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib\
 ole32.lib oleaut32.lib uuid.lib libpvm3.lib libgpvm3.lib /nologo /version:4.2\
 /subsystem:console /incremental:yes /pdb:"$(OUTDIR)\PVMSubdue.pdb" /debug\
 /machine:I386 /nodefaultlib /out:"$(OUTDIR)\PVMSubdue.exe" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\abstract.obj" \
	"$(INTDIR)\cluster.obj" \
	"$(INTDIR)\compress.obj" \
	"$(INTDIR)\concept.obj" \
	"$(INTDIR)\dl.obj" \
	"$(INTDIR)\eval.obj" \
	"$(INTDIR)\extemp.obj" \
	"$(INTDIR)\extend.obj" \
	"$(INTDIR)\fuzzymat.obj" \
	"$(INTDIR)\graphop.obj" \
	"$(INTDIR)\labels.obj" \
	"$(INTDIR)\main.obj" \
	"$(INTDIR)\matchq.obj" \
	"$(INTDIR)\maths.obj" \
	"$(INTDIR)\prntstct.obj" \
	"$(INTDIR)\pvm.obj" \
	"$(INTDIR)\readgrph.obj" \
	"$(INTDIR)\rvrsesub.obj" \
	"$(INTDIR)\subdue.obj" \
	"$(INTDIR)\subgphop.obj" \
	"$(INTDIR)\subsop.obj" \
	"$(INTDIR)\tempop.obj"

"$(OUTDIR)\PVMSubdue.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

SOURCE=$(InputPath)
DS_POSTBUILD_DEP=$(INTDIR)\postbld.dep

ALL : $(DS_POSTBUILD_DEP)

# Begin Custom Macros
OutDir=.\PVM_Debug
# End Custom Macros

$(DS_POSTBUILD_DEP) : "$(OUTDIR)\PVMSubdue.exe" "$(OUTDIR)\d4R.bsc"
   copy     PVM_Release\PVMSubdue.exe     %PVM_ROOT%\bin\%PVM_ARCH%\
	echo Helper for Post-build step > "$(DS_POSTBUILD_DEP)"

!ELSEIF  "$(CFG)" == "d4R - Win32 PVM Release"

OUTDIR=.\PVM_Release
INTDIR=.\PVM_Release
# Begin Custom Macros
OutDir=.\PVM_Release
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\PVMSubdue.exe" "$(OUTDIR)\d4R.bsc"

!ELSE 

ALL : "$(OUTDIR)\PVMSubdue.exe" "$(OUTDIR)\d4R.bsc"

!ENDIF 

CLEAN :
	-@erase "$(INTDIR)\abstract.obj"
	-@erase "$(INTDIR)\abstract.sbr"
	-@erase "$(INTDIR)\cluster.obj"
	-@erase "$(INTDIR)\cluster.sbr"
	-@erase "$(INTDIR)\compress.obj"
	-@erase "$(INTDIR)\compress.sbr"
	-@erase "$(INTDIR)\concept.obj"
	-@erase "$(INTDIR)\concept.sbr"
	-@erase "$(INTDIR)\dl.obj"
	-@erase "$(INTDIR)\dl.sbr"
	-@erase "$(INTDIR)\eval.obj"
	-@erase "$(INTDIR)\eval.sbr"
	-@erase "$(INTDIR)\extemp.obj"
	-@erase "$(INTDIR)\extemp.sbr"
	-@erase "$(INTDIR)\extend.obj"
	-@erase "$(INTDIR)\extend.sbr"
	-@erase "$(INTDIR)\fuzzymat.obj"
	-@erase "$(INTDIR)\fuzzymat.sbr"
	-@erase "$(INTDIR)\graphop.obj"
	-@erase "$(INTDIR)\graphop.sbr"
	-@erase "$(INTDIR)\labels.obj"
	-@erase "$(INTDIR)\labels.sbr"
	-@erase "$(INTDIR)\main.obj"
	-@erase "$(INTDIR)\main.sbr"
	-@erase "$(INTDIR)\matchq.obj"
	-@erase "$(INTDIR)\matchq.sbr"
	-@erase "$(INTDIR)\maths.obj"
	-@erase "$(INTDIR)\maths.sbr"
	-@erase "$(INTDIR)\prntstct.obj"
	-@erase "$(INTDIR)\prntstct.sbr"
	-@erase "$(INTDIR)\pvm.obj"
	-@erase "$(INTDIR)\pvm.sbr"
	-@erase "$(INTDIR)\readgrph.obj"
	-@erase "$(INTDIR)\readgrph.sbr"
	-@erase "$(INTDIR)\rvrsesub.obj"
	-@erase "$(INTDIR)\rvrsesub.sbr"
	-@erase "$(INTDIR)\subdue.obj"
	-@erase "$(INTDIR)\subdue.sbr"
	-@erase "$(INTDIR)\subgphop.obj"
	-@erase "$(INTDIR)\subgphop.sbr"
	-@erase "$(INTDIR)\subsop.obj"
	-@erase "$(INTDIR)\subsop.sbr"
	-@erase "$(INTDIR)\tempop.obj"
	-@erase "$(INTDIR)\tempop.sbr"
	-@erase "$(INTDIR)\vc50.idb"
	-@erase "$(OUTDIR)\d4R.bsc"
	-@erase "$(OUTDIR)\PVMSubdue.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /ML /W3 /GX /O2 /I "C:\Program Files\pvm3.4\include" /I\
 "C:\Program Files\pvm3.4\tracer" /I "C:\Program Files\pvm3.4\src" /D "NDEBUG"\
 /D "_PVM_SUBDUE_" /D "WIN32" /D "_CONSOLE" /D "_MBCS" /FR"$(INTDIR)\\"\
 /Fp"$(INTDIR)\d4R.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
CPP_OBJS=.\PVM_Release/
CPP_SBRS=.\PVM_Release/

.c{$(CPP_OBJS)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(CPP_OBJS)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(CPP_OBJS)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(CPP_SBRS)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(CPP_SBRS)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(CPP_SBRS)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

RSC=rc.exe
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\d4R.bsc" 
BSC32_SBRS= \
	"$(INTDIR)\abstract.sbr" \
	"$(INTDIR)\cluster.sbr" \
	"$(INTDIR)\compress.sbr" \
	"$(INTDIR)\concept.sbr" \
	"$(INTDIR)\dl.sbr" \
	"$(INTDIR)\eval.sbr" \
	"$(INTDIR)\extemp.sbr" \
	"$(INTDIR)\extend.sbr" \
	"$(INTDIR)\fuzzymat.sbr" \
	"$(INTDIR)\graphop.sbr" \
	"$(INTDIR)\labels.sbr" \
	"$(INTDIR)\main.sbr" \
	"$(INTDIR)\matchq.sbr" \
	"$(INTDIR)\maths.sbr" \
	"$(INTDIR)\prntstct.sbr" \
	"$(INTDIR)\pvm.sbr" \
	"$(INTDIR)\readgrph.sbr" \
	"$(INTDIR)\rvrsesub.sbr" \
	"$(INTDIR)\subdue.sbr" \
	"$(INTDIR)\subgphop.sbr" \
	"$(INTDIR)\subsop.sbr" \
	"$(INTDIR)\tempop.sbr"

"$(OUTDIR)\d4R.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
LINK32_FLAGS=oldnames.lib kernel32.lib libcpmtd.lib wsock32.lib libc.lib\
 gdi32.lib user32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib\
 ole32.lib oleaut32.lib uuid.lib libpvm3.lib libgpvm3.lib /nologo\
 /subsystem:console /incremental:no /pdb:"$(OUTDIR)\PVMSubdue.pdb" /machine:I386\
 /nodefaultlib /out:"$(OUTDIR)\PVMSubdue.exe" 
LINK32_OBJS= \
	"$(INTDIR)\abstract.obj" \
	"$(INTDIR)\cluster.obj" \
	"$(INTDIR)\compress.obj" \
	"$(INTDIR)\concept.obj" \
	"$(INTDIR)\dl.obj" \
	"$(INTDIR)\eval.obj" \
	"$(INTDIR)\extemp.obj" \
	"$(INTDIR)\extend.obj" \
	"$(INTDIR)\fuzzymat.obj" \
	"$(INTDIR)\graphop.obj" \
	"$(INTDIR)\labels.obj" \
	"$(INTDIR)\main.obj" \
	"$(INTDIR)\matchq.obj" \
	"$(INTDIR)\maths.obj" \
	"$(INTDIR)\prntstct.obj" \
	"$(INTDIR)\pvm.obj" \
	"$(INTDIR)\readgrph.obj" \
	"$(INTDIR)\rvrsesub.obj" \
	"$(INTDIR)\subdue.obj" \
	"$(INTDIR)\subgphop.obj" \
	"$(INTDIR)\subsop.obj" \
	"$(INTDIR)\tempop.obj"

"$(OUTDIR)\PVMSubdue.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

SOURCE=$(InputPath)
DS_POSTBUILD_DEP=$(INTDIR)\postbld.dep

ALL : $(DS_POSTBUILD_DEP)

# Begin Custom Macros
OutDir=.\PVM_Release
# End Custom Macros

$(DS_POSTBUILD_DEP) : "$(OUTDIR)\PVMSubdue.exe" "$(OUTDIR)\d4R.bsc"
   copy     PVM_Release\PVMSubdue.exe     %PVM_ROOT%\bin\%PVM_ARCH%\
	echo Helper for Post-build step > "$(DS_POSTBUILD_DEP)"

!ENDIF 


!IF "$(CFG)" == "d4R - Win32 Release" || "$(CFG)" == "d4R - Win32 Debug" ||\
 "$(CFG)" == "d4R - Win32 PVM Debug" || "$(CFG)" == "d4R - Win32 PVM Release"
SOURCE=.\abstract.c

!IF  "$(CFG)" == "d4R - Win32 Release"

DEP_CPP_ABSTR=\
	".\subdue.h"\
	

"$(INTDIR)\abstract.obj"	"$(INTDIR)\abstract.sbr" : $(SOURCE) $(DEP_CPP_ABSTR)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 Debug"

DEP_CPP_ABSTR=\
	".\subdue.h"\
	
NODEP_CPP_ABSTR=\
	".\dmalloc.h"\
	".\pvm3.h"\
	

"$(INTDIR)\abstract.obj"	"$(INTDIR)\abstract.sbr" : $(SOURCE) $(DEP_CPP_ABSTR)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 PVM Debug"

DEP_CPP_ABSTR=\
	".\subdue.h"\
	"c:\program files\pvm3.4\include\pvm3.h"\
	
NODEP_CPP_ABSTR=\
	".\dmalloc.h"\
	

"$(INTDIR)\abstract.obj"	"$(INTDIR)\abstract.sbr" : $(SOURCE) $(DEP_CPP_ABSTR)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 PVM Release"

DEP_CPP_ABSTR=\
	".\subdue.h"\
	"c:\program files\pvm3.4\include\pvm3.h"\
	"c:\program files\pvm3.4\src\pvmwin.h"\
	

"$(INTDIR)\abstract.obj"	"$(INTDIR)\abstract.sbr" : $(SOURCE) $(DEP_CPP_ABSTR)\
 "$(INTDIR)"


!ENDIF 

SOURCE=.\cluster.c

!IF  "$(CFG)" == "d4R - Win32 Release"

DEP_CPP_CLUST=\
	".\cluster.h"\
	".\subdue.h"\
	

"$(INTDIR)\cluster.obj"	"$(INTDIR)\cluster.sbr" : $(SOURCE) $(DEP_CPP_CLUST)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 Debug"

DEP_CPP_CLUST=\
	".\cluster.h"\
	".\subdue.h"\
	
NODEP_CPP_CLUST=\
	".\dmalloc.h"\
	".\pvm3.h"\
	

"$(INTDIR)\cluster.obj"	"$(INTDIR)\cluster.sbr" : $(SOURCE) $(DEP_CPP_CLUST)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 PVM Debug"

DEP_CPP_CLUST=\
	".\cluster.h"\
	".\subdue.h"\
	"c:\program files\pvm3.4\include\pvm3.h"\
	
NODEP_CPP_CLUST=\
	".\dmalloc.h"\
	

"$(INTDIR)\cluster.obj"	"$(INTDIR)\cluster.sbr" : $(SOURCE) $(DEP_CPP_CLUST)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 PVM Release"

DEP_CPP_CLUST=\
	".\cluster.h"\
	".\subdue.h"\
	"c:\program files\pvm3.4\include\pvm3.h"\
	"c:\program files\pvm3.4\src\pvmwin.h"\
	

"$(INTDIR)\cluster.obj"	"$(INTDIR)\cluster.sbr" : $(SOURCE) $(DEP_CPP_CLUST)\
 "$(INTDIR)"


!ENDIF 

SOURCE=.\compress.c

!IF  "$(CFG)" == "d4R - Win32 Release"

DEP_CPP_COMPR=\
	".\subdue.h"\
	

"$(INTDIR)\compress.obj"	"$(INTDIR)\compress.sbr" : $(SOURCE) $(DEP_CPP_COMPR)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 Debug"

DEP_CPP_COMPR=\
	".\subdue.h"\
	
NODEP_CPP_COMPR=\
	".\dmalloc.h"\
	".\pvm3.h"\
	

"$(INTDIR)\compress.obj"	"$(INTDIR)\compress.sbr" : $(SOURCE) $(DEP_CPP_COMPR)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 PVM Debug"

DEP_CPP_COMPR=\
	".\subdue.h"\
	"c:\program files\pvm3.4\include\pvm3.h"\
	
NODEP_CPP_COMPR=\
	".\dmalloc.h"\
	

"$(INTDIR)\compress.obj"	"$(INTDIR)\compress.sbr" : $(SOURCE) $(DEP_CPP_COMPR)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 PVM Release"

DEP_CPP_COMPR=\
	".\subdue.h"\
	"c:\program files\pvm3.4\include\pvm3.h"\
	"c:\program files\pvm3.4\src\pvmwin.h"\
	

"$(INTDIR)\compress.obj"	"$(INTDIR)\compress.sbr" : $(SOURCE) $(DEP_CPP_COMPR)\
 "$(INTDIR)"


!ENDIF 

SOURCE=.\concept.c

!IF  "$(CFG)" == "d4R - Win32 Release"

DEP_CPP_CONCE=\
	".\subdue.h"\
	

"$(INTDIR)\concept.obj"	"$(INTDIR)\concept.sbr" : $(SOURCE) $(DEP_CPP_CONCE)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 Debug"

DEP_CPP_CONCE=\
	".\subdue.h"\
	
NODEP_CPP_CONCE=\
	".\dmalloc.h"\
	".\pvm3.h"\
	

"$(INTDIR)\concept.obj"	"$(INTDIR)\concept.sbr" : $(SOURCE) $(DEP_CPP_CONCE)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 PVM Debug"

DEP_CPP_CONCE=\
	".\subdue.h"\
	"c:\program files\pvm3.4\include\pvm3.h"\
	
NODEP_CPP_CONCE=\
	".\dmalloc.h"\
	

"$(INTDIR)\concept.obj"	"$(INTDIR)\concept.sbr" : $(SOURCE) $(DEP_CPP_CONCE)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 PVM Release"

DEP_CPP_CONCE=\
	".\subdue.h"\
	"c:\program files\pvm3.4\include\pvm3.h"\
	"c:\program files\pvm3.4\src\pvmwin.h"\
	

"$(INTDIR)\concept.obj"	"$(INTDIR)\concept.sbr" : $(SOURCE) $(DEP_CPP_CONCE)\
 "$(INTDIR)"


!ENDIF 

SOURCE=.\dl.c

!IF  "$(CFG)" == "d4R - Win32 Release"

DEP_CPP_DL_C8=\
	".\subdue.h"\
	

"$(INTDIR)\dl.obj"	"$(INTDIR)\dl.sbr" : $(SOURCE) $(DEP_CPP_DL_C8) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 Debug"

DEP_CPP_DL_C8=\
	".\subdue.h"\
	
NODEP_CPP_DL_C8=\
	".\dmalloc.h"\
	".\pvm3.h"\
	

"$(INTDIR)\dl.obj"	"$(INTDIR)\dl.sbr" : $(SOURCE) $(DEP_CPP_DL_C8) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 PVM Debug"

DEP_CPP_DL_C8=\
	".\subdue.h"\
	"c:\program files\pvm3.4\include\pvm3.h"\
	
NODEP_CPP_DL_C8=\
	".\dmalloc.h"\
	

"$(INTDIR)\dl.obj"	"$(INTDIR)\dl.sbr" : $(SOURCE) $(DEP_CPP_DL_C8) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 PVM Release"

DEP_CPP_DL_C8=\
	".\subdue.h"\
	"c:\program files\pvm3.4\include\pvm3.h"\
	"c:\program files\pvm3.4\src\pvmwin.h"\
	

"$(INTDIR)\dl.obj"	"$(INTDIR)\dl.sbr" : $(SOURCE) $(DEP_CPP_DL_C8) "$(INTDIR)"


!ENDIF 

SOURCE=.\eval.c

!IF  "$(CFG)" == "d4R - Win32 Release"

DEP_CPP_EVAL_=\
	".\subdue.h"\
	

"$(INTDIR)\eval.obj"	"$(INTDIR)\eval.sbr" : $(SOURCE) $(DEP_CPP_EVAL_)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 Debug"

DEP_CPP_EVAL_=\
	".\subdue.h"\
	
NODEP_CPP_EVAL_=\
	".\dmalloc.h"\
	".\pvm3.h"\
	

"$(INTDIR)\eval.obj"	"$(INTDIR)\eval.sbr" : $(SOURCE) $(DEP_CPP_EVAL_)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 PVM Debug"

DEP_CPP_EVAL_=\
	".\subdue.h"\
	"c:\program files\pvm3.4\include\pvm3.h"\
	
NODEP_CPP_EVAL_=\
	".\dmalloc.h"\
	

"$(INTDIR)\eval.obj"	"$(INTDIR)\eval.sbr" : $(SOURCE) $(DEP_CPP_EVAL_)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 PVM Release"

DEP_CPP_EVAL_=\
	".\subdue.h"\
	"c:\program files\pvm3.4\include\pvm3.h"\
	"c:\program files\pvm3.4\src\pvmwin.h"\
	

"$(INTDIR)\eval.obj"	"$(INTDIR)\eval.sbr" : $(SOURCE) $(DEP_CPP_EVAL_)\
 "$(INTDIR)"


!ENDIF 

SOURCE=.\extemp.c

!IF  "$(CFG)" == "d4R - Win32 Release"

DEP_CPP_EXTEM=\
	".\subdue.h"\
	

"$(INTDIR)\extemp.obj"	"$(INTDIR)\extemp.sbr" : $(SOURCE) $(DEP_CPP_EXTEM)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 Debug"

DEP_CPP_EXTEM=\
	".\subdue.h"\
	
NODEP_CPP_EXTEM=\
	".\dmalloc.h"\
	".\pvm3.h"\
	

"$(INTDIR)\extemp.obj"	"$(INTDIR)\extemp.sbr" : $(SOURCE) $(DEP_CPP_EXTEM)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 PVM Debug"

DEP_CPP_EXTEM=\
	".\subdue.h"\
	"c:\program files\pvm3.4\include\pvm3.h"\
	
NODEP_CPP_EXTEM=\
	".\dmalloc.h"\
	

"$(INTDIR)\extemp.obj"	"$(INTDIR)\extemp.sbr" : $(SOURCE) $(DEP_CPP_EXTEM)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 PVM Release"

DEP_CPP_EXTEM=\
	".\subdue.h"\
	"c:\program files\pvm3.4\include\pvm3.h"\
	"c:\program files\pvm3.4\src\pvmwin.h"\
	

"$(INTDIR)\extemp.obj"	"$(INTDIR)\extemp.sbr" : $(SOURCE) $(DEP_CPP_EXTEM)\
 "$(INTDIR)"


!ENDIF 

SOURCE=.\extend.c

!IF  "$(CFG)" == "d4R - Win32 Release"

DEP_CPP_EXTEN=\
	".\subdue.h"\
	

"$(INTDIR)\extend.obj"	"$(INTDIR)\extend.sbr" : $(SOURCE) $(DEP_CPP_EXTEN)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 Debug"

DEP_CPP_EXTEN=\
	".\subdue.h"\
	
NODEP_CPP_EXTEN=\
	".\dmalloc.h"\
	".\pvm3.h"\
	

"$(INTDIR)\extend.obj"	"$(INTDIR)\extend.sbr" : $(SOURCE) $(DEP_CPP_EXTEN)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 PVM Debug"

DEP_CPP_EXTEN=\
	".\subdue.h"\
	"c:\program files\pvm3.4\include\pvm3.h"\
	
NODEP_CPP_EXTEN=\
	".\dmalloc.h"\
	

"$(INTDIR)\extend.obj"	"$(INTDIR)\extend.sbr" : $(SOURCE) $(DEP_CPP_EXTEN)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 PVM Release"

DEP_CPP_EXTEN=\
	".\subdue.h"\
	"c:\program files\pvm3.4\include\pvm3.h"\
	"c:\program files\pvm3.4\src\pvmwin.h"\
	

"$(INTDIR)\extend.obj"	"$(INTDIR)\extend.sbr" : $(SOURCE) $(DEP_CPP_EXTEN)\
 "$(INTDIR)"


!ENDIF 

SOURCE=.\fuzzymat.c

!IF  "$(CFG)" == "d4R - Win32 Release"

DEP_CPP_FUZZY=\
	".\subdue.h"\
	

"$(INTDIR)\fuzzymat.obj"	"$(INTDIR)\fuzzymat.sbr" : $(SOURCE) $(DEP_CPP_FUZZY)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 Debug"

DEP_CPP_FUZZY=\
	".\subdue.h"\
	
NODEP_CPP_FUZZY=\
	".\dmalloc.h"\
	".\pvm3.h"\
	

"$(INTDIR)\fuzzymat.obj"	"$(INTDIR)\fuzzymat.sbr" : $(SOURCE) $(DEP_CPP_FUZZY)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 PVM Debug"

DEP_CPP_FUZZY=\
	".\subdue.h"\
	"c:\program files\pvm3.4\include\pvm3.h"\
	
NODEP_CPP_FUZZY=\
	".\dmalloc.h"\
	

"$(INTDIR)\fuzzymat.obj"	"$(INTDIR)\fuzzymat.sbr" : $(SOURCE) $(DEP_CPP_FUZZY)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 PVM Release"

DEP_CPP_FUZZY=\
	".\subdue.h"\
	"c:\program files\pvm3.4\include\pvm3.h"\
	"c:\program files\pvm3.4\src\pvmwin.h"\
	

"$(INTDIR)\fuzzymat.obj"	"$(INTDIR)\fuzzymat.sbr" : $(SOURCE) $(DEP_CPP_FUZZY)\
 "$(INTDIR)"


!ENDIF 

SOURCE=.\graphop.c

!IF  "$(CFG)" == "d4R - Win32 Release"

DEP_CPP_GRAPH=\
	".\subdue.h"\
	

"$(INTDIR)\graphop.obj"	"$(INTDIR)\graphop.sbr" : $(SOURCE) $(DEP_CPP_GRAPH)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 Debug"

DEP_CPP_GRAPH=\
	".\subdue.h"\
	
NODEP_CPP_GRAPH=\
	".\dmalloc.h"\
	".\pvm3.h"\
	

"$(INTDIR)\graphop.obj"	"$(INTDIR)\graphop.sbr" : $(SOURCE) $(DEP_CPP_GRAPH)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 PVM Debug"

DEP_CPP_GRAPH=\
	".\subdue.h"\
	"c:\program files\pvm3.4\include\pvm3.h"\
	
NODEP_CPP_GRAPH=\
	".\dmalloc.h"\
	

"$(INTDIR)\graphop.obj"	"$(INTDIR)\graphop.sbr" : $(SOURCE) $(DEP_CPP_GRAPH)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 PVM Release"

DEP_CPP_GRAPH=\
	".\subdue.h"\
	"c:\program files\pvm3.4\include\pvm3.h"\
	"c:\program files\pvm3.4\src\pvmwin.h"\
	

"$(INTDIR)\graphop.obj"	"$(INTDIR)\graphop.sbr" : $(SOURCE) $(DEP_CPP_GRAPH)\
 "$(INTDIR)"


!ENDIF 

SOURCE=.\labels.c

!IF  "$(CFG)" == "d4R - Win32 Release"

DEP_CPP_LABEL=\
	".\subdue.h"\
	

"$(INTDIR)\labels.obj"	"$(INTDIR)\labels.sbr" : $(SOURCE) $(DEP_CPP_LABEL)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 Debug"

DEP_CPP_LABEL=\
	".\subdue.h"\
	
NODEP_CPP_LABEL=\
	".\dmalloc.h"\
	".\pvm3.h"\
	

"$(INTDIR)\labels.obj"	"$(INTDIR)\labels.sbr" : $(SOURCE) $(DEP_CPP_LABEL)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 PVM Debug"

DEP_CPP_LABEL=\
	".\subdue.h"\
	"c:\program files\pvm3.4\include\pvm3.h"\
	
NODEP_CPP_LABEL=\
	".\dmalloc.h"\
	

"$(INTDIR)\labels.obj"	"$(INTDIR)\labels.sbr" : $(SOURCE) $(DEP_CPP_LABEL)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 PVM Release"

DEP_CPP_LABEL=\
	".\subdue.h"\
	"c:\program files\pvm3.4\include\pvm3.h"\
	"c:\program files\pvm3.4\src\pvmwin.h"\
	

"$(INTDIR)\labels.obj"	"$(INTDIR)\labels.sbr" : $(SOURCE) $(DEP_CPP_LABEL)\
 "$(INTDIR)"


!ENDIF 

SOURCE=.\main.c

!IF  "$(CFG)" == "d4R - Win32 Release"

DEP_CPP_MAIN_=\
	".\subdue.h"\
	

"$(INTDIR)\main.obj"	"$(INTDIR)\main.sbr" : $(SOURCE) $(DEP_CPP_MAIN_)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 Debug"

DEP_CPP_MAIN_=\
	".\subdue.h"\
	
NODEP_CPP_MAIN_=\
	".\dmalloc.h"\
	".\pvm3.h"\
	

"$(INTDIR)\main.obj"	"$(INTDIR)\main.sbr" : $(SOURCE) $(DEP_CPP_MAIN_)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 PVM Debug"

DEP_CPP_MAIN_=\
	".\subdue.h"\
	"c:\program files\pvm3.4\include\pvm3.h"\
	
NODEP_CPP_MAIN_=\
	".\dmalloc.h"\
	

"$(INTDIR)\main.obj"	"$(INTDIR)\main.sbr" : $(SOURCE) $(DEP_CPP_MAIN_)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 PVM Release"

DEP_CPP_MAIN_=\
	".\subdue.h"\
	"c:\program files\pvm3.4\include\pvm3.h"\
	"c:\program files\pvm3.4\src\pvmwin.h"\
	

"$(INTDIR)\main.obj"	"$(INTDIR)\main.sbr" : $(SOURCE) $(DEP_CPP_MAIN_)\
 "$(INTDIR)"


!ENDIF 

SOURCE=.\matchq.c

!IF  "$(CFG)" == "d4R - Win32 Release"

DEP_CPP_MATCH=\
	".\subdue.h"\
	

"$(INTDIR)\matchq.obj"	"$(INTDIR)\matchq.sbr" : $(SOURCE) $(DEP_CPP_MATCH)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 Debug"

DEP_CPP_MATCH=\
	".\subdue.h"\
	
NODEP_CPP_MATCH=\
	".\dmalloc.h"\
	".\pvm3.h"\
	

"$(INTDIR)\matchq.obj"	"$(INTDIR)\matchq.sbr" : $(SOURCE) $(DEP_CPP_MATCH)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 PVM Debug"

DEP_CPP_MATCH=\
	".\subdue.h"\
	"c:\program files\pvm3.4\include\pvm3.h"\
	
NODEP_CPP_MATCH=\
	".\dmalloc.h"\
	

"$(INTDIR)\matchq.obj"	"$(INTDIR)\matchq.sbr" : $(SOURCE) $(DEP_CPP_MATCH)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 PVM Release"

DEP_CPP_MATCH=\
	".\subdue.h"\
	"c:\program files\pvm3.4\include\pvm3.h"\
	"c:\program files\pvm3.4\src\pvmwin.h"\
	

"$(INTDIR)\matchq.obj"	"$(INTDIR)\matchq.sbr" : $(SOURCE) $(DEP_CPP_MATCH)\
 "$(INTDIR)"


!ENDIF 

SOURCE=.\maths.c

!IF  "$(CFG)" == "d4R - Win32 Release"

DEP_CPP_MATHS=\
	".\subdue.h"\
	

"$(INTDIR)\maths.obj"	"$(INTDIR)\maths.sbr" : $(SOURCE) $(DEP_CPP_MATHS)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 Debug"

DEP_CPP_MATHS=\
	".\subdue.h"\
	
NODEP_CPP_MATHS=\
	".\dmalloc.h"\
	".\pvm3.h"\
	

"$(INTDIR)\maths.obj"	"$(INTDIR)\maths.sbr" : $(SOURCE) $(DEP_CPP_MATHS)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 PVM Debug"

DEP_CPP_MATHS=\
	".\subdue.h"\
	"c:\program files\pvm3.4\include\pvm3.h"\
	
NODEP_CPP_MATHS=\
	".\dmalloc.h"\
	

"$(INTDIR)\maths.obj"	"$(INTDIR)\maths.sbr" : $(SOURCE) $(DEP_CPP_MATHS)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 PVM Release"

DEP_CPP_MATHS=\
	".\subdue.h"\
	"c:\program files\pvm3.4\include\pvm3.h"\
	"c:\program files\pvm3.4\src\pvmwin.h"\
	

"$(INTDIR)\maths.obj"	"$(INTDIR)\maths.sbr" : $(SOURCE) $(DEP_CPP_MATHS)\
 "$(INTDIR)"


!ENDIF 

SOURCE=.\prntstct.c

!IF  "$(CFG)" == "d4R - Win32 Release"

DEP_CPP_PRNTS=\
	".\subdue.h"\
	

"$(INTDIR)\prntstct.obj"	"$(INTDIR)\prntstct.sbr" : $(SOURCE) $(DEP_CPP_PRNTS)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 Debug"

DEP_CPP_PRNTS=\
	".\subdue.h"\
	
NODEP_CPP_PRNTS=\
	".\dmalloc.h"\
	".\pvm3.h"\
	

"$(INTDIR)\prntstct.obj"	"$(INTDIR)\prntstct.sbr" : $(SOURCE) $(DEP_CPP_PRNTS)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 PVM Debug"

DEP_CPP_PRNTS=\
	".\subdue.h"\
	"c:\program files\pvm3.4\include\pvm3.h"\
	
NODEP_CPP_PRNTS=\
	".\dmalloc.h"\
	

"$(INTDIR)\prntstct.obj"	"$(INTDIR)\prntstct.sbr" : $(SOURCE) $(DEP_CPP_PRNTS)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 PVM Release"

DEP_CPP_PRNTS=\
	".\subdue.h"\
	"c:\program files\pvm3.4\include\pvm3.h"\
	"c:\program files\pvm3.4\src\pvmwin.h"\
	

"$(INTDIR)\prntstct.obj"	"$(INTDIR)\prntstct.sbr" : $(SOURCE) $(DEP_CPP_PRNTS)\
 "$(INTDIR)"


!ENDIF 

SOURCE=.\pvm.c

!IF  "$(CFG)" == "d4R - Win32 Release"


"$(INTDIR)\pvm.obj"	"$(INTDIR)\pvm.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 Debug"

DEP_CPP_PVM_C=\
	".\subdue.h"\
	
NODEP_CPP_PVM_C=\
	".\dmalloc.h"\
	".\pvm3.h"\
	

"$(INTDIR)\pvm.obj"	"$(INTDIR)\pvm.sbr" : $(SOURCE) $(DEP_CPP_PVM_C)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 PVM Debug"

DEP_CPP_PVM_C=\
	".\subdue.h"\
	"c:\program files\pvm3.4\include\pvm3.h"\
	
NODEP_CPP_PVM_C=\
	".\dmalloc.h"\
	

"$(INTDIR)\pvm.obj"	"$(INTDIR)\pvm.sbr" : $(SOURCE) $(DEP_CPP_PVM_C)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 PVM Release"

DEP_CPP_PVM_C=\
	".\subdue.h"\
	"c:\program files\pvm3.4\include\pvm3.h"\
	"c:\program files\pvm3.4\src\pvmwin.h"\
	

"$(INTDIR)\pvm.obj"	"$(INTDIR)\pvm.sbr" : $(SOURCE) $(DEP_CPP_PVM_C)\
 "$(INTDIR)"


!ENDIF 

SOURCE=.\readgrph.c

!IF  "$(CFG)" == "d4R - Win32 Release"

DEP_CPP_READG=\
	".\subdue.h"\
	

"$(INTDIR)\readgrph.obj"	"$(INTDIR)\readgrph.sbr" : $(SOURCE) $(DEP_CPP_READG)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 Debug"

DEP_CPP_READG=\
	".\subdue.h"\
	
NODEP_CPP_READG=\
	".\dmalloc.h"\
	".\pvm3.h"\
	

"$(INTDIR)\readgrph.obj"	"$(INTDIR)\readgrph.sbr" : $(SOURCE) $(DEP_CPP_READG)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 PVM Debug"

DEP_CPP_READG=\
	".\subdue.h"\
	"c:\program files\pvm3.4\include\pvm3.h"\
	
NODEP_CPP_READG=\
	".\dmalloc.h"\
	

"$(INTDIR)\readgrph.obj"	"$(INTDIR)\readgrph.sbr" : $(SOURCE) $(DEP_CPP_READG)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 PVM Release"

DEP_CPP_READG=\
	".\subdue.h"\
	"c:\program files\pvm3.4\include\pvm3.h"\
	"c:\program files\pvm3.4\src\pvmwin.h"\
	

"$(INTDIR)\readgrph.obj"	"$(INTDIR)\readgrph.sbr" : $(SOURCE) $(DEP_CPP_READG)\
 "$(INTDIR)"


!ENDIF 

SOURCE=.\rvrsesub.c

!IF  "$(CFG)" == "d4R - Win32 Release"

DEP_CPP_RVRSE=\
	".\subdue.h"\
	

"$(INTDIR)\rvrsesub.obj"	"$(INTDIR)\rvrsesub.sbr" : $(SOURCE) $(DEP_CPP_RVRSE)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 Debug"

DEP_CPP_RVRSE=\
	".\subdue.h"\
	
NODEP_CPP_RVRSE=\
	".\dmalloc.h"\
	".\pvm3.h"\
	

"$(INTDIR)\rvrsesub.obj"	"$(INTDIR)\rvrsesub.sbr" : $(SOURCE) $(DEP_CPP_RVRSE)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 PVM Debug"

DEP_CPP_RVRSE=\
	".\subdue.h"\
	"c:\program files\pvm3.4\include\pvm3.h"\
	
NODEP_CPP_RVRSE=\
	".\dmalloc.h"\
	

"$(INTDIR)\rvrsesub.obj"	"$(INTDIR)\rvrsesub.sbr" : $(SOURCE) $(DEP_CPP_RVRSE)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 PVM Release"

DEP_CPP_RVRSE=\
	".\subdue.h"\
	"c:\program files\pvm3.4\include\pvm3.h"\
	"c:\program files\pvm3.4\src\pvmwin.h"\
	

"$(INTDIR)\rvrsesub.obj"	"$(INTDIR)\rvrsesub.sbr" : $(SOURCE) $(DEP_CPP_RVRSE)\
 "$(INTDIR)"


!ENDIF 

SOURCE=.\subdue.c

!IF  "$(CFG)" == "d4R - Win32 Release"

DEP_CPP_SUBDU=\
	".\subdue.h"\
	

"$(INTDIR)\subdue.obj"	"$(INTDIR)\subdue.sbr" : $(SOURCE) $(DEP_CPP_SUBDU)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 Debug"

DEP_CPP_SUBDU=\
	".\subdue.h"\
	
NODEP_CPP_SUBDU=\
	".\dmalloc.h"\
	".\pvm3.h"\
	

"$(INTDIR)\subdue.obj"	"$(INTDIR)\subdue.sbr" : $(SOURCE) $(DEP_CPP_SUBDU)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 PVM Debug"

DEP_CPP_SUBDU=\
	".\subdue.h"\
	"c:\program files\pvm3.4\include\pvm3.h"\
	
NODEP_CPP_SUBDU=\
	".\dmalloc.h"\
	

"$(INTDIR)\subdue.obj"	"$(INTDIR)\subdue.sbr" : $(SOURCE) $(DEP_CPP_SUBDU)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 PVM Release"

DEP_CPP_SUBDU=\
	".\subdue.h"\
	"c:\program files\pvm3.4\include\pvm3.h"\
	"c:\program files\pvm3.4\src\pvmwin.h"\
	

"$(INTDIR)\subdue.obj"	"$(INTDIR)\subdue.sbr" : $(SOURCE) $(DEP_CPP_SUBDU)\
 "$(INTDIR)"


!ENDIF 

SOURCE=.\subgphop.c

!IF  "$(CFG)" == "d4R - Win32 Release"

DEP_CPP_SUBGP=\
	".\subdue.h"\
	

"$(INTDIR)\subgphop.obj"	"$(INTDIR)\subgphop.sbr" : $(SOURCE) $(DEP_CPP_SUBGP)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 Debug"

DEP_CPP_SUBGP=\
	".\subdue.h"\
	
NODEP_CPP_SUBGP=\
	".\dmalloc.h"\
	".\pvm3.h"\
	

"$(INTDIR)\subgphop.obj"	"$(INTDIR)\subgphop.sbr" : $(SOURCE) $(DEP_CPP_SUBGP)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 PVM Debug"

DEP_CPP_SUBGP=\
	".\subdue.h"\
	"c:\program files\pvm3.4\include\pvm3.h"\
	
NODEP_CPP_SUBGP=\
	".\dmalloc.h"\
	

"$(INTDIR)\subgphop.obj"	"$(INTDIR)\subgphop.sbr" : $(SOURCE) $(DEP_CPP_SUBGP)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 PVM Release"

DEP_CPP_SUBGP=\
	".\subdue.h"\
	"c:\program files\pvm3.4\include\pvm3.h"\
	"c:\program files\pvm3.4\src\pvmwin.h"\
	

"$(INTDIR)\subgphop.obj"	"$(INTDIR)\subgphop.sbr" : $(SOURCE) $(DEP_CPP_SUBGP)\
 "$(INTDIR)"


!ENDIF 

SOURCE=.\subsop.c

!IF  "$(CFG)" == "d4R - Win32 Release"

DEP_CPP_SUBSO=\
	".\subdue.h"\
	

"$(INTDIR)\subsop.obj"	"$(INTDIR)\subsop.sbr" : $(SOURCE) $(DEP_CPP_SUBSO)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 Debug"

DEP_CPP_SUBSO=\
	".\subdue.h"\
	
NODEP_CPP_SUBSO=\
	".\dmalloc.h"\
	".\pvm3.h"\
	

"$(INTDIR)\subsop.obj"	"$(INTDIR)\subsop.sbr" : $(SOURCE) $(DEP_CPP_SUBSO)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 PVM Debug"

DEP_CPP_SUBSO=\
	".\subdue.h"\
	"c:\program files\pvm3.4\include\pvm3.h"\
	
NODEP_CPP_SUBSO=\
	".\dmalloc.h"\
	

"$(INTDIR)\subsop.obj"	"$(INTDIR)\subsop.sbr" : $(SOURCE) $(DEP_CPP_SUBSO)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 PVM Release"

DEP_CPP_SUBSO=\
	".\subdue.h"\
	"c:\program files\pvm3.4\include\pvm3.h"\
	"c:\program files\pvm3.4\src\pvmwin.h"\
	

"$(INTDIR)\subsop.obj"	"$(INTDIR)\subsop.sbr" : $(SOURCE) $(DEP_CPP_SUBSO)\
 "$(INTDIR)"


!ENDIF 

SOURCE=.\tempop.c

!IF  "$(CFG)" == "d4R - Win32 Release"

DEP_CPP_TEMPO=\
	".\subdue.h"\
	

"$(INTDIR)\tempop.obj"	"$(INTDIR)\tempop.sbr" : $(SOURCE) $(DEP_CPP_TEMPO)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 Debug"

DEP_CPP_TEMPO=\
	".\subdue.h"\
	
NODEP_CPP_TEMPO=\
	".\dmalloc.h"\
	".\pvm3.h"\
	

"$(INTDIR)\tempop.obj"	"$(INTDIR)\tempop.sbr" : $(SOURCE) $(DEP_CPP_TEMPO)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 PVM Debug"

DEP_CPP_TEMPO=\
	".\subdue.h"\
	"c:\program files\pvm3.4\include\pvm3.h"\
	
NODEP_CPP_TEMPO=\
	".\dmalloc.h"\
	

"$(INTDIR)\tempop.obj"	"$(INTDIR)\tempop.sbr" : $(SOURCE) $(DEP_CPP_TEMPO)\
 "$(INTDIR)"


!ELSEIF  "$(CFG)" == "d4R - Win32 PVM Release"

DEP_CPP_TEMPO=\
	".\subdue.h"\
	"c:\program files\pvm3.4\include\pvm3.h"\
	"c:\program files\pvm3.4\src\pvmwin.h"\
	

"$(INTDIR)\tempop.obj"	"$(INTDIR)\tempop.sbr" : $(SOURCE) $(DEP_CPP_TEMPO)\
 "$(INTDIR)"


!ENDIF 


!ENDIF 

