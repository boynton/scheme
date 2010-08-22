# Microsoft Developer Studio Generated NMAKE File, Format Version 4.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

!IF "$(CFG)" == ""
CFG=scheme - Win32 Debug
!MESSAGE No configuration specified.  Defaulting to scheme - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "scheme - Win32 Release" && "$(CFG)" != "scheme - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "scheme.mak" CFG="scheme - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "scheme - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "scheme - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 
################################################################################
# Begin Project
# PROP Target_Last_Scanned "scheme - Win32 Release"
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "scheme - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
OUTDIR=.\Release
INTDIR=.\Release

ALL : "$(OUTDIR)\scheme.exe"

CLEAN : 
	-@erase ".\Release\scheme.exe"
	-@erase ".\Release\port.obj"
	-@erase ".\Release\list.obj"
	-@erase ".\Release\macros.obj"
	-@erase ".\Release\string.obj"
	-@erase ".\Release\scheme.obj"
	-@erase ".\Release\vector.obj"
	-@erase ".\Release\heap.obj"
	-@erase ".\Release\number.obj"
	-@erase ".\Release\proc.obj"
	-@erase ".\Release\logical.obj"
	-@erase ".\Release\compiler.obj"
	-@erase ".\Release\runtime.obj"
	-@erase ".\Release\io.obj"
	-@erase ".\Release\symbol.obj"
	-@erase ".\Release\syswin95.obj"
	-@erase ".\Release\scheme.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /YX /c
# ADD CPP /nologo /G5 /Zp4 /Ze /W3 /WX /Ox /Ot /Og /Oi /Ob2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /c
# SUBTRACT CPP /Oa /YX
CPP_PROJ=/nologo /G5 /Zp4 /ML /Ze /W3 /WX /Ox /Ot /Og /Oi /Ob2 /D "WIN32" /D\
 "NDEBUG" /D "_CONSOLE" /Fo"$(INTDIR)/" /c 
CPP_OBJS=.\Release/
CPP_SBRS=
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/scheme.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /profile /debug /machine:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib /nologo /subsystem:console /profile /debug /machine:I386\
 /out:"$(OUTDIR)/scheme.exe" 
LINK32_OBJS= \
	"$(INTDIR)/port.obj" \
	"$(INTDIR)/list.obj" \
	"$(INTDIR)/macros.obj" \
	"$(INTDIR)/string.obj" \
	"$(INTDIR)/scheme.obj" \
	"$(INTDIR)/vector.obj" \
	"$(INTDIR)/heap.obj" \
	"$(INTDIR)/number.obj" \
	"$(INTDIR)/proc.obj" \
	"$(INTDIR)/logical.obj" \
	"$(INTDIR)/compiler.obj" \
	"$(INTDIR)/runtime.obj" \
	"$(INTDIR)/io.obj" \
	"$(INTDIR)/symbol.obj" \
	"$(INTDIR)/syswin95.obj"

"$(OUTDIR)\scheme.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "scheme - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
OUTDIR=.\Debug
INTDIR=.\Debug

ALL : "$(OUTDIR)\scheme.exe"

CLEAN : 
	-@erase ".\Debug\vc40.pdb"
	-@erase ".\Debug\vc40.idb"
	-@erase ".\Debug\scheme.exe"
	-@erase ".\Debug\compiler.obj"
	-@erase ".\Debug\io.obj"
	-@erase ".\Debug\proc.obj"
	-@erase ".\Debug\symbol.obj"
	-@erase ".\Debug\logical.obj"
	-@erase ".\Debug\port.obj"
	-@erase ".\Debug\list.obj"
	-@erase ".\Debug\macros.obj"
	-@erase ".\Debug\string.obj"
	-@erase ".\Debug\runtime.obj"
	-@erase ".\Debug\scheme.obj"
	-@erase ".\Debug\vector.obj"
	-@erase ".\Debug\heap.obj"
	-@erase ".\Debug\number.obj"
	-@erase ".\Debug\syswin95.obj"
	-@erase ".\Debug\scheme.ilk"
	-@erase ".\Debug\scheme.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /YX /c
# ADD CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /YX /c
CPP_PROJ=/nologo /MLd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE"\
 /Fp"$(INTDIR)/scheme.pch" /YX /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=.\Debug/
CPP_SBRS=
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/scheme.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib /nologo /subsystem:console /incremental:yes\
 /pdb:"$(OUTDIR)/scheme.pdb" /debug /machine:I386 /out:"$(OUTDIR)/scheme.exe" 
LINK32_OBJS= \
	"$(INTDIR)/compiler.obj" \
	"$(INTDIR)/io.obj" \
	"$(INTDIR)/proc.obj" \
	"$(INTDIR)/symbol.obj" \
	"$(INTDIR)/logical.obj" \
	"$(INTDIR)/port.obj" \
	"$(INTDIR)/list.obj" \
	"$(INTDIR)/macros.obj" \
	"$(INTDIR)/string.obj" \
	"$(INTDIR)/runtime.obj" \
	"$(INTDIR)/scheme.obj" \
	"$(INTDIR)/vector.obj" \
	"$(INTDIR)/heap.obj" \
	"$(INTDIR)/number.obj" \
	"$(INTDIR)/syswin95.obj"

"$(OUTDIR)\scheme.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

.c{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cpp{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cxx{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.c{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

.cpp{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

.cxx{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

################################################################################
# Begin Target

# Name "scheme - Win32 Release"
# Name "scheme - Win32 Debug"

!IF  "$(CFG)" == "scheme - Win32 Release"

!ELSEIF  "$(CFG)" == "scheme - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=.\compiler.c
DEP_CPP_COMPI=\
	".\scheme.h"\
	".\object.h"\
	".\sys.h"\
	".\runtime.h"\
	".\heap.h"\
	".\symbol.h"\
	".\port.h"\
	".\io.h"\
	".\proc.h"\
	".\logical.h"\
	".\number.h"\
	".\string.h"\
	".\list.h"\
	".\vector.h"\
	".\compiler.h"\
	".\macros.h"\
	

"$(INTDIR)\compiler.obj" : $(SOURCE) $(DEP_CPP_COMPI) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\heap.c
DEP_CPP_HEAP_=\
	".\scheme.h"\
	{$(INCLUDE)}"\String.h"\
	".\object.h"\
	".\sys.h"\
	".\runtime.h"\
	".\heap.h"\
	".\symbol.h"\
	".\port.h"\
	".\io.h"\
	".\proc.h"\
	".\logical.h"\
	".\number.h"\
	".\string.h"\
	".\list.h"\
	".\vector.h"\
	".\compiler.h"\
	".\macros.h"\
	

"$(INTDIR)\heap.obj" : $(SOURCE) $(DEP_CPP_HEAP_) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\io.c
DEP_CPP_IO_C4=\
	".\scheme.h"\
	{$(INCLUDE)}"\String.h"\
	".\object.h"\
	".\sys.h"\
	".\runtime.h"\
	".\heap.h"\
	".\symbol.h"\
	".\port.h"\
	".\io.h"\
	".\proc.h"\
	".\logical.h"\
	".\number.h"\
	".\string.h"\
	".\list.h"\
	".\vector.h"\
	".\compiler.h"\
	".\macros.h"\
	

"$(INTDIR)\io.obj" : $(SOURCE) $(DEP_CPP_IO_C4) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\list.c
DEP_CPP_LIST_=\
	".\scheme.h"\
	".\object.h"\
	".\sys.h"\
	".\runtime.h"\
	".\heap.h"\
	".\symbol.h"\
	".\port.h"\
	".\io.h"\
	".\proc.h"\
	".\logical.h"\
	".\number.h"\
	".\string.h"\
	".\list.h"\
	".\vector.h"\
	".\compiler.h"\
	".\macros.h"\
	

"$(INTDIR)\list.obj" : $(SOURCE) $(DEP_CPP_LIST_) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\logical.c
DEP_CPP_LOGIC=\
	".\scheme.h"\
	".\object.h"\
	".\sys.h"\
	".\runtime.h"\
	".\heap.h"\
	".\symbol.h"\
	".\port.h"\
	".\io.h"\
	".\proc.h"\
	".\logical.h"\
	".\number.h"\
	".\string.h"\
	".\list.h"\
	".\vector.h"\
	".\compiler.h"\
	".\macros.h"\
	

"$(INTDIR)\logical.obj" : $(SOURCE) $(DEP_CPP_LOGIC) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\macros.c
DEP_CPP_MACRO=\
	".\scheme.h"\
	".\object.h"\
	".\sys.h"\
	".\runtime.h"\
	".\heap.h"\
	".\symbol.h"\
	".\port.h"\
	".\io.h"\
	".\proc.h"\
	".\logical.h"\
	".\number.h"\
	".\string.h"\
	".\list.h"\
	".\vector.h"\
	".\compiler.h"\
	".\macros.h"\
	

"$(INTDIR)\macros.obj" : $(SOURCE) $(DEP_CPP_MACRO) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\number.c
DEP_CPP_NUMBE=\
	".\scheme.h"\
	".\object.h"\
	".\sys.h"\
	".\runtime.h"\
	".\heap.h"\
	".\symbol.h"\
	".\port.h"\
	".\io.h"\
	".\proc.h"\
	".\logical.h"\
	".\number.h"\
	".\string.h"\
	".\list.h"\
	".\vector.h"\
	".\compiler.h"\
	".\macros.h"\
	

"$(INTDIR)\number.obj" : $(SOURCE) $(DEP_CPP_NUMBE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\port.c
DEP_CPP_PORT_=\
	".\scheme.h"\
	{$(INCLUDE)}"\String.h"\
	".\object.h"\
	".\sys.h"\
	".\runtime.h"\
	".\heap.h"\
	".\symbol.h"\
	".\port.h"\
	".\io.h"\
	".\proc.h"\
	".\logical.h"\
	".\number.h"\
	".\string.h"\
	".\list.h"\
	".\vector.h"\
	".\compiler.h"\
	".\macros.h"\
	

"$(INTDIR)\port.obj" : $(SOURCE) $(DEP_CPP_PORT_) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\proc.c
DEP_CPP_PROC_=\
	".\scheme.h"\
	{$(INCLUDE)}"\String.h"\
	".\object.h"\
	".\sys.h"\
	".\runtime.h"\
	".\heap.h"\
	".\symbol.h"\
	".\port.h"\
	".\io.h"\
	".\proc.h"\
	".\logical.h"\
	".\number.h"\
	".\string.h"\
	".\list.h"\
	".\vector.h"\
	".\compiler.h"\
	".\macros.h"\
	

"$(INTDIR)\proc.obj" : $(SOURCE) $(DEP_CPP_PROC_) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\runtime.c
DEP_CPP_RUNTI=\
	".\scheme.h"\
	{$(INCLUDE)}"\String.h"\
	".\object.h"\
	".\sys.h"\
	".\runtime.h"\
	".\heap.h"\
	".\symbol.h"\
	".\port.h"\
	".\io.h"\
	".\proc.h"\
	".\logical.h"\
	".\number.h"\
	".\string.h"\
	".\list.h"\
	".\vector.h"\
	".\compiler.h"\
	".\macros.h"\
	

"$(INTDIR)\runtime.obj" : $(SOURCE) $(DEP_CPP_RUNTI) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\scheme.c
DEP_CPP_SCHEM=\
	".\scheme.h"\
	".\object.h"\
	".\sys.h"\
	".\runtime.h"\
	".\heap.h"\
	".\symbol.h"\
	".\port.h"\
	".\io.h"\
	".\proc.h"\
	".\logical.h"\
	".\number.h"\
	".\string.h"\
	".\list.h"\
	".\vector.h"\
	".\compiler.h"\
	".\macros.h"\
	

"$(INTDIR)\scheme.obj" : $(SOURCE) $(DEP_CPP_SCHEM) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\string.c
DEP_CPP_STRIN=\
	".\scheme.h"\
	{$(INCLUDE)}"\String.h"\
	".\object.h"\
	".\sys.h"\
	".\runtime.h"\
	".\heap.h"\
	".\symbol.h"\
	".\port.h"\
	".\io.h"\
	".\proc.h"\
	".\logical.h"\
	".\number.h"\
	".\string.h"\
	".\list.h"\
	".\vector.h"\
	".\compiler.h"\
	".\macros.h"\
	

"$(INTDIR)\string.obj" : $(SOURCE) $(DEP_CPP_STRIN) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\symbol.c
DEP_CPP_SYMBO=\
	".\scheme.h"\
	{$(INCLUDE)}"\String.h"\
	".\object.h"\
	".\sys.h"\
	".\runtime.h"\
	".\heap.h"\
	".\symbol.h"\
	".\port.h"\
	".\io.h"\
	".\proc.h"\
	".\logical.h"\
	".\number.h"\
	".\string.h"\
	".\list.h"\
	".\vector.h"\
	".\compiler.h"\
	".\macros.h"\
	

"$(INTDIR)\symbol.obj" : $(SOURCE) $(DEP_CPP_SYMBO) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\vector.c
DEP_CPP_VECTO=\
	".\scheme.h"\
	".\object.h"\
	".\sys.h"\
	".\runtime.h"\
	".\heap.h"\
	".\symbol.h"\
	".\port.h"\
	".\io.h"\
	".\proc.h"\
	".\logical.h"\
	".\number.h"\
	".\string.h"\
	".\list.h"\
	".\vector.h"\
	".\compiler.h"\
	".\macros.h"\
	

"$(INTDIR)\vector.obj" : $(SOURCE) $(DEP_CPP_VECTO) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\syswin95.c
DEP_CPP_SYSWI=\
	".\scheme.h"\
	".\sys.h"\
	{$(INCLUDE)}"\sys\Stat.h"\
	{$(INCLUDE)}"\Io.h"\
	".\object.h"\
	".\runtime.h"\
	".\heap.h"\
	".\symbol.h"\
	".\port.h"\
	".\io.h"\
	".\proc.h"\
	".\logical.h"\
	".\number.h"\
	".\string.h"\
	".\list.h"\
	".\vector.h"\
	".\compiler.h"\
	".\macros.h"\
	{$(INCLUDE)}"\sys\Types.h"\
	

"$(INTDIR)\syswin95.obj" : $(SOURCE) $(DEP_CPP_SYSWI) "$(INTDIR)"


# End Source File
# End Target
# End Project
################################################################################
