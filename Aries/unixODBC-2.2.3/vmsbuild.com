$ set nover
$
$ cwd = f$directory()
$ basedir = cwd - "]"
$ 
$ includes = "''basedir'.INCLUDE],''basedir'.EXTRAS]"
$ defines = "UNIXODBC,HAVE_PWD_H," + """""readonly""""=""""__readonly"""""
$
$ if p1 .eqs. "" then $goto ERR_NOPARAMS
$
$ CFLAGS="/opt/nodeb/name=as_is/prefix=all/include=(''includes')/warning=nowarnings/define=(''defines')"
$ LFLAGS="/nodeb/notrace"
$
$! DEBUG DEFINES
$! CFLAGS="/noopt/deb/name=as_is/prefix=all/include=(''includes')/warning=nowarnings/define=(''defines')"
$! LFLAGS="/deb/trace"
$
$ if p1 .eqs. "CLEAN" 
$ then
$   set def 'cwd'
$   write sys$output "Removing all object files"
$   del [...]*.obj;*
$   write sys$output "Removing all object libraries"
$   del [...]*.olb;*
$ endif
$
$ if p1 .eqs. "COMPILE" .or. p1 .eqs. "ALL"
$ then
$   write sys$output "Compiling unixODBC"
$   call compile "[.extras]" "*.c"
$   call compile "[.ini]" "*.c"
$   call compile "[.log]" "*.c"
$   call compile "[.lst]" "*.c"
$   call compile "[.odbcinst]" "*.c"
$   call compile "[.drivermanager]" "*.c"
$   call compile "[.exe]" "isql*.c"
$   call compile "[.drivers.postgresql]" "*.c"
$   write sys$output "Building Object Libraries"
$   set def 'cwd'
$   library/create [.vms]libodbcinst.olb
$   library/insert [.vms]libodbcinst.olb [.extras]*.obj, [.ini]*.obj, [.log]*.obj, [.lst]*.obj, [.odbcinst]*.obj
$   library/create [.vms]libodbc.olb
$   library/insert [.vms]libodbc.olb [.extras]*.obj,[.ini]*.obj, [.lst]*.obj, [.drivermanager]*.obj
$   library/create [.vms]libodbcpsql.olb
$   library/insert [.vms]libodbcpsql.olb [.drivers.postgresql]*.obj
$
$ endif
$!
$! Build Shared objects
$!
$
$!
$! Install them
$!
$ if p1 .eqs. "LINK" .or. p1 .eqs. "ALL"
$ then
$   set def 'cwd'
$   set def [.vms]
$
$   write sys$output "Linking ODBCINST.EXE"
$   link 'LFLAGS' libodbcinst.olb/lib,odbcinst_axp.opt/opt /exec=odbcinst.exe/share
$   write sys$output "Installing image SYS$SHARE:ODBCINST.EXE"
$   copy odbcinst.exe sys$share:odbcinst.exe
$   @install_image sys$share:odbcinst.exe
$
$   write sys$output "Linking ODBC.EXE"
$   link 'LFLAGS' libodbc.olb/lib,odbc_axp.opt/opt /exec=odbc.exe/share
$   write sys$output "Installing image SYS$SHARE:ODBC.EXE"
$   copy odbc.exe sys$share:odbc.exe
$   @install_image sys$share:odbc.exe
$
$   write sys$output "Linking ODBCPSQL.EXE"
$   link 'LFLAGS' libodbcpsql.olb/lib/inc=psqlodbc,odbc2_axp.opt/opt /exec=odbcpsql.exe/share
$   write sys$output "Installing image SYS$SHARE:ODBCPSQL.EXE"
$   copy odbcpsql.exe sys$share:odbcpsql.exe
$   @install_image sys$share:odbcpsql.exe
$
$   write sys$output "Linking ISQL.EXE"
$   set def [-.exe]
$   link 'LFLAGS' ISQL.OBJ,ISQL_AXP.OPT/OPT
$
$   set def 'cwd'
$
$!
$! check for odbc.ini and odbcinst.ini in SYS$SHARE
$! 
$   file = f$search ("SYS$SHARE:ODBC.INI")
$   if file .eqs. ""
$   then 
$     write sys$output "Creating SYS$SHARE:ODBC.INI"
$     create sys$share:odbc.ini

$   endif
$!
$   file = f$search ("SYS$SHARE:ODBCINST.INI")
$   if file .eqs. ""
$   then 
$     write sys$output "Creating SYS$SHARE:ODBCINST.INI"
$     create sys$share:odbcinst.ini

$   endif
$ endif
$!
$ set def 'cwd'
$ exit (1)
$
$ ERR_NOPARAMS:
$ write sys$output " "
$ write sys$output "The correct calling sequence is: "
$ write sys$output " "
$ write sys$output "$ @vmsbuild p1
$ write sys$output " "
$ write sys$output "Where p1 is : "
$ write sys$output " "
$ write sys$output "    ALL      : COMPILE and LINK steps."
$ write sys$output "    CLEAN    : Remove all objects and object libraries."
$ write sys$output "    COMPILE  : Compile all source and produce object libraries"
$ write sys$output "    LINK     : Link, copy and install driver manager shared objects into SYS$SHARE:"
$ write sys$output " "
$ exit 44
$
$!
$! compile subroutine will compile all p1 source files
$!
$ compile : subroutine
$ set def 'p1'
$ loop:
$   file = f$search ("''p2'",1)
$   if file .eqs. "" then goto complete
$   filename = f$parse (file,,,"name")
$   object = F$SEARCH ("''filename'.OBJ;*",2)
$   if object .eqs. ""
$   then
$      write sys$output "cc ''CFLAGS' ''filename'.C"
$      cc 'CFLAGS' 'filename'
$   endif
$   goto loop
$ complete:
$ set def 'cwd'
$ exit
$ endsubroutine : compile
$
