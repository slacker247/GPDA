dnl aclocal.m4 generated automatically by aclocal 1.4

dnl Copyright (C) 1994, 1995-8, 1999 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl This program is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY, to the extent permitted by law; without
dnl even the implied warranty of MERCHANTABILITY or FITNESS FOR A
dnl PARTICULAR PURPOSE.


dnl unixODBC
dnl
dnl This file has been customized for unixODBC.
dnl
dnl 1. Looks for either libqt OR libqt-mt; grep for LIBQTEXACT for details.
dnl    - pharvey@codebydesign.com
dnl

dnl    This file is part of the KDE libraries/packages
dnl    Copyright (C) 1997 Janos Farkas (chexum@shadow.banki.hu)
dnl              (C) 1997,98,99 Stephan Kulow (coolo@kde.org)

dnl    This file is free software; you can redistribute it and/or
dnl    modify it under the terms of the GNU Library General Public
dnl    License as published by the Free Software Foundation; either
dnl    version 2 of the License, or (at your option) any later version.

dnl    This library is distributed in the hope that it will be useful,
dnl    but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
dnl    Library General Public License for more details.

dnl    You should have received a copy of the GNU Library General Public License
dnl    along with this library; see the file COPYING.LIB.  If not, write to
dnl    the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
dnl    Boston, MA 02111-1307, USA.        

dnl IMPORTANT NOTE:
dnl Please do not modify this file unless you expect your modifications to be
dnl carried into every other module in the repository. If you decide that you
dnl really want to modify it, contact coolo@kde.org mentioning that you have
dnl and that the modified file should be committed to every module.
dnl
dnl Single-module modifications are best placed in configure.in for kdelibs
dnl and kdebase or configure.in.in if present.


dnl ------------------------------------------------------------------------
dnl Find a file (or one of more files in a list of dirs)
dnl ------------------------------------------------------------------------
dnl
AC_DEFUN(AC_FIND_FILE,
[
$3=NO
for i in $2;
do
  for j in $1;
  do
    if test -r "$i/$j"; then
      $3=$i
      break 2
    fi
  done
done
])

AC_DEFUN(KDE_FIND_PATH,
[
   AC_MSG_CHECKING([for $1])
   AC_CACHE_VAL(kde_cv_path_$1,
   [
     kde_cv_path_$1="NONE"
     if test -n "$$2"; then
        kde_cv_path_$1="$$2";
     else
	dirs="$3"
	kde_save_IFS=$IFS
	IFS=':'
	for dir in $PATH; do
	  dirs="$dirs $dir"
        done
	IFS=$kde_save_IFS

        for dir in $dirs; do
	  if test -x "$dir/$1"; then
	    if test -n "$5"
	    then
              evalstr="$dir/$1 $5 2>&1 "
	      if eval $evalstr; then
                kde_cv_path_$1="$dir/$1"
                break
	      fi
            else
		kde_cv_path_$1="$dir/$1"
                break
	    fi
          fi
	done

     fi
         
   ])

   if test -z "$kde_cv_path_$1" || test "$kde_cv_path_$1" = "NONE"; then
      AC_MSG_RESULT(not found)
      $4
   else 
     AC_MSG_RESULT($kde_cv_path_$1)
     $2=$kde_cv_path_$1
   fi
])

AC_DEFUN(KDE_MOC_ERROR_MESSAGE,
[
    AC_MSG_ERROR([No Qt meta object compiler (moc) found!
Please check whether you installed Qt correctly. 
You need to have a running moc binary.
configure tried to run $ac_cv_path_moc and the test didn't
succeed. If configure shouldn't have tried this one, set
the environment variable MOC to the right one before running
configure.
])
])

dnl ------------------------------------------------------------------------
dnl Find the meta object compiler in the PATH, in $QTDIR/bin, and some
dnl more usual places
dnl ------------------------------------------------------------------------
dnl
AC_DEFUN(AC_PATH_QT_MOC,
[
   KDE_FIND_PATH(moc, MOC, [$ac_qt_bindir $QTDIR/bin $QTDIR/src/moc \
	    /usr/bin /usr/X11R6/bin /usr/lib/qt/bin \
	    /usr/local/qt/bin])
 
   if test -z "$MOC"; then
     if test -n "$ac_cv_path_moc"; then
       output=`eval "$ac_cv_path_moc --help 2>&1 | sed -e '1q' | grep Qt"`
     fi
     echo "configure:__oline__: tried to call $ac_cv_path_moc --help 2>&1 | sed -e '1q' | grep Qt" >&AC_FD_CC
     echo "configure:__oline__: moc output: $output" >&AC_FD_CC
#
#     if test -z "$output"; then
#       KDE_MOC_ERROR_MESSAGE
#     fi
   fi
   
   AC_SUBST(MOC)
])

AC_DEFUN(AC_CREATE_KFSSTND,
[
AC_MSG_CHECKING([for KDE paths])
kde_result=""

AC_CACHE_VAL(kde_cv_all_paths,
[
  if test -z "$kde_htmldir"; then
    kde_htmldir='\$(prefix)/share/doc/HTML'
  fi
  if test -z "$kde_appsdir"; then
    kde_appsdir='\$(prefix)/share/applnk'
  fi
  if test -z "$kde_icondir"; then
    kde_icondir='\$(prefix)/share/icons'
  fi
  if test -z "$kde_sounddir"; then
    kde_sounddir='\$(prefix)/share/sounds'
  fi
  if test -z "$kde_datadir"; then
    kde_datadir='\$(prefix)/share/apps'
  fi
  if test -z "$kde_locale"; then
    kde_locale='\$(prefix)/share/locale'
  fi
  if test -z "$kde_cgidir"; then
    kde_cgidir='\$(exec_prefix)/cgi-bin'
  fi
  if test -z "$kde_confdir"; then
    kde_confdir='\$(prefix)/share/config'
  fi
  if test -z "$kde_mimedir"; then
    kde_mimedir='\$(prefix)/share/mimelnk'
  fi
  if test -z "$kde_toolbardir"; then
    kde_toolbardir='\$(prefix)/share/toolbar'
  fi
  if test -z "$kde_wallpaperdir"; then
    kde_wallpaperdir='\$(prefix)/share/wallpapers'
  fi
  if test -z "$kde_bindir"; then
    kde_bindir='\$(exec_prefix)/bin'
  fi
  if test -z "$kde_servicesdir"; then
    kde_servicesdir='\$(prefix)/share/services'
  fi
  if test -z "$kde_servicetypesdir"; then
    kde_servicetypesdir='\$(prefix)/share/servicetypes'
  fi

  kde_cv_all_paths="kde_have_all_paths=\"yes\" \
	kde_htmldir=\"$kde_htmldir\" \
	kde_appsdir=\"$kde_appsdir\" \
	kde_icondir=\"$kde_icondir\" \
	kde_sounddir=\"$kde_sounddir\" \
	kde_datadir=\"$kde_datadir\" \
	kde_locale=\"$kde_locale\" \
	kde_cgidir=\"$kde_cgidir\" \
	kde_confdir=\"$kde_confdir\" \
	kde_mimedir=\"$kde_mimedir\" \
	kde_toolbardir=\"$kde_toolbardir\" \
	kde_wallpaperdir=\"$kde_wallpaperdir\" \
	kde_bindir=\"$kde_bindir\" \
	kde_servicesdir=\"$kde_servicesdir\" \
	kde_servicetypesdir=\"$kde_servicetypesdir\" \
	kde_result=defaults"

])

eval "$kde_cv_all_paths"

if test -z "$kde_htmldir" || test -z "$kde_appsdir" ||
   test -z "$kde_icondir" || test -z "$kde_sounddir" ||
   test -z "$kde_datadir" || test -z "$kde_locale"  ||
   test -z "$kde_cgidir"  || test -z "$kde_confdir" ||
   test -z "$kde_mimedir" || test -z "$kde_toolbardir" ||
   test -z "$kde_wallpaperdir" || test -z "$kde_bindir" ||
   test -z "$kde_servicesdir" ||
   test -z "$kde_servicetypesdir" || test "$kde_have_all_paths" != "yes"; then
  kde_have_all_paths=no
  AC_MSG_ERROR([configure could not run a little KDE program to test the environment. 
Since it had compiled and linked before, it must be a strange problem on your system.
Look at config.log for details. If you are not able to fix this, look at
http://www.kde.org/faq/installation.html or any www.kde.org mirror.
(If you're using an egcs version on Linux, you may update binutils!)
])
else
  rm -f conftest*
  AC_MSG_RESULT($kde_result)
fi

bindir=$kde_bindir

])

AC_DEFUN(AC_SUBST_KFSSTND,
[
AC_SUBST(kde_htmldir)
AC_SUBST(kde_appsdir)
AC_SUBST(kde_icondir)
AC_SUBST(kde_sounddir)
kde_minidir="$kde_icondir/mini"
AC_SUBST(kde_minidir)
AC_SUBST(kde_datadir)
AC_SUBST(kde_locale)
AC_SUBST(kde_cgidir)
AC_SUBST(kde_confdir)
AC_SUBST(kde_mimedir)
AC_SUBST(kde_toolbardir)
AC_SUBST(kde_wallpaperdir)
AC_SUBST(kde_bindir)
AC_SUBST(kde_servicesdir)
AC_SUBST(kde_servicetypesdir)
])

AC_DEFUN(KDE_MISC_TESTS,
[
   AC_LANG_C
   dnl Checks for libraries. 
   AC_CHECK_LIB(compat, main, [LIBCOMPAT="-lcompat"]) dnl for FreeBSD
   AC_SUBST(LIBCOMPAT)
   AC_CHECK_LIB(crypt, main, [LIBCRYPT="-lcrypt"]) dnl for BSD
   AC_SUBST(LIBCRYPT)
   AC_CHECK_KSIZE_T
   AC_LANG_C
   AC_CHECK_LIB(dnet, dnet_ntoa, [X_EXTRA_LIBS="$X_EXTRA_LIBS -ldnet"])
   if test $ac_cv_lib_dnet_dnet_ntoa = no; then
      AC_CHECK_LIB(dnet_stub, dnet_ntoa,
        [X_EXTRA_LIBS="$X_EXTRA_LIBS -ldnet_stub"])
   fi
   AC_CHECK_FUNC(inet_ntoa)
   if test $ac_cv_func_inet_ntoa = no; then
     AC_CHECK_LIB(nsl, inet_ntoa, X_EXTRA_LIBS="$X_EXTRA_LIBS -lnsl")
   fi
   AC_CHECK_FUNC(connect)
   if test $ac_cv_func_connect = no; then
      AC_CHECK_LIB(socket, connect, X_EXTRA_LIBS="-lsocket $X_EXTRA_LIBS", ,
        $X_EXTRA_LIBS)
   fi

   AC_CHECK_FUNC(remove)
   if test $ac_cv_func_remove = no; then
      AC_CHECK_LIB(posix, remove, X_EXTRA_LIBS="$X_EXTRA_LIBS -lposix")
   fi
 
   # BSDI BSD/OS 2.1 needs -lipc for XOpenDisplay.
   AC_CHECK_FUNC(shmat)
   if test $ac_cv_func_shmat = no; then
     AC_CHECK_LIB(ipc, shmat, X_EXTRA_LIBS="$X_EXTRA_LIBS -lipc")
   fi

   LIBSOCKET="$X_EXTRA_LIBS"
   AC_SUBST(LIBSOCKET)
   AC_SUBST(X_EXTRA_LIBS)
   AC_CHECK_LIB(ucb, killpg, [LIBUCB="-lucb"]) dnl for Solaris2.4
   AC_SUBST(LIBUCB)

   case $host in  dnl this *is* LynxOS specific
   *-*-lynxos* )
        AC_MSG_CHECKING([LynxOS header file wrappers])
        [CFLAGS="$CFLAGS -D__NO_INCLUDE_WARN__"]
        AC_MSG_RESULT(disabled)
        AC_CHECK_LIB(bsd, gethostbyname, [LIBSOCKET="-lbsd"]) dnl for LynxOS
         ;;
    esac

   AC_REQUIRE([KDE_CHECK_LIBDL])
])

dnl ------------------------------------------------------------------------
dnl Find the header files and libraries for X-Windows. Extended the 
dnl macro AC_PATH_X
dnl ------------------------------------------------------------------------
dnl
AC_DEFUN(K_PATH_X,
[
AC_REQUIRE([AC_PROG_CPP])
AC_MSG_CHECKING(for X)
AC_LANG_SAVE
AC_LANG_C
AC_CACHE_VAL(ac_cv_have_x,
[# One or both of the vars are not set, and there is no cached value.
if test "{$x_includes+set}" = set || test "$x_includes" = NONE; then
   kde_x_includes=NO
else
   kde_x_includes=$x_includes
fi
if test "{$x_libraries+set}" = set || test "$x_libraries" = NONE; then
   kde_x_libraries=NO
else
   kde_x_libraries=$x_libraries
fi

# below we use the standard autoconf calls
ac_x_libraries=$kde_x_libraries
ac_x_includes=$kde_x_includes

AC_PATH_X_DIRECT
AC_PATH_X_XMKMF
if test -z "$ac_x_includes"; then
ac_x_includes="."
fi
if test -z "$ac_x_libraries"; then
ac_x_libraries="/usr/lib"
fi
#from now on we use our own again 

# when the user already gave --x-includes, we ignore
# what the standard autoconf macros told us.
if test "$kde_x_includes" = NO; then
  kde_x_includes=$ac_x_includes  
fi

if test "$kde_x_includes" = NO; then
  AC_MSG_ERROR([Can't find X includes. Please check your installation and add the correct paths!])
fi

if test "$ac_x_libraries" = NO; then
  AC_MSG_ERROR([Can't find X libraries. Please check your installation and add the correct paths!])
fi

# Record where we found X for the cache.
ac_cv_have_x="have_x=yes \
         kde_x_includes=$kde_x_includes ac_x_libraries=$ac_x_libraries"
])dnl
eval "$ac_cv_have_x"
 
if test "$have_x" != yes; then
  AC_MSG_RESULT($have_x)
  no_x=yes
else
  AC_MSG_RESULT([libraries $ac_x_libraries, headers $kde_x_includes])
fi

if test -z "$kde_x_includes" || test "x$kde_x_includes" = xNONE; then
  X_INCLUDES=""
  x_includes="."; dnl better than nothing :-
 else
  x_includes=$kde_x_includes
  X_INCLUDES="-I$x_includes"
fi

if test -z "$ac_x_libraries" || test "x$ac_x_libraries" = xNONE; then
  X_LDFLAGS=""
  x_libraries="/usr/lib"; dnl better than nothing :-
 else
  x_libraries=$ac_x_libraries
  X_LDFLAGS="-L$x_libraries"
fi
all_includes="$all_includes $X_INCLUDES"  
all_libraries="$all_libraries $X_LDFLAGS"  

AC_SUBST(X_INCLUDES)
AC_SUBST(X_LDFLAGS)
AC_SUBST(x_libraries)
AC_SUBST(x_includes)

LIB_X11='-lX11 $(LIBSOCKET)'
AC_SUBST(LIB_X11)

AC_MSG_CHECKING(for libXext)
AC_CACHE_VAL(kde_cv_have_libXext,
[
kde_ldflags_safe="$LDFLAGS"
kde_libs_safe="$LIBS"

LDFLAGS="$X_LDFLAGS $USER_LDFLAGS"
LIBS="-lXext -lX11 $LIBSOCKET"

AC_TRY_LINK([
#include <stdio.h>
],
[
printf("hello Xext\n");
],
kde_cv_have_libXext=yes,
kde_cv_have_libXext=no
   )

LDFLAGS=$kde_ldflags_safe
LIBS=$kde_libs_safe
 ])

AC_MSG_RESULT($kde_cv_have_libXext)

if test "kde_cv_have_libXext" = "no"; then
  AC_MSG_ERROR([We need a working libXext to proceed. Since configure
can't find it itself, we stop here assuming that make wouldn't find
them either.])
fi

])

AC_DEFUN(KDE_PRINT_QT_PROGRAM,
[
AC_REQUIRE([KDE_USE_QT])
cat > conftest.$ac_ext <<EOF
#include "confdefs.h"
#include <qglobal.h>
#include <qapplication.h>
#include <qapp.h>
#include <qobjcoll.h>
EOF
if test $kde_qtver = 2; then
cat >> conftest.$ac_ext <<EOF
#include <qstyle.h>
EOF
fi

echo "#if ! ($kde_qt_verstring)" >> conftest.$ac_ext
cat >> conftest.$ac_ext <<EOF
#error 1
#endif

int main() {
EOF
if test $kde_qtver = 2; then
cat >> conftest.$ac_ext <<EOF
    QStringList *t = new QStringList();
EOF
fi
cat >> conftest.$ac_ext <<EOF
    return 0;
}
EOF
])

AC_DEFUN(KDE_USE_QT,
[
if test -z "$1"; then
  kde_qtver=2
else
  kde_qtver=$1
fi

if test -z "$2"; then
  if test $kde_qtver = 2; then
    kde_qt_minversion=">= 2.2"
   else
    kde_qt_minversion=">= 1.42 and < 2.0"
  fi
else
   kde_qt_minversion=$2
fi

if test -z "$3"; then
    if test $kde_qtver = 2; then
    kde_qt_verstring="QT_VERSION >= 220"
   else
    kde_qt_verstring="QT_VERSION >= 142 && QT_VERSION < 200"
  fi
else
   kde_qt_verstring=$3
fi
])

AC_DEFUN(KDE_CHECK_QT_DIRECT,
[
AC_REQUIRE([KDE_USE_QT])
AC_MSG_CHECKING([if Qt compiles without flags])
AC_CACHE_VAL(kde_cv_qt_direct,
[
AC_LANG_SAVE
AC_LANG_CPLUSPLUS
ac_LD_LIBRARY_PATH_safe=$LD_LIBRARY_PATH
ac_LIBRARY_PATH="$LIBRARY_PATH"
ac_cxxflags_safe="$CXXFLAGS"
ac_ldflags_safe="$LDFLAGS"
ac_libs_safe="$LIBS"

CXXFLAGS="$CXXFLAGS -I$qt_includes"
LDFLAGS="$X_LDFLAGS"
LIBS="$LIBQTEXACT -lXext -lX11 $LIBSOCKET"
LD_LIBRARY_PATH=
export LD_LIBRARY_PATH
LIBRARY_PATH=
export LIBRARY_PATH

KDE_PRINT_QT_PROGRAM

if AC_TRY_EVAL(ac_link) && test -s conftest; then
  kde_cv_qt_direct="yes"
else
  kde_cv_qt_direct="no"
  echo "configure: failed program was:" >&AC_FD_CC
  cat conftest.$ac_ext >&AC_FD_CC
fi

rm -f conftest*
CXXFLAGS="$ac_cxxflags_safe"
LDFLAGS="$ac_ldflags_safe"
LIBS="$ac_libs_safe"

LD_LIBRARY_PATH="$ac_LD_LIBRARY_PATH_safe"
export LD_LIBRARY_PATH
LIBRARY_PATH="$ac_LIBRARY_PATH"
export LIBRARY_PATH
AC_LANG_RESTORE
])

if test "$kde_cv_qt_direct" = "yes"; then
  AC_MSG_RESULT(yes)
  $1
else
  AC_MSG_RESULT(no)
  $2
fi
])

AC_DEFUN(AC_QT_TEXT,
[
ac_qt_includes=NO ac_qt_libraries=NO ac_qt_bindir=NO
qt_libraries=""
qt_includes=""
AC_ARG_WITH(qt-dir,
    [  --with-qt-dir=DIR       where the root of Qt is installed ],
    [  ac_qt_includes="$withval"/include
       ac_qt_libraries="$withval"/lib
       ac_qt_bindir="$withval"/bin
    ])

AC_ARG_WITH(qt-includes,
    [  --with-qt-includes=DIR  where the Qt includes are. ],
    [  
       ac_qt_includes="$withval"
    ])
    
kde_qt_libs_given=no

AC_ARG_WITH(qt-libraries,
    [  --with-qt-libraries=DIR where the Qt library is installed.],
    [  ac_qt_libraries="$withval"
       kde_qt_libs_given=yes
    ])
])

dnl ------------------------------------------------------------------------
dnl Try to find the Qt headers and libraries.
dnl $(QT_LDFLAGS) will be -Lqtliblocation (if needed)
dnl and $(QT_INCLUDES) will be -Iqthdrlocation (if needed)
dnl ------------------------------------------------------------------------
dnl
AC_DEFUN(AC_PATH_QT_1_3,
[
AC_REQUIRE([K_PATH_X])
AC_REQUIRE([KDE_USE_QT])
AC_REQUIRE([AC_QT_TEXT])
LIBQT=""
LIBQTEXACT="-lqt"
if test $kde_qtver = 2; then
  AC_REQUIRE([AC_FIND_PNG])
  LIBQT="$LIBQT $LIBPNG"
fi
AC_MSG_CHECKING([for Qt])

LIBQT="$LIBQT $EXTRA_XLIBS -lXext -lX11 $LIBSOCKET"

AC_CACHE_VAL(ac_cv_have_qt,
[#try to guess Qt locations

qt_incdirs="$QTINC /usr/lib/qt/include /usr/local/qt/include /usr/include/qt /usr/include /usr/X11R6/include/X11/qt $x_includes"
test -n "$QTDIR" && qt_incdirs="$QTDIR/include $QTDIR $qt_incdirs"
qt_incdirs="$ac_qt_includes $qt_incdirs"
AC_FIND_FILE(qmovie.h, $qt_incdirs, qt_incdir)
ac_qt_includes="$qt_incdir"

qt_libdirs="$QTLIB /usr/lib/qt/lib /usr/X11R6/lib /usr/lib /usr/local/qt/lib /usr/lib/qt $x_libraries"
test -n "$QTDIR" && qt_libdirs="$QTDIR/lib $QTDIR $qt_libdirs"
if test ! "$ac_qt_libraries" = "NO"; then
  qt_libdirs="$ac_qt_libraries $qt_libdirs"
fi

test=NONE
qt_libdir=NONE
for dir in $qt_libdirs; do
  try="ls -1 $dir/libqt-mt*"
  if test=`eval $try 2> /dev/null`; then 
    qt_libdir=$dir;
    LIBQTEXACT="-lqt-mt"
    break; 
  else 
    try="ls -1 $dir/libqt*"
    if test=`eval $try 2> /dev/null`; then 
      qt_libdir=$dir;
      break; 
    else 
      echo "tried $dir" >&AC_FD_CC ; fi
    echo "tried $dir" >&AC_FD_CC ; fi
done

LIBQT="$LIBQTEXACT $EXTRA_XLIBS -lXext -lX11 $LIBSOCKET"

ac_qt_libraries="$qt_libdir"

AC_LANG_SAVE
AC_LANG_CPLUSPLUS

ac_cxxflags_safe="$CXXFLAGS"
ac_ldflags_safe="$LDFLAGS"
ac_libs_safe="$LIBS"

CXXFLAGS="$CXXFLAGS -I$qt_incdir $all_includes"
LDFLAGS="-L$qt_libdir $all_libraries"
LIBS="$LIBS $LIBQT"

KDE_PRINT_QT_PROGRAM

if AC_TRY_EVAL(ac_link) && test -s conftest; then
  rm -f conftest*
else
  echo "configure: failed program was:" >&AC_FD_CC
  cat conftest.$ac_ext >&AC_FD_CC
  ac_qt_libraries="NO"
fi
rm -f conftest*
CXXFLAGS="$ac_cxxflags_safe"
LDFLAGS="$ac_ldflags_safe"
LIBS="$ac_libs_safe"

AC_LANG_RESTORE
if test "$ac_qt_includes" = NO || test "$ac_qt_libraries" = NO; then
  ac_cv_have_qt="have_qt=no"
  ac_qt_notfound=""
  if test "$ac_qt_includes" = NO; then
    if test "$ac_qt_libraries" = NO; then
      ac_qt_notfound="(headers and libraries)";
    else
      ac_qt_notfound="(headers)";
    fi
  else
    ac_qt_notfound="(libraries)";
  fi
  have_qt="no"
else
  have_qt="yes"
fi
])

eval "$ac_cv_have_qt"

if test "$have_qt" != yes; then
  AC_MSG_RESULT([$have_qt]);
else
  ac_cv_have_qt="have_qt=yes \
    ac_qt_includes=$ac_qt_includes ac_qt_libraries=$ac_qt_libraries"
  AC_MSG_RESULT([libraries $ac_qt_libraries, headers $ac_qt_includes])
  
  qt_libraries="$ac_qt_libraries"
  qt_includes="$ac_qt_includes"
fi

if test ! "$kde_qt_libs_given" = "yes"; then
KDE_CHECK_QT_DIRECT(qt_libraries= ,[])
fi

AC_SUBST(qt_libraries)
AC_SUBST(qt_includes)

if test "$qt_includes" = "$x_includes" || test -z "$qt_includes"; then
 QT_INCLUDES="";
else
 QT_INCLUDES="-I$qt_includes"
 all_includes="$QT_INCLUDES $all_includes"
fi

if test "$qt_libraries" = "$x_libraries" || test -z "$qt_libraries"; then
 QT_LDFLAGS=""
else
 QT_LDFLAGS="-L$qt_libraries"
 all_libraries="$QT_LDFLAGS $all_libraries"
fi

AC_SUBST(QT_INCLUDES)
AC_SUBST(QT_LDFLAGS)
AC_PATH_QT_MOC

LIB_QT='$(LIBPNG) -lXext $(LIB_X11)'
LIB_QT="$LIBQTEXACT $LIB_QT"
AC_SUBST(LIB_QT)

])

dnl ------------------------------------------------------------------------
dnl Try to find the Qt headers and libraries without expecting X
dnl $(QT_LDFLAGS) will be -Lqtliblocation (if needed)
dnl and $(QT_INCLUDES) will be -Iqthdrlocation (if needed)
dnl ------------------------------------------------------------------------
dnl
AC_DEFUN(AC_PATH_QT_2_2OSX,
[
AC_REQUIRE([KDE_USE_QT])
AC_REQUIRE([AC_QT_TEXT])
LIBQT="-lqt"
if test kde_qtver = 2; then
  AC_REQUIRE([AC_FIND_PNG])
  LIBQT="$LIBQT $LIBPNG"
fi
AC_MSG_CHECKING([for Qt])

LIBQT="$LIBQT $LIBSOCKET"

AC_CACHE_VAL(ac_cv_have_qt,
[#try to guess Qt locations

qt_incdirs="$QTINC /usr/lib/qt/include /usr/local/qt/include /usr/include/qt /usr/include"
test -n "$QTDIR" && qt_incdirs="$QTDIR/include $QTDIR $qt_incdirs"
qt_incdirs="$ac_qt_includes $qt_incdirs"
AC_FIND_FILE(qmovie.h, $qt_incdirs, qt_incdir)
ac_qt_includes="$qt_incdir"

qt_libdirs="$QTLIB /usr/lib/qt/lib /usr/lib /usr/local/qt/lib /usr/lib/qt"
test -n "$QTDIR" && qt_libdirs="$QTDIR/lib $QTDIR $qt_libdirs"
if test ! "$ac_qt_libraries" = "NO"; then
  qt_libdirs="$ac_qt_libraries $qt_libdirs"
fi

test=NONE
qt_libdir=NONE
for dir in $qt_libdirs; do
  try="ls -1 $dir/libqt*"
  if test=`eval $try 2> /dev/null`; then qt_libdir=$dir; break; else echo "tried $dir" >&AC_FD_CC ; fi
done

ac_qt_libraries="$qt_libdir"

AC_LANG_SAVE
AC_LANG_CPLUSPLUS

ac_cxxflags_safe="$CXXFLAGS"
ac_ldflags_safe="$LDFLAGS"
ac_libs_safe="$LIBS"

CXXFLAGS="$CXXFLAGS -I$qt_incdir $all_includes"
LDFLAGS="-L$qt_libdir $all_libraries"
LIBS="$LIBS $LIBQT"

KDE_PRINT_QT_PROGRAM

if AC_TRY_EVAL(ac_link) && test -s conftest; then
  rm -f conftest*
else
  echo "configure: failed program was:" >&AC_FD_CC
  cat conftest.$ac_ext >&AC_FD_CC
  ac_qt_libraries="NO"
fi
rm -f conftest*
CXXFLAGS="$ac_cxxflags_safe"
LDFLAGS="$ac_ldflags_safe"
LIBS="$ac_libs_safe"

AC_LANG_RESTORE
if test "$ac_qt_includes" = NO || test "$ac_qt_libraries" = NO; then
  ac_cv_have_qt="have_qt=no"
  ac_qt_notfound=""
  if test "$ac_qt_includes" = NO; then
    if test "$ac_qt_libraries" = NO; then
      ac_qt_notfound="(headers and libraries)";
    else
      ac_qt_notfound="(headers)";
    fi
  else
    ac_qt_notfound="(libraries)";
  fi
  have_qt="no"
else
  have_qt="yes"
fi
])

eval "$ac_cv_have_qt"

if test "$have_qt" != yes; then
  AC_MSG_RESULT([$have_qt]);
  LIBQT="-lqt-mt"
  if test kde_qtver = 2; then
    AC_REQUIRE([AC_FIND_PNG])
    LIBQT="$LIBQT $LIBPNG"
  fi
  AC_MSG_CHECKING([for Qt-Mt])

  LIBQT="$LIBQT $LIBSOCKET"
  
  AC_CACHE_VAL(ac_cv_have_qt_mt,
  [#try to guess Qt locations
  
  qt_incdirs="$QTINC /usr/lib/qt/include /usr/local/qt/include /usr/include/qt /usr/include"
  test -n "$QTDIR" && qt_incdirs="$QTDIR/include $QTDIR $qt_incdirs"
  qt_incdirs="$ac_qt_includes $qt_incdirs"
  AC_FIND_FILE(qmovie.h, $qt_incdirs, qt_incdir)
  ac_qt_includes="$qt_incdir"
  
  qt_libdirs="$QTLIB /usr/lib/qt/lib /usr/lib /usr/local/qt/lib /usr/lib/qt"
  test -n "$QTDIR" && qt_libdirs="$QTDIR/lib $QTDIR $qt_libdirs"
  if test ! "$ac_qt_libraries" = "NO"; then
    qt_libdirs="$ac_qt_libraries $qt_libdirs"
  fi
  
  test=NONE
  qt_libdir=NONE
  for dir in $qt_libdirs; do
    try="ls -1 $dir/libqt*"
    if test=`eval $try 2> /dev/null`; then qt_libdir=$dir; break; else echo "tried $dir" >&AC_FD_CC ; fi
  done

  ac_qt_libraries="$qt_libdir"
  
  AC_LANG_SAVE
  AC_LANG_CPLUSPLUS
  
  ac_cxxflags_safe="$CXXFLAGS"
  ac_ldflags_safe="$LDFLAGS"
  ac_libs_safe="$LIBS"
  
  CXXFLAGS="$CXXFLAGS -I$qt_incdir $all_includes"
  LDFLAGS="-L$qt_libdir $all_libraries"
  LIBS="$LIBS $LIBQT"
  
  KDE_PRINT_QT_PROGRAM
  
  if AC_TRY_EVAL(ac_link) && test -s conftest; then
    rm -f conftest*
  else
    echo "configure: failed program was:" >&AC_FD_CC
    cat conftest.$ac_ext >&AC_FD_CC
    ac_qt_libraries="NO"
  fi
  rm -f conftest*
  CXXFLAGS="$ac_cxxflags_safe"
  LDFLAGS="$ac_ldflags_safe"
  LIBS="$ac_libs_safe"

  AC_LANG_RESTORE
  if test "$ac_qt_includes" = NO || test "$ac_qt_libraries" = NO; then
    ac_cv_have_qt_mt="have_qt=no"
    ac_qt_notfound=""
    if test "$ac_qt_includes" = NO; then
      if test "$ac_qt_libraries" = NO; then
        ac_qt_notfound="(headers and libraries)";
      else
        ac_qt_notfound="(headers)";
      fi
    else
      ac_qt_notfound="(libraries)";
    fi
    have_qt="no"
  else
    have_qt="yes"
  fi
  ])
  if test "$have_qt" != yes; then
    AC_MSG_RESULT([$have_qt]);
  fi
fi

if test "$have_qt" = yes; then
  ac_cv_have_qt="have_qt=yes \
    ac_qt_includes=$ac_qt_includes ac_qt_libraries=$ac_qt_libraries"
  AC_MSG_RESULT([libraries $ac_qt_libraries, headers $ac_qt_includes])
  
  qt_libraries="$ac_qt_libraries"
  qt_includes="$ac_qt_includes"
fi

if test ! "$kde_qt_libs_given" = "yes"; then
KDE_CHECK_QT_DIRECT(qt_libraries= ,[])
fi

AC_SUBST(qt_libraries)
AC_SUBST(qt_includes)

if test "$qt_includes" = "$x_includes" || test -z "$qt_includes"; then
 QT_INCLUDES="";
else
 QT_INCLUDES="-I$qt_includes"
 all_includes="$QT_INCLUDES $all_includes"
fi

if test "$qt_libraries" = "$x_libraries" || test -z "$qt_libraries"; then
 QT_LDFLAGS=""
else
 QT_LDFLAGS="-L$qt_libraries"
 all_libraries="$QT_LDFLAGS $all_libraries"
fi

AC_SUBST(QT_INCLUDES)
AC_SUBST(QT_LDFLAGS)
AC_PATH_QT_MOC

LIB_QT="$LIBQT"
AC_SUBST(LIB_QT)

])

AC_DEFUN(AC_PATH_QT,
[
KDE_USE_QT
AC_PATH_QT_1_3
])

AC_DEFUN(AC_PATH_QTOSX,
[
KDE_USE_QT
AC_PATH_QT_2_2OSX
])

AC_DEFUN(KDE_CHECK_FINAL,
[
  AC_ARG_ENABLE(final, [  --enable-final          build size optimized apps (needs lots of memory)],
	kde_use_final=yes, kde_use_final=no, kde_use_final=no)

  if test "x$kde_use_final" = "xyes"; then
      KDE_USE_FINAL_TRUE=""
      KDE_USE_FINAL_FALSE="#"
   else
      KDE_USE_FINAL_TRUE="#"
      KDE_USE_FINAL_FALSE=""
  fi
  AC_SUBST(KDE_USE_FINAL_TRUE)
  AC_SUBST(KDE_USE_FINAL_FALSE)
])

dnl ------------------------------------------------------------------------
dnl Now, the same with KDE
dnl $(KDE_LDFLAGS) will be the kdeliblocation (if needed)
dnl and $(kde_includes) will be the kdehdrlocation (if needed)
dnl ------------------------------------------------------------------------
dnl
AC_DEFUN(AC_BASE_PATH_KDE,
[
AC_PREREQ([2.13])
AC_REQUIRE([KDE_MISC_TESTS])
AC_REQUIRE([AC_PATH_QT])dnl
AC_CHECK_RPATH
AC_MSG_CHECKING([for KDE])

if test "${prefix}" != NONE; then
  kde_includes=${prefix}/include
  ac_kde_includes=$prefix/include

  if test "${exec_prefix}" != NONE; then
    kde_libraries=${exec_prefix}/lib
    ac_kde_libraries=$exec_prefix/lib
  else
    kde_libraries=${prefix}/lib
    ac_kde_libraries=$prefix/lib
  fi
else
  ac_kde_includes=
  ac_kde_libraries=
  kde_libraries=""
  kde_includes=""
fi

AC_CACHE_VAL(ac_cv_have_kde,
[#try to guess kde locations

if test -z "$1"; then

kde_incdirs="/usr/lib/kde/include /usr/local/kde/include /usr/kde/include /usr/include/kde /usr/include /opt/kde/include $x_includes $qt_includes"
test -n "$KDEDIR" && kde_incdirs="$KDEDIR/include $KDEDIR $kde_incdirs"
kde_incdirs="$ac_kde_includes $kde_incdirs"
AC_FIND_FILE(ksock.h, $kde_incdirs, kde_incdir)
ac_kde_includes="$kde_incdir"

if test -n "$ac_kde_includes" && test ! -r "$ac_kde_includes/ksock.h"; then
  AC_MSG_ERROR([
in the prefix, you've chosen, are no KDE headers installed. This will fail.
So, check this please and use another prefix!])
fi

kde_libdirs="/usr/lib/kde/lib /usr/local/kde/lib /usr/kde/lib /usr/lib/kde /usr/lib /usr/X11R6/lib /opt/kde/lib /usr/X11R6/kde/lib"
test -n "$KDEDIR" && kde_libdirs="$KDEDIR/lib $KDEDIR $kde_libdirs"
kde_libdirs="$ac_kde_libraries $kde_libdirs"
AC_FIND_FILE(libkdecore.la, $kde_libdirs, kde_libdir)
ac_kde_libraries="$kde_libdir"

if test -n "$ac_kde_libraries" && test ! -r "$ac_kde_libraries/libkdecore.la"; then
AC_MSG_ERROR([
in the prefix, you've chosen, are no KDE libraries installed. This will fail.
So, check this please and use another prefix!])
fi
ac_kde_libraries="$kde_libdir"

if test "$ac_kde_includes" = NO || test "$ac_kde_libraries" = NO; then
  ac_cv_have_kde="have_kde=no"
else
  ac_cv_have_kde="have_kde=yes \
    ac_kde_includes=$ac_kde_includes ac_kde_libraries=$ac_kde_libraries"
fi

else dnl test -z $1 
  
  ac_cv_have_kde="have_kde=no"

fi
])dnl

eval "$ac_cv_have_kde"

if test "$have_kde" != "yes"; then
 if test "${prefix}" = NONE; then
  ac_kde_prefix="$ac_default_prefix"
 else
  ac_kde_prefix="$prefix"
 fi
 if test "$exec_prefix" = NONE; then
  ac_kde_exec_prefix="$ac_kde_prefix"
  AC_MSG_RESULT([will be installed in $ac_kde_prefix])
 else
  ac_kde_exec_prefix="$exec_prefix"
  AC_MSG_RESULT([will be installed in $ac_kde_prefix and $ac_kde_exec_prefix])
 fi

 kde_libraries="${ac_kde_exec_prefix}/lib"
 kde_includes=${ac_kde_prefix}/include

else
  ac_cv_have_kde="have_kde=yes \
    ac_kde_includes=$ac_kde_includes ac_kde_libraries=$ac_kde_libraries"
  AC_MSG_RESULT([libraries $ac_kde_libraries, headers $ac_kde_includes])
  
  kde_libraries="$ac_kde_libraries"
  kde_includes="$ac_kde_includes"
fi
AC_SUBST(kde_libraries)
AC_SUBST(kde_includes)

if test "$kde_includes" = "$x_includes" || test "$kde_includes" = "$qt_includes" ; then
 KDE_INCLUDES=""
else
 KDE_INCLUDES="-I$kde_includes"
 all_includes="$KDE_INCLUDES $all_includes"
fi

if test "$kde_libraries" = "$x_libraries" || test "$kde_libraries" = "$qt_libraries" ; then
 KDE_LDFLAGS=""
else
 KDE_LDFLAGS="-L$kde_libraries"
 all_libraries="$KDE_LDFLAGS $all_libraries"
fi

AC_SUBST(KDE_LDFLAGS)
AC_SUBST(KDE_INCLUDES)

AC_REQUIRE([KDE_CHECK_EXTRA_LIBS])

AC_SUBST(all_includes)
AC_SUBST(all_libraries)

])

AC_DEFUN(KDE_CHECK_EXTRA_LIBS,
[
AC_MSG_CHECKING(for extra includes)
AC_ARG_WITH(extra-includes, [  --with-extra-includes=DIR
                          adds non standard include paths], 
  kde_use_extra_includes="$withval",
  kde_use_extra_includes=NONE
)
kde_extra_includes=
if test -n "$kde_use_extra_includes" && \
   test "$kde_use_extra_includes" != "NONE"; then

   ac_save_ifs=$IFS
   IFS=':'
   for dir in $kde_use_extra_includes; do
     kde_extra_includes="$kde_extra_includes $dir"
     all_includes="$all_includes -I$dir"
     USER_INCLUDES="$USER_INCLUDES -I$dir"
   done
   IFS=$ac_save_ifs
   kde_use_extra_includes="added"
else
   kde_use_extra_includes="no"
fi
AC_SUBST(USER_INCLUDES)

AC_MSG_RESULT($kde_use_extra_includes)
 
kde_extra_libs=
AC_MSG_CHECKING(for extra libs)
AC_ARG_WITH(extra-libs, [  --with-extra-libs=DIR   adds non standard library paths], 
  kde_use_extra_libs=$withval,
  kde_use_extra_libs=NONE
)
if test -n "$kde_use_extra_libs" && \
   test "$kde_use_extra_libs" != "NONE"; then

   ac_save_ifs=$IFS
   IFS=':'
   for dir in $kde_use_extra_libs; do
     kde_extra_libs="$kde_extra_libs $dir"
     all_libraries="$all_libraries -L$dir"
     KDE_EXTRA_RPATH="$KDE_EXTRA_RPATH -rpath $dir"
     USER_LDFLAGS="$USER_LDFLAGS -L$dir"
   done
   IFS=$ac_save_ifs
   kde_use_extra_libs="added"
else
   kde_use_extra_libs="no"
fi

AC_MSG_RESULT($kde_use_extra_libs)

kde_extra_xlibs=
AC_MSG_CHECKING(for extra xlibs)
AC_ARG_WITH(extra-xlibs, [  --with-extra-xlibs=     adds non standard library for X ], 
  kde_use_extra_xlibs=$withval,
  kde_use_extra_xlibs=NONE
)
if test -n "$kde_use_extra_xlibs" && \
   test "$kde_use_extra_xlibs" != "NONE"; then

   EXTRA_XLIBS="$kde_use_extra_xlibs"
   USER_LDFLAGS="$USER_LDFLAGS $EXTRA_XLIBS"
   kde_use_extra_xlibs="added"
else
   kde_use_extra_xlibs="no"
fi

AC_MSG_RESULT($kde_use_extra_xlibs)

AC_SUBST(USER_LDFLAGS)

])


AC_DEFUN(KDE_CHECK_KIMGIO,
[
   AC_REQUIRE([AC_BASE_PATH_KDE])
   AC_REQUIRE([KDE_CHECK_EXTRA_LIBS])
   AC_REQUIRE([AC_FIND_TIFF])
   AC_REQUIRE([AC_FIND_JPEG]) 
   AC_REQUIRE([AC_FIND_PNG]) 
   AC_REQUIRE([KDE_CREATE_LIBS_ALIASES])
   
   if test "$1" = "existance"; then
     AC_LANG_SAVE
     AC_LANG_CPLUSPLUS
     kde_save_LIBS="$LIBS"
     LIBS="$LIBS $all_libraries $LIBJPEG $LIBTIFF $LIBPNG $LIBQT -lm"
     AC_CHECK_LIB(kimgio, kimgioRegister, [
      LIBKIMGIO_EXISTS=yes],LIBKIMGIO_EXISTS=no)
      LIBS="$kde_save_LIBS"
      AC_LANG_RESTORE   
   else
      LIBKIMGIO_EXISTS=yes
   fi

   if test "$LIBKIMGIO_EXISTS" = "yes"; then
     LIB_KIMGIO='-lkimgio'
   else
     LIB_KIMGIO=''
   fi
   AC_SUBST(LIB_KIMGIO)
   
   LIB_KHTML='-lkhtml'
   AC_SUBST(LIB_KHTML)
])

AC_DEFUN(KDE_CREATE_LIBS_ALIASES,
[
   AC_REQUIRE([KDE_MISC_TESTS])
   AC_REQUIRE([KDE_CHECK_LIBDL])
   AC_REQUIRE([K_PATH_X])

if test $kde_qtver = 2; then
   LIB_KDECORE='-lkdecore'
   AC_SUBST(LIB_KDECORE)
   LIB_KDEUI='-lkdeui'
   AC_SUBST(LIB_KDEUI)
   LIB_KFORMULA='-lformula'
   AC_SUBST(LIB_KFORMULA)
   LIB_KIO='-lkio'
   AC_SUBST(LIB_KIO)
   LIB_SMB='-lsmb'
   AC_SUBST(LIB_SMB)
   LIB_KFILE='-lkfile'
   AC_SUBST(LIB_KFILE)
   LIB_KAB='-lkab'
   AC_SUBST(LIB_KAB)
   LIB_MEDIATOOL='-lmediatool'
   AC_SUBST(LIB_MEDIATOOL) 
else
   LIB_KDECORE='-lkdecore -lXext $(LIB_QT)'
   AC_SUBST(LIB_KDECORE)
   LIB_KDEUI='-lkdeui $(LIB_KDECORE)'
   AC_SUBST(LIB_KDEUI)
   LIB_KFM='-lkfm $(LIB_KDECORE)'
   AC_SUBST(LIB_KFM)
   LIB_KFILE='-lkfile $(LIB_KFM) $(LIB_KDEUI)'
   AC_SUBST(LIB_KFILE)
   LIB_KAB='-lkab $(LIB_KIMGIO) $(LIB_KDECORE)'
   AC_SUBST(LIB_KAB)
   LIB_MEDIATOOL='-lmediatool $(LIB_KDECORE)'
   AC_SUBST(LIB_MEDIATOOL)
fi
])

AC_DEFUN(AC_PATH_KDE,
[
  AC_BASE_PATH_KDE
  AC_ARG_ENABLE(path-check, [  --disable-path-check    don't try to find out, where to install],
  [
  if test "$enableval" = "no"; 
    then ac_use_path_checking="default"
    else ac_use_path_checking=""
  fi
  ], [ac_use_path_checking="default"]
  )

  AC_CREATE_KFSSTND($ac_use_path_checking)

  AC_SUBST_KFSSTND
  KDE_CREATE_LIBS_ALIASES
])

dnl slightly changed version of AC_CHECK_FUNC(setenv)
AC_DEFUN(AC_CHECK_SETENV,
[AC_MSG_CHECKING([for setenv])
AC_CACHE_VAL(ac_cv_func_setenv,
[AC_LANG_C
AC_TRY_LINK(
dnl Don't include <ctype.h> because on OSF/1 3.0 it includes <sys/types.h>
dnl which includes <sys/select.h> which contains a prototype for
dnl select.  Similarly for bzero.
[#include <assert.h>
]ifelse(AC_LANG, CPLUSPLUS, [#ifdef __cplusplus
extern "C"
#endif
])dnl
[/* We use char because int might match the return type of a gcc2
    builtin and then its argument prototype would still apply.  */
#include <stdlib.h>
], [
/* The GNU C library defines this for functions which it implements
    to always fail with ENOSYS.  Some functions are actually named
    something starting with __ and the normal name is an alias.  */
#if defined (__stub_$1) || defined (__stub___$1)
choke me
#else
setenv("TEST", "alle", 1);
#endif
], eval "ac_cv_func_setenv=yes", eval "ac_cv_func_setenv=no")])

if test "$ac_cv_func_setenv" = "yes"; then
  AC_MSG_RESULT(yes)
  AC_DEFINE_UNQUOTED(HAVE_FUNC_SETENV, 1, [Define if you have setenv])
else
  AC_MSG_RESULT(no)
fi
])

AC_DEFUN(AC_CHECK_GETDOMAINNAME,
[
AC_LANG_CPLUSPLUS
save_CXXFLAGS="$CXXFLAGS"
if test "$GCC" = "yes"; then
CXXFLAGS="$CXXFLAGS -pedantic-errors"
fi
AC_MSG_CHECKING(for getdomainname)
AC_CACHE_VAL(ac_cv_func_getdomainname,
[
AC_TRY_COMPILE([
#include <stdlib.h>
#include <unistd.h>
],
[
char buffer[200];
getdomainname(buffer, 200);
],
ac_cv_func_getdomainname=yes,
ac_cv_func_getdomainname=no)
])
AC_MSG_RESULT($ac_cv_func_getdomainname)
if eval "test \"`echo $ac_cv_func_getdomainname`\" = yes"; then
  AC_DEFINE(HAVE_GETDOMAINNAME, 1, [Define if you have getdomainname])
fi
CXXFLAGS="$save_CXXFLAGS"
])

AC_DEFUN(AC_CHECK_GETHOSTNAME,
[
AC_LANG_CPLUSPLUS
save_CXXFLAGS="$CXXFLAGS"
if test "$GCC" = "yes"; then
CXXFLAGS="$CXXFLAGS -pedantic-errors"
fi

AC_MSG_CHECKING([for gethostname])
AC_CACHE_VAL(ac_cv_func_gethostname,
[
AC_TRY_COMPILE([
#include <stdlib.h>
#include <unistd.h>
],
[
char buffer[200];
gethostname(buffer, 200);
],
ac_cv_func_gethostname=yes,
ac_cv_func_gethostname=no)
])
AC_MSG_RESULT($ac_cv_func_gethostname)
if eval "test \"`echo $ac_cv_func_gethostname`\" = yes"; then
  AC_DEFINE(HAVE_GETHOSTNAME, 1, [Define if you have getdomainname])
fi
CXXFLAGS="$save_CXXFLAGS"
])

AC_DEFUN(AC_CHECK_USLEEP,
[
AC_LANG_CPLUSPLUS

AC_MSG_CHECKING([for usleep])
AC_CACHE_VAL(ac_cv_func_usleep,
[
ac_libs_safe="$LIBS"
LIBS="$LIBS $LIBUCB"
AC_TRY_LINK([
#include <stdlib.h>
#include <unistd.h>
],
[
usleep(200);
],
ac_cv_func_usleep=yes,
ac_cv_func_usleep=no)
])
AC_MSG_RESULT($ac_cv_func_usleep)
if eval "test \"`echo $ac_cv_func_usleep`\" = yes"; then
  AC_DEFINE(HAVE_USLEEP, 1, [Define if you have the usleep function])
fi
LIBS="$ac_libs_safe"
])

AC_DEFUN(AC_CHECK_RANDOM,
[
AC_LANG_CPLUSPLUS

AC_MSG_CHECKING([for random])
AC_CACHE_VAL(ac_cv_func_random,
[
ac_libs_safe="$LIBS"
LIBS="$LIBS $LIBUCB"
AC_TRY_LINK([
#include <stdlib.h>
],
[
random();
],
ac_cv_func_random=yes,
ac_cv_func_random=no)
])
AC_MSG_RESULT($ac_cv_func_random)
if eval "test \"`echo $ac_cv_func_random`\" = yes"; then
  AC_DEFINE(HAVE_RANDOM, 1, [Define if you have random])
fi
LIBS="$ac_libs_safe"
])

AC_DEFUN(AC_CHECK_SEMUNDOO,
[
AC_LANG_C

AC_MSG_CHECKING([for semundo union])
AC_CACHE_VAL(ac_cv_semundo_union,
[
AC_TRY_LINK([
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>
],
[
union semun                 semctl_arg;
],
ac_cv_semundo_union=no,
ac_cv_semundo_union=yes)
])
AC_MSG_RESULT($ac_cv_semundo_union)
if eval "test \"`echo $ac_cv_semundo_union`\" = yes"; then
  AC_DEFINE(NEED_SEMUNDO_UNION, 1, [Define if you need semundo union])
fi
])

AC_DEFUN(AC_FIND_GIF,
   [AC_MSG_CHECKING([for giflib])
AC_CACHE_VAL(ac_cv_lib_gif,
[ac_save_LIBS="$LIBS"
LIBS="$all_libraries -lgif -lX11 $LIBSOCKET"
AC_TRY_LINK(dnl
[
#ifdef __cplusplus
extern "C" {
#endif
int GifLastError(void);
#ifdef __cplusplus
}
#endif
/* We use char because int might match the return type of a gcc2
    builtin and then its argument prototype would still apply.  */
],
            [return GifLastError();],
            eval "ac_cv_lib_gif=yes",
            eval "ac_cv_lib_gif=no")
LIBS="$ac_save_LIBS"
])dnl
if eval "test \"`echo $ac_cv_lib_gif`\" = yes"; then
  AC_MSG_RESULT(yes)
  AC_DEFINE_UNQUOTED(HAVE_LIBGIF, 1, [Define if you have libgif])
else
  AC_MSG_ERROR(You need giflib30. Please install the kdesupport package)
fi
])

AC_DEFUN(KDE_FIND_JPEG_HELPER,
[
AC_MSG_CHECKING([for libjpeg$2])
AC_CACHE_VAL(ac_cv_lib_jpeg_$1,
[
AC_LANG_C
ac_save_LIBS="$LIBS"
LIBS="$all_libraries -ljpeg$2 -lm"
AC_TRY_LINK(
[/* Override any gcc2 internal prototype to avoid an error.  */
struct jpeg_decompress_struct;
typedef struct jpeg_decompress_struct * j_decompress_ptr;
typedef int size_t;
#ifdef __cplusplus
extern "C" {
#endif
    void jpeg_CreateDecompress(j_decompress_ptr cinfo,
                                    int version, size_t structsize);
#ifdef __cplusplus
}
#endif
/* We use char because int might match the return type of a gcc2
    builtin and then its argument prototype would still apply.  */
],
            [jpeg_CreateDecompress(0L, 0, 0);],
            eval "ac_cv_lib_jpeg_$1=-ljpeg$2",
            eval "ac_cv_lib_jpeg_$1=no")
LIBS="$ac_save_LIBS"
])

if eval "test ! \"`echo $ac_cv_lib_jpeg_$1`\" = no"; then
  LIBJPEG="$ac_cv_lib_jpeg_$1"
  AC_MSG_RESULT($ac_cv_lib_jpeg_$1)
else
  AC_MSG_RESULT(no)
  $3
fi

])

AC_DEFUN(AC_FIND_JPEG,
[
KDE_FIND_JPEG_HELPER(6b, 6b,
   KDE_FIND_JPEG_HELPER(normal, [],
    [
dnl what to do, if the normal way fails:
	if test -f "$kde_libraries/libjpeg.so"; then
	   test -f ./libjpegkde.so || $LN_S $kde_libraries/libjpeg.so ./libjpegkde.so
	   ac_cv_lib_jpeg="-L\${topdir} -ljpegkde"
	else if test -f "$kde_libraries/libjpeg.sl"; then
	   test -f ./libjpegkde.sl ||$LN_S $kde_libraries/libjpeg.sl ./libjpegkde.sl
	   ac_cv_lib_jpeg="-L\${topdir} -ljpegkde"	
	else if test -f "$kde_libraries/libjpeg.a"; then
	   test -f ./libjpegkde.a || $LN_S $kde_libraries/libjpeg.a ./libjpegkde.a
	   ac_cv_lib_jpeg="-L\${topdir} -ljpegkde"
        else
	  AC_MSG_ERROR([
You need jpeglib6b. Please install the kdesupport package.
If you have already installed kdesupport you may have an
old libjpeg somewhere. 
In this case copy $KDEDIR/lib/libjpeg* to /usr/lib.
])
	fi
      fi
   fi

   LIBJPEG=$ac_cv_lib_jpeg
]))

AC_SUBST(LIBJPEG)
AC_DEFINE_UNQUOTED(HAVE_LIBJPEG, 1, [Define if you have libjpeg])

])

AC_DEFUN(AC_FIND_ZLIB,
[
AC_REQUIRE([KDE_CHECK_EXTRA_LIBS])
AC_MSG_CHECKING([for libz])
AC_CACHE_VAL(ac_cv_lib_z,
[
AC_LANG_C
kde_save_LIBS="$LIBS"
LIBS="$all_libraries -lz $LIBSOCKET"
kde_save_CFLAGS="$CFLAGS"
CFLAGS="$CFLAGS $all_includes"
AC_TRY_LINK(dnl
[
#include<zlib.h>
],
            [return (zlibVersion() == ZLIB_VERSION); ],
            eval "ac_cv_lib_z='-lz'",
            eval "ac_cv_lib_z=no")
LIBS="$kde_save_LIBS"
CFLAGS="$kde_save_CFLAGS"
])dnl
if eval "test ! \"`echo $ac_cv_lib_z`\" = no"; then
  AC_DEFINE_UNQUOTED(HAVE_LIBZ, 1, [Define if you have libz])
  LIBZ="$ac_cv_lib_z"
  AC_SUBST(LIBZ)
  AC_MSG_RESULT($ac_cv_lib_z)
else
  AC_MSG_RESULT(no)
  LIBZ=""
  AC_SUBST(LIBZ)
fi
])

AC_DEFUN(KDE_TRY_TIFFLIB,
[
AC_MSG_CHECKING([for libtiff $1])

AC_CACHE_VAL(kde_cv_libtiff_$1, 
[
AC_LANG_C
kde_save_LIBS="$LIBS"
LIBS="$all_libraries -l$1 $LIBJPEG $LIBZ -lX11 $LIBSOCKET -lm"
kde_save_CFLAGS="$CFLAGS"
CFLAGS="$CFLAGS $all_includes"

AC_TRY_LINK(dnl
[
#include<tiffio.h>
],
    [return (TIFFOpen( "", "r") == 0); ],
[
    kde_cv_libtiff_$1="-l$1 $LIBJPEG $LIBZ"
], [
    kde_cv_libtiff_$1=no
])

LIBS="$kde_save_LIBS"
CFLAGS="$kde_save_CFLAGS"

])

if test "$kde_cv_libtiff_$1" = "no"; then
    AC_MSG_RESULT(no)
    LIBTIFF=""
    $3
else
    LIBTIFF="$kde_cv_libtiff_$1"
    AC_MSG_RESULT(yes)
    AC_DEFINE_UNQUOTED(HAVE_LIBTIFF, 1, [Define if you have libtiff])
    $2
fi

])

AC_DEFUN(AC_FIND_TIFF,
[
AC_REQUIRE([K_PATH_X])
AC_REQUIRE([AC_FIND_ZLIB])
AC_REQUIRE([AC_FIND_JPEG])
AC_REQUIRE([KDE_CHECK_EXTRA_LIBS])

KDE_TRY_TIFFLIB(tiff, [],
   KDE_TRY_TIFFLIB(tiff34))

AC_SUBST(LIBTIFF)
])


AC_DEFUN(AC_FIND_PNG,
[
AC_REQUIRE([AC_FIND_ZLIB])
AC_REQUIRE([KDE_CHECK_EXTRA_LIBS])
AC_MSG_CHECKING([for libpng])
AC_CACHE_VAL(ac_cv_lib_png,
[
kde_save_LIBS="$LIBS"
LIBS="$LIBS $all_libraries -lpng $LIBZ -lm -lX11 $LIBSOCKET"
kde_save_CFLAGS="$CFLAGS"
CFLAGS="$CFLAGS $all_includes"
AC_LANG_C
AC_TRY_LINK(dnl
    [
    #include<png.h>
    ],
    [
    png_structp png_ptr = png_create_read_struct(  /* image ptr */
		PNG_LIBPNG_VER_STRING, 0, 0, 0 );
    return( png_ptr != 0 ); 
    ],
    eval "ac_cv_lib_png='-lpng $LIBZ -lm'",
    eval "ac_cv_lib_png=no"
)
LIBS="$kde_save_LIBS"
CFLAGS="$kde_save_CFLAGS"
])dnl
if eval "test ! \"`echo $ac_cv_lib_png`\" = no"; then
  AC_DEFINE_UNQUOTED(HAVE_LIBPNG, 1, [Define if you have libpng])
  LIBPNG="$ac_cv_lib_png"
  AC_SUBST(LIBPNG)
  AC_MSG_RESULT($ac_cv_lib_png)
else
  AC_MSG_RESULT(no)
  LIBPNG=""
  AC_SUBST(LIBPNG)
fi
])

AC_DEFUN(AC_CHECK_BOOL,
[
	AC_MSG_CHECKING([for bool])
        AC_CACHE_VAL(ac_cv_have_bool,
        [
		AC_LANG_CPLUSPLUS
          	AC_TRY_COMPILE([],
                 [bool aBool = true;],
                 [ac_cv_have_bool="yes"],
                 [ac_cv_have_bool="no"])
        ]) dnl end AC_CHECK_VAL
        AC_MSG_RESULT($ac_cv_have_bool)
        if test "$ac_cv_have_bool" = "yes"; then
        	AC_DEFINE(HAVE_BOOL, 1, [Define if the C++ compiler supports BOOL])
        fi 
])

AC_DEFUN(AC_CHECK_GNU_EXTENSIONS,
[
AC_MSG_CHECKING(if you need GNU extensions)
AC_CACHE_VAL(ac_cv_gnu_extensions,
[
cat > conftest.c << EOF
#include <features.h>

#ifdef __GNU_LIBRARY__
yes
#endif
EOF

if (eval "$ac_cpp conftest.c") 2>&5 |
  egrep "yes" >/dev/null 2>&1; then
  rm -rf conftest*
  ac_cv_gnu_extensions=yes
else
  ac_cv_gnu_extensions=no
fi
])

AC_MSG_RESULT($ac_cv_gnu_extensions)
if test "$ac_cv_gnu_extensions" = "yes"; then
  AC_DEFINE_UNQUOTED(_GNU_SOURCE, 1, [Define if you need to use the GNU extensions])
fi
])

AC_DEFUN(AC_CHECK_COMPILERS,
[
  dnl this is somehow a fat lie, but prevents other macros from double checking
  AC_PROVIDE([AC_PROG_CC])
  AC_PROVIDE([AC_PROG_CPP])
  AC_ARG_ENABLE(debug,[  --enable-debug          creates debugging code [default=no]],
  [ 
   if test $enableval = "no"; dnl 
     then 
       kde_use_debug_code="no"
       kde_use_debug_define=yes
     else 
       kde_use_debug_code="yes"
       kde_use_debug_define=no
   fi
  ], [kde_use_debug_code="no"
      kde_use_debug_define=no
    ])

  AC_ARG_ENABLE(strict,[  --enable-strict         compiles with strict compiler options (may not work!)],
   [ 
    if test $enableval = "no"; then 
         kde_use_strict_options="no"
       else 
         kde_use_strict_options="yes"
    fi
   ], [kde_use_strict_options="no"])

dnl this was AC_PROG_CC. I had to include it manualy, since I had to patch it
  AC_MSG_CHECKING(for a C-Compiler)
  dnl if there is one, print out. if not, don't matter
  AC_MSG_RESULT($CC) 
 
  if test -z "$CC"; then AC_CHECK_PROG(CC, gcc, gcc) fi
  if test -z "$CC"; then AC_CHECK_PROG(CC, cc, cc, , , /usr/ucb/cc) fi
  if test -z "$CC"; then AC_CHECK_PROG(CC, xlc, xlc) fi
  test -z "$CC" && AC_MSG_ERROR([no acceptable cc found in \$PATH])

  AC_PROG_CC_WORKS
  AC_PROG_CC_GNU

  if test $ac_cv_prog_gcc = yes; then
    GCC=yes
  else
    GCC=
  fi

  if test -z "$CFLAGS"; then
    if test "$kde_use_debug_code" = "yes"; then
      AC_PROG_CC_G
      if test $ac_cv_prog_cc_g = yes; then
        CFLAGS="-g"
      fi
    else
      if test "$GCC" = "yes"; then
        CFLAGS="-O2"
      else
        CFLAGS=""
      fi
      if test "$kde_use_debug_define" = "yes"; then
         CFLAGS="$CFLAGS -DNDEBUG"
      fi
    fi

    if test "$GCC" = "yes"; then
     CFLAGS="$CFLAGS"

     if test "$kde_use_strict_options" = "yes"; then
	CFLAGS="$CFLAGS -W -Wall -ansi -pedantic -Wshadow -Wpointer-arith -Wmissing-prototypes -Wwrite-strings"
     fi
    fi

  fi

  case "$host" in 
  *-*-sysv4.2uw*) CFLAGS="$CFLAGS -D_UNIXWARE";;
  esac

  if test -z "$LDFLAGS" && test "$kde_use_debug_code" = "no" && test "$GCC" = "yes"; then
     LDFLAGS="-s"
  fi


dnl this is AC_PROG_CPP. I had to include it here, since autoconf checks
dnl dependecies between AC_PROG_CPP and AC_PROG_CC (or is it automake?)

  AC_MSG_CHECKING(how to run the C preprocessor)
  # On Suns, sometimes $CPP names a directory.
  if test -n "$CPP" && test -d "$CPP"; then
    CPP=
  fi
  if test -z "$CPP"; then
  AC_CACHE_VAL(ac_cv_prog_CPP,
  [  # This must be in double quotes, not single quotes, because CPP may get
    # substituted into the Makefile and "${CC-cc}" will confuse make.
    CPP="${CC-cc} -E"
    # On the NeXT, cc -E runs the code through the compiler's parser,
    # not just through cpp.
    dnl Use a header file that comes with gcc, so configuring glibc    
    dnl with a fresh cross-compiler works.
    AC_TRY_CPP([#include <assert.h>
    Syntax Error], ,
    CPP="${CC-cc} -E -traditional-cpp"
    AC_TRY_CPP([#include <assert.h>
    Syntax Error], , CPP=/lib/cpp))
    ac_cv_prog_CPP="$CPP"])dnl
    CPP="$ac_cv_prog_CPP"
  else
    ac_cv_prog_CPP="$CPP"
  fi
  AC_MSG_RESULT($CPP)
  AC_SUBST(CPP)dnl


  AC_MSG_CHECKING(for a C++-Compiler)
  dnl if there is one, print out. if not, don't matter
  AC_MSG_RESULT($CXX) 
 
  if test -z "$CXX"; then AC_CHECK_PROG(CXX, g++, g++) fi
  if test -z "$CXX"; then AC_CHECK_PROG(CXX, CC, CC) fi
  if test -z "$CXX"; then AC_CHECK_PROG(CXX, xlC, xlC) fi
  if test -z "$CXX"; then AC_CHECK_PROG(CXX, DCC, DCC) fi
  test -z "$CXX" && AC_MSG_ERROR([no acceptable C++-compiler found in \$PATH])

  AC_PROG_CXX_WORKS
  AC_PROG_CXX_GNU

  if test $ac_cv_prog_gxx = yes; then
    GXX=yes
  else
    AC_MSG_CHECKING(whether we are using SPARC CC)
    GXX=
    cat > conftest.C << EOF
#ifdef __SUNPRO_CC
   yes;
#endif
EOF

    ac_try="$CXX -E conftest.C"
    if { (eval echo configure:__online__: \"$ac_try\") 1>&5; (eval $ac_try) 2>&5; } | egrep yes >/dev/null 2>&1; then
      ac_cv_prog_CC=yes
    else
      ac_cv_prog_CC=no
    fi
    AC_MSG_RESULT($ac_cv_prog_CC)
    if test "$ac_cv_prog_CC" = "yes"; then
      AC_MSG_ERROR([Sparc CC doesn't work on KDE sources at least til version
4.2. This compiler has several problems accepting legal C++ code 
including the builtin C++ type bool. Since KDE is a freeware project,
we decided, that we don't want to support that broken compilers. 

If you really want to see support for Sparc CC within KDE, make reasonable
patches and join the KDE project. More information about KDE can be found
on www.kde.org])
    fi
  fi

  if test -z "$CXXFLAGS"; then 
    if test "$kde_use_debug_code" = "yes"; then
      AC_PROG_CXX_G
      if test $ac_cv_prog_cxx_g = yes; then
        CXXFLAGS="-g"
      fi
      if test "$ac_cv_prog_CC" = "yes"; then
        CXXFLAGS="$CXXFLAGS -pto"
      fi
    else
      if test "$GXX" = "yes"; then
         CXXFLAGS="-O2"
      else
         if test "$ac_cv_prog_CC" = "yes"; then
            CXXFLAGS="-pto -O2"
         else
            CXXFLAGS=""
         fi
      fi
      if test "$kde_use_debug_define" = "yes"; then
         CXXFLAGS="$CXXFLAGS -DNDEBUG"
      fi
    fi

    if test "$GXX" = "yes"; then
       CXXFLAGS="$CXXFLAGS"
       
       if test "$kde_use_debug_code" = "yes"; then
         CXXFLAGS="$CXXFLAGS -Wall -pedantic -Wno-long-long -W -Wpointer-arith -Wmissing-prototypes -Wwrite-strings"
       fi

       if test "$kde_use_strict_options" = "yes"; then
	CXXFLAGS="$CXXFLAGS -Wcast-qual -Wbad-function-cast -Wshadow -Wcast-align -Woverloaded-virtual"
       fi

       if test "$kde_very_strict" = "yes"; then
         CXXFLAGS="$CXXFLAGS -Wold-style-cast -Wredundant-decls -Wconversion"
       fi
    fi
  fi  

    case "$host" in
      *-*-sysv4.2uw*) CXXFLAGS="$CXXFLAGS -D_UNIXWARE";;
    esac    
  
    AC_PROVIDE(AC_PROG_CXXCPP)

    AC_MSG_CHECKING(how to run the C++ preprocessor)
    if test -z "$CXXCPP"; then
      AC_CACHE_VAL(ac_cv_prog_CXXCPP,
      [
         AC_LANG_SAVE[]dnl
         AC_LANG_CPLUSPLUS[]dnl
         CXXCPP="${CXX-g++} -E"
         AC_TRY_CPP([#include <stdlib.h>], , CXXCPP=/lib/cpp)
         ac_cv_prog_CXXCPP="$CXXCPP"
         AC_LANG_RESTORE[]dnl
     ])dnl
     CXXCPP="$ac_cv_prog_CXXCPP"
     fi
    AC_MSG_RESULT($CXXCPP)
    AC_SUBST(CXXCPP)dnl

    KDE_CHECK_FINAL    
])

dnl just a wrapper to clean up configure.in
AC_DEFUN(KDE_PROG_LIBTOOL,
[
AC_REQUIRE([AC_CHECK_COMPILERS])
AC_REQUIRE([AC_ENABLE_SHARED])
AC_REQUIRE([AC_ENABLE_STATIC])
dnl libtool is only for C, so I must force him
dnl to find the correct flags for C++
kde_save_cc=$CC
kde_save_cflags="$CFLAGS"
CC=$CXX
CFLAGS="$CXXFLAGS"
AC_LANG_SAVE
AC_LANG_C
AM_PROG_LIBTOOL dnl for libraries
AC_REQUIRE([AC_PROG_LIBTOOL]) dnl faking :=/
LIBTOOL="$LIBTOOL --silent"
dnl AC_SUBST(LIBTOOL)
AC_LANG_RESTORE
CC=$kde_save_cc
CFLAGS="$kde_save_cflags"
])

AC_DEFUN(KDE_DO_IT_ALL,
[
AC_CANONICAL_SYSTEM 
AC_ARG_PROGRAM 
AM_INIT_AUTOMAKE($1, $2)
AM_DISABLE_LIBRARIES
AC_PREFIX_DEFAULT(${KDEDIR:-/usr/local/kde})
AC_CHECK_COMPILERS
KDE_PROG_LIBTOOL
AM_KDE_WITH_NLS
AC_PATH_KDE
])

AC_DEFUN(AC_CHECK_RPATH,
[
AC_MSG_CHECKING(for rpath)
AC_ARG_ENABLE(rpath,
      [  --disable-rpath         do not use the rpath feature of ld],
      USE_RPATH=$enableval, USE_RPATH=yes)

if test -z "$KDE_RPATH" && test "$USE_RPATH" = "yes"; then

  KDE_RPATH="-rpath \$(kde_libraries)"

  if test -n "$qt_libraries"; then
    KDE_RPATH="$KDE_RPATH -rpath \$(qt_libraries)"
  fi
  dnl $x_libraries is set to /usr/lib in case
  if test -n "$X_LDFLAGS"; then 
    KDE_RPATH="$KDE_RPATH -rpath \$(x_libraries)"
  fi
  if test -n "$KDE_EXTRA_RPATH"; then
    KDE_RPATH="$KDE_RPATH \$(KDE_EXTRA_RPATH)"
  fi
fi 
AC_SUBST(KDE_EXTRA_RPATH)
AC_SUBST(KDE_RPATH)
AC_MSG_RESULT($USE_RPATH)
])

dnl Check for the type of the third argument of getsockname
AC_DEFUN(AC_CHECK_KSIZE_T,
[AC_MSG_CHECKING(for the third argument of getsockname)  
AC_CACHE_VAL(ac_cv_ksize_t,
AC_LANG_SAVE
AC_LANG_CPLUSPLUS
[AC_TRY_COMPILE([
#include <sys/types.h>
#include <sys/socket.h>
],[
socklen_t a=0; 
getsockname(0,(struct sockaddr*)0, &a);
],
ac_cv_ksize_t=socklen_t,
ac_cv_ksize_t=)
if test -z "$ac_cv_ksize_t"; then
ac_safe_cxxflags="$CXXFLAGS"
if test "$GCC" = "yes"; then
  CXXFLAGS="-Werror $CXXFLAGS"
fi
AC_TRY_COMPILE([
#include <sys/types.h>
#include <sys/socket.h>
],[
int a=0; 
getsockname(0,(struct sockaddr*)0, &a);
],
ac_cv_ksize_t=int,
ac_cv_ksize_t=size_t)
CXXFLAGS="$ac_safe_cxxflags"
fi
AC_LANG_RESTORE
])

if test -z "$ac_cv_ksize_t"; then
  ac_cv_ksize_t=int
fi

AC_MSG_RESULT($ac_cv_ksize_t)
AC_DEFINE_UNQUOTED(ksize_t, $ac_cv_ksize_t, 
      [Define the type of the third argument for getsockname]
)

])

dnl This is a merge of some macros out of the gettext aclocal.m4
dnl since we don't need anything, I took the things we need
dnl the copyright for them is:
dnl >
dnl Copyright (C) 1994, 1995, 1996, 1997, 1998 Free Software Foundation, Inc.
dnl This Makefile.in is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl This program is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY, to the extent permitted by law; without
dnl even the implied warranty of MERCHANTABILITY or FITNESS FOR A
dnl PARTICULAR PURPOSE.
dnl >
dnl for this file it is relicensed under LGPL

AC_DEFUN(AM_KDE_WITH_NLS,
  [AC_MSG_CHECKING([whether NLS is requested])
    dnl Default is enabled NLS
    AC_ARG_ENABLE(nls,
      [  --disable-nls           do not use Native Language Support],
      USE_NLS=$enableval, USE_NLS=yes)
    AC_MSG_RESULT($USE_NLS)
    AC_SUBST(USE_NLS)

    dnl If we use NLS figure out what method
    if test "$USE_NLS" = "yes"; then
      AC_DEFINE(ENABLE_NLS, 1, [Define if NLS is requested])

      AM_PATH_PROG_WITH_TEST_KDE(MSGFMT, msgfmt, 
         [test -n "`$ac_dir/$ac_word --version 2>&1 | grep 'GNU gettext'`"], msgfmt)
      AC_PATH_PROG(GMSGFMT, gmsgfmt, $MSGFMT)

      if test -z "`$GMSGFMT --version 2>&1 | grep 'GNU gettext'`"; then
        AC_MSG_RESULT([found msgfmt program is not GNU msgfmt; ignore it])
        GMSGFMT=":"
      fi
      MSGFMT=$GMSGFMT
      AC_SUBST(GMSGFMT)
      AC_SUBST(MSGFMT)

      AM_PATH_PROG_WITH_TEST_KDE(XGETTEXT, xgettext,
	[test -z "`$ac_dir/$ac_word -h 2>&1 | grep '(HELP)'`"], :)

      dnl Test whether we really found GNU xgettext.
      if test "$XGETTEXT" != ":"; then
	dnl If it is no GNU xgettext we define it as : so that the
	dnl Makefiles still can work.
	if $XGETTEXT --omit-header /dev/null 2> /dev/null; then
	  : ;
	else
	  AC_MSG_RESULT(
	    [found xgettext programs is not GNU xgettext; ignore it])
	  XGETTEXT=":"
	fi
      fi
     AC_SUBST(XGETTEXT)
    fi

  ])

# Search path for a program which passes the given test.
# Ulrich Drepper <drepper@cygnus.com>, 1996.

# serial 1
# Stephan Kulow: I appended a _KDE against name conflicts

dnl AM_PATH_PROG_WITH_TEST_KDE(VARIABLE, PROG-TO-CHECK-FOR,
dnl   TEST-PERFORMED-ON-FOUND_PROGRAM [, VALUE-IF-NOT-FOUND [, PATH]])
AC_DEFUN(AM_PATH_PROG_WITH_TEST_KDE,
[# Extract the first word of "$2", so it can be a program name with args.
set dummy $2; ac_word=[$]2
AC_MSG_CHECKING([for $ac_word])
AC_CACHE_VAL(ac_cv_path_$1,
[case "[$]$1" in
  /*)
  ac_cv_path_$1="[$]$1" # Let the user override the test with a path.
  ;;
  *)
  IFS="${IFS= 	}"; ac_save_ifs="$IFS"; IFS="${IFS}:"
  for ac_dir in ifelse([$5], , $PATH, [$5]); do
    test -z "$ac_dir" && ac_dir=.
    if test -f $ac_dir/$ac_word; then
      if [$3]; then
	ac_cv_path_$1="$ac_dir/$ac_word"
	break
      fi
    fi
  done
  IFS="$ac_save_ifs"
dnl If no 4th arg is given, leave the cache variable unset,
dnl so AC_PATH_PROGS will keep looking.
ifelse([$4], , , [  test -z "[$]ac_cv_path_$1" && ac_cv_path_$1="$4"
])dnl
  ;;
esac])dnl
$1="$ac_cv_path_$1"
if test -n "[$]$1"; then
  AC_MSG_RESULT([$]$1)
else
  AC_MSG_RESULT(no)
fi
AC_SUBST($1)dnl
])


# Check whether LC_MESSAGES is available in <locale.h>.
# Ulrich Drepper <drepper@cygnus.com>, 1995.
 
# serial 1
 
AC_DEFUN(AM_LC_MESSAGES,
  [if test $ac_cv_header_locale_h = yes; then
    AC_CACHE_CHECK([for LC_MESSAGES], am_cv_val_LC_MESSAGES,
      [AC_TRY_LINK([#include <locale.h>], [return LC_MESSAGES],
       am_cv_val_LC_MESSAGES=yes, am_cv_val_LC_MESSAGES=no)])
    if test $am_cv_val_LC_MESSAGES = yes; then
      AC_DEFINE(HAVE_LC_MESSAGES, 1, [Define if your locale.h file contains LC_MESSAGES])
    fi
  fi])

# Macro to add for using GNU gettext.
# Ulrich Drepper <drepper@cygnus.com>, 1995.

# serial 1
# Stephan Kulow: I put a KDE in it to avoid name conflicts

AC_DEFUN(AM_KDE_GNU_GETTEXT,
  [AC_REQUIRE([AC_PROG_MAKE_SET])dnl
   AC_REQUIRE([AC_PROG_RANLIB])dnl
   AC_REQUIRE([AC_HEADER_STDC])dnl
   AC_REQUIRE([AC_C_INLINE])dnl
   AC_REQUIRE([AC_TYPE_OFF_T])dnl
   AC_REQUIRE([AC_TYPE_SIZE_T])dnl
   AC_REQUIRE([AC_FUNC_ALLOCA])dnl
   AC_REQUIRE([AC_FUNC_MMAP])dnl
   AC_REQUIRE([AM_KDE_WITH_NLS])dnl
   AC_CHECK_HEADERS([argz.h limits.h locale.h nl_types.h malloc.h string.h \
unistd.h values.h alloca.h])
   AC_CHECK_FUNCS([getcwd munmap putenv setenv setlocale strchr strcasecmp \
__argz_count __argz_stringify __argz_next])

   AC_MSG_CHECKING(for stpcpy)
   AC_CACHE_VAL(kde_cv_func_stpcpy,
   [
   kde_safe_cxxflags=$CXXFLAGS
   CXXFLAGS="-Wmissing-prototypes -Werror"
   AC_LANG_SAVE
   AC_LANG_CPLUSPLUS
   AC_TRY_COMPILE([
   #include <string.h>
   ],
   [
   char buffer[200];
   stpcpy(buffer, buffer);
   ],
   kde_cv_func_stpcpy=yes,
   kde_cv_func_stpcpy=no)
   AC_LANG_RESTORE
   CXXFLAGS=$kde_safe_cxxflags
   ])
   AC_MSG_RESULT($kde_cv_func_stpcpy)
   if eval "test \"`echo $kde_cv_func_stpcpy`\" = yes"; then
     AC_DEFINE(HAVE_STPCPY, 1, [Define if you have stpcpy])
   fi

   AM_LC_MESSAGES

   if test "x$CATOBJEXT" != "x"; then
     if test "x$ALL_LINGUAS" = "x"; then
       LINGUAS=
     else
       AC_MSG_CHECKING(for catalogs to be installed)
       NEW_LINGUAS=
       for lang in ${LINGUAS=$ALL_LINGUAS}; do
         case "$ALL_LINGUAS" in
          *$lang*) NEW_LINGUAS="$NEW_LINGUAS $lang" ;;
         esac
       done
       LINGUAS=$NEW_LINGUAS
       AC_MSG_RESULT($LINGUAS)
     fi

     dnl Construct list of names of catalog files to be constructed.
     if test -n "$LINGUAS"; then
       for lang in $LINGUAS; do CATALOGS="$CATALOGS $lang$CATOBJEXT"; done
     fi
   fi

  ])

AC_DEFUN(AC_HAVE_XPM,
 [AC_REQUIRE_CPP()dnl
  AC_REQUIRE([KDE_CHECK_EXTRA_LIBS])

 test -z "$XPM_LDFLAGS" && XPM_LDFLAGS=
 test -z "$XPM_INCLUDE" && XPM_INCLUDE=

 AC_ARG_WITH(xpm, [  --without-xpm           disable color pixmap XPM tests],
	xpm_test=$withval, xpm_test="yes")
 if test "x$xpm_test" = xno; then
   ac_cv_have_xpm=no
 else
   AC_MSG_CHECKING(for XPM)
   AC_CACHE_VAL(ac_cv_have_xpm,
   [
    AC_LANG_C
    ac_save_ldflags="$LDFLAGS"
    ac_save_cflags="$CFLAGS"
    LDFLAGS="$LDFLAGS $XPM_LDFLAGS $all_libraries -lXpm -lX11 -lXext $LIBSOCKET"
    CFLAGS="$CFLAGS $X_INCLUDES"
    test ! -z "$XPM_INCLUDE" && CFLAGS="-I$XPM_INCLUDE $CFLAGS"
    AC_TRY_LINK([#include <X11/xpm.h>],[],
	ac_cv_have_xpm="yes",ac_cv_have_xpm="no")
    LDFLAGS="$ac_save_ldflags"
    CFLAGS="$ac_save_cflags"
   ])dnl
 
  if test "$ac_cv_have_xpm" = no; then
    AC_MSG_RESULT(no)
    XPM_LDFLAGS=""
    XPMINC=""
    $2
  else
    AC_DEFINE(HAVE_XPM, 1, [Define if you have XPM support])
    if test "$XPM_LDFLAGS" = ""; then
       XPMLIB='-lXpm $(LIB_X11)'
    else
       XPMLIB="-L$XPM_LDFLAGS -lXpm "'$(LIB_X11)'
    fi
    if test "$XPM_INCLUDE" = ""; then
       XPMINC=""
    else
       XPMINC="-I$XPM_INCLUDE"
    fi
    AC_MSG_RESULT(yes)
    $1
  fi
 fi
 AC_SUBST(XPMINC)
 AC_SUBST(XPMLIB)
]) 

AC_DEFUN(AC_HAVE_DPMS,
 [AC_REQUIRE_CPP()dnl
  AC_REQUIRE([KDE_CHECK_EXTRA_LIBS])

 test -z "$DPMS_LDFLAGS" && DPMS_LDFLAGS=
 test -z "$DPMS_INCLUDE" && DPMS_INCLUDE=

 AC_ARG_WITH(dpms, [  --without-dpms          disable DPMS power saving],
	dpms_test=$withval, dpms_test="yes")
 if test "x$dpms_test" = xno; then
   ac_cv_have_dpms=no
 else
   AC_MSG_CHECKING(for DPMS)
   AC_CACHE_VAL(ac_cv_have_dpms,
   [
    AC_LANG_C
    ac_save_ldflags="$LDFLAGS"
    ac_save_cflags="$CFLAGS"
    LDFLAGS="$LDFLAGS $DPMS_LDFLAGS $all_libraries -lXpm -lX11 -lXext $LIBSOCKET"
    CFLAGS="$CFLAGS $X_INCLUDES"
    test ! -z "$DPMS_INCLUDE" && CFLAGS="-I$DPMS_INCLUDE $CFLAGS"
    AC_TRY_LINK([#include <X11/extensions/dpms.h>],[],
	ac_cv_have_dpms="yes",ac_cv_have_dpms="no")
    LDFLAGS="$ac_save_ldflags"
    CFLAGS="$ac_save_cflags"
   ])dnl
 
  if test "$ac_cv_have_dpms" = no; then
    AC_MSG_RESULT(no)
    DPMS_LDFLAGS=""
    DPMSINC=""
    $2
  else
    AC_DEFINE(HAVE_DPMS, 1, [Define if you have DPMS support])
    if test "$DPMS_LDFLAGS" = ""; then
       DPMSLIB='-lXdpms $(LIB_X11)'
    else
       DPMSLIB="-L$DPMS_LDFLAGS -lXdpms "'$(LIB_X11)'
    fi
    if test "$DPMS_INCLUDE" = ""; then
       DPMSINC=""
    else
       DPMSINC="-I$DPMS_INCLUDE"
    fi
    AC_MSG_RESULT(yes)
    $1
  fi
 fi
 AC_SUBST(DPMSINC)
 AC_SUBST(DPMSLIB)
]) 

AC_DEFUN(AC_HAVE_GL,
 [AC_REQUIRE_CPP()dnl
  AC_REQUIRE([KDE_CHECK_EXTRA_LIBS])

 test -z "$GL_LDFLAGS" && GL_LDFLAGS=
 test -z "$GL_INCLUDE" && GL_INCLUDE=

 AC_ARG_WITH(gl, [  --without-gl            disable 3D GL modes],
	gl_test=$withval, gl_test="yes")
 if test "x$gl_test" = xno; then
   ac_cv_have_gl=no
 else
   AC_MSG_CHECKING(for GL)
   AC_CACHE_VAL(ac_cv_have_gl,
   [
    AC_LANG_C
    ac_save_ldflags="$LDFLAGS"
    ac_save_cflags="$CFLAGS"
    LDFLAGS="$LDFLAGS $GL_LDFLAGS $all_libraries -lMesaGL -lMesaGLU -lX11 -lXext -lm $LIBSOCKET"
    CFLAGS="$CFLAGS $X_INCLUDES"
    test ! -z "$GL_INCLUDE" && CFLAGS="-I$GL_INCLUDE $CFLAGS"
    AC_TRY_LINK([],[],
	ac_cv_have_gl="yes",ac_cv_have_gl="no")
    LDFLAGS="$ac_save_ldflags"
    CFLAGS="$ac_save_cflags"
   ])dnl
 
  if test "$ac_cv_have_gl" = no; then
    AC_MSG_RESULT(no)
    GL_LDFLAGS=""
    GLINC=""
    $2
  else
    AC_DEFINE(HAVE_GL, 1, [Defines if you have GL (Mesa, OpenGL, ...)])
    if test "$GL_LDFLAGS" = ""; then
       GLLIB='-lMesaGL -lMesaGLU $(LIB_X11)'
    else
       GLLIB="$GL_LDFLAGS -lMesaGL -lMesaGLU "'$(LIB_X11)'
    fi
    if test "$GL_INCLUDE" = ""; then
       GLINC=""
    else
       GLINC="-I$GL_INCLUDE"
    fi
    AC_MSG_RESULT(yes)
    $1
  fi
 fi
 AC_SUBST(GLINC)
 AC_SUBST(GLLIB)
]) 

 dnl PAM pam
 
 dnl Should test for PAM (Pluggable Authentication Modules)
 AC_DEFUN(AC_PATH_PAM_DIRECT,
 [
 test -z "$pam_direct_test_library" && pam_direct_test_library=pam
 test -z "$pam_direct_test_include" && pam_direct_test_include=security/pam_appl.h
 
   for ac_dir in               \
                               \
     /usr/local/include        \
     /usr/include              \
     /usr/unsupported/include  \
     /opt/include              \
     /usr/pam/include          \
     /usr/local/pam/include    \
     /usr/lib/pam/include      \
 			      \
     $extra_include            \
     ; \
   do
     if test -r "$ac_dir/$pam_direct_test_include"; then
       no_pam= ac_pam_includes=$ac_dir
       break
     fi
   done
 
 # First see if replacing the include by lib works.
 for ac_dir in `echo "$ac_pam_includes" | sed s/include/lib/` \
                           \
     /lib                  \
     /usr/lib              \
     /usr/local/lib        \
     /usr/unsupported/lib  \
     /lib/security         \
     /usr/security/lib     \
     $extra_lib            \
     ; \
 do
   for ac_extension in a so sl; do
     if test -r $ac_dir/lib${pam_direct_test_library}.$ac_extension; then
       no_pam= ac_pam_libraries=$ac_dir
       break 2
     fi
   done
 done
])

AC_DEFUN(AC_PATH_PAM,
 [
  AC_REQUIRE([KDE_CHECK_LIBDL])
  AC_REQUIRE_CPP()dnl

  AC_CHECK_LIB(pam_misc, main, [PAM_MISC_LIB="-lpam_misc"
               AC_DEFINE_UNQUOTED(HAVE_PAM_MISC, 1, [Define if you have a PAM implementation with the pam_misc library])], [], [-lpam $LIBDL])

 AC_MSG_CHECKING(for PAM)
 AC_ARG_WITH(pam, 
[  --with-pam[=ARG]        enable support for PAM: ARG=[yes|no|service name]],
  [
    if test "x$withval" = "xyes"; then
      no_pam=
      default_pam=yes
    elif test "x$withval" = "xno"; then
      no_pam=yes
    else
      no_pam=
      pam_service="$withval"
        if test -z "$pam_service"; then
        default_pam=yes
        else
        default_pam=
        fi 
      fi
  ], no_pam=yes
 )

 if test ! "$no_pam" = yes; then

 AC_CACHE_VAL(ac_cv_path_pam,
 [
 ac_pam_includes=NONE
 ac_pam_libraries=NONE
 if test -z "$pam_libraries"; then
   pam_libraries=NONE
 fi
 if test -z "$pam_includes"; then
   pam_includes=NONE
 fi

 AC_PATH_PAM_DIRECT
 
 test "x$pam_includes" = xNONE && pam_includes=$ac_pam_includes
 test "x$pam_libraries" = xNONE && pam_libraries=$ac_pam_libraries
 
 if test ! "x$pam_includes" = xNONE && test ! "x$pam_libraries" = xNONE; then
   ac_pam_libs="-lpam $PAM_MISC_LIB $LIBDL"
   ac_cv_path_pam="no_pam= ac_pam_includes=$ac_pam_includes ac_pam_libraries=$ac_pam_libraries ac_pam_libs=\"$ac_pam_libs\""
 else
   ac_cv_path_pam="no_pam=yes"
 fi
 ])

 eval "$ac_cv_path_pam"
 
 fi

 if test "$no_pam" = yes; then
   AC_MSG_RESULT(no)
 else
   AC_DEFINE(HAVE_PAM, 1, [Defines if you have PAM (Pluggable Authentication Modules); Redhat-Users!])
   PAMLIBS="$ac_pam_libs"
   test "x$pam_includes" = xNONE && pam_includes=$ac_pam_includes
   test "x$pam_libraries" = xNONE && pam_libraries=$ac_pam_libraries
   AC_MSG_RESULT([libraries $pam_libraries, headers $pam_includes])
 if test "$default_pam" = yes; then
   AC_MSG_RESULT(["default pam service name will be used"])
 else
   AC_DEFINE_UNQUOTED(KDE_PAM_SERVICE,"$pam_service", [Define to change the default name of the PAM service used by KDE])
   AC_MSG_RESULT(["pam service name will be: " $pam_service])
 fi
dnl test whether struct pam_message is const (Linux) or not (Sun)
   pam_appl_h="$ac_pam_includes/security/pam_appl.h"
   AC_MSG_CHECKING(for const pam_message)
   AC_EGREP_HEADER([struct pam_message],
      $pam_appl_h,
      [ AC_EGREP_HEADER([const struct pam_message],
                        $pam_appl_h,
                        [AC_MSG_RESULT(["const: Linux-type PAM"]) ],
                        [AC_MSG_RESULT(["nonconst: Sun-type PAM"])
                        AC_DEFINE(PAM_MESSAGE_NONCONST, 1, [Define if your PAM support takes non-const arguments (Solaris)])] 
                        )],
       [AC_MSG_RESULT(["not found - assume const, Linux-type PAM"])]
       )
 fi
 
 if test "x$pam_libraries" != x && test "x$pam_libraries" != xNONE ; then
     PAMLIBPATHS="-L$pam_libraries"
 fi
 if test "x$pam_includes" != x && test "x$pam_includes" != xNONE ; then
     PAMINC="-I$pam_includes"
 fi
 
 AC_SUBST(PAMINC)
 AC_SUBST(PAMLIBS)
 AC_SUBST(PAMLIBPATHS)

]) 

AC_DEFUN(KDE_CHECK_LIBDL,
[
AC_CHECK_LIB(dl, dlopen, [
LIBDL="-ldl"
ac_cv_have_dlfcn=yes
])

AC_CHECK_LIB(dld, shl_unload, [
LIBDL="-ldld"
ac_cv_have_shload=yes
])

AC_SUBST(LIBDL)
])

AC_DEFUN(KDE_CHECK_DLOPEN,
[
KDE_CHECK_LIBDL
AC_CHECK_HEADERS(dlfcn.h dl.h)
if test "$ac_cv_header_dlfcn_h" = "no"; then
  ac_cv_have_dlfcn=no
fi

if test "$ac_cv_header_dl_h" = "no"; then
  ac_cv_have_shload=no
fi

enable_dlopen=no
AC_ARG_ENABLE(dlopen,
[  --disable-dlopen        link staticly [default=no]] ,
[if test "$enableval" = yes; then
  enable_dlopen=yes
fi],
enable_dlopen=yes)

# override the user's opinion, if we know it better ;)
if test "$ac_cv_have_dlfcn" = "no" && test "$ac_cv_have_shload" = "no"; then
  enable_dlopen=no
fi

if test "$ac_cv_have_dlfcn" = "yes"; then
  AC_DEFINE_UNQUOTED(HAVE_DLFCN)
fi

if test "$ac_cv_have_shload" = "yes"; then
  AC_DEFINE_UNQUOTED(HAVE_SHLOAD)
fi

if test "$enable_dlopen" = no ; then
  test -n "$1" && eval $1
else
  test -n "$2" && eval $2
fi

])

AC_DEFUN(KDE_CHECK_DYNAMIC_LOADING,
[
KDE_CHECK_DLOPEN(libtool_enable_shared=no, libtool_enable_static=no)
KDE_PROG_LIBTOOL
AC_MSG_CHECKING([dynamic loading])
eval "`egrep '^build_libtool_libs=' libtool`"
if test "$build_libtool_libs" = "yes" && test "$enable_dlopen" = "yes"; then
  dynamic_loading=yes
  AC_DEFINE_UNQUOTED(HAVE_DYNAMIC_LOADING)
else
  dynamic_loading=no
fi
AC_MSG_RESULT($dynamic_loading)
if test "$dynamic_loading" = "yes"; then
  $1
else
  $2
fi
])

AC_DEFUN(KDE_ADD_INCLUDES,
[
if test -z "$1"; then 
  test_include="Pix.h"
else
  test_include="$1"
fi

AC_MSG_CHECKING([for libg++ ($test_include)])

AC_CACHE_VAL(kde_cv_libgpp_includes,
[
kde_cv_libgpp_includes=no

   for ac_dir in               \
                               \
     /usr/include/g++          \
     /usr/include              \
     /usr/unsupported/include  \
     /opt/include              \
     $extra_include            \
     ; \
   do
     if test -r "$ac_dir/$test_include"; then
       kde_cv_libgpp_includes=$ac_dir
       break
     fi
   done
])

AC_MSG_RESULT($kde_cv_libgpp_includes)
if test "$kde_cv_libgpp_includes" != "no"; then
  all_includes="-I$kde_cv_libgpp_includes $all_includes"
fi
])
])


AC_DEFUN(KDE_CHECK_MICO,
[
AC_REQUIRE([KDE_CHECK_LIBDL])
AC_REQUIRE([KDE_MISC_TESTS])
AC_MSG_CHECKING(for MICO)

if test -z "$MICODIR"; then
    kde_micodir=/usr/local
 else
    kde_micodir="$MICODIR"
fi

AC_ARG_WITH(micodir,
  [  --with-micodir=micodir  where mico is installed ],
  kde_micodir=$withval,
  kde_micodir=$kde_micodir
)

AC_CACHE_VAL(kde_cv_mico_incdir,
[
  mico_incdirs="$kde_micodir/include /usr/include /usr/local/include /usr/local/include /opt/local/include $kde_extra_includes"
AC_FIND_FILE(CORBA.h, $mico_incdirs, kde_cv_mico_incdir)

])
kde_micodir=`echo $kde_cv_mico_incdir | sed -e 's#/include##'`

if test ! -r  $kde_micodir/include/CORBA.h; then
  AC_MSG_ERROR([No CORBA.h found, specify another micodir])
fi

AC_MSG_RESULT($kde_micodir)

MICO_INCLUDES=-I$kde_micodir/include
AC_SUBST(MICO_INCLUDES)
MICO_LDFLAGS=-L$kde_micodir/lib
AC_SUBST(MICO_LDFLAGS)
micodir=$kde_micodir
AC_SUBST(micodir)

AC_MSG_CHECKING([for MICO version])
AC_CACHE_VAL(kde_cv_mico_version,
[
AC_LANG_C
cat >conftest.$ac_ext <<EOF
#include <stdio.h>
#include <mico/version.h>
int main() { 
    
   printf("MICO_VERSION=%s\n",MICO_VERSION); 
   return (0); 
}
EOF
ac_compile='${CC-gcc} $CFLAGS $MICO_INCLUDES conftest.$ac_ext -o conftest'
if AC_TRY_EVAL(ac_compile); then
  if eval `./conftest 2>&5`; then
    kde_cv_mico_version=$MICO_VERSION
  else
    AC_MSG_ERROR([your system is not able to execute a small application to
    find MICO version! Check $kde_micodir/include/mico/version.h])
  fi 
else
  AC_MSG_ERROR([your system is not able to compile a small application to
  find MICO version! Check $kde_micodir/include/mico/version.h])
fi
])

dnl installed MICO version
mico_v_maj=`echo $kde_cv_mico_version | sed -e 's/^\(.*\)\..*\..*$/\1/'`
mico_v_mid=`echo $kde_cv_mico_version | sed -e 's/^.*\.\(.*\)\..*$/\1/'`
mico_v_min=`echo $kde_cv_mico_version | sed -e 's/^.*\..*\.\(.*\)$/\1/'`

if test "x$1" = "x"; then
 req_version="2.2.7"
else
 req_version=$1
fi

dnl required MICO version
req_v_maj=`echo $req_version | sed -e 's/^\(.*\)\..*\..*$/\1/'`
req_v_mid=`echo $req_version | sed -e 's/^.*\.\(.*\)\..*$/\1/'`
req_v_min=`echo $req_version | sed -e 's/^.*\..*\.\(.*\)$/\1/'` 

if test "$mico_v_maj" -lt "$req_v_maj" || \
   ( test "$mico_v_maj" -eq "$req_v_maj" && \
        test "$mico_v_mid" -lt "$req_v_mid" ) || \
   ( test "$mico_v_mid" -eq "$req_v_mid" && \
        test "$mico_v_min" -lt "$req_v_min" )

then
  AC_MSG_ERROR([found MICO version $kde_cv_mico_version but version $req_version \
at least is required. You should upgrade MICO.])
else
  AC_MSG_RESULT([$kde_cv_mico_version (minimum version $req_version, ok)])
fi

LIBMICO="-lmico$kde_cv_mico_version $LIBCRYPT $LIBSOCKET $LIBDL"
AC_SUBST(LIBMICO)
IDL=$kde_micodir/bin/idl
AC_SUBST(IDL)

KDE_CHECK_MINI_STL
 
if test "$kde_cv_have_mini_stl" = "yes"; then
  AC_MSG_ERROR([compile mico with --disable-mini-stl])
fi

idldir="\$(includedir)/idl"
AC_SUBST(idldir)

])

AC_DEFUN(KDE_CHECK_MINI_STL,
[
AC_REQUIRE([KDE_CHECK_MICO])

AC_MSG_CHECKING(if we use mico's mini-STL)
AC_CACHE_VAL(kde_cv_have_mini_stl,
[
AC_LANG_SAVE
AC_LANG_CPLUSPLUS
kde_save_cxxflags="$CXXFLAGS"
CXXFLAGS="$CXXFLAGS $MICO_INCLUDES"
AC_TRY_COMPILE(
[
#include <mico/config.h>
],
[
#ifdef HAVE_MINI_STL
#error "nothing"
#endif
],
kde_cv_have_mini_stl=no,
kde_cv_have_mini_stl=yes)
CXXFLAGS="$kde_save_cxxflags"
AC_LANG_RESTORE
])

AC_MSG_RESULT($kde_cv_have_mini_stl)
])
 
dnl ])

AC_DEFUN(KDE_CHECK_LIBPTHREAD,
[
AC_CHECK_LIB(pthread, pthread_create, [LIBPTHREAD="-lpthread"], LIBPTHREAD= )
AC_SUBST(LIBPTHREAD)
])

AC_DEFUN(KDE_TRY_LINK_PYTHON,
[
if test "$kde_python_link_found" = no; then
  
if test "$1" = normal; then
  AC_MSG_CHECKING(if a Python application links)
else
  AC_MSG_CHECKING(if Python depends on $2)
fi

AC_CACHE_VAL(kde_cv_try_link_python_$1,
[
AC_LANG_SAVE
AC_LANG_C
kde_save_cflags="$CFLAGS"
CFLAGS="$CFLAGS $PYTHONINC"
kde_save_libs="$LIBS"
LIBS="$LIBS $LIBPYTHON $2 $LIBDL $LIBSOCKET"
kde_save_ldflags="$LDFLAGS"
LDFLAGS="$LDFLAGS $PYTHONLIB"

AC_TRY_LINK(
[
#include <Python.h>
],[
	PySys_SetArgv(1, 0);
],
	[kde_cv_try_link_python_$1=yes],
	[kde_cv_try_link_python_$1=no]
)
CFLAGS="$kde_save_cflags"
LIBS="$kde_save_libs"
LDFLAGS="$kde_save_ldflags"
])

if test "$kde_cv_try_link_python_$1" = "yes"; then
  AC_MSG_RESULT(yes)
  kde_python_link_found=yes
  if test ! "$1" = normal; then
    LIBPYTHON="$LIBPYTHON $2"
  fi
  $3
else
  AC_MSG_RESULT(no)
  $4
fi
AC_LANG_RESTORE

fi

])

AC_DEFUN(KDE_CHECK_PYTHON,
[
AC_REQUIRE([KDE_CHECK_LIBDL])
AC_REQUIRE([KDE_CHECK_LIBPTHREAD])

AC_MSG_CHECKING([for Python directory])

AC_CACHE_VAL(kde_cv_pythondir,
[
  if test -z "$PYTHONDIR"; then
    kde_cv_pythondir=/usr/local
  else
    kde_cv_pythondir="$PYTHONDIR"
  fi
])

AC_ARG_WITH(pythondir, 
[  --with-pythondir=pythondir   use python installed in pythondir ],
[
  ac_python_dir=$withval
], ac_python_dir=$kde_cv_pythondir
)

AC_MSG_RESULT($ac_python_dir)

if test -z "$1"; then 
  version="1.5"
else
  version="$1"
fi

AC_MSG_CHECKING([for Python$version])

python_incdirs="$ac_python_dir/include /usr/include /usr/local/include/ $kde_extra_includes"
AC_FIND_FILE(Python.h, $python_incdirs, python_incdir)
if test ! -r $python_incdir/Python.h; then
  AC_FIND_FILE(python$version/Python.h, $python_incdirs, python_incdir)
  python_incdir=$python_incdir/python$version
  if test ! -r $python_incdir/Python.h; then
    AC_MSG_ERROR(Python.h not found.)
  fi
fi

PYTHONINC=-I$python_incdir

python_libdirs="$ac_python_dir/lib /usr/lib /usr/local /usr/lib $kde_extra_libs"
AC_FIND_FILE(libpython$version.a, $python_libdirs, python_libdir)
if test ! -r $python_libdir/libpython$version.a; then
  AC_FIND_FILE(python$version/config/libpython$version.a, $python_libdirs, python_libdir)
  python_libdir=$python_libdir/python$version/config
  if test ! -r $python_libdir/libpython$version.a; then
    AC_MSG_ERROR(libpython$version.a not found.)
  fi
fi

PYTHONLIB=-L$python_libdir
if test -z "$LIBPYTHON"; then
LIBPYTHON=-lpython$version
fi

AC_MSG_RESULT(header $python_incdir library $python_libdir)

dnl Note: this test is very weak
kde_python_link_found=no
KDE_TRY_LINK_PYTHON(normal)
KDE_TRY_LINK_PYTHON(m, -lm)
KDE_TRY_LINK_PYTHON(pthread, $LIBPTHREAD)
KDE_TRY_LINK_PYTHON(tcl, -ltcl)
KDE_TRY_LINK_PYTHON(m_and_thread, [$LIBPTHREAD -lm], [],
	[AC_MSG_WARN([it seems, Python depends on another library. 
    Pleae use \"make LIBPTYHON='-lpython$version -lotherlib'\" to fix this
    and contact the authors to let them know about this problem])
	])

LIBPYTHON="$LIBPYTHON $LIBDL $LIBSOCKET"
AC_SUBST(PYTHONINC)
AC_SUBST(PYTHONLIB)
AC_SUBST(LIBPYTHON)

])


AC_DEFUN(KDE_CHECK_STL_SGI,
[
    AC_MSG_CHECKING([if STL implementation is SGI like])
    AC_CACHE_VAL(kde_cv_stl_type_sgi,
    [
      AC_TRY_COMPILE([
#include <string>
],[
  string astring="Hallo Welt.";
  astring.erase(0, 6); // now astring is "Welt"
  return 0;
], kde_cv_stl_type_sgi=yes,
   kde_cv_stl_type_sgi=no)
])

   AC_MSG_RESULT($kde_cv_stl_type_sgi)

   if test "$kde_cv_stl_type_sgi" = "yes"; then
	AC_DEFINE_UNQUOTED(HAVE_SGI_STL, 1, [Define if you have a STL implementation by SGI]) 
   fi
])

AC_DEFUN(KDE_CHECK_STL_HP,
[
    AC_MSG_CHECKING([if STL implementation is HP like])
    AC_CACHE_VAL(kde_cv_stl_type_hp,
    [
      AC_TRY_COMPILE([
#include <string>
],[
  string astring="Hello World";
  astring.remove(0, 6); // now astring is "World"
  return 0;
], kde_cv_stl_type_hp=yes,
   kde_cv_stl_type_hp=no)
])
   AC_MSG_RESULT($kde_cv_stl_type_hp)

   if test "$kde_cv_stl_type_hp" = "yes"; then
	AC_DEFINE_UNQUOTED(HAVE_HP_STL, 1, [Define if you have a STL implementation by SGI]) 
   fi
])

AC_DEFUN(KDE_CHECK_STL,
[
    AC_LANG_SAVE
    AC_LANG_CPLUSPLUS
    KDE_CHECK_STL_SGI
    
    if test "$kde_cv_stl_type_sgi" = "no"; then
       KDE_CHECK_STL_HP

       if test "$kde_cv_stl_type_hp" = "no"; then
         AC_MSG_ERROR("no known STL type found")
       fi
    fi

    AC_LANG_RESTORE
])

AC_DEFUN(AC_FIND_QIMGIO,
   [AC_REQUIRE([AC_FIND_JPEG])
AC_REQUIRE([KDE_CHECK_EXTRA_LIBS])
AC_MSG_CHECKING([for qimgio])
AC_CACHE_VAL(ac_cv_lib_qimgio,
[
AC_LANG_SAVE
AC_LANG_CPLUSPLUS
ac_save_LIBS="$LIBS"
LIBS="$all_libraries -lqimgio $LIBPNG -lz $LIBJPEG $LIBQT"
AC_TRY_LINK(dnl
[
void qInitImageIO ();
],
            [qInitImageIO();],
            eval "ac_cv_lib_qimgio=yes",
            eval "ac_cv_lib_qimgio=no")
LIBS="$ac_save_LIBS"
AC_LANG_RESTORE
])dnl
if eval "test \"`echo $ac_cv_lib_qimgio`\" = yes"; then
  LIBQIMGIO="-lqimgio $LIBPNG -lz $LIBJPEG"
  AC_MSG_RESULT(yes)
  AC_DEFINE_UNQUOTED(HAVE_QIMGIO, 1, [Define if you have the Qt extension qimgio available])
  AC_SUBST(LIBQIMGIO)
else
  AC_MSG_RESULT(not found)
fi
])

AC_DEFUN(KDE_CHECK_ANSI,
[
])

AC_DEFUN(KDE_CHECK_INSURE,
[
  AC_ARG_ENABLE(insure, [  --enable-insure             use insure++ for debugging [default=no]],
  [
  if test $enableval = "no"; dnl
	then ac_use_insure="no"
	else ac_use_insure="yes"
   fi
  ], [ac_use_insure="no"])

  AC_MSG_CHECKING(if we will use Insure++ to debug)
  AC_MSG_RESULT($ac_use_insure)
  if test "$ac_use_insure" = "yes"; dnl
       then CC="insure"; CXX="insure"; dnl CFLAGS="$CLAGS -fno-rtti -fno-exceptions "????
   fi
])          

AC_DEFUN(AM_DISABLE_LIBRARIES,
[
    AC_PROVIDE([AM_ENABLE_STATIC])
    AC_PROVIDE([AM_ENABLE_SHARED])
    enable_static=no
    enable_shared=yes
])


AC_DEFUN(AC_CHECK_UTMP_FILE,
[
    AC_MSG_CHECKING([for utmp file])
 
    AC_CACHE_VAL(kde_cv_utmp_file,
    [
    kde_cv_utmp_file=no
 
    for ac_file in    \
                      \
	/var/run/utmp \
	/var/adm/utmp \
	/etc/utmp     \
     ; \
    do
     if test -r "$ac_file"; then
       kde_cv_utmp_file=$ac_file
       break
     fi
    done
    ])
 
    AC_MSG_RESULT($kde_cv_utmp_file)
    if test "$kde_cv_utmp_file" != "no"; then
	AC_DEFINE_UNQUOTED(UTMP, "$kde_cv_utmp_file", [Define the file for utmp entries])
    fi
])


AC_DEFUN(KDE_CREATE_SUBDIRSLIST,
[

DO_NOT_COMPILE="$DO_NOT_COMPILE CVS debian bsd-port admin"

if test ! -s $srcdir/inst-apps && test -s $srcdir/subdirs; then
 cp $srcdir/subdirs $srcdir/inst-apps
fi

if test ! -s $srcdir/inst-apps; then
  TOPSUBDIRS=""
  files=`cd $srcdir && ls -1`
  dirs=`for i in $files; do if test -d $i; then echo $i; fi; done`
  for i in $dirs; do 
    echo $i >> $srcdir/subdirs
  done
  cp $srcdir/subdirs $srcdir/inst-apps
fi    

if test -s $srcdir/inst-apps; then
  ac_topsubdirs="`cat $srcdir/inst-apps`"
fi  

for i in $ac_topsubdirs; do
  AC_MSG_CHECKING([if $i should be compiled])
  if test -d $srcdir/$i; then
    install_it="yes"
    for j in $DO_NOT_COMPILE; do 
      if test $i = $j; then
        install_it="no"
      fi
    done
  else
    install_it="no"
  fi
  AC_MSG_RESULT($install_it)
  if test $install_it = "yes"; then 
    TOPSUBDIRS="$TOPSUBDIRS $i"
  fi
done

AC_SUBST(TOPSUBDIRS)
])

AC_DEFUN(KDE_CHECK_NAMESPACES,
[
AC_MSG_CHECKING(whether C++ compiler supports namespaces)
AC_LANG_SAVE
AC_LANG_CPLUSPLUS
AC_TRY_COMPILE([
],
[
namespace Foo {
  extern int i;
  namespace Bar {
    extern int i;
  }
}

int Foo::i = 0;
int Foo::Bar::i = 1;
],[
  AC_MSG_RESULT(yes)
  AC_DEFINE(HAVE_NAMESPACES)
], [
AC_MSG_RESULT(no)
])
AC_LANG_RESTORE
])

AC_DEFUN(KDE_CHECK_NEWLIBS,
[

])

dnl ------------------------------------------------------------------------
dnl Check for S_ISSOCK macro. Doesn't exist on Unix SCO. faure@kde.org
dnl ------------------------------------------------------------------------
dnl
AC_DEFUN(AC_CHECK_S_ISSOCK,
[
AC_MSG_CHECKING(for S_ISSOCK)
AC_CACHE_VAL(ac_cv_have_s_issock,
[
AC_LANG_SAVE
AC_LANG_C
AC_TRY_LINK(
[
#include <sys/stat.h>
],
[
struct stat buff;
int b = S_ISSOCK( buff.st_mode );
],
ac_cv_have_s_issock=yes,
ac_cv_have_s_issock=no)
AC_LANG_RESTORE 
])
AC_MSG_RESULT($ac_cv_have_s_issock)
if test "$ac_cv_have_s_issock" = "yes"; then
  AC_DEFINE_UNQUOTED(HAVE_S_ISSOCK, 1, [Define if sys/stat.h declares S_ISSOCK.])
fi
])

dnl ------------------------------------------------------------------------
dnl Check for MAXPATHLEN macro, defines KDEMAXPATHLEN. faure@kde.org
dnl ------------------------------------------------------------------------
dnl
AC_DEFUN(AC_CHECK_KDEMAXPATHLEN,
[
AC_MSG_CHECKING(for MAXPATHLEN)
AC_CACHE_VAL(ac_cv_maxpathlen,
[
AC_LANG_C
cat > conftest.$ac_ext <<EOF
#include <stdio.h>
#include <sys/param.h>
#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif

KDE_HELLO MAXPATHLEN

EOF

ac_try="$ac_cpp conftest.$ac_ext 2>/dev/null | grep '^KDE_HELLO' >conftest.out"

if AC_TRY_EVAL(ac_try) && test -s conftest.out; then 
    ac_cv_maxpathlen=`sed 's#KDE_HELLO ##' conftest.out`
else
    ac_cv_maxpathlen=1024
fi

rm conftest.*

])
AC_MSG_RESULT($ac_cv_maxpathlen)
AC_DEFINE_UNQUOTED(KDEMAXPATHLEN,$ac_cv_maxpathlen, [Define a safe value for MAXLENPATH] )
])

dnl AC_CHECK_LIBPT_NOC(LIBRARY, FUNCTION [, ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND
dnl              [, OTHER-LIBRARIES]]])
AC_DEFUN(AC_CHECK_LIBPT_NOC,
[AC_MSG_CHECKING([for $2 in -l$1])
dnl Use a cache variable name containing both the library and function name,
dnl because the test really is for library $1 defining function $2, not
dnl just for library $1.  Separate tests with the same $1 and different $2s
dnl may have different results.
ac_save_LIBS="$LIBS"
LIBS="-l$1 $5 $LIBS"
AC_TRY_LINK(dnl
ifelse(AC_LANG, [FORTRAN77], ,
ifelse([$2], [main], , dnl Avoid conflicting decl of main.
[/* Override any gcc2 internal prototype to avoid an error.  */
]ifelse(AC_LANG, CPLUSPLUS, [#ifdef __cplusplus
extern "C"
#endif
])dnl
[/* Use this to make sure it works with Tru64 */
#include <pthread.h>
])),
	    [$2(0)],
	    eval "ac_cv_lib_$ac_lib_var=yes",
	    eval "ac_cv_lib_$ac_lib_var=no")
LIBS="$ac_save_LIBS"
dnl
if eval "test \"`echo '$ac_cv_lib_'$ac_lib_var`\" = yes"; then
  AC_MSG_RESULT(yes)
  ifelse([$3], ,
[changequote(, )dnl
  ac_tr_lib=HAVE_LIB`echo $1 | sed -e 's/[^a-zA-Z0-9_]/_/g' \
    -e 'y/abcdefghijklmnopqrstuvwxyz/ABCDEFGHIJKLMNOPQRSTUVWXYZ/'`
changequote([, ])dnl
  AC_DEFINE_UNQUOTED($ac_tr_lib)
  LIBS="-l$1 $LIBS"
], [$3])
else
  AC_MSG_RESULT(no)
ifelse([$4], , , [$4
])dnl
fi
])

dnl Check if the compiler works with a given command line option
dnl AC_CHECK_COMP_OPT(OPTION)
AC_DEFUN(AC_CHECK_COMP_OPT,
[AC_MSG_CHECKING([if compiler accepts -$1])
echo 'void f(){}' > conftest.c
if test -z "`${CC-cc} -$1 -c conftest.c 2>&1`"; then
  AC_MSG_RESULT(yes)
  CFLAGS="$CFLAGS -$1"
else
  AC_MSG_RESULT(no)
fi
rm -f conftest*
])

dnl Check for a lib, without checking the cache first
dnl AC_CHECK_LIB_NOC(LIBRARY, FUNCTION [, ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND
dnl              [, OTHER-LIBRARIES]]])
AC_DEFUN(AC_CHECK_LIB_NOC,
[AC_MSG_CHECKING([for $2 in -l$1 $5])
ac_save_LIBS="$LIBS"
LIBS="-l$1 $5 $LIBS"
AC_TRY_LINK(dnl
ifelse(AC_LANG, [FORTRAN77], ,
ifelse([$2], [main], , dnl Avoid conflicting decl of main.
[/* Override any gcc2 internal prototype to avoid an error.  */
]ifelse(AC_LANG, CPLUSPLUS, [#ifdef __cplusplus
extern "C"
#endif
])dnl
[/* We use char because int might match the return type of a gcc2
    builtin and then its argument prototype would still apply.  */
char $2();
])),
	    [$2()],
	    eval "ac_cv_lib_$ac_lib_var=yes",
	    eval "ac_cv_lib_$ac_lib_var=no")
LIBS="$ac_save_LIBS"

if eval "test \"`echo '$ac_cv_lib_'$ac_lib_var`\" = yes"; then
  AC_MSG_RESULT(yes)
  ifelse([$3], ,
[changequote(, )dnl
  ac_tr_lib=HAVE_LIB`echo $1 | sed -e 's/[^a-zA-Z0-9_]/_/g' \
    -e 'y/abcdefghijklmnopqrstuvwxyz/ABCDEFGHIJKLMNOPQRSTUVWXYZ/'`
changequote([, ])dnl
  AC_DEFINE_UNQUOTED($ac_tr_lib)
  LIBS="-l$1 $LIBS"
], [$3])
else
  AC_MSG_RESULT(no)
ifelse([$4], , , [$4
])dnl
fi
])

dnl ##
dnl ##  GNU Pth - The GNU Portable Threads
dnl ##  Copyright (c) 1999-2000 Ralf S. Engelschall <rse@engelschall.com>
dnl ##
dnl ##  This file is part of GNU Pth, a non-preemptive thread scheduling
dnl ##  library which can be found at http://www.gnu.org/software/pth/.
dnl ##
dnl ##  This library is free software; you can redistribute it and/or
dnl ##  modify it under the terms of the GNU Lesser General Public
dnl ##  License as published by the Free Software Foundation; either
dnl ##  version 2.1 of the License, or (at your option) any later version.
dnl ##
dnl ##  This library is distributed in the hope that it will be useful,
dnl ##  but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl ##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
dnl ##  Lesser General Public License for more details.
dnl ##
dnl ##  You should have received a copy of the GNU Lesser General Public
dnl ##  License along with this library; if not, write to the Free Software
dnl ##  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
dnl ##  USA, or contact Ralf S. Engelschall <rse@engelschall.com>.
dnl ##
dnl ##  pth.m4: Autoconf macro for locating GNU Pth from within
dnl ##          configure.in of third-party software packages
dnl ##

dnl ##
dnl ##  Synopsis:
dnl ##  AC_CHECK_PTH([MIN-VERSION [,          # minimum Pth version, e.g. 1.2b3
dnl ##                DEFAULT-WITH-PTH [,     # default value for --with-pth option
dnl ##                DEFAULT-WITH-PTH-TEST [,# default value for --with-pth-test option
dnl ##                EXTEND-VARS [,          # whether CFLAGS/LDFLAGS/etc are extended
dnl ##                ACTION-IF-FOUND [,      # action to perform if Pth was found
dnl ##                ACTION-IF-NOT-FOUND     # action to perform if Pth was not found
dnl ##                ]]]]]])
dnl ##  Examples:
dnl ##  AC_CHECK_PTH(1.2.0)
dnl ##  AC_CHECK_PTH(1.2.0,,,no,CFLAGS="$CFLAGS -DHAVE_PTH $PTH_CFLAGS")
dnl ##  AC_CHECK_PTH(1.2.0,yes,yes,yes,CFLAGS="$CFLAGS -DHAVE_PTH")
dnl ##
dnl
dnl #   auxilliary macros
AC_DEFUN(_AC_PTH_ERROR, [dnl
AC_MSG_RESULT([*FAILED*])
define(_ac_pth_line,dnl
"+------------------------------------------------------------------------+")
echo " _ac_pth_line" 1>&2
cat <<EOT | sed -e 's/^[[ 	]]*/ | /' -e 's/>>/  /' 1>&2
$1
EOT
echo " _ac_pth_line" 1>&2
undefine(_ac_pth_line)
exit 1
])
AC_DEFUN(_AC_PTH_VERBOSE, [dnl
if test ".$verbose" = .yes; then
    AC_MSG_RESULT([  $1])
fi
])
dnl #   the user macro
AC_DEFUN(AC_CHECK_PTH, [dnl
dnl
dnl #   prerequisites
AC_REQUIRE([AC_PROG_CC])dnl
AC_REQUIRE([AC_PROG_CPP])dnl
dnl
PTH_CPPFLAGS=''
PTH_CFLAGS=''
PTH_LDFLAGS=''
PTH_LIBS=''
AC_SUBST(PTH_CPPFLAGS)
AC_SUBST(PTH_CFLAGS)
AC_SUBST(PTH_LDFLAGS)
AC_SUBST(PTH_LIBS)
dnl #   command line options
AC_MSG_CHECKING(for GNU Pth)
_AC_PTH_VERBOSE([])
AC_ARG_WITH(pth,dnl
[  --with-pth[=ARG]        Build with GNU Pth Library  (default=]ifelse([$2],,yes,$2)[)],dnl
,dnl
with_pth="ifelse([$2],,yes,$2)"
)dnl
AC_ARG_WITH(pth-test,dnl
[  --with-pth-test         Perform GNU Pth Sanity Test (default=]ifelse([$3],,yes,$3)[)],dnl
,dnl
with_pth_test="ifelse([$3],,yes,$3)"
)dnl
_AC_PTH_VERBOSE([+ Command Line Options:])
_AC_PTH_VERBOSE([    o --with-pth=$with_pth])
_AC_PTH_VERBOSE([    o --with-pth-test=$with_pth_test])
dnl
dnl #   configuration
if test ".$with_pth" != .no; then
    _pth_subdir=no
    _pth_subdir_opts=''
    case "$with_pth" in
        subdir:* )
            _pth_subdir=yes
            changequote(, )dnl
            _pth_subdir_opts=`echo $with_pth | sed -e 's/^subdir:[^ 	]*[ 	]*//'`
            with_pth=`echo $with_pth | sed -e 's/^subdir:\([^ 	]*\).*$/\1/'`
            changequote([, ])dnl
            ;;
    esac
    _pth_version=""
    _pth_location=""
    _pth_type=""
    _pth_cppflags=""
    _pth_cflags=""
    _pth_ldflags=""
    _pth_libs=""
    if test ".$with_pth" = .yes; then
        #   via config script in $PATH
        changequote(, )dnl
        _pth_version=`(pth-config --version) 2>/dev/null |\
                      sed -e 's/^.*\([0-9]\.[0-9]*[ab.][0-9]*\).*$/\1/'`
        changequote([, ])dnl
        if test ".$_pth_version" != .; then
            _pth_location=`pth-config --prefix`
            _pth_type='installed'
            _pth_cppflags=`pth-config --cflags`
            _pth_cflags=`pth-config --cflags`
            _pth_ldflags=`pth-config --ldflags`
            _pth_libs=`pth-config --libs`
        fi
    elif test -d "$with_pth"; then
        with_pth=`echo $with_pth | sed -e 's;/*$;;'`
        _pth_found=no
        #   via locally included source tree
        if test ".$_pth_subdir" = .yes; then
            _pth_location="$with_pth"
            _pth_type='local'
            _pth_cppflags="-I$with_pth"
            _pth_cflags="-I$with_pth"
            if test -f "$with_pth/ltconfig"; then
                _pth_ldflags="-L$with_pth/.libs"
            else
                _pth_ldflags="-L$with_pth"
            fi
            _pth_libs="-lpth"
            changequote(, )dnl
            _pth_version=`grep '^const char PTH_Hello' $with_pth/pth_vers.c |\
                          sed -e 's;^.*Version[ 	]*\([0-9]*\.[0-9]*[.ab][0-9]*\)[ 	].*$;\1;'`
            changequote([, ])dnl
            _pth_found=yes
            ac_configure_args="$ac_configure_args --enable-subdir $_pth_subdir_opts"
            with_pth_test=no
        fi
        #   via config script under a specified directory
        #   (a standard installation, but not a source tree)
        if test ".$_pth_found" = .no; then
            for _dir in $with_pth/bin $with_pth; do
                if test -f "$_dir/pth-config"; then
                    test -f "$_dir/pth-config.in" && continue # pth-config in source tree!
                    changequote(, )dnl
                    _pth_version=`($_dir/pth-config --version) 2>/dev/null |\
                                  sed -e 's/^.*\([0-9]\.[0-9]*[ab.][0-9]*\).*$/\1/'`
                    changequote([, ])dnl
                    if test ".$_pth_version" != .; then
                        _pth_location=`$_dir/pth-config --prefix`
                        _pth_type="installed"
                        _pth_cppflags=`$_dir/pth-config --cflags`
                        _pth_cflags=`$_dir/pth-config --cflags`
                        _pth_ldflags=`$_dir/pth-config --ldflags`
                        _pth_libs=`$_dir/pth-config --libs`
                        _pth_found=yes
                        break
                    fi
                fi
            done
        fi
        #   in any subarea under a specified directory
        #   (either a special installation or a Pth source tree)
        if test ".$_pth_found" = .no; then
            changequote(, )dnl
            _pth_found=0
            for _file in x `find $with_pth -name "pth.h" -type f -print`; do
                test .$_file = .x && continue
                _dir=`echo $_file | sed -e 's;[^/]*$;;' -e 's;\(.\)/$;\1;'`
                _pth_version=`($_dir/pth-config --version) 2>/dev/null |\
                              sed -e 's/^.*\([0-9]\.[0-9]*[ab.][0-9]*\).*$/\1/'`
                if test ".$_pth_version" = .; then
                    _pth_version=`grep '^#define PTH_VERSION_STR' $_file |\
                                  sed -e 's;^#define[ 	]*PTH_VERSION_STR[ 	]*"\([0-9]*\.[0-9]*[.ab][0-9]*\)[ 	].*$;\1;'`
                fi
                _pth_cppflags="-I$_dir"
                _pth_cflags="-I$_dir"
                _pth_found=`expr $_pth_found + 1`
            done
            for _file in x `find $with_pth -name "libpth.[aso]" -type f -print`; do
                test .$_file = .x && continue
                _dir=`echo $_file | sed -e 's;[^/]*$;;' -e 's;\(.\)/$;\1;'`
                _pth_ldflags="-L$_dir"
                _pth_libs="-lpth"
                _pth_found=`expr $_pth_found + 1`
            done
            changequote([, ])dnl
            if test ".$_pth_found" = .2; then
                _pth_location="$with_pth"
                _pth_type="uninstalled"
            else
                _pth_version=''
            fi
        fi
    fi
    _AC_PTH_VERBOSE([+ Determined Location:])
    _AC_PTH_VERBOSE([    o path: $_pth_location])
    _AC_PTH_VERBOSE([    o type: $_pth_type])
    if test ".$_pth_version" = .; then
        if test ".$with_pth" != .yes; then
             _AC_PTH_ERROR([dnl
             Unable to locate GNU Pth under $with_pth.
             Please specify the correct path to either a GNU Pth installation tree
             (use --with-pth=DIR if you used --prefix=DIR for installing GNU Pth in
             the past) or to a GNU Pth source tree (use --with-pth=DIR if DIR is a
             path to a pth-X.Y.Z/ directory; but make sure the package is already
             built, i.e., the "configure; make" step was already performed there).])
        else
             _AC_PTH_ERROR([dnl
             Unable to locate GNU Pth in any system-wide location (see \$PATH).
             Please specify the correct path to either a GNU Pth installation tree
             (use --with-pth=DIR if you used --prefix=DIR for installing GNU Pth in
             the past) or to a GNU Pth source tree (use --with-pth=DIR if DIR is a
             path to a pth-X.Y.Z/ directory; but make sure the package is already
             built, i.e., the "configure; make" step was already performed there).])
        fi
    fi
    dnl #
    dnl #  Check whether the found version is sufficiently new
    dnl #
    _req_version="ifelse([$1],,1.0.0,$1)"
    for _var in _pth_version _req_version; do
        eval "_val=\"\$${_var}\""
        _major=`echo $_val | sed 's/\([[0-9]]*\)\.\([[0-9]]*\)\([[ab.]]\)\([[0-9]]*\)/\1/'`
        _minor=`echo $_val | sed 's/\([[0-9]]*\)\.\([[0-9]]*\)\([[ab.]]\)\([[0-9]]*\)/\2/'`
        _rtype=`echo $_val | sed 's/\([[0-9]]*\)\.\([[0-9]]*\)\([[ab.]]\)\([[0-9]]*\)/\3/'`
        _micro=`echo $_val | sed 's/\([[0-9]]*\)\.\([[0-9]]*\)\([[ab.]]\)\([[0-9]]*\)/\4/'`
        case $_rtype in
            "a" ) _rtype=0 ;;
            "b" ) _rtype=1 ;;
            "." ) _rtype=2 ;;
        esac
        _hex=`echo dummy | awk '{ printf("%d%02d%1d%02d", major, minor, rtype, micro); }' \
              "major=$_major" "minor=$_minor" "rtype=$_rtype" "micro=$_micro"`
        eval "${_var}_hex=\"\$_hex\""
    done
    _AC_PTH_VERBOSE([+ Determined Versions:])
    _AC_PTH_VERBOSE([    o existing: $_pth_version -> 0x$_pth_version_hex])
    _AC_PTH_VERBOSE([    o required: $_req_version -> 0x$_req_version_hex])
    _ok=0
    if test ".$_pth_version_hex" != .; then
        if test ".$_req_version_hex" != .; then
            if test $_pth_version_hex -ge $_req_version_hex; then
                _ok=1
            fi
        fi
    fi
    if test ".$_ok" = .0; then
        _AC_PTH_ERROR([dnl
        Found Pth version $_pth_version, but required at least version $_req_version.
        Upgrade Pth under $_pth_location to $_req_version or higher first, please.])
    fi
    dnl #
    dnl #   Perform Pth Sanity Compile Check
    dnl #
    if test ".$with_pth_test" = .yes; then
        _ac_save_CPPFLAGS="$CPPFLAGS"
        _ac_save_CFLAGS="$CFLAGS"
        _ac_save_LDFLAGS="$LDFLAGS"
        _ac_save_LIBS="$LIBS"
        CPPFLAGS="$CPPFLAGS $_pth_cppflags"
        CFLAGS="$CFLAGS $_pth_cflags"
        LDFLAGS="$LDFLAGS $_pth_ldflags"
        LIBS="$LIBS $_pth_libs"
        _AC_PTH_VERBOSE([+ Test Build Environment:])
        _AC_PTH_VERBOSE([    o CPPFLAGS=\"$CPPFLAGS\"])
        _AC_PTH_VERBOSE([    o CFLAGS=\"$CFLAGS\"])
        _AC_PTH_VERBOSE([    o LDFLAGS=\"$LDFLAGS\"])
        _AC_PTH_VERBOSE([    o LIBS=\"$LIBS\"])
        cross_compile=no
        define(_code1, [dnl
        #include <stdio.h>
        #include <pth.h>
        ])
        define(_code2, [dnl
        int main(int argc, char *argv[])
        {
            FILE *fp;
            if (!(fp = fopen("conftestval", "w")))
                exit(1);
            fprintf(fp, "hmm");
            fclose(fp);
            pth_init();
            pth_kill();
            if (!(fp = fopen("conftestval", "w")))
                exit(1);
            fprintf(fp, "yes");
            fclose(fp);
            exit(0);
        }
        ])
        _AC_PTH_VERBOSE([+ Performing Sanity Checks:])
        _AC_PTH_VERBOSE([    o pre-processor test])
        AC_TRY_CPP(_code1, _ok=yes, _ok=no)
        if test ".$_ok" != .yes; then
            _AC_PTH_ERROR([dnl
            Found GNU Pth $_pth_version under $_pth_location, but
            was unable to perform a sanity pre-processor check. This means
            the GNU Pth header pth.h was not found.
            We used the following build environment:
            >> CPP="$CPP"
            >> CPPFLAGS="$CPPFLAGS"
            See config.log for possibly more details.])
        fi
        _AC_PTH_VERBOSE([    o link check])
        AC_TRY_LINK(_code1, _code2, _ok=yes, _ok=no)
        if test ".$_ok" != .yes; then
            _AC_PTH_ERROR([dnl
            Found GNU Pth $_pth_version under $_pth_location, but
            was unable to perform a sanity linker check. This means
            the GNU Pth library libpth.a was not found.
            We used the following build environment:
            >> CC="$CC"
            >> CFLAGS="$CFLAGS"
            >> LDFLAGS="$LDFLAGS"
            >> LIBS="$LIBS"
            See config.log for possibly more details.])
        fi
        _AC_PTH_VERBOSE([    o run-time check])
        AC_TRY_RUN(_code1 _code2, _ok=`cat conftestval`, _ok=no, _ok=no)
        if test ".$_ok" != .yes; then
            if test ".$_ok" = .no; then
                _AC_PTH_ERROR([dnl
                Found GNU Pth $_pth_version under $_pth_location, but
                was unable to perform a sanity execution check. This usually
                means that the GNU Pth shared library libpth.so is present
                but \$LD_LIBRARY_PATH is incomplete to execute a Pth test.
                In this case either disable this test via --without-pth-test,
                or extend \$LD_LIBRARY_PATH, or build GNU Pth as a static
                library only via its --disable-shared Autoconf option.
                We used the following build environment:
                >> CC="$CC"
                >> CFLAGS="$CFLAGS"
                >> LDFLAGS="$LDFLAGS"
                >> LIBS="$LIBS"
                See config.log for possibly more details.])
            else
                _AC_PTH_ERROR([dnl
                Found GNU Pth $_pth_version under $_pth_location, but
                was unable to perform a sanity run-time check. This usually
                means that the GNU Pth library failed to work and possibly
                caused a core dump in the test program. In this case it
                is strongly recommended that you re-install GNU Pth and this
                time make sure that it really passes its "make test" procedure.
                We used the following build environment:
                >> CC="$CC"
                >> CFLAGS="$CFLAGS"
                >> LDFLAGS="$LDFLAGS"
                >> LIBS="$LIBS"
                See config.log for possibly more details.])
            fi
        fi
        _extendvars="ifelse([$4],,yes,$4)"
        if test ".$_extendvars" != .yes; then
            CPPFLAGS="$_ac_save_CPPFLAGS"
            CFLAGS="$_ac_save_CFLAGS"
            LDFLAGS="$_ac_save_LDFLAGS"
            LIBS="$_ac_save_LIBS"
        fi
    else
        _extendvars="ifelse([$4],,yes,$4)"
        if test ".$_extendvars" = .yes; then
            if test ".$_pth_subdir" = .yes; then
                CPPFLAGS="$CPPFLAGS $_pth_cppflags"
                CFLAGS="$CFLAGS $_pth_cflags"
                LDFLAGS="$LDFLAGS $_pth_ldflags"
                LIBS="$LIBS $_pth_libs"
            fi
        fi
    fi
    PTH_CPPFLAGS="$_pth_cppflags"
    PTH_CFLAGS="$_pth_cflags"
    PTH_LDFLAGS="$_pth_ldflags"
    PTH_LIBS="$_pth_libs"
    AC_SUBST(PTH_CPPFLAGS)
    AC_SUBST(PTH_CFLAGS)
    AC_SUBST(PTH_LDFLAGS)
    AC_SUBST(PTH_LIBS)
    _AC_PTH_VERBOSE([+ Final Results:])
    _AC_PTH_VERBOSE([    o PTH_CPPFLAGS=\"$PTH_CPPFLAGS\"])
    _AC_PTH_VERBOSE([    o PTH_CFLAGS=\"$PTH_CFLAGS\"])
    _AC_PTH_VERBOSE([    o PTH_LDFLAGS=\"$PTH_LDFLAGS\"])
    _AC_PTH_VERBOSE([    o PTH_LIBS=\"$PTH_LIBS\"])
fi
if test ".$with_pth" != .no; then
    AC_MSG_RESULT([version $_pth_version, $_pth_type under $_pth_location])
    ifelse([$5], , :, [$5])
else
    AC_MSG_RESULT([no])
    ifelse([$6], , :, [$6])
fi
])

dnl This is from libtool

# AC_LTDL_SHLIBEXT
# ----------------
AC_DEFUN(AC_LTDL_SHLIBEXT,
[AC_CACHE_CHECK([which extension is used for shared libraries],
  libltdl_cv_shlibext,
[ac_last=
  for ac_spec in $library_names_spec; do
    ac_last="$ac_spec"
  done
  echo "$ac_last" | [sed 's/\[.*\]//;s/^[^.]*//;s/\$.*$//;s/\.$//'] > conftest
libltdl_cv_shlibext=`cat conftest`
rm -f conftest
])
if test -n "$libltdl_cv_shlibext"; then
  AC_DEFINE_UNQUOTED(LTDL_SHLIB_EXT, "$libltdl_cv_shlibext",
    [Define to the extension used for shared libraries, say, ".so". ])
fi
])# AC_LTDL_SHLIBEXT

# AC_CHECK_LONG_LONG
#-------------------
AC_DEFUN(AC_CHECK_LONG_LONG,
[
AC_MSG_CHECKING(for long long)
AC_CACHE_VAL(ac_cv_type_long_long,
[
AC_TRY_COMPILE([
#include <stdio.h>
],
[
long long x;
],
ac_cv_type_long_long=yes,
ac_cv_type_long_long=no)
])
AC_MSG_RESULT($ac_cv_type_long_long)
if eval "test \"`echo $ac_cv_type_long_long`\" = yes"; then
  AC_DEFINE(HAVE_LONG_LONG, 1, [Define if you have long long])
fi
])# AC_CHECK_LONG_LONG

dnl From Bruno Haible.

AC_DEFUN([AM_ICONV],
[
  dnl Some systems have iconv in libc, some have it in libiconv (OSF/1 and
  dnl those with the standalone portable GNU libiconv installed).

  AC_ARG_WITH([libiconv-prefix],
[  --with-libiconv-prefix=DIR  search for libiconv in DIR/include and DIR/lib], [
    for dir in `echo "$withval" | tr : ' '`; do
      if test -d $dir/include; then CPPFLAGS="$CPPFLAGS -I$dir/include"; fi
      if test -d $dir/lib; then LDFLAGS="$LDFLAGS -L$dir/lib"; fi
    done
   ])

  AC_CACHE_CHECK(for iconv, am_cv_func_iconv, [
    am_cv_func_iconv="no, consider installing GNU libiconv"
    am_cv_lib_iconv=no
    AC_TRY_LINK([#include <stdlib.h>
#include <iconv.h>],
      [iconv_t cd = iconv_open("","");
       iconv(cd,NULL,NULL,NULL,NULL);
       iconv_close(cd);],
      am_cv_func_iconv=yes)
    if test "$am_cv_func_iconv" != yes; then
      am_save_LIBS="$LIBS"
      LIBS="$LIBS -liconv"
      AC_TRY_LINK([#include <stdlib.h>
#include <iconv.h>],
        [iconv_t cd = iconv_open("","");
         iconv(cd,NULL,NULL,NULL,NULL);
         iconv_close(cd);],
        am_cv_lib_iconv=yes
        am_cv_func_iconv=yes)
      LIBS="$am_save_LIBS"
    fi
  ])
  if test "$am_cv_func_iconv" = yes; then
    AC_DEFINE(HAVE_ICONV, 1, [Define if you have the iconv() function.])
    AC_MSG_CHECKING([for iconv declaration])
    AC_CACHE_VAL(am_cv_proto_iconv, [
      AC_TRY_COMPILE([
#include <stdlib.h>
#include <iconv.h>
extern
#ifdef __cplusplus
"C"
#endif
#if defined(__STDC__) || defined(__cplusplus)
size_t iconv (iconv_t cd, char * *inbuf, size_t *inbytesleft, char * *outbuf, size_t *outbytesleft);
#else
size_t iconv();
#endif
], [], am_cv_proto_iconv_arg1="", am_cv_proto_iconv_arg1="const")
      am_cv_proto_iconv="extern size_t iconv (iconv_t cd, $am_cv_proto_iconv_arg1 char * *inbuf, size_t *inbytesleft, char * *outbuf, size_t *outbytesleft);"])
    am_cv_proto_iconv=`echo "[$]am_cv_proto_iconv" | tr -s ' ' | sed -e 's/( /(/'`
    AC_MSG_RESULT([$]{ac_t:-
         }[$]am_cv_proto_iconv)
    AC_DEFINE_UNQUOTED(ICONV_CONST, $am_cv_proto_iconv_arg1,
      [Define as const if the declaration of iconv() needs const.])
  fi
  LIBICONV=
  if test "$am_cv_lib_iconv" = yes; then
    LIBICONV="-liconv"
  fi
  AC_SUBST(LIBICONV)
])

# libtool.m4 - Configure libtool for the host system. -*-Shell-script-*-

# serial 46 AC_PROG_LIBTOOL

AC_DEFUN([AC_PROG_LIBTOOL],
[AC_REQUIRE([AC_LIBTOOL_SETUP])dnl

# This can be used to rebuild libtool when needed
LIBTOOL_DEPS="$ac_aux_dir/ltmain.sh"

# Always use our own libtool.
LIBTOOL='$(SHELL) $(top_builddir)/libtool'
AC_SUBST(LIBTOOL)dnl

# Prevent multiple expansion
define([AC_PROG_LIBTOOL], [])
])

AC_DEFUN([AC_LIBTOOL_SETUP],
[AC_PREREQ(2.13)dnl
AC_REQUIRE([AC_ENABLE_SHARED])dnl
AC_REQUIRE([AC_ENABLE_STATIC])dnl
AC_REQUIRE([AC_ENABLE_FAST_INSTALL])dnl
AC_REQUIRE([AC_CANONICAL_HOST])dnl
AC_REQUIRE([AC_CANONICAL_BUILD])dnl
AC_REQUIRE([AC_PROG_CC])dnl
AC_REQUIRE([AC_PROG_LD])dnl
AC_REQUIRE([AC_PROG_LD_RELOAD_FLAG])dnl
AC_REQUIRE([AC_PROG_NM])dnl
AC_REQUIRE([AC_PROG_LN_S])dnl
AC_REQUIRE([AC_DEPLIBS_CHECK_METHOD])dnl
AC_REQUIRE([AC_OBJEXT])dnl
AC_REQUIRE([AC_EXEEXT])dnl
dnl

_LT_AC_PROG_ECHO_BACKSLASH
# Only perform the check for file, if the check method requires it
case $deplibs_check_method in
file_magic*)
  if test "$file_magic_cmd" = '$MAGIC_CMD'; then
    AC_PATH_MAGIC
  fi
  ;;
esac

AC_CHECK_TOOL(RANLIB, ranlib, :)
AC_CHECK_TOOL(STRIP, strip, :)

ifdef([AC_PROVIDE_AC_LIBTOOL_DLOPEN], enable_dlopen=yes, enable_dlopen=no)
ifdef([AC_PROVIDE_AC_LIBTOOL_WIN32_DLL],
enable_win32_dll=yes, enable_win32_dll=no)

AC_ARG_ENABLE(libtool-lock,
  [  --disable-libtool-lock  avoid locking (might break parallel builds)])
test "x$enable_libtool_lock" != xno && enable_libtool_lock=yes

# Some flags need to be propagated to the compiler or linker for good
# libtool support.
case $host in
*-*-irix6*)
  # Find out which ABI we are using.
  echo '[#]line __oline__ "configure"' > conftest.$ac_ext
  if AC_TRY_EVAL(ac_compile); then
    case `/usr/bin/file conftest.$ac_objext` in
    *32-bit*)
      LD="${LD-ld} -32"
      ;;
    *N32*)
      LD="${LD-ld} -n32"
      ;;
    *64-bit*)
      LD="${LD-ld} -64"
      ;;
    esac
  fi
  rm -rf conftest*
  ;;

*-*-sco3.2v5*)
  # On SCO OpenServer 5, we need -belf to get full-featured binaries.
  SAVE_CFLAGS="$CFLAGS"
  CFLAGS="$CFLAGS -belf"
  AC_CACHE_CHECK([whether the C compiler needs -belf], lt_cv_cc_needs_belf,
    [AC_LANG_SAVE
     AC_LANG_C
     AC_TRY_LINK([],[],[lt_cv_cc_needs_belf=yes],[lt_cv_cc_needs_belf=no])
     AC_LANG_RESTORE])
  if test x"$lt_cv_cc_needs_belf" != x"yes"; then
    # this is probably gcc 2.8.0, egcs 1.0 or newer; no need for -belf
    CFLAGS="$SAVE_CFLAGS"
  fi
  ;;

ifdef([AC_PROVIDE_AC_LIBTOOL_WIN32_DLL],
[*-*-cygwin* | *-*-mingw* | *-*-pw32*)
  AC_CHECK_TOOL(DLLTOOL, dlltool, false)
  AC_CHECK_TOOL(AS, as, false)
  AC_CHECK_TOOL(OBJDUMP, objdump, false)

  # recent cygwin and mingw systems supply a stub DllMain which the user
  # can override, but on older systems we have to supply one
  AC_CACHE_CHECK([if libtool should supply DllMain function], lt_cv_need_dllmain,
    [AC_TRY_LINK([],
      [extern int __attribute__((__stdcall__)) DllMain(void*, int, void*);
      DllMain (0, 0, 0);],
      [lt_cv_need_dllmain=no],[lt_cv_need_dllmain=yes])])

  case $host/$CC in
  *-*-cygwin*/gcc*-mno-cygwin*|*-*-mingw*)
    # old mingw systems require "-dll" to link a DLL, while more recent ones
    # require "-mdll"
    SAVE_CFLAGS="$CFLAGS"
    CFLAGS="$CFLAGS -mdll"
    AC_CACHE_CHECK([how to link DLLs], lt_cv_cc_dll_switch,
      [AC_TRY_LINK([], [], [lt_cv_cc_dll_switch=-mdll],[lt_cv_cc_dll_switch=-dll])])
    CFLAGS="$SAVE_CFLAGS" ;;
  *-*-cygwin* | *-*-pw32*)
    # cygwin systems need to pass --dll to the linker, and not link
    # crt.o which will require a WinMain@16 definition.
    lt_cv_cc_dll_switch="-Wl,--dll -nostartfiles" ;;
  esac
  ;;
  ])
esac

_LT_AC_LTCONFIG_HACK

])

# AC_LIBTOOL_HEADER_ASSERT
# ------------------------
AC_DEFUN([AC_LIBTOOL_HEADER_ASSERT],
[AC_CACHE_CHECK([whether $CC supports assert without backlinking],
    [lt_cv_func_assert_works],
    [case $host in
    *-*-solaris*)
      if test "$GCC" = yes && test "$with_gnu_ld" != yes; then
        case `$CC --version 2>/dev/null` in
        [[12]].*) lt_cv_func_assert_works=no ;;
        *)        lt_cv_func_assert_works=yes ;;
        esac
      fi
      ;;
    esac])

if test "x$lt_cv_func_assert_works" = xyes; then
  AC_CHECK_HEADERS(assert.h)
fi
])# AC_LIBTOOL_HEADER_ASSERT

# _LT_AC_CHECK_DLFCN
# --------------------
AC_DEFUN([_LT_AC_CHECK_DLFCN],
[AC_CHECK_HEADERS(dlfcn.h)
])# _LT_AC_CHECK_DLFCN

# AC_LIBTOOL_SYS_GLOBAL_SYMBOL_PIPE
# ---------------------------------
AC_DEFUN([AC_LIBTOOL_SYS_GLOBAL_SYMBOL_PIPE],
[AC_REQUIRE([AC_CANONICAL_HOST])
AC_REQUIRE([AC_PROG_NM])
AC_REQUIRE([AC_OBJEXT])
# Check for command to grab the raw symbol name followed by C symbol from nm.
AC_MSG_CHECKING([command to parse $NM output])
AC_CACHE_VAL([lt_cv_sys_global_symbol_pipe], [dnl

# These are sane defaults that work on at least a few old systems.
# [They come from Ultrix.  What could be older than Ultrix?!! ;)]

# Character class describing NM global symbol codes.
symcode='[[BCDEGRST]]'

# Regexp to match symbols that can be accessed directly from C.
sympat='\([[_A-Za-z]][[_A-Za-z0-9]]*\)'

# Transform the above into a raw symbol and a C symbol.
symxfrm='\1 \2\3 \3'

# Transform an extracted symbol line into a proper C declaration
lt_cv_global_symbol_to_cdecl="sed -n -e 's/^. .* \(.*\)$/extern char \1;/p'"

# Transform an extracted symbol line into symbol name and symbol address
lt_cv_global_symbol_to_c_name_address="sed -n -e 's/^: \([[^ ]]*\) $/  {\\\"\1\\\", (lt_ptr) 0},/p' -e 's/^$symcode \([[^ ]]*\) \([[^ ]]*\)$/  {\"\2\", (lt_ptr) \&\2},/p'"

# Define system-specific variables.
case $host_os in
aix*)
  symcode='[[BCDT]]'
  ;;
cygwin* | mingw* | pw32*)
  symcode='[[ABCDGISTW]]'
  ;;
hpux*) # Its linker distinguishes data from code symbols
  lt_cv_global_symbol_to_cdecl="sed -n -e 's/^T .* \(.*\)$/extern char \1();/p' -e 's/^$symcode* .* \(.*\)$/extern char \1;/p'"
  lt_cv_global_symbol_to_c_name_address="sed -n -e 's/^: \([[^ ]]*\) $/  {\\\"\1\\\", (lt_ptr) 0},/p' -e 's/^$symcode* \([[^ ]]*\) \([[^ ]]*\)$/  {\"\2\", (lt_ptr) \&\2},/p'"
  ;;
irix*)
  symcode='[[BCDEGRST]]'
  ;;
solaris* | sysv5*)
  symcode='[[BDT]]'
  ;;
sysv4)
  symcode='[[DFNSTU]]'
  ;;
esac

# Handle CRLF in mingw tool chain
opt_cr=
case $host_os in
mingw*)
  opt_cr=`echo 'x\{0,1\}' | tr x '\015'` # option cr in regexp
  ;;
esac

# If we're using GNU nm, then use its standard symbol codes.
if $NM -V 2>&1 | egrep '(GNU|with BFD)' > /dev/null; then
  symcode='[[ABCDGISTW]]'
fi

# Try without a prefix undercore, then with it.
for ac_symprfx in "" "_"; do

  # Write the raw and C identifiers.
lt_cv_sys_global_symbol_pipe="sed -n -e 's/^.*[[ 	]]\($symcode$symcode*\)[[ 	]][[ 	]]*\($ac_symprfx\)$sympat$opt_cr$/$symxfrm/p'"

  # Check to see that the pipe works correctly.
  pipe_works=no
  rm -f conftest*
  cat > conftest.$ac_ext <<EOF
#ifdef __cplusplus
extern "C" {
#endif
char nm_test_var;
void nm_test_func(){}
#ifdef __cplusplus
}
#endif
int main(){nm_test_var='a';nm_test_func();return(0);}
EOF

  if AC_TRY_EVAL(ac_compile); then
    # Now try to grab the symbols.
    nlist=conftest.nm
    if AC_TRY_EVAL(NM conftest.$ac_objext \| $lt_cv_sys_global_symbol_pipe \> $nlist) && test -s "$nlist"; then
      # Try sorting and uniquifying the output.
      if sort "$nlist" | uniq > "$nlist"T; then
	mv -f "$nlist"T "$nlist"
      else
	rm -f "$nlist"T
      fi

      # Make sure that we snagged all the symbols we need.
      if egrep ' nm_test_var$' "$nlist" >/dev/null; then
	if egrep ' nm_test_func$' "$nlist" >/dev/null; then
	  cat <<EOF > conftest.$ac_ext
#ifdef __cplusplus
extern "C" {
#endif

EOF
	  # Now generate the symbol file.
	  eval "$lt_cv_global_symbol_to_cdecl"' < "$nlist" >> conftest.$ac_ext'

	  cat <<EOF >> conftest.$ac_ext
#if defined (__STDC__) && __STDC__
# define lt_ptr void *
#else
# define lt_ptr char *
# define const
#endif

/* The mapping between symbol names and symbols. */
const struct {
  const char *name;
  lt_ptr address;
}
lt_preloaded_symbols[[]] =
{
EOF
	  sed "s/^$symcode$symcode* \(.*\) \(.*\)$/  {\"\2\", (lt_ptr) \&\2},/" < "$nlist" >> conftest.$ac_ext
	  cat <<\EOF >> conftest.$ac_ext
  {0, (lt_ptr) 0}
};

#ifdef __cplusplus
}
#endif
EOF
	  # Now try linking the two files.
	  mv conftest.$ac_objext conftstm.$ac_objext
	  save_LIBS="$LIBS"
	  save_CFLAGS="$CFLAGS"
	  LIBS="conftstm.$ac_objext"
	  CFLAGS="$CFLAGS$no_builtin_flag"
	  if AC_TRY_EVAL(ac_link) && test -s conftest; then
	    pipe_works=yes
	  fi
	  LIBS="$save_LIBS"
	  CFLAGS="$save_CFLAGS"
	else
	  echo "cannot find nm_test_func in $nlist" >&AC_FD_CC
	fi
      else
	echo "cannot find nm_test_var in $nlist" >&AC_FD_CC
      fi
    else
      echo "cannot run $lt_cv_sys_global_symbol_pipe" >&AC_FD_CC
    fi
  else
    echo "$progname: failed program was:" >&AC_FD_CC
    cat conftest.$ac_ext >&5
  fi
  rm -f conftest* conftst*

  # Do not use the global_symbol_pipe unless it works.
  if test "$pipe_works" = yes; then
    break
  else
    lt_cv_sys_global_symbol_pipe=
  fi
done
])
global_symbol_pipe="$lt_cv_sys_global_symbol_pipe"
if test -z "$lt_cv_sys_global_symbol_pipe"; then
  global_symbol_to_cdecl=
  global_symbol_to_c_name_address=
else
  global_symbol_to_cdecl="$lt_cv_global_symbol_to_cdecl"
  global_symbol_to_c_name_address="$lt_cv_global_symbol_to_c_name_address"
fi
if test -z "$global_symbol_pipe$global_symbol_to_cdec$global_symbol_to_c_name_address";
then
  AC_MSG_RESULT(failed)
else
  AC_MSG_RESULT(ok)
fi
]) # AC_LIBTOOL_SYS_GLOBAL_SYMBOL_PIPE

# _LT_AC_LIBTOOL_SYS_PATH_SEPARATOR
# ---------------------------------
AC_DEFUN([_LT_AC_LIBTOOL_SYS_PATH_SEPARATOR],
[# Find the correct PATH separator.  Usually this is `:', but
# DJGPP uses `;' like DOS.
if test "X${PATH_SEPARATOR+set}" != Xset; then
  UNAME=${UNAME-`uname 2>/dev/null`}
  case X$UNAME in
    *-DOS) lt_cv_sys_path_separator=';' ;;
    *)     lt_cv_sys_path_separator=':' ;;
  esac
  PATH_SEPARATOR=$lt_cv_sys_path_separator
fi
])# _LT_AC_LIBTOOL_SYS_PATH_SEPARATOR

# _LT_AC_PROG_ECHO_BACKSLASH
# --------------------------
# Add some code to the start of the generated configure script which
# will find an echo command which doesn't interpret backslashes.
AC_DEFUN([_LT_AC_PROG_ECHO_BACKSLASH],
[ifdef([AC_DIVERSION_NOTICE], [AC_DIVERT_PUSH(AC_DIVERSION_NOTICE)],
			      [AC_DIVERT_PUSH(NOTICE)])
_LT_AC_LIBTOOL_SYS_PATH_SEPARATOR

# Check that we are running under the correct shell.
SHELL=${CONFIG_SHELL-/bin/sh}

case X$ECHO in
X*--fallback-echo)
  # Remove one level of quotation (which was required for Make).
  ECHO=`echo "$ECHO" | sed 's,\\\\\[$]\\[$]0,'[$]0','`
  ;;
esac

echo=${ECHO-echo}
if test "X[$]1" = X--no-reexec; then
  # Discard the --no-reexec flag, and continue.
  shift
elif test "X[$]1" = X--fallback-echo; then
  # Avoid inline document here, it may be left over
  :
elif test "X`($echo '\t') 2>/dev/null`" = 'X\t'; then
  # Yippee, $echo works!
  :
else
  # Restart under the correct shell.
  exec $SHELL "[$]0" --no-reexec ${1+"[$]@"}
fi

if test "X[$]1" = X--fallback-echo; then
  # used as fallback echo
  shift
  cat <<EOF
$*
EOF
  exit 0
fi

# The HP-UX ksh and POSIX shell print the target directory to stdout
# if CDPATH is set.
if test "X${CDPATH+set}" = Xset; then CDPATH=:; export CDPATH; fi

if test -z "$ECHO"; then
if test "X${echo_test_string+set}" != Xset; then
# find a string as large as possible, as long as the shell can cope with it
  for cmd in 'sed 50q "[$]0"' 'sed 20q "[$]0"' 'sed 10q "[$]0"' 'sed 2q "[$]0"' 'echo test'; do
    # expected sizes: less than 2Kb, 1Kb, 512 bytes, 16 bytes, ...
    if (echo_test_string="`eval $cmd`") 2>/dev/null &&
       echo_test_string="`eval $cmd`" &&
       (test "X$echo_test_string" = "X$echo_test_string") 2>/dev/null
    then
      break
    fi
  done
fi

if test "X`($echo '\t') 2>/dev/null`" = 'X\t' &&
   echo_testing_string=`($echo "$echo_test_string") 2>/dev/null` &&
   test "X$echo_testing_string" = "X$echo_test_string"; then
  :
else
  # The Solaris, AIX, and Digital Unix default echo programs unquote
  # backslashes.  This makes it impossible to quote backslashes using
  #   echo "$something" | sed 's/\\/\\\\/g'
  #
  # So, first we look for a working echo in the user's PATH.

  IFS="${IFS= 	}"; save_ifs="$IFS"; IFS=$PATH_SEPARATOR
  for dir in $PATH /usr/ucb; do
    if (test -f $dir/echo || test -f $dir/echo$ac_exeext) &&
       test "X`($dir/echo '\t') 2>/dev/null`" = 'X\t' &&
       echo_testing_string=`($dir/echo "$echo_test_string") 2>/dev/null` &&
       test "X$echo_testing_string" = "X$echo_test_string"; then
      echo="$dir/echo"
      break
    fi
  done
  IFS="$save_ifs"

  if test "X$echo" = Xecho; then
    # We didn't find a better echo, so look for alternatives.
    if test "X`(print -r '\t') 2>/dev/null`" = 'X\t' &&
       echo_testing_string=`(print -r "$echo_test_string") 2>/dev/null` &&
       test "X$echo_testing_string" = "X$echo_test_string"; then
      # This shell has a builtin print -r that does the trick.
      echo='print -r'
    elif (test -f /bin/ksh || test -f /bin/ksh$ac_exeext) &&
	 test "X$CONFIG_SHELL" != X/bin/ksh; then
      # If we have ksh, try running configure again with it.
      ORIGINAL_CONFIG_SHELL=${CONFIG_SHELL-/bin/sh}
      export ORIGINAL_CONFIG_SHELL
      CONFIG_SHELL=/bin/ksh
      export CONFIG_SHELL
      exec $CONFIG_SHELL "[$]0" --no-reexec ${1+"[$]@"}
    else
      # Try using printf.
      echo='printf %s\n'
      if test "X`($echo '\t') 2>/dev/null`" = 'X\t' &&
	 echo_testing_string=`($echo "$echo_test_string") 2>/dev/null` &&
	 test "X$echo_testing_string" = "X$echo_test_string"; then
	# Cool, printf works
	:
      elif echo_testing_string=`($ORIGINAL_CONFIG_SHELL "[$]0" --fallback-echo '\t') 2>/dev/null` &&
	   test "X$echo_testing_string" = 'X\t' &&
	   echo_testing_string=`($ORIGINAL_CONFIG_SHELL "[$]0" --fallback-echo "$echo_test_string") 2>/dev/null` &&
	   test "X$echo_testing_string" = "X$echo_test_string"; then
	CONFIG_SHELL=$ORIGINAL_CONFIG_SHELL
	export CONFIG_SHELL
	SHELL="$CONFIG_SHELL"
	export SHELL
	echo="$CONFIG_SHELL [$]0 --fallback-echo"
      elif echo_testing_string=`($CONFIG_SHELL "[$]0" --fallback-echo '\t') 2>/dev/null` &&
	   test "X$echo_testing_string" = 'X\t' &&
	   echo_testing_string=`($CONFIG_SHELL "[$]0" --fallback-echo "$echo_test_string") 2>/dev/null` &&
	   test "X$echo_testing_string" = "X$echo_test_string"; then
	echo="$CONFIG_SHELL [$]0 --fallback-echo"
      else
	# maybe with a smaller string...
	prev=:

	for cmd in 'echo test' 'sed 2q "[$]0"' 'sed 10q "[$]0"' 'sed 20q "[$]0"' 'sed 50q "[$]0"'; do
	  if (test "X$echo_test_string" = "X`eval $cmd`") 2>/dev/null
	  then
	    break
	  fi
	  prev="$cmd"
	done

	if test "$prev" != 'sed 50q "[$]0"'; then
	  echo_test_string=`eval $prev`
	  export echo_test_string
	  exec ${ORIGINAL_CONFIG_SHELL-${CONFIG_SHELL-/bin/sh}} "[$]0" ${1+"[$]@"}
	else
	  # Oops.  We lost completely, so just stick with echo.
	  echo=echo
	fi
      fi
    fi
  fi
fi
fi

# Copy echo and quote the copy suitably for passing to libtool from
# the Makefile, instead of quoting the original, which is used later.
ECHO=$echo
if test "X$ECHO" = "X$CONFIG_SHELL [$]0 --fallback-echo"; then
   ECHO="$CONFIG_SHELL \\\$\[$]0 --fallback-echo"
fi

AC_SUBST(ECHO)
AC_DIVERT_POP
])# _LT_AC_PROG_ECHO_BACKSLASH

# _LT_AC_TRY_DLOPEN_SELF (ACTION-IF-TRUE, ACTION-IF-TRUE-W-USCORE,
#                           ACTION-IF-FALSE, ACTION-IF-CROSS-COMPILING)
# ------------------------------------------------------------------
AC_DEFUN([_LT_AC_TRY_DLOPEN_SELF],
[if test "$cross_compiling" = yes; then :
  [$4]
else
  AC_REQUIRE([_LT_AC_CHECK_DLFCN])dnl
  lt_dlunknown=0; lt_dlno_uscore=1; lt_dlneed_uscore=2
  lt_status=$lt_dlunknown
  cat > conftest.$ac_ext <<EOF
[#line __oline__ "configure"
#include "confdefs.h"

#if HAVE_DLFCN_H
#include <dlfcn.h>
#endif

#include <stdio.h>

#ifdef RTLD_GLOBAL
#  define LT_DLGLOBAL		RTLD_GLOBAL
#else
#  ifdef DL_GLOBAL
#    define LT_DLGLOBAL		DL_GLOBAL
#  else
#    define LT_DLGLOBAL		0
#  endif
#endif

/* We may have to define LT_DLLAZY_OR_NOW in the command line if we
   find out it does not work in some platform. */
#ifndef LT_DLLAZY_OR_NOW
#  ifdef RTLD_LAZY
#    define LT_DLLAZY_OR_NOW		RTLD_LAZY
#  else
#    ifdef DL_LAZY
#      define LT_DLLAZY_OR_NOW		DL_LAZY
#    else
#      ifdef RTLD_NOW
#        define LT_DLLAZY_OR_NOW	RTLD_NOW
#      else
#        ifdef DL_NOW
#          define LT_DLLAZY_OR_NOW	DL_NOW
#        else
#          define LT_DLLAZY_OR_NOW	0
#        endif
#      endif
#    endif
#  endif
#endif

#ifdef __cplusplus
extern "C" void exit (int);
#endif

void fnord() { int i=42;}
int main ()
{
  void *self = dlopen (0, LT_DLGLOBAL|LT_DLLAZY_OR_NOW);
  int status = $lt_dlunknown;

  if (self)
    {
      if (dlsym (self,"fnord"))       status = $lt_dlno_uscore;
      else if (dlsym( self,"_fnord")) status = $lt_dlneed_uscore;
      /* dlclose (self); */
    }

    exit (status);
}]
EOF
  if AC_TRY_EVAL(ac_link) && test -s conftest${ac_exeext} 2>/dev/null; then
    (./conftest; exit; ) 2>/dev/null
    lt_status=$?
    case x$lt_status in
      x$lt_dlno_uscore) $1 ;;
      x$lt_dlneed_uscore) $2 ;;
      x$lt_unknown|x*) $3 ;;
    esac
  else :
    # compilation failed
    $3
  fi
fi
rm -fr conftest*
])# _LT_AC_TRY_DLOPEN_SELF

# AC_LIBTOOL_DLOPEN_SELF
# -------------------
AC_DEFUN([AC_LIBTOOL_DLOPEN_SELF],
[if test "x$enable_dlopen" != xyes; then
  enable_dlopen=unknown
  enable_dlopen_self=unknown
  enable_dlopen_self_static=unknown
else
  lt_cv_dlopen=no
  lt_cv_dlopen_libs=

  case $host_os in
  beos*)
    lt_cv_dlopen="load_add_on"
    lt_cv_dlopen_libs=
    lt_cv_dlopen_self=yes
    ;;

  cygwin* | mingw* | pw32*)
    lt_cv_dlopen="LoadLibrary"
    lt_cv_dlopen_libs=
   ;;

  *)
    AC_CHECK_FUNC([shl_load],
          [lt_cv_dlopen="shl_load"],
      [AC_CHECK_LIB([dld], [shl_load],
            [lt_cv_dlopen="shl_load" lt_cv_dlopen_libs="-dld"],
	[AC_CHECK_FUNC([dlopen],
	      [lt_cv_dlopen="dlopen"],
	  [AC_CHECK_LIB([dl], [dlopen],
	        [lt_cv_dlopen="dlopen" lt_cv_dlopen_libs="-ldl"],
	    [AC_CHECK_LIB([svld], [dlopen],
	          [lt_cv_dlopen="dlopen" lt_cv_dlopen_libs="-lsvld"],
	      [AC_CHECK_LIB([dld], [dld_link],
	            [lt_cv_dlopen="dld_link" lt_cv_dlopen_libs="-dld"])
	      ])
	    ])
	  ])
	])
      ])
    ;;
  esac

  if test "x$lt_cv_dlopen" != xno; then
    enable_dlopen=yes
  else
    enable_dlopen=no
  fi

  case $lt_cv_dlopen in
  dlopen)
    save_CPPFLAGS="$CPPFLAGS"
    AC_REQUIRE([_LT_AC_CHECK_DLFCN])dnl
    test "x$ac_cv_header_dlfcn_h" = xyes && CPPFLAGS="$CPPFLAGS -DHAVE_DLFCN_H"

    save_LDFLAGS="$LDFLAGS"
    eval LDFLAGS=\"\$LDFLAGS $export_dynamic_flag_spec\"

    save_LIBS="$LIBS"
    LIBS="$lt_cv_dlopen_libs $LIBS"

    AC_CACHE_CHECK([whether a program can dlopen itself],
	  lt_cv_dlopen_self, [dnl
	  _LT_AC_TRY_DLOPEN_SELF(
	    lt_cv_dlopen_self=yes, lt_cv_dlopen_self=yes,
	    lt_cv_dlopen_self=no, lt_cv_dlopen_self=cross)
    ])

    if test "x$lt_cv_dlopen_self" = xyes; then
      LDFLAGS="$LDFLAGS $link_static_flag"
      AC_CACHE_CHECK([whether a statically linked program can dlopen itself],
    	  lt_cv_dlopen_self_static, [dnl
	  _LT_AC_TRY_DLOPEN_SELF(
	    lt_cv_dlopen_self_static=yes, lt_cv_dlopen_self_static=yes,
	    lt_cv_dlopen_self_static=no,  lt_cv_dlopen_self_static=cross)
      ])
    fi

    CPPFLAGS="$save_CPPFLAGS"
    LDFLAGS="$save_LDFLAGS"
    LIBS="$save_LIBS"
    ;;
  esac

  case $lt_cv_dlopen_self in
  yes|no) enable_dlopen_self=$lt_cv_dlopen_self ;;
  *) enable_dlopen_self=unknown ;;
  esac

  case $lt_cv_dlopen_self_static in
  yes|no) enable_dlopen_self_static=$lt_cv_dlopen_self_static ;;
  *) enable_dlopen_self_static=unknown ;;
  esac
fi
])# AC_LIBTOOL_DLOPEN_SELF

AC_DEFUN([_LT_AC_LTCONFIG_HACK],
[AC_REQUIRE([AC_LIBTOOL_SYS_GLOBAL_SYMBOL_PIPE])dnl
# Sed substitution that helps us do robust quoting.  It backslashifies
# metacharacters that are still active within double-quoted strings.
Xsed='sed -e s/^X//'
sed_quote_subst='s/\([[\\"\\`$\\\\]]\)/\\\1/g'

# Same as above, but do not quote variable references.
double_quote_subst='s/\([[\\"\\`\\\\]]\)/\\\1/g'

# Sed substitution to delay expansion of an escaped shell variable in a
# double_quote_subst'ed string.
delay_variable_subst='s/\\\\\\\\\\\$/\\\\\\$/g'

# Constants:
rm="rm -f"

# Global variables:
default_ofile=libtool
can_build_shared=yes

# All known linkers require a `.a' archive for static linking (except M$VC,
# which needs '.lib').
libext=a
ltmain="$ac_aux_dir/ltmain.sh"
ofile="$default_ofile"
with_gnu_ld="$lt_cv_prog_gnu_ld"
need_locks="$enable_libtool_lock"

old_CC="$CC"
old_CFLAGS="$CFLAGS"

# Set sane defaults for various variables
test -z "$AR" && AR=ar
test -z "$AR_FLAGS" && AR_FLAGS=cru
test -z "$AS" && AS=as
test -z "$CC" && CC=cc
test -z "$DLLTOOL" && DLLTOOL=dlltool
test -z "$LD" && LD=ld
test -z "$LN_S" && LN_S="ln -s"
test -z "$MAGIC_CMD" && MAGIC_CMD=file
test -z "$NM" && NM=nm
test -z "$OBJDUMP" && OBJDUMP=objdump
test -z "$RANLIB" && RANLIB=:
test -z "$STRIP" && STRIP=:
test -z "$ac_objext" && ac_objext=o

if test x"$host" != x"$build"; then
  ac_tool_prefix=${host_alias}-
else
  ac_tool_prefix=
fi

# Transform linux* to *-*-linux-gnu*, to support old configure scripts.
case $host_os in
linux-gnu*) ;;
linux*) host=`echo $host | sed 's/^\(.*-.*-linux\)\(.*\)$/\1-gnu\2/'`
esac

case $host_os in
aix3*)
  # AIX sometimes has problems with the GCC collect2 program.  For some
  # reason, if we set the COLLECT_NAMES environment variable, the problems
  # vanish in a puff of smoke.
  if test "X${COLLECT_NAMES+set}" != Xset; then
    COLLECT_NAMES=
    export COLLECT_NAMES
  fi
  ;;
esac

# Determine commands to create old-style static archives.
old_archive_cmds='$AR $AR_FLAGS $oldlib$oldobjs$old_deplibs'
old_postinstall_cmds='chmod 644 $oldlib'
old_postuninstall_cmds=

if test -n "$RANLIB"; then
  case $host_os in
  openbsd*)
    old_postinstall_cmds="\$RANLIB -t \$oldlib~$old_postinstall_cmds"
    ;;
  *)
    old_postinstall_cmds="\$RANLIB \$oldlib~$old_postinstall_cmds"
    ;;
  esac
  old_archive_cmds="$old_archive_cmds~\$RANLIB \$oldlib"
fi

# Allow CC to be a program name with arguments.
set dummy $CC
compiler="[$]2"

AC_MSG_CHECKING([for objdir])
rm -f .libs 2>/dev/null
mkdir .libs 2>/dev/null
if test -d .libs; then
  objdir=.libs
else
  # MS-DOS does not allow filenames that begin with a dot.
  objdir=_libs
fi
rmdir .libs 2>/dev/null
AC_MSG_RESULT($objdir)


AC_ARG_WITH(pic,
[  --with-pic              try to use only PIC/non-PIC objects [default=use both]],
pic_mode="$withval", pic_mode=default)
test -z "$pic_mode" && pic_mode=default

# We assume here that the value for lt_cv_prog_cc_pic will not be cached
# in isolation, and that seeing it set (from the cache) indicates that
# the associated values are set (in the cache) correctly too.
AC_MSG_CHECKING([for $compiler option to produce PIC])
AC_CACHE_VAL(lt_cv_prog_cc_pic,
[ lt_cv_prog_cc_pic=
  lt_cv_prog_cc_shlib=
  lt_cv_prog_cc_wl=
  lt_cv_prog_cc_static=
  lt_cv_prog_cc_no_builtin=
  lt_cv_prog_cc_can_build_shared=$can_build_shared

  if test "$GCC" = yes; then
    lt_cv_prog_cc_wl='-Wl,'
    lt_cv_prog_cc_static='-static'

    case $host_os in
    aix*)
      # Below there is a dirty hack to force normal static linking with -ldl
      # The problem is because libdl dynamically linked with both libc and
      # libC (AIX C++ library), which obviously doesn't included in libraries
      # list by gcc. This cause undefined symbols with -static flags.
      # This hack allows C programs to be linked with "-static -ldl", but
      # not sure about C++ programs.
      lt_cv_prog_cc_static="$lt_cv_prog_cc_static ${lt_cv_prog_cc_wl}-lC"
      ;;
    amigaos*)
      # FIXME: we need at least 68020 code to build shared libraries, but
      # adding the `-m68020' flag to GCC prevents building anything better,
      # like `-m68040'.
      lt_cv_prog_cc_pic='-m68020 -resident32 -malways-restore-a4'
      ;;
    beos* | irix5* | irix6* | osf3* | osf4* | osf5*)
      # PIC is the default for these OSes.
      ;;
    darwin* | rhapsody*)
      # PIC is the default on this platform
      # Common symbols not allowed in MH_DYLIB files
      lt_cv_prog_cc_pic='-fno-common'
      ;;
    cygwin* | mingw* | pw32* | os2*)
      # This hack is so that the source file can tell whether it is being
      # built for inclusion in a dll (and should export symbols for example).
      lt_cv_prog_cc_pic='-DDLL_EXPORT'
      ;;
    sysv4*MP*)
      if test -d /usr/nec; then
	 lt_cv_prog_cc_pic=-Kconform_pic
      fi
      ;;
    *)
      lt_cv_prog_cc_pic='-fPIC'
      ;;
    esac
  else
    # PORTME Check for PIC flags for the system compiler.
    case $host_os in
    aix3* | aix4* | aix5*)
      lt_cv_prog_cc_wl='-Wl,'
      # All AIX code is PIC.
      if test "$host_cpu" = ia64; then
	# AIX 5 now supports IA64 processor
	lt_cv_prog_cc_static='-Bstatic'
      else
	lt_cv_prog_cc_static='-bnso -bI:/lib/syscalls.exp'
      fi
      ;;

    hpux9* | hpux10* | hpux11*)
      # Is there a better lt_cv_prog_cc_static that works with the bundled CC?
      lt_cv_prog_cc_wl='-Wl,'
      lt_cv_prog_cc_static="${lt_cv_prog_cc_wl}-a ${lt_cv_prog_cc_wl}archive"
      lt_cv_prog_cc_pic='+Z'
      ;;

    irix5* | irix6*)
      lt_cv_prog_cc_wl='-Wl,'
      lt_cv_prog_cc_static='-non_shared'
      # PIC (with -KPIC) is the default.
      ;;

    cygwin* | mingw* | pw32* | os2*)
      # This hack is so that the source file can tell whether it is being
      # built for inclusion in a dll (and should export symbols for example).
      lt_cv_prog_cc_pic='-DDLL_EXPORT'
      ;;

    newsos6)
      lt_cv_prog_cc_pic='-KPIC'
      lt_cv_prog_cc_static='-Bstatic'
      ;;

    osf3* | osf4* | osf5*)
      # All OSF/1 code is PIC.
      lt_cv_prog_cc_wl='-Wl,'
      lt_cv_prog_cc_static='-non_shared'
      ;;

    sco3.2v5*)
      lt_cv_prog_cc_pic='-Kpic'
      lt_cv_prog_cc_static='-dn'
      lt_cv_prog_cc_shlib='-belf'
      ;;

    solaris*)
      lt_cv_prog_cc_pic='-KPIC'
      lt_cv_prog_cc_static='-Bstatic'
      lt_cv_prog_cc_wl='-Wl,'
      ;;

    sunos4*)
      lt_cv_prog_cc_pic='-PIC'
      lt_cv_prog_cc_static='-Bstatic'
      lt_cv_prog_cc_wl='-Qoption ld '
      ;;

    sysv4 | sysv4.2uw2* | sysv4.3* | sysv5* | sysv5Open*)
      lt_cv_prog_cc_pic='-KPIC'
      lt_cv_prog_cc_static='-Bstatic'
      if test "x$host_vendor" = xsni; then
	lt_cv_prog_cc_wl='-LD'
      else
	lt_cv_prog_cc_wl='-Wl,'
      fi
      ;;

    uts4*)
      lt_cv_prog_cc_pic='-pic'
      lt_cv_prog_cc_static='-Bstatic'
      ;;

    sysv4*MP*)
      if test -d /usr/nec ;then
	lt_cv_prog_cc_pic='-Kconform_pic'
	lt_cv_prog_cc_static='-Bstatic'
      fi
      ;;

    *)
      lt_cv_prog_cc_can_build_shared=no
      ;;
    esac
  fi
])
if test -z "$lt_cv_prog_cc_pic"; then
  AC_MSG_RESULT([none])
else
  AC_MSG_RESULT([$lt_cv_prog_cc_pic])

  # Check to make sure the pic_flag actually works.
  AC_MSG_CHECKING([if $compiler PIC flag $lt_cv_prog_cc_pic works])
  AC_CACHE_VAL(lt_cv_prog_cc_pic_works, [dnl
    save_CFLAGS="$CFLAGS"
    CFLAGS="$CFLAGS $lt_cv_prog_cc_pic -DPIC"
    AC_TRY_COMPILE([], [], [dnl
      case $host_os in
      hpux9* | hpux10* | hpux11*)
	# On HP-UX, both CC and GCC only warn that PIC is supported... then
	# they create non-PIC objects.  So, if there were any warnings, we
	# assume that PIC is not supported.
	if test -s conftest.err; then
	  lt_cv_prog_cc_pic_works=no
	else
	  lt_cv_prog_cc_pic_works=yes
	fi
	;;
      *)
	lt_cv_prog_cc_pic_works=yes
	;;
      esac
    ], [dnl
      lt_cv_prog_cc_pic_works=no
    ])
    CFLAGS="$save_CFLAGS"
  ])

  if test "X$lt_cv_prog_cc_pic_works" = Xno; then
    lt_cv_prog_cc_pic=
    lt_cv_prog_cc_can_build_shared=no
  else
    lt_cv_prog_cc_pic=" $lt_cv_prog_cc_pic"
  fi

  AC_MSG_RESULT([$lt_cv_prog_cc_pic_works])
fi

# Check for any special shared library compilation flags.
if test -n "$lt_cv_prog_cc_shlib"; then
  AC_MSG_WARN([\`$CC' requires \`$lt_cv_prog_cc_shlib' to build shared libraries])
  if echo "$old_CC $old_CFLAGS " | egrep -e "[[ 	]]$lt_cv_prog_cc_shlib[[ 	]]" >/dev/null; then :
  else
   AC_MSG_WARN([add \`$lt_cv_prog_cc_shlib' to the CC or CFLAGS env variable and reconfigure])
    lt_cv_prog_cc_can_build_shared=no
  fi
fi

AC_MSG_CHECKING([if $compiler static flag $lt_cv_prog_cc_static works])
AC_CACHE_VAL([lt_cv_prog_cc_static_works], [dnl
  lt_cv_prog_cc_static_works=no
  save_LDFLAGS="$LDFLAGS"
  LDFLAGS="$LDFLAGS $lt_cv_prog_cc_static"
  AC_TRY_LINK([], [], [lt_cv_prog_cc_static_works=yes])
  LDFLAGS="$save_LDFLAGS"
])

# Belt *and* braces to stop my trousers falling down:
test "X$lt_cv_prog_cc_static_works" = Xno && lt_cv_prog_cc_static=
AC_MSG_RESULT([$lt_cv_prog_cc_static_works])

pic_flag="$lt_cv_prog_cc_pic"
special_shlib_compile_flags="$lt_cv_prog_cc_shlib"
wl="$lt_cv_prog_cc_wl"
link_static_flag="$lt_cv_prog_cc_static"
no_builtin_flag="$lt_cv_prog_cc_no_builtin"
can_build_shared="$lt_cv_prog_cc_can_build_shared"


# Check to see if options -o and -c are simultaneously supported by compiler
AC_MSG_CHECKING([if $compiler supports -c -o file.$ac_objext])
AC_CACHE_VAL([lt_cv_compiler_c_o], [
$rm -r conftest 2>/dev/null
mkdir conftest
cd conftest
echo "int some_variable = 0;" > conftest.$ac_ext
mkdir out
# According to Tom Tromey, Ian Lance Taylor reported there are C compilers
# that will create temporary files in the current directory regardless of
# the output directory.  Thus, making CWD read-only will cause this test
# to fail, enabling locking or at least warning the user not to do parallel
# builds.
chmod -w .
save_CFLAGS="$CFLAGS"
CFLAGS="$CFLAGS -o out/conftest2.$ac_objext"
compiler_c_o=no
if { (eval echo configure:__oline__: \"$ac_compile\") 1>&5; (eval $ac_compile) 2>out/conftest.err; } && test -s out/conftest2.$ac_objext; then
  # The compiler can only warn and ignore the option if not recognized
  # So say no if there are warnings
  if test -s out/conftest.err; then
    lt_cv_compiler_c_o=no
  else
    lt_cv_compiler_c_o=yes
  fi
else
  # Append any errors to the config.log.
  cat out/conftest.err 1>&AC_FD_CC
  lt_cv_compiler_c_o=no
fi
CFLAGS="$save_CFLAGS"
chmod u+w .
$rm conftest* out/*
rmdir out
cd ..
rmdir conftest
$rm -r conftest 2>/dev/null
])
compiler_c_o=$lt_cv_compiler_c_o
AC_MSG_RESULT([$compiler_c_o])

if test x"$compiler_c_o" = x"yes"; then
  # Check to see if we can write to a .lo
  AC_MSG_CHECKING([if $compiler supports -c -o file.lo])
  AC_CACHE_VAL([lt_cv_compiler_o_lo], [
  lt_cv_compiler_o_lo=no
  save_CFLAGS="$CFLAGS"
  CFLAGS="$CFLAGS -c -o conftest.lo"
  save_objext="$ac_objext"
  ac_objext=lo
  AC_TRY_COMPILE([], [int some_variable = 0;], [dnl
    # The compiler can only warn and ignore the option if not recognized
    # So say no if there are warnings
    if test -s conftest.err; then
      lt_cv_compiler_o_lo=no
    else
      lt_cv_compiler_o_lo=yes
    fi
  ])
  ac_objext="$save_objext"
  CFLAGS="$save_CFLAGS"
  ])
  compiler_o_lo=$lt_cv_compiler_o_lo
  AC_MSG_RESULT([$compiler_o_lo])
else
  compiler_o_lo=no
fi

# Check to see if we can do hard links to lock some files if needed
hard_links="nottested"
if test "$compiler_c_o" = no && test "$need_locks" != no; then
  # do not overwrite the value of need_locks provided by the user
  AC_MSG_CHECKING([if we can lock with hard links])
  hard_links=yes
  $rm conftest*
  ln conftest.a conftest.b 2>/dev/null && hard_links=no
  touch conftest.a
  ln conftest.a conftest.b 2>&5 || hard_links=no
  ln conftest.a conftest.b 2>/dev/null && hard_links=no
  AC_MSG_RESULT([$hard_links])
  if test "$hard_links" = no; then
    AC_MSG_WARN([\`$CC' does not support \`-c -o', so \`make -j' may be unsafe])
    need_locks=warn
  fi
else
  need_locks=no
fi

if test "$GCC" = yes; then
  # Check to see if options -fno-rtti -fno-exceptions are supported by compiler
  AC_MSG_CHECKING([if $compiler supports -fno-rtti -fno-exceptions])
  echo "int some_variable = 0;" > conftest.$ac_ext
  save_CFLAGS="$CFLAGS"
  CFLAGS="$CFLAGS -fno-rtti -fno-exceptions -c conftest.$ac_ext"
  compiler_rtti_exceptions=no
  AC_TRY_COMPILE([], [int some_variable = 0;], [dnl
    # The compiler can only warn and ignore the option if not recognized
    # So say no if there are warnings
    if test -s conftest.err; then
      compiler_rtti_exceptions=no
    else
      compiler_rtti_exceptions=yes
    fi
  ])
  CFLAGS="$save_CFLAGS"
  AC_MSG_RESULT([$compiler_rtti_exceptions])

  if test "$compiler_rtti_exceptions" = "yes"; then
    no_builtin_flag=' -fno-builtin -fno-rtti -fno-exceptions'
  else
    no_builtin_flag=' -fno-builtin'
  fi
fi

# See if the linker supports building shared libraries.
AC_MSG_CHECKING([whether the linker ($LD) supports shared libraries])

allow_undefined_flag=
no_undefined_flag=
need_lib_prefix=unknown
need_version=unknown
# when you set need_version to no, make sure it does not cause -set_version
# flags to be left without arguments
archive_cmds=
archive_expsym_cmds=
old_archive_from_new_cmds=
old_archive_from_expsyms_cmds=
export_dynamic_flag_spec=
whole_archive_flag_spec=
thread_safe_flag_spec=
hardcode_into_libs=no
hardcode_libdir_flag_spec=
hardcode_libdir_separator=
hardcode_direct=no
hardcode_minus_L=no
hardcode_shlibpath_var=unsupported
runpath_var=
link_all_deplibs=unknown
always_export_symbols=no
export_symbols_cmds='$NM $libobjs $convenience | $global_symbol_pipe | sed '\''s/.* //'\'' | sort | uniq > $export_symbols'
# include_expsyms should be a list of space-separated symbols to be *always*
# included in the symbol list
include_expsyms=
# exclude_expsyms can be an egrep regular expression of symbols to exclude
# it will be wrapped by ` (' and `)$', so one must not match beginning or
# end of line.  Example: `a|bc|.*d.*' will exclude the symbols `a' and `bc',
# as well as any symbol that contains `d'.
exclude_expsyms="_GLOBAL_OFFSET_TABLE_"
# Although _GLOBAL_OFFSET_TABLE_ is a valid symbol C name, most a.out
# platforms (ab)use it in PIC code, but their linkers get confused if
# the symbol is explicitly referenced.  Since portable code cannot
# rely on this symbol name, it's probably fine to never include it in
# preloaded symbol tables.
extract_expsyms_cmds=

case $host_os in
cygwin* | mingw* | pw32*)
  # FIXME: the MSVC++ port hasn't been tested in a loooong time
  # When not using gcc, we currently assume that we are using
  # Microsoft Visual C++.
  if test "$GCC" != yes; then
    with_gnu_ld=no
  fi
  ;;
openbsd*)
  with_gnu_ld=no
  ;;
esac

ld_shlibs=yes
if test "$with_gnu_ld" = yes; then
  # If archive_cmds runs LD, not CC, wlarc should be empty
  wlarc='${wl}'

  # See if GNU ld supports shared libraries.
  case $host_os in
  aix3* | aix4* | aix5*)
    # On AIX, the GNU linker is very broken
    # Note:Check GNU linker on AIX 5-IA64 when/if it becomes available.
    ld_shlibs=no
    cat <<EOF 1>&2

*** Warning: the GNU linker, at least up to release 2.9.1, is reported
*** to be unable to reliably create shared libraries on AIX.
*** Therefore, libtool is disabling shared libraries support.  If you
*** really care for shared libraries, you may want to modify your PATH
*** so that a non-GNU linker is found, and then restart.

EOF
    ;;

  amigaos*)
    archive_cmds='$rm $output_objdir/a2ixlibrary.data~$echo "#define NAME $libname" > $output_objdir/a2ixlibrary.data~$echo "#define LIBRARY_ID 1" >> $output_objdir/a2ixlibrary.data~$echo "#define VERSION $major" >> $output_objdir/a2ixlibrary.data~$echo "#define REVISION $revision" >> $output_objdir/a2ixlibrary.data~$AR $AR_FLAGS $lib $libobjs~$RANLIB $lib~(cd $output_objdir && a2ixlibrary -32)'
    hardcode_libdir_flag_spec='-L$libdir'
    hardcode_minus_L=yes

    # Samuel A. Falvo II <kc5tja@dolphin.openprojects.net> reports
    # that the semantics of dynamic libraries on AmigaOS, at least up
    # to version 4, is to share data among multiple programs linked
    # with the same dynamic library.  Since this doesn't match the
    # behavior of shared libraries on other platforms, we can use
    # them.
    ld_shlibs=no
    ;;

  beos*)
    if $LD --help 2>&1 | egrep ': supported targets:.* elf' > /dev/null; then
      allow_undefined_flag=unsupported
      # Joseph Beckenbach <jrb3@best.com> says some releases of gcc
      # support --undefined.  This deserves some investigation.  FIXME
      archive_cmds='$CC -nostart $libobjs $deplibs $compiler_flags ${wl}-soname $wl$soname -o $lib'
    else
      ld_shlibs=no
    fi
    ;;

  cygwin* | mingw* | pw32*)
    # hardcode_libdir_flag_spec is actually meaningless, as there is
    # no search path for DLLs.
    hardcode_libdir_flag_spec='-L$libdir'
    allow_undefined_flag=unsupported
    always_export_symbols=yes

    extract_expsyms_cmds='test -f $output_objdir/impgen.c || \
      sed -e "/^# \/\* impgen\.c starts here \*\//,/^# \/\* impgen.c ends here \*\// { s/^# //;s/^# *$//; p; }" -e d < $''0 > $output_objdir/impgen.c~
      test -f $output_objdir/impgen.exe || (cd $output_objdir && \
      if test "x$HOST_CC" != "x" ; then $HOST_CC -o impgen impgen.c ; \
      else $CC -o impgen impgen.c ; fi)~
      $output_objdir/impgen $dir/$soroot > $output_objdir/$soname-def'

    old_archive_from_expsyms_cmds='$DLLTOOL --as=$AS --dllname $soname --def $output_objdir/$soname-def --output-lib $output_objdir/$newlib'

    # cygwin and mingw dlls have different entry points and sets of symbols
    # to exclude.
    # FIXME: what about values for MSVC?
    dll_entry=__cygwin_dll_entry@12
    dll_exclude_symbols=DllMain@12,_cygwin_dll_entry@12,_cygwin_noncygwin_dll_entry@12~
    case $host_os in
    mingw*)
      # mingw values
      dll_entry=_DllMainCRTStartup@12
      dll_exclude_symbols=DllMain@12,DllMainCRTStartup@12,DllEntryPoint@12~
      ;;
    esac

    # mingw and cygwin differ, and it's simplest to just exclude the union
    # of the two symbol sets.
    dll_exclude_symbols=DllMain@12,_cygwin_dll_entry@12,_cygwin_noncygwin_dll_entry@12,DllMainCRTStartup@12,DllEntryPoint@12

    # recent cygwin and mingw systems supply a stub DllMain which the user
    # can override, but on older systems we have to supply one (in ltdll.c)
    if test "x$lt_cv_need_dllmain" = "xyes"; then
      ltdll_obj='$output_objdir/$soname-ltdll.'"$ac_objext "
      ltdll_cmds='test -f $output_objdir/$soname-ltdll.c || sed -e "/^# \/\* ltdll\.c starts here \*\//,/^# \/\* ltdll.c ends here \*\// { s/^# //; p; }" -e d < $''0 > $output_objdir/$soname-ltdll.c~
	test -f $output_objdir/$soname-ltdll.$ac_objext || (cd $output_objdir && $CC -c $soname-ltdll.c)~'
    else
      ltdll_obj=
      ltdll_cmds=
    fi

    # Extract the symbol export list from an `--export-all' def file,
    # then regenerate the def file from the symbol export list, so that
    # the compiled dll only exports the symbol export list.
    # Be careful not to strip the DATA tag left be newer dlltools.
    export_symbols_cmds="$ltdll_cmds"'
      $DLLTOOL --export-all --exclude-symbols '$dll_exclude_symbols' --output-def $output_objdir/$soname-def '$ltdll_obj'$libobjs $convenience~
      sed -e "1,/EXPORTS/d" -e "s/ @ [[0-9]]*//" -e "s/ *;.*$//" < $output_objdir/$soname-def > $export_symbols'

    # If the export-symbols file already is a .def file (1st line
    # is EXPORTS), use it as is.
    # If DATA tags from a recent dlltool are present, honour them!
    archive_expsym_cmds='if test "x`head -1 $export_symbols`" = xEXPORTS; then
	cp $export_symbols $output_objdir/$soname-def;
      else
	echo EXPORTS > $output_objdir/$soname-def;
	_lt_hint=1;
	cat $export_symbols | while read symbol; do
	 set dummy \$symbol;
	 case \[$]# in
	   2) echo "   \[$]2 @ \$_lt_hint ; " >> $output_objdir/$soname-def;;
	   *) echo "     \[$]2 @ \$_lt_hint \[$]3 ; " >> $output_objdir/$soname-def;;
	 esac;
	 _lt_hint=`expr 1 + \$_lt_hint`;
	done;
      fi~
      '"$ltdll_cmds"'
      $CC -Wl,--base-file,$output_objdir/$soname-base '$lt_cv_cc_dll_switch' -Wl,-e,'$dll_entry' -o $output_objdir/$soname '$ltdll_obj'$libobjs $deplibs $compiler_flags~
      $DLLTOOL --as=$AS --dllname $soname --exclude-symbols '$dll_exclude_symbols' --def $output_objdir/$soname-def --base-file $output_objdir/$soname-base --output-exp $output_objdir/$soname-exp~
      $CC -Wl,--base-file,$output_objdir/$soname-base $output_objdir/$soname-exp '$lt_cv_cc_dll_switch' -Wl,-e,'$dll_entry' -o $output_objdir/$soname '$ltdll_obj'$libobjs $deplibs $compiler_flags~
      $DLLTOOL --as=$AS --dllname $soname --exclude-symbols '$dll_exclude_symbols' --def $output_objdir/$soname-def --base-file $output_objdir/$soname-base --output-exp $output_objdir/$soname-exp --output-lib $output_objdir/$libname.dll.a~
      $CC $output_objdir/$soname-exp '$lt_cv_cc_dll_switch' -Wl,-e,'$dll_entry' -o $output_objdir/$soname '$ltdll_obj'$libobjs $deplibs $compiler_flags'
    ;;

  netbsd*)
    if echo __ELF__ | $CC -E - | grep __ELF__ >/dev/null; then
      archive_cmds='$LD -Bshareable $libobjs $deplibs $linker_flags -o $lib'
      wlarc=
    else
      archive_cmds='$CC -shared -nodefaultlibs $libobjs $deplibs $compiler_flags ${wl}-soname $wl$soname -o $lib'
      archive_expsym_cmds='$CC -shared -nodefaultlibs $libobjs $deplibs $compiler_flags ${wl}-soname $wl$soname ${wl}-retain-symbols-file $wl$export_symbols -o $lib'
    fi
    ;;

  solaris* | sysv5*)
    if $LD -v 2>&1 | egrep 'BFD 2\.8' > /dev/null; then
      ld_shlibs=no
      cat <<EOF 1>&2

*** Warning: The releases 2.8.* of the GNU linker cannot reliably
*** create shared libraries on Solaris systems.  Therefore, libtool
*** is disabling shared libraries support.  We urge you to upgrade GNU
*** binutils to release 2.9.1 or newer.  Another option is to modify
*** your PATH or compiler configuration so that the native linker is
*** used, and then restart.

EOF
    elif $LD --help 2>&1 | egrep ': supported targets:.* elf' > /dev/null; then
      archive_cmds='$CC -shared $libobjs $deplibs $compiler_flags ${wl}-soname $wl$soname -o $lib'
      archive_expsym_cmds='$CC -shared $libobjs $deplibs $compiler_flags ${wl}-soname $wl$soname ${wl}-retain-symbols-file $wl$export_symbols -o $lib'
    else
      ld_shlibs=no
    fi
    ;;

  sunos4*)
    archive_cmds='$LD -assert pure-text -Bshareable -o $lib $libobjs $deplibs $linker_flags'
    wlarc=
    hardcode_direct=yes
    hardcode_shlibpath_var=no
    ;;

  *)
    if $LD --help 2>&1 | egrep ': supported targets:.* elf' > /dev/null; then
      archive_cmds='$CC -shared $libobjs $deplibs $compiler_flags ${wl}-soname $wl$soname -o $lib'
      archive_expsym_cmds='$CC -shared $libobjs $deplibs $compiler_flags ${wl}-soname $wl$soname ${wl}-retain-symbols-file $wl$export_symbols -o $lib'
    else
      ld_shlibs=no
    fi
    ;;
  esac

  if test "$ld_shlibs" = yes; then
    runpath_var=LD_RUN_PATH
    hardcode_libdir_flag_spec='${wl}--rpath ${wl}$libdir'
    export_dynamic_flag_spec='${wl}--export-dynamic'
    case $host_os in
    cygwin* | mingw* | pw32*)
      # dlltool doesn't understand --whole-archive et. al.
      whole_archive_flag_spec=
      ;;
    *)
      # ancient GNU ld didn't support --whole-archive et. al.
      if $LD --help 2>&1 | egrep 'no-whole-archive' > /dev/null; then
	whole_archive_flag_spec="$wlarc"'--whole-archive$convenience '"$wlarc"'--no-whole-archive'
      else
	whole_archive_flag_spec=
      fi
      ;;
    esac
  fi
else
  # PORTME fill in a description of your system's linker (not GNU ld)
  case $host_os in
  aix3*)
    allow_undefined_flag=unsupported
    always_export_symbols=yes
    archive_expsym_cmds='$LD -o $output_objdir/$soname $libobjs $deplibs $linker_flags -bE:$export_symbols -T512 -H512 -bM:SRE~$AR $AR_FLAGS $lib $output_objdir/$soname'
    # Note: this linker hardcodes the directories in LIBPATH if there
    # are no directories specified by -L.
    hardcode_minus_L=yes
    if test "$GCC" = yes && test -z "$link_static_flag"; then
      # Neither direct hardcoding nor static linking is supported with a
      # broken collect2.
      hardcode_direct=unsupported
    fi
    ;;

  aix4* | aix5*)
    if test "$host_cpu" = ia64; then
      # On IA64, the linker does run time linking by default, so we don't
      # have to do anything special.
      aix_use_runtimelinking=no
      exp_sym_flag='-Bexport'
      no_entry_flag=""
    else
      aix_use_runtimelinking=no

      # Test if we are trying to use run time linking or normal
      # AIX style linking. If -brtl is somewhere in LDFLAGS, we
      # need to do runtime linking.
      case $host_os in aix4.[[23]]|aix4.[[23]].*|aix5*)
	for ld_flag in $LDFLAGS; do
	  if (test $ld_flag = "-brtl" || test $ld_flag = "-Wl,-brtl"); then
	    aix_use_runtimelinking=yes
	    break
	  fi
	done
      esac

      exp_sym_flag='-bexport'
      no_entry_flag='-bnoentry'
    fi

    # When large executables or shared objects are built, AIX ld can
    # have problems creating the table of contents.  If linking a library
    # or program results in "error TOC overflow" add -mminimal-toc to
    # CXXFLAGS/CFLAGS for g++/gcc.  In the cases where that is not
    # enough to fix the problem, add -Wl,-bbigtoc to LDFLAGS.

    hardcode_direct=yes
    archive_cmds=''
    hardcode_libdir_separator=':'
    if test "$GCC" = yes; then
      case $host_os in aix4.[[012]]|aix4.[[012]].*)
	collect2name=`${CC} -print-prog-name=collect2`
	if test -f "$collect2name" && \
	  strings "$collect2name" | grep resolve_lib_name >/dev/null
	then
	  # We have reworked collect2
	  hardcode_direct=yes
	else
	  # We have old collect2
	  hardcode_direct=unsupported
	  # It fails to find uninstalled libraries when the uninstalled
	  # path is not listed in the libpath.  Setting hardcode_minus_L
	  # to unsupported forces relinking
	  hardcode_minus_L=yes
	  hardcode_libdir_flag_spec='-L$libdir'
	  hardcode_libdir_separator=
	fi
      esac

      shared_flag='-shared'
    else
      # not using gcc
      if test "$host_cpu" = ia64; then
	shared_flag='${wl}-G'
      else
	if test "$aix_use_runtimelinking" = yes; then
	  shared_flag='${wl}-G'
	else
	  shared_flag='${wl}-bM:SRE'
	fi
      fi
    fi

    # It seems that -bexpall can do strange things, so it is better to
    # generate a list of symbols to export.
    always_export_symbols=yes
    if test "$aix_use_runtimelinking" = yes; then
      # Warning - without using the other runtime loading flags (-brtl),
      # -berok will link without error, but may produce a broken library.
      allow_undefined_flag='-berok'
      hardcode_libdir_flag_spec='${wl}-blibpath:$libdir:/usr/lib:/lib'
      archive_expsym_cmds="\$CC"' -o $output_objdir/$soname $libobjs $sys_deplibs $compiler_flags `if test "x${allow_undefined_flag}" != "x"; then echo "${wl}${allow_undefined_flag}"; else :; fi` '"\${wl}$no_entry_flag \${wl}$exp_sym_flag:\$export_symbols $shared_flag"
    else
      if test "$host_cpu" = ia64; then
	hardcode_libdir_flag_spec='${wl}-R $libdir:/usr/lib:/lib'
	allow_undefined_flag="-z nodefs"
	archive_expsym_cmds="\$CC $shared_flag"' -o $output_objdir/$soname ${wl}-h$soname $libobjs $sys_deplibs $compiler_flags ${wl}${allow_undefined_flag} '"\${wl}$no_entry_flag \${wl}$exp_sym_flag:\$export_symbols"
      else
	hardcode_libdir_flag_spec='${wl}-bnolibpath ${wl}-blibpath:$libdir:/usr/lib:/lib'
	# Warning - without using the other run time loading flags,
	# -berok will link without error, but may produce a broken library.
	allow_undefined_flag='${wl}-berok'
	# This is a bit strange, but is similar to how AIX traditionally builds
	# it's shared libraries.
	archive_expsym_cmds="\$CC $shared_flag"' -o $output_objdir/$soname $libobjs $sys_deplibs $compiler_flags ${allow_undefined_flag} '"\${wl}$no_entry_flag \${wl}$exp_sym_flag:\$export_symbols"' ~$AR -crlo $objdir/$libname$release.a $objdir/$soname'
      fi
    fi
    ;;

  amigaos*)
    archive_cmds='$rm $output_objdir/a2ixlibrary.data~$echo "#define NAME $libname" > $output_objdir/a2ixlibrary.data~$echo "#define LIBRARY_ID 1" >> $output_objdir/a2ixlibrary.data~$echo "#define VERSION $major" >> $output_objdir/a2ixlibrary.data~$echo "#define REVISION $revision" >> $output_objdir/a2ixlibrary.data~$AR $AR_FLAGS $lib $libobjs~$RANLIB $lib~(cd $output_objdir && a2ixlibrary -32)'
    hardcode_libdir_flag_spec='-L$libdir'
    hardcode_minus_L=yes
    # see comment about different semantics on the GNU ld section
    ld_shlibs=no
    ;;

  cygwin* | mingw* | pw32*)
    # When not using gcc, we currently assume that we are using
    # Microsoft Visual C++.
    # hardcode_libdir_flag_spec is actually meaningless, as there is
    # no search path for DLLs.
    hardcode_libdir_flag_spec=' '
    allow_undefined_flag=unsupported
    # Tell ltmain to make .lib files, not .a files.
    libext=lib
    # FIXME: Setting linknames here is a bad hack.
    archive_cmds='$CC -o $lib $libobjs $compiler_flags `echo "$deplibs" | sed -e '\''s/ -lc$//'\''` -link -dll~linknames='
    # The linker will automatically build a .lib file if we build a DLL.
    old_archive_from_new_cmds='true'
    # FIXME: Should let the user specify the lib program.
    old_archive_cmds='lib /OUT:$oldlib$oldobjs$old_deplibs'
    fix_srcfile_path='`cygpath -w "$srcfile"`'
    ;;

  darwin* | rhapsody*)
    case "$host_os" in
    rhapsody* | darwin1.[[012]])
      allow_undefined_flag='-undefined suppress'
      ;;
    *) # Darwin 1.3 on
      allow_undefined_flag='-flat_namespace -undefined suppress'
      ;;
    esac
    # FIXME: Relying on posixy $() will cause problems for
    #        cross-compilation, but unfortunately the echo tests do not
    #        yet detect zsh echo's removal of \ escapes.
    archive_cmds='$nonopt $(test "x$module" = xyes && echo -bundle || echo -dynamiclib) $allow_undefined_flag -o $lib $libobjs $deplibs$linker_flags -install_name $rpath/$soname $verstring'
    # We need to add '_' to the symbols in $export_symbols first
    #archive_expsym_cmds="$archive_cmds"' && strip -s $export_symbols'
    hardcode_direct=yes
    hardcode_shlibpath_var=no
    whole_archive_flag_spec='-all_load $convenience'
    ;;

  freebsd1*)
    ld_shlibs=no
    ;;

  # FreeBSD 2.2.[012] allows us to include c++rt0.o to get C++ constructor
  # support.  Future versions do this automatically, but an explicit c++rt0.o
  # does not break anything, and helps significantly (at the cost of a little
  # extra space).
  freebsd2.2*)
    archive_cmds='$LD -Bshareable -o $lib $libobjs $deplibs $linker_flags /usr/lib/c++rt0.o'
    hardcode_libdir_flag_spec='-R$libdir'
    hardcode_direct=yes
    hardcode_shlibpath_var=no
    ;;

  # Unfortunately, older versions of FreeBSD 2 do not have this feature.
  freebsd2*)
    archive_cmds='$LD -Bshareable -o $lib $libobjs $deplibs $linker_flags'
    hardcode_direct=yes
    hardcode_minus_L=yes
    hardcode_shlibpath_var=no
    ;;

  # FreeBSD 3 and greater uses gcc -shared to do shared libraries.
  freebsd*)
    archive_cmds='$CC -shared -o $lib $libobjs $deplibs $compiler_flags'
    hardcode_libdir_flag_spec='-R$libdir'
    hardcode_direct=yes
    hardcode_shlibpath_var=no
    ;;

  hpux9* | hpux10* | hpux11*)
    case $host_os in
    hpux9*) archive_cmds='$rm $output_objdir/$soname~$LD -b +b $install_libdir -o $output_objdir/$soname $libobjs $deplibs $linker_flags~test $output_objdir/$soname = $lib || mv $output_objdir/$soname $lib' ;;
    *) archive_cmds='$LD -b +h $soname +b $install_libdir -o $lib $libobjs $deplibs $linker_flags' ;;
    esac
    hardcode_libdir_flag_spec='${wl}+b ${wl}$libdir'
    hardcode_libdir_separator=:
    hardcode_direct=yes
    hardcode_minus_L=yes # Not in the search PATH, but as the default
			 # location of the library.
    export_dynamic_flag_spec='${wl}-E'
    ;;

  irix5* | irix6*)
    if test "$GCC" = yes; then
      archive_cmds='$CC -shared $libobjs $deplibs $compiler_flags ${wl}-soname ${wl}$soname `test -n "$verstring" && echo ${wl}-set_version ${wl}$verstring` ${wl}-update_registry ${wl}${output_objdir}/so_locations -o $lib'
    else
      archive_cmds='$LD -shared $libobjs $deplibs $linker_flags -soname $soname `test -n "$verstring" && echo -set_version $verstring` -update_registry ${output_objdir}/so_locations -o $lib'
    fi
    hardcode_libdir_flag_spec='${wl}-rpath ${wl}$libdir'
    hardcode_libdir_separator=:
    link_all_deplibs=yes
    ;;

  netbsd*)
    if echo __ELF__ | $CC -E - | grep __ELF__ >/dev/null; then
      archive_cmds='$LD -Bshareable -o $lib $libobjs $deplibs $linker_flags'  # a.out
    else
      archive_cmds='$LD -shared -o $lib $libobjs $deplibs $linker_flags'      # ELF
    fi
    hardcode_libdir_flag_spec='-R$libdir'
    hardcode_direct=yes
    hardcode_shlibpath_var=no
    ;;

  newsos6)
    archive_cmds='$LD -G -h $soname -o $lib $libobjs $deplibs $linker_flags'
    hardcode_direct=yes
    hardcode_libdir_flag_spec='${wl}-rpath ${wl}$libdir'
    hardcode_libdir_separator=:
    hardcode_shlibpath_var=no
    ;;

  openbsd*)
    hardcode_direct=yes
    hardcode_shlibpath_var=no
    if test -z "`echo __ELF__ | $CC -E - | grep __ELF__`" || test "$host_os-$host_cpu" = "openbsd2.8-powerpc"; then
      archive_cmds='$CC -shared $pic_flag -o $lib $libobjs $deplibs $linker_flags'
      hardcode_libdir_flag_spec='${wl}-rpath,$libdir'
      export_dynamic_flag_spec='${wl}-E'
    else
      case "$host_os" in
      openbsd[[01]].* | openbsd2.[[0-7]] | openbsd2.[[0-7]].*)
	archive_cmds='$LD -Bshareable -o $lib $libobjs $deplibs $linker_flags'
	hardcode_libdir_flag_spec='-R$libdir'
        ;;
      *)
        archive_cmds='$CC -shared $pic_flag -o $lib $libobjs $deplibs $linker_flags'
        hardcode_libdir_flag_spec='${wl}-rpath,$libdir'
        ;;
      esac
    fi
    ;;

  os2*)
    hardcode_libdir_flag_spec='-L$libdir'
    hardcode_minus_L=yes
    allow_undefined_flag=unsupported
    archive_cmds='$echo "LIBRARY $libname INITINSTANCE" > $output_objdir/$libname.def~$echo "DESCRIPTION \"$libname\"" >> $output_objdir/$libname.def~$echo DATA >> $output_objdir/$libname.def~$echo " SINGLE NONSHARED" >> $output_objdir/$libname.def~$echo EXPORTS >> $output_objdir/$libname.def~emxexp $libobjs >> $output_objdir/$libname.def~$CC -Zdll -Zcrtdll -o $lib $libobjs $deplibs $compiler_flags $output_objdir/$libname.def'
    old_archive_from_new_cmds='emximp -o $output_objdir/$libname.a $output_objdir/$libname.def'
    ;;

  osf3*)
    if test "$GCC" = yes; then
      allow_undefined_flag=' ${wl}-expect_unresolved ${wl}\*'
      archive_cmds='$CC -shared${allow_undefined_flag} $libobjs $deplibs $compiler_flags ${wl}-soname ${wl}$soname `test -n "$verstring" && echo ${wl}-set_version ${wl}$verstring` ${wl}-update_registry ${wl}${output_objdir}/so_locations -o $lib'
    else
      allow_undefined_flag=' -expect_unresolved \*'
      archive_cmds='$LD -shared${allow_undefined_flag} $libobjs $deplibs $linker_flags -soname $soname `test -n "$verstring" && echo -set_version $verstring` -update_registry ${output_objdir}/so_locations -o $lib'
    fi
    hardcode_libdir_flag_spec='${wl}-rpath ${wl}$libdir'
    hardcode_libdir_separator=:
    ;;

  osf4* | osf5*)	# as osf3* with the addition of -msym flag
    if test "$GCC" = yes; then
      allow_undefined_flag=' ${wl}-expect_unresolved ${wl}\*'
      archive_cmds='$CC -shared${allow_undefined_flag} $libobjs $deplibs $compiler_flags ${wl}-msym ${wl}-soname ${wl}$soname `test -n "$verstring" && echo ${wl}-set_version ${wl}$verstring` ${wl}-update_registry ${wl}${output_objdir}/so_locations -o $lib'
      hardcode_libdir_flag_spec='${wl}-rpath ${wl}$libdir'
    else
      allow_undefined_flag=' -expect_unresolved \*'
      archive_cmds='$LD -shared${allow_undefined_flag} $libobjs $deplibs $linker_flags -msym -soname $soname `test -n "$verstring" && echo -set_version $verstring` -update_registry ${output_objdir}/so_locations -o $lib'
      archive_expsym_cmds='for i in `cat $export_symbols`; do printf "-exported_symbol " >> $lib.exp; echo "\$i" >> $lib.exp; done; echo "-hidden">> $lib.exp~
      $LD -shared${allow_undefined_flag} -input $lib.exp $linker_flags $libobjs $deplibs -soname $soname `test -n "$verstring" && echo -set_version $verstring` -update_registry ${objdir}/so_locations -o $lib~$rm $lib.exp'

      #Both c and cxx compiler support -rpath directly
      hardcode_libdir_flag_spec='-rpath $libdir'
    fi
    hardcode_libdir_separator=:
    ;;

  sco3.2v5*)
    archive_cmds='$LD -G -h $soname -o $lib $libobjs $deplibs $linker_flags'
    hardcode_shlibpath_var=no
    runpath_var=LD_RUN_PATH
    hardcode_runpath_var=yes
    export_dynamic_flag_spec='${wl}-Bexport'
    ;;

  solaris*)
    # gcc --version < 3.0 without binutils cannot create self contained
    # shared libraries reliably, requiring libgcc.a to resolve some of
    # the object symbols generated in some cases.  Libraries that use
    # assert need libgcc.a to resolve __eprintf, for example.  Linking
    # a copy of libgcc.a into every shared library to guarantee resolving
    # such symbols causes other problems:  According to Tim Van Holder
    # <tim.van.holder@pandora.be>, C++ libraries end up with a separate
    # (to the application) exception stack for one thing.
    no_undefined_flag=' -z defs'
    if test "$GCC" = yes; then
      case `$CC --version 2>/dev/null` in
      [[12]].*)
	cat <<EOF 1>&2

*** Warning: Releases of GCC earlier than version 3.0 cannot reliably
*** create self contained shared libraries on Solaris systems, without
*** introducing a dependency on libgcc.a.  Therefore, libtool is disabling
*** -no-undefined support, which will at least allow you to build shared
*** libraries.  However, you may find that when you link such libraries
*** into an application without using GCC, you have to manually add
*** \`gcc --print-libgcc-file-name\` to the link command.  We urge you to
*** upgrade to a newer version of GCC.  Another option is to rebuild your
*** current GCC to use the GNU linker from GNU binutils 2.9.1 or newer.

EOF
        no_undefined_flag=
	;;
      esac
    fi
    # $CC -shared without GNU ld will not create a library from C++
    # object files and a static libstdc++, better avoid it by now
    archive_cmds='$LD -G${allow_undefined_flag} -h $soname -o $lib $libobjs $deplibs $linker_flags'
    archive_expsym_cmds='$echo "{ global:" > $lib.exp~cat $export_symbols | sed -e "s/\(.*\)/\1;/" >> $lib.exp~$echo "local: *; };" >> $lib.exp~
		$LD -G${allow_undefined_flag} -M $lib.exp -h $soname -o $lib $libobjs $deplibs $linker_flags~$rm $lib.exp'
    hardcode_libdir_flag_spec='-R$libdir'
    hardcode_shlibpath_var=no
    case $host_os in
    solaris2.[[0-5]] | solaris2.[[0-5]].*) ;;
    *) # Supported since Solaris 2.6 (maybe 2.5.1?)
      whole_archive_flag_spec='-z allextract$convenience -z defaultextract' ;;
    esac
    link_all_deplibs=yes
    ;;

  sunos4*)
    if test "x$host_vendor" = xsequent; then
      # Use $CC to link under sequent, because it throws in some extra .o
      # files that make .init and .fini sections work.
      archive_cmds='$CC -G ${wl}-h $soname -o $lib $libobjs $deplibs $compiler_flags'
    else
      archive_cmds='$LD -assert pure-text -Bstatic -o $lib $libobjs $deplibs $linker_flags'
    fi
    hardcode_libdir_flag_spec='-L$libdir'
    hardcode_direct=yes
    hardcode_minus_L=yes
    hardcode_shlibpath_var=no
    ;;

  sysv4)
    if test "x$host_vendor" = xsno; then
      archive_cmds='$LD -G -Bsymbolic -h $soname -o $lib $libobjs $deplibs $linker_flags'
      hardcode_direct=yes # is this really true???
    else
      archive_cmds='$LD -G -h $soname -o $lib $libobjs $deplibs $linker_flags'
      hardcode_direct=no #Motorola manual says yes, but my tests say they lie
    fi
    runpath_var='LD_RUN_PATH'
    hardcode_shlibpath_var=no
    ;;

  sysv4.3*)
    archive_cmds='$LD -G -h $soname -o $lib $libobjs $deplibs $linker_flags'
    hardcode_shlibpath_var=no
    export_dynamic_flag_spec='-Bexport'
    ;;

  sysv5uw7* | unixware7* | sysv5Open* )
    no_undefined_flag='${wl}-z ${wl}text'
    if test "$GCC" = yes; then
      archive_cmds='$CC -shared ${wl}-h ${wl}$soname -o $lib $libobjs $deplibs $compiler_flags'
    else
      archive_cmds='$CC -G ${wl}-h ${wl}$soname -o $lib $libobjs $deplibs $compiler_flags'
    fi
    runpath_var='LD_RUN_PATH'
    hardcode_shlibpath_var=no
    ;;

  sysv5*)
    no_undefined_flag=' -z text'
    # $CC -shared without GNU ld will not create a library from C++
    # object files and a static libstdc++, better avoid it by now
    archive_cmds='$LD -G${allow_undefined_flag} -h $soname -o $lib $libobjs $deplibs $linker_flags'
    archive_expsym_cmds='$echo "{ global:" > $lib.exp~cat $export_symbols | sed -e "s/\(.*\)/\1;/" >> $lib.exp~$echo "local: *; };" >> $lib.exp~
		$LD -G${allow_undefined_flag} -M $lib.exp -h $soname -o $lib $libobjs $deplibs $linker_flags~$rm $lib.exp'
    hardcode_libdir_flag_spec=
    hardcode_shlibpath_var=no
    runpath_var='LD_RUN_PATH'
    ;;

  uts4*)
    archive_cmds='$LD -G -h $soname -o $lib $libobjs $deplibs $linker_flags'
    hardcode_libdir_flag_spec='-L$libdir'
    hardcode_shlibpath_var=no
    ;;

  dgux*)
    archive_cmds='$LD -G -h $soname -o $lib $libobjs $deplibs $linker_flags'
    hardcode_libdir_flag_spec='-L$libdir'
    hardcode_shlibpath_var=no
    ;;

  sysv4*MP*)
    if test -d /usr/nec; then
      archive_cmds='$LD -G -h $soname -o $lib $libobjs $deplibs $linker_flags'
      hardcode_shlibpath_var=no
      runpath_var=LD_RUN_PATH
      hardcode_runpath_var=yes
      ld_shlibs=yes
    fi
    ;;

  sysv4.2uw2*)
    archive_cmds='$LD -G -o $lib $libobjs $deplibs $linker_flags'
    hardcode_direct=yes
    hardcode_minus_L=no
    hardcode_shlibpath_var=no
    hardcode_runpath_var=yes
    runpath_var=LD_RUN_PATH
    ;;

  *)
    ld_shlibs=no
    ;;
  esac
fi
AC_MSG_RESULT([$ld_shlibs])
test "$ld_shlibs" = no && can_build_shared=no

# Check hardcoding attributes.
AC_MSG_CHECKING([how to hardcode library paths into programs])
hardcode_action=
if test -n "$hardcode_libdir_flag_spec" || \
   test -n "$runpath_var"; then

  # We can hardcode non-existant directories.
  if test "$hardcode_direct" != no &&
     # If the only mechanism to avoid hardcoding is shlibpath_var, we
     # have to relink, otherwise we might link with an installed library
     # when we should be linking with a yet-to-be-installed one
     ## test "$hardcode_shlibpath_var" != no &&
     test "$hardcode_minus_L" != no; then
    # Linking always hardcodes the temporary library directory.
    hardcode_action=relink
  else
    # We can link without hardcoding, and we can hardcode nonexisting dirs.
    hardcode_action=immediate
  fi
else
  # We cannot hardcode anything, or else we can only hardcode existing
  # directories.
  hardcode_action=unsupported
fi
AC_MSG_RESULT([$hardcode_action])

striplib=
old_striplib=
AC_MSG_CHECKING([whether stripping libraries is possible])
if test -n "$STRIP" && $STRIP -V 2>&1 | grep "GNU strip" >/dev/null; then
  test -z "$old_striplib" && old_striplib="$STRIP --strip-debug"
  test -z "$striplib" && striplib="$STRIP --strip-unneeded"
  AC_MSG_RESULT([yes])
else
  AC_MSG_RESULT([no])
fi

reload_cmds='$LD$reload_flag -o $output$reload_objs'
test -z "$deplibs_check_method" && deplibs_check_method=unknown

# PORTME Fill in your ld.so characteristics
AC_MSG_CHECKING([dynamic linker characteristics])
library_names_spec=
libname_spec='lib$name'
soname_spec=
postinstall_cmds=
postuninstall_cmds=
finish_cmds=
finish_eval=
shlibpath_var=
shlibpath_overrides_runpath=unknown
version_type=none
dynamic_linker="$host_os ld.so"
sys_lib_dlsearch_path_spec="/lib /usr/lib"
sys_lib_search_path_spec="/lib /usr/lib /usr/local/lib"

case $host_os in
aix3*)
  version_type=linux
  library_names_spec='${libname}${release}.so$versuffix $libname.a'
  shlibpath_var=LIBPATH

  # AIX has no versioning support, so we append a major version to the name.
  soname_spec='${libname}${release}.so$major'
  ;;

aix4* | aix5*)
  version_type=linux
  if test "$host_cpu" = ia64; then
    # AIX 5 supports IA64
    library_names_spec='${libname}${release}.so$major ${libname}${release}.so$versuffix $libname.so'
    shlibpath_var=LD_LIBRARY_PATH
  else
    # With GCC up to 2.95.x, collect2 would create an import file
    # for dependence libraries.  The import file would start with
    # the line `#! .'.  This would cause the generated library to
    # depend on `.', always an invalid library.  This was fixed in
    # development snapshots of GCC prior to 3.0.
    case $host_os in
      aix4 | aix4.[[01]] | aix4.[[01]].*)
	if { echo '#if __GNUC__ > 2 || (__GNUC__ == 2 && __GNUC_MINOR__ >= 97)'
	     echo ' yes '
	     echo '#endif'; } | ${CC} -E - | grep yes > /dev/null; then
	  :
	else
	  can_build_shared=no
	fi
	;;
    esac
    # AIX (on Power*) has no versioning support, so currently we can
    # not hardcode correct soname into executable. Probably we can
    # add versioning support to collect2, so additional links can
    # be useful in future.
    if test "$aix_use_runtimelinking" = yes; then
      # If using run time linking (on AIX 4.2 or later) use lib<name>.so
      # instead of lib<name>.a to let people know that these are not
      # typical AIX shared libraries.
      library_names_spec='${libname}${release}.so$versuffix ${libname}${release}.so$major $libname.so'
    else
      # We preserve .a as extension for shared libraries through AIX4.2
      # and later when we are not doing run time linking.
      library_names_spec='${libname}${release}.a $libname.a'
      soname_spec='${libname}${release}.so$major'
    fi
    shlibpath_var=LIBPATH
  fi
  ;;

amigaos*)
  library_names_spec='$libname.ixlibrary $libname.a'
  # Create ${libname}_ixlibrary.a entries in /sys/libs.
  finish_eval='for lib in `ls $libdir/*.ixlibrary 2>/dev/null`; do libname=`$echo "X$lib" | $Xsed -e '\''s%^.*/\([[^/]]*\)\.ixlibrary$%\1%'\''`; test $rm /sys/libs/${libname}_ixlibrary.a; $show "(cd /sys/libs && $LN_S $lib ${libname}_ixlibrary.a)"; (cd /sys/libs && $LN_S $lib ${libname}_ixlibrary.a) || exit 1; done'
  ;;

beos*)
  library_names_spec='${libname}.so'
  dynamic_linker="$host_os ld.so"
  shlibpath_var=LIBRARY_PATH
  ;;

bsdi4*)
  version_type=linux
  need_version=no
  library_names_spec='${libname}${release}.so$versuffix ${libname}${release}.so$major $libname.so'
  soname_spec='${libname}${release}.so$major'
  finish_cmds='PATH="\$PATH:/sbin" ldconfig $libdir'
  shlibpath_var=LD_LIBRARY_PATH
  sys_lib_search_path_spec="/shlib /usr/lib /usr/X11/lib /usr/contrib/lib /lib /usr/local/lib"
  sys_lib_dlsearch_path_spec="/shlib /usr/lib /usr/local/lib"
  export_dynamic_flag_spec=-rdynamic
  # the default ld.so.conf also contains /usr/contrib/lib and
  # /usr/X11R6/lib (/usr/X11 is a link to /usr/X11R6), but let us allow
  # libtool to hard-code these into programs
  ;;

cygwin* | mingw* | pw32*)
  version_type=windows
  need_version=no
  need_lib_prefix=no
  case $GCC,$host_os in
  yes,cygwin*)
    library_names_spec='$libname.dll.a'
    soname_spec='`echo ${libname} | sed -e 's/^lib/cyg/'``echo ${release} | sed -e 's/[[.]]/-/g'`${versuffix}.dll'
    postinstall_cmds='dlpath=`bash 2>&1 -c '\''. $dir/${file}i;echo \$dlname'\''`~
      dldir=$destdir/`dirname \$dlpath`~
      test -d \$dldir || mkdir -p \$dldir~
      $install_prog .libs/$dlname \$dldir/$dlname'
    postuninstall_cmds='dldll=`bash 2>&1 -c '\''. $file; echo \$dlname'\''`~
      dlpath=$dir/\$dldll~
       $rm \$dlpath'
    ;;
  yes,mingw*)
    library_names_spec='${libname}`echo ${release} | sed -e 's/[[.]]/-/g'`${versuffix}.dll'
    sys_lib_search_path_spec=`$CC -print-search-dirs | grep "^libraries:" | sed -e "s/^libraries://" -e "s/;/ /g"`
    ;;
  yes,pw32*)
    library_names_spec='`echo ${libname} | sed -e 's/^lib/pw/'``echo ${release} | sed -e 's/[.]/-/g'`${versuffix}.dll'
    ;;
  *)
    library_names_spec='${libname}`echo ${release} | sed -e 's/[[.]]/-/g'`${versuffix}.dll $libname.lib'
    ;;
  esac
  dynamic_linker='Win32 ld.exe'
  # FIXME: first we should search . and the directory the executable is in
  shlibpath_var=PATH
  ;;

darwin* | rhapsody*)
  dynamic_linker="$host_os dyld"
  version_type=darwin
  need_lib_prefix=no
  need_version=no
  # FIXME: Relying on posixy $() will cause problems for
  #        cross-compilation, but unfortunately the echo tests do not
  #        yet detect zsh echo's removal of \ escapes.
  library_names_spec='${libname}${release}${versuffix}.$(test .$module = .yes && echo so || echo dylib) ${libname}${release}${major}.$(test .$module = .yes && echo so || echo dylib) ${libname}.$(test .$module = .yes && echo so || echo dylib)'
  soname_spec='${libname}${release}${major}.$(test .$module = .yes && echo so || echo dylib)'
  shlibpath_overrides_runpath=yes
  shlibpath_var=DYLD_LIBRARY_PATH
  ;;

freebsd1*)
  dynamic_linker=no
  ;;

freebsd*)
  objformat=`test -x /usr/bin/objformat && /usr/bin/objformat || echo aout`
  version_type=freebsd-$objformat
  case $version_type in
    freebsd-elf*)
      library_names_spec='${libname}${release}.so$versuffix ${libname}${release}.so $libname.so'
      need_version=no
      need_lib_prefix=no
      ;;
    freebsd-*)
      library_names_spec='${libname}${release}.so$versuffix $libname.so$versuffix'
      need_version=yes
      ;;
  esac
  shlibpath_var=LD_LIBRARY_PATH
  case $host_os in
  freebsd2*)
    shlibpath_overrides_runpath=yes
    ;;
  *)
    shlibpath_overrides_runpath=no
    hardcode_into_libs=yes
    ;;
  esac
  ;;

gnu*)
  version_type=linux
  need_lib_prefix=no
  need_version=no
  library_names_spec='${libname}${release}.so$versuffix ${libname}${release}.so${major} ${libname}.so'
  soname_spec='${libname}${release}.so$major'
  shlibpath_var=LD_LIBRARY_PATH
  hardcode_into_libs=yes
  ;;

hpux9* | hpux10* | hpux11*)
  # Give a soname corresponding to the major version so that dld.sl refuses to
  # link against other versions.
  dynamic_linker="$host_os dld.sl"
  version_type=sunos
  need_lib_prefix=no
  need_version=no
  shlibpath_var=SHLIB_PATH
  shlibpath_overrides_runpath=no # +s is required to enable SHLIB_PATH
  library_names_spec='${libname}${release}.sl$versuffix ${libname}${release}.sl$major $libname.sl'
  soname_spec='${libname}${release}.sl$major'
  # HP-UX runs *really* slowly unless shared libraries are mode 555.
  postinstall_cmds='chmod 555 $lib'
  ;;

irix5* | irix6*)
  version_type=irix
  need_lib_prefix=no
  need_version=no
  soname_spec='${libname}${release}.so$major'
  library_names_spec='${libname}${release}.so$versuffix ${libname}${release}.so$major ${libname}${release}.so $libname.so'
  case $host_os in
  irix5*)
    libsuff= shlibsuff=
    ;;
  *)
    case $LD in # libtool.m4 will add one of these switches to LD
    *-32|*"-32 ") libsuff= shlibsuff= libmagic=32-bit;;
    *-n32|*"-n32 ") libsuff=32 shlibsuff=N32 libmagic=N32;;
    *-64|*"-64 ") libsuff=64 shlibsuff=64 libmagic=64-bit;;
    *) libsuff= shlibsuff= libmagic=never-match;;
    esac
    ;;
  esac
  shlibpath_var=LD_LIBRARY${shlibsuff}_PATH
  shlibpath_overrides_runpath=no
  sys_lib_search_path_spec="/usr/lib${libsuff} /lib${libsuff} /usr/local/lib${libsuff}"
  sys_lib_dlsearch_path_spec="/usr/lib${libsuff} /lib${libsuff}"
  ;;

# No shared lib support for Linux oldld, aout, or coff.
linux-gnuoldld* | linux-gnuaout* | linux-gnucoff*)
  dynamic_linker=no
  ;;

# This must be Linux ELF.
linux-gnu*)
  version_type=linux
  need_lib_prefix=no
  need_version=no
  library_names_spec='${libname}${release}.so$versuffix ${libname}${release}.so$major $libname.so'
  soname_spec='${libname}${release}.so$major'
  finish_cmds='PATH="\$PATH:/sbin" ldconfig -n $libdir'
  shlibpath_var=LD_LIBRARY_PATH
  shlibpath_overrides_runpath=no
  # This implies no fast_install, which is unacceptable.
  # Some rework will be needed to allow for fast_install
  # before this can be enabled.
  hardcode_into_libs=yes

  # We used to test for /lib/ld.so.1 and disable shared libraries on
  # powerpc, because MkLinux only supported shared libraries with the
  # GNU dynamic linker.  Since this was broken with cross compilers,
  # most powerpc-linux boxes support dynamic linking these days and
  # people can always --disable-shared, the test was removed, and we
  # assume the GNU/Linux dynamic linker is in use.
  dynamic_linker='GNU/Linux ld.so'
  ;;

netbsd*)
  version_type=sunos
  need_lib_prefix=no
  need_version=no
  if echo __ELF__ | $CC -E - | grep __ELF__ >/dev/null; then
    library_names_spec='${libname}${release}.so$versuffix ${libname}.so$versuffix'
    finish_cmds='PATH="\$PATH:/sbin" ldconfig -m $libdir'
    dynamic_linker='NetBSD (a.out) ld.so'
  else
    library_names_spec='${libname}${release}.so$versuffix ${libname}${release}.so$major ${libname}${release}.so ${libname}.so'
    soname_spec='${libname}${release}.so$major'
    dynamic_linker='NetBSD ld.elf_so'
  fi
  shlibpath_var=LD_LIBRARY_PATH
  shlibpath_overrides_runpath=yes
  hardcode_into_libs=yes
  ;;

newsos6)
  version_type=linux
  library_names_spec='${libname}${release}.so$versuffix ${libname}${release}.so$major $libname.so'
  shlibpath_var=LD_LIBRARY_PATH
  shlibpath_overrides_runpath=yes
  ;;

openbsd*)
  version_type=sunos
  need_lib_prefix=no
  need_version=no
  if test -z "`echo __ELF__ | $CC -E - | grep __ELF__`" || test "$host_os-$host_cpu" = "openbsd2.8-powerpc"; then
    case "$host_os" in
    openbsd2.[[89]] | openbsd2.[[89]].*)
      shlibpath_overrides_runpath=no
      ;;
    *)
      shlibpath_overrides_runpath=yes
      ;;
    esac
  else
    shlibpath_overrides_runpath=yes
  fi
  library_names_spec='${libname}${release}.so$versuffix ${libname}.so$versuffix'
  finish_cmds='PATH="\$PATH:/sbin" ldconfig -m $libdir'
  shlibpath_var=LD_LIBRARY_PATH
  ;;

os2*)
  libname_spec='$name'
  need_lib_prefix=no
  library_names_spec='$libname.dll $libname.a'
  dynamic_linker='OS/2 ld.exe'
  shlibpath_var=LIBPATH
  ;;

osf3* | osf4* | osf5*)
  version_type=osf
  need_version=no
  soname_spec='${libname}${release}.so'
  library_names_spec='${libname}${release}.so$versuffix ${libname}${release}.so $libname.so'
  shlibpath_var=LD_LIBRARY_PATH
  sys_lib_search_path_spec="/usr/shlib /usr/ccs/lib /usr/lib/cmplrs/cc /usr/lib /usr/local/lib /var/shlib"
  sys_lib_dlsearch_path_spec="$sys_lib_search_path_spec"
  ;;

sco3.2v5*)
  version_type=osf
  soname_spec='${libname}${release}.so$major'
  library_names_spec='${libname}${release}.so$versuffix ${libname}${release}.so$major $libname.so'
  shlibpath_var=LD_LIBRARY_PATH
  ;;

solaris*)
  version_type=linux
  need_lib_prefix=no
  need_version=no
  library_names_spec='${libname}${release}.so$versuffix ${libname}${release}.so$major $libname.so'
  soname_spec='${libname}${release}.so$major'
  shlibpath_var=LD_LIBRARY_PATH
  shlibpath_overrides_runpath=yes
  hardcode_into_libs=yes
  # ldd complains unless libraries are executable
  postinstall_cmds='chmod +x $lib'
  ;;

sunos4*)
  version_type=sunos
  library_names_spec='${libname}${release}.so$versuffix ${libname}.so$versuffix'
  finish_cmds='PATH="\$PATH:/usr/etc" ldconfig $libdir'
  shlibpath_var=LD_LIBRARY_PATH
  shlibpath_overrides_runpath=yes
  if test "$with_gnu_ld" = yes; then
    need_lib_prefix=no
  fi
  need_version=yes
  ;;

sysv4 | sysv4.2uw2* | sysv4.3* | sysv5*)
  version_type=linux
  library_names_spec='${libname}${release}.so$versuffix ${libname}${release}.so$major $libname.so'
  soname_spec='${libname}${release}.so$major'
  shlibpath_var=LD_LIBRARY_PATH
  case $host_vendor in
    sni)
      shlibpath_overrides_runpath=no
      ;;
    motorola)
      need_lib_prefix=no
      need_version=no
      shlibpath_overrides_runpath=no
      sys_lib_search_path_spec='/lib /usr/lib /usr/ccs/lib'
      ;;
  esac
  ;;

uts4*)
  version_type=linux
  library_names_spec='${libname}${release}.so$versuffix ${libname}${release}.so$major $libname.so'
  soname_spec='${libname}${release}.so$major'
  shlibpath_var=LD_LIBRARY_PATH
  ;;

dgux*)
  version_type=linux
  need_lib_prefix=no
  need_version=no
  library_names_spec='${libname}${release}.so$versuffix ${libname}${release}.so$major $libname.so'
  soname_spec='${libname}${release}.so$major'
  shlibpath_var=LD_LIBRARY_PATH
  ;;

sysv4*MP*)
  if test -d /usr/nec ;then
    version_type=linux
    library_names_spec='$libname.so.$versuffix $libname.so.$major $libname.so'
    soname_spec='$libname.so.$major'
    shlibpath_var=LD_LIBRARY_PATH
  fi
  ;;

*)
  dynamic_linker=no
  ;;
esac
AC_MSG_RESULT([$dynamic_linker])
test "$dynamic_linker" = no && can_build_shared=no

# Report the final consequences.
AC_MSG_CHECKING([if libtool supports shared libraries])
AC_MSG_RESULT([$can_build_shared])

AC_MSG_CHECKING([whether to build shared libraries])
test "$can_build_shared" = "no" && enable_shared=no

# On AIX, shared libraries and static libraries use the same namespace, and
# are all built from PIC.
case "$host_os" in
aix3*)
  test "$enable_shared" = yes && enable_static=no
  if test -n "$RANLIB"; then
    archive_cmds="$archive_cmds~\$RANLIB \$lib"
    postinstall_cmds='$RANLIB $lib'
  fi
  ;;

aix4*)
  if test "$host_cpu" != ia64 && test "$aix_use_runtimelinking" = no ; then
    test "$enable_shared" = yes && enable_static=no
  fi
  ;;
esac
AC_MSG_RESULT([$enable_shared])

AC_MSG_CHECKING([whether to build static libraries])
# Make sure either enable_shared or enable_static is yes.
test "$enable_shared" = yes || enable_static=yes
AC_MSG_RESULT([$enable_static])

if test "$hardcode_action" = relink; then
  # Fast installation is not supported
  enable_fast_install=no
elif test "$shlibpath_overrides_runpath" = yes ||
     test "$enable_shared" = no; then
  # Fast installation is not necessary
  enable_fast_install=needless
fi

variables_saved_for_relink="PATH $shlibpath_var $runpath_var"
if test "$GCC" = yes; then
  variables_saved_for_relink="$variables_saved_for_relink GCC_EXEC_PREFIX COMPILER_PATH LIBRARY_PATH"
fi

AC_LIBTOOL_DLOPEN_SELF

if test "$enable_shared" = yes && test "$GCC" = yes; then
  case $archive_cmds in
  *'~'*)
    # FIXME: we may have to deal with multi-command sequences.
    ;;
  '$CC '*)
    # Test whether the compiler implicitly links with -lc since on some
    # systems, -lgcc has to come before -lc. If gcc already passes -lc
    # to ld, don't add -lc before -lgcc.
    AC_MSG_CHECKING([whether -lc should be explicitly linked in])
    AC_CACHE_VAL([lt_cv_archive_cmds_need_lc],
    [$rm conftest*
    echo 'static int dummy;' > conftest.$ac_ext

    if AC_TRY_EVAL(ac_compile); then
      soname=conftest
      lib=conftest
      libobjs=conftest.$ac_objext
      deplibs=
      wl=$lt_cv_prog_cc_wl
      compiler_flags=-v
      linker_flags=-v
      verstring=
      output_objdir=.
      libname=conftest
      save_allow_undefined_flag=$allow_undefined_flag
      allow_undefined_flag=
      if AC_TRY_EVAL(archive_cmds 2\>\&1 \| grep \" -lc \" \>/dev/null 2\>\&1)
      then
	lt_cv_archive_cmds_need_lc=no
      else
	lt_cv_archive_cmds_need_lc=yes
      fi
      allow_undefined_flag=$save_allow_undefined_flag
    else
      cat conftest.err 1>&5
    fi])
    AC_MSG_RESULT([$lt_cv_archive_cmds_need_lc])
    ;;
  esac
fi

dnl The newest unixware seems to dislike linking with -lc
case $host_os in
  sysv5Open*)
    lt_cv_archive_cmds_need_lc=no
    ;;
esac

need_lc=${lt_cv_archive_cmds_need_lc-yes}

# The second clause should only fire when bootstrapping the
# libtool distribution, otherwise you forgot to ship ltmain.sh
# with your package, and you will get complaints that there are
# no rules to generate ltmain.sh.
if test -f "$ltmain"; then
  :
else
  # If there is no Makefile yet, we rely on a make rule to execute
  # `config.status --recheck' to rerun these tests and create the
  # libtool script then.
  test -f Makefile && make "$ltmain"
fi

if test -f "$ltmain"; then
  trap "$rm \"${ofile}T\"; exit 1" 1 2 15
  $rm -f "${ofile}T"

  echo creating $ofile

  # Now quote all the things that may contain metacharacters while being
  # careful not to overquote the AC_SUBSTed values.  We take copies of the
  # variables and quote the copies for generation of the libtool script.
  for var in echo old_CC old_CFLAGS \
    AR AR_FLAGS CC LD LN_S NM SHELL \
    reload_flag reload_cmds wl \
    pic_flag link_static_flag no_builtin_flag export_dynamic_flag_spec \
    thread_safe_flag_spec whole_archive_flag_spec libname_spec \
    library_names_spec soname_spec \
    RANLIB old_archive_cmds old_archive_from_new_cmds old_postinstall_cmds \
    old_postuninstall_cmds archive_cmds archive_expsym_cmds postinstall_cmds \
    postuninstall_cmds extract_expsyms_cmds old_archive_from_expsyms_cmds \
    old_striplib striplib file_magic_cmd export_symbols_cmds \
    deplibs_check_method allow_undefined_flag no_undefined_flag \
    finish_cmds finish_eval global_symbol_pipe global_symbol_to_cdecl \
    global_symbol_to_c_name_address \
    hardcode_libdir_flag_spec hardcode_libdir_separator  \
    sys_lib_search_path_spec sys_lib_dlsearch_path_spec \
    compiler_c_o compiler_o_lo need_locks exclude_expsyms include_expsyms; do

    case $var in
    reload_cmds | old_archive_cmds | old_archive_from_new_cmds | \
    old_postinstall_cmds | old_postuninstall_cmds | \
    export_symbols_cmds | archive_cmds | archive_expsym_cmds | \
    extract_expsyms_cmds | old_archive_from_expsyms_cmds | \
    postinstall_cmds | postuninstall_cmds | \
    finish_cmds | sys_lib_search_path_spec | sys_lib_dlsearch_path_spec)
      # Double-quote double-evaled strings.
      eval "lt_$var=\\\"\`\$echo \"X\$$var\" | \$Xsed -e \"\$double_quote_subst\" -e \"\$sed_quote_subst\" -e \"\$delay_variable_subst\"\`\\\""
      ;;
    *)
      eval "lt_$var=\\\"\`\$echo \"X\$$var\" | \$Xsed -e \"\$sed_quote_subst\"\`\\\""
      ;;
    esac
  done

  cat <<__EOF__ > "${ofile}T"
#! $SHELL

# `$echo "$ofile" | sed 's%^.*/%%'` - Provide generalized library-building support services.
# Generated automatically by $PROGRAM (GNU $PACKAGE $VERSION$TIMESTAMP)
# NOTE: Changes made to this file will be lost: look at ltmain.sh.
#
# Copyright (C) 1996-2000 Free Software Foundation, Inc.
# Originally by Gordon Matzigkeit <gord@gnu.ai.mit.edu>, 1996
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
#
# As a special exception to the GNU General Public License, if you
# distribute this file as part of a program that contains a
# configuration script generated by Autoconf, you may include it under
# the same distribution terms that you use for the rest of that program.

# Sed that helps us avoid accidentally triggering echo(1) options like -n.
Xsed="sed -e s/^X//"

# The HP-UX ksh and POSIX shell print the target directory to stdout
# if CDPATH is set.
if test "X\${CDPATH+set}" = Xset; then CDPATH=:; export CDPATH; fi

# ### BEGIN LIBTOOL CONFIG

# Libtool was configured on host `(hostname || uname -n) 2>/dev/null | sed 1q`:

# Shell to use when invoking shell scripts.
SHELL=$lt_SHELL

# Whether or not to build shared libraries.
build_libtool_libs=$enable_shared

# Whether or not to build static libraries.
build_old_libs=$enable_static

# Whether or not to add -lc for building shared libraries.
build_libtool_need_lc=$need_lc

# Whether or not to optimize for fast installation.
fast_install=$enable_fast_install

# The host system.
host_alias=$host_alias
host=$host

# An echo program that does not interpret backslashes.
echo=$lt_echo

# The archiver.
AR=$lt_AR
AR_FLAGS=$lt_AR_FLAGS

# The default C compiler.
CC=$lt_CC

# Is the compiler the GNU C compiler?
with_gcc=$GCC

# The linker used to build libraries.
LD=$lt_LD

# Whether we need hard or soft links.
LN_S=$lt_LN_S

# A BSD-compatible nm program.
NM=$lt_NM

# A symbol stripping program
STRIP=$STRIP

# Used to examine libraries when file_magic_cmd begins "file"
MAGIC_CMD=$MAGIC_CMD

# Used on cygwin: DLL creation program.
DLLTOOL="$DLLTOOL"

# Used on cygwin: object dumper.
OBJDUMP="$OBJDUMP"

# Used on cygwin: assembler.
AS="$AS"

# The name of the directory that contains temporary libtool files.
objdir=$objdir

# How to create reloadable object files.
reload_flag=$lt_reload_flag
reload_cmds=$lt_reload_cmds

# How to pass a linker flag through the compiler.
wl=$lt_wl

# Object file suffix (normally "o").
objext="$ac_objext"

# Old archive suffix (normally "a").
libext="$libext"

# Executable file suffix (normally "").
exeext="$exeext"

# Additional compiler flags for building library objects.
pic_flag=$lt_pic_flag
pic_mode=$pic_mode

# Does compiler simultaneously support -c and -o options?
compiler_c_o=$lt_compiler_c_o

# Can we write directly to a .lo ?
compiler_o_lo=$lt_compiler_o_lo

# Must we lock files when doing compilation ?
need_locks=$lt_need_locks

# Do we need the lib prefix for modules?
need_lib_prefix=$need_lib_prefix

# Do we need a version for libraries?
need_version=$need_version

# Whether dlopen is supported.
dlopen_support=$enable_dlopen

# Whether dlopen of programs is supported.
dlopen_self=$enable_dlopen_self

# Whether dlopen of statically linked programs is supported.
dlopen_self_static=$enable_dlopen_self_static

# Compiler flag to prevent dynamic linking.
link_static_flag=$lt_link_static_flag

# Compiler flag to turn off builtin functions.
no_builtin_flag=$lt_no_builtin_flag

# Compiler flag to allow reflexive dlopens.
export_dynamic_flag_spec=$lt_export_dynamic_flag_spec

# Compiler flag to generate shared objects directly from archives.
whole_archive_flag_spec=$lt_whole_archive_flag_spec

# Compiler flag to generate thread-safe objects.
thread_safe_flag_spec=$lt_thread_safe_flag_spec

# Library versioning type.
version_type=$version_type

# Format of library name prefix.
libname_spec=$lt_libname_spec

# List of archive names.  First name is the real one, the rest are links.
# The last name is the one that the linker finds with -lNAME.
library_names_spec=$lt_library_names_spec

# The coded name of the library, if different from the real name.
soname_spec=$lt_soname_spec

# Commands used to build and install an old-style archive.
RANLIB=$lt_RANLIB
old_archive_cmds=$lt_old_archive_cmds
old_postinstall_cmds=$lt_old_postinstall_cmds
old_postuninstall_cmds=$lt_old_postuninstall_cmds

# Create an old-style archive from a shared archive.
old_archive_from_new_cmds=$lt_old_archive_from_new_cmds

# Create a temporary old-style archive to link instead of a shared archive.
old_archive_from_expsyms_cmds=$lt_old_archive_from_expsyms_cmds

# Commands used to build and install a shared archive.
archive_cmds=$lt_archive_cmds
archive_expsym_cmds=$lt_archive_expsym_cmds
postinstall_cmds=$lt_postinstall_cmds
postuninstall_cmds=$lt_postuninstall_cmds

# Commands to strip libraries.
old_striplib=$lt_old_striplib
striplib=$lt_striplib

# Method to check whether dependent libraries are shared objects.
deplibs_check_method=$lt_deplibs_check_method

# Command to use when deplibs_check_method == file_magic.
file_magic_cmd=$lt_file_magic_cmd

# Flag that allows shared libraries with undefined symbols to be built.
allow_undefined_flag=$lt_allow_undefined_flag

# Flag that forces no undefined symbols.
no_undefined_flag=$lt_no_undefined_flag

# Commands used to finish a libtool library installation in a directory.
finish_cmds=$lt_finish_cmds

# Same as above, but a single script fragment to be evaled but not shown.
finish_eval=$lt_finish_eval

# Take the output of nm and produce a listing of raw symbols and C names.
global_symbol_pipe=$lt_global_symbol_pipe

# Transform the output of nm in a proper C declaration
global_symbol_to_cdecl=$lt_global_symbol_to_cdecl

# Transform the output of nm in a C name address pair
global_symbol_to_c_name_address=$lt_global_symbol_to_c_name_address

# This is the shared library runtime path variable.
runpath_var=$runpath_var

# This is the shared library path variable.
shlibpath_var=$shlibpath_var

# Is shlibpath searched before the hard-coded library search path?
shlibpath_overrides_runpath=$shlibpath_overrides_runpath

# How to hardcode a shared library path into an executable.
hardcode_action=$hardcode_action

# Whether we should hardcode library paths into libraries.
hardcode_into_libs=$hardcode_into_libs

# Flag to hardcode \$libdir into a binary during linking.
# This must work even if \$libdir does not exist.
hardcode_libdir_flag_spec=$lt_hardcode_libdir_flag_spec

# Whether we need a single -rpath flag with a separated argument.
hardcode_libdir_separator=$lt_hardcode_libdir_separator

# Set to yes if using DIR/libNAME.so during linking hardcodes DIR into the
# resulting binary.
hardcode_direct=$hardcode_direct

# Set to yes if using the -LDIR flag during linking hardcodes DIR into the
# resulting binary.
hardcode_minus_L=$hardcode_minus_L

# Set to yes if using SHLIBPATH_VAR=DIR during linking hardcodes DIR into
# the resulting binary.
hardcode_shlibpath_var=$hardcode_shlibpath_var

# Variables whose values should be saved in libtool wrapper scripts and
# restored at relink time.
variables_saved_for_relink="$variables_saved_for_relink"

# Whether libtool must link a program against all its dependency libraries.
link_all_deplibs=$link_all_deplibs

# Compile-time system search path for libraries
sys_lib_search_path_spec=$lt_sys_lib_search_path_spec

# Run-time system search path for libraries
sys_lib_dlsearch_path_spec=$lt_sys_lib_dlsearch_path_spec

# Fix the shell variable \$srcfile for the compiler.
fix_srcfile_path="$fix_srcfile_path"

# Set to yes if exported symbols are required.
always_export_symbols=$always_export_symbols

# The commands to list exported symbols.
export_symbols_cmds=$lt_export_symbols_cmds

# The commands to extract the exported symbol list from a shared archive.
extract_expsyms_cmds=$lt_extract_expsyms_cmds

# Symbols that should not be listed in the preloaded symbols.
exclude_expsyms=$lt_exclude_expsyms

# Symbols that must always be exported.
include_expsyms=$lt_include_expsyms

# ### END LIBTOOL CONFIG

__EOF__

  case $host_os in
  aix3*)
    cat <<\EOF >> "${ofile}T"

# AIX sometimes has problems with the GCC collect2 program.  For some
# reason, if we set the COLLECT_NAMES environment variable, the problems
# vanish in a puff of smoke.
if test "X${COLLECT_NAMES+set}" != Xset; then
  COLLECT_NAMES=
  export COLLECT_NAMES
fi
EOF
    ;;
  esac

  case $host_os in
  cygwin* | mingw* | pw32* | os2*)
    cat <<'EOF' >> "${ofile}T"
      # This is a source program that is used to create dlls on Windows
      # Don't remove nor modify the starting and closing comments
# /* ltdll.c starts here */
# #define WIN32_LEAN_AND_MEAN
# #include <windows.h>
# #undef WIN32_LEAN_AND_MEAN
# #include <stdio.h>
#
# #ifndef __CYGWIN__
# #  ifdef __CYGWIN32__
# #    define __CYGWIN__ __CYGWIN32__
# #  endif
# #endif
#
# #ifdef __cplusplus
# extern "C" {
# #endif
# BOOL APIENTRY DllMain (HINSTANCE hInst, DWORD reason, LPVOID reserved);
# #ifdef __cplusplus
# }
# #endif
#
# #ifdef __CYGWIN__
# #include <cygwin/cygwin_dll.h>
# DECLARE_CYGWIN_DLL( DllMain );
# #endif
# HINSTANCE __hDllInstance_base;
#
# BOOL APIENTRY
# DllMain (HINSTANCE hInst, DWORD reason, LPVOID reserved)
# {
#   __hDllInstance_base = hInst;
#   return TRUE;
# }
# /* ltdll.c ends here */
	# This is a source program that is used to create import libraries
	# on Windows for dlls which lack them. Don't remove nor modify the
	# starting and closing comments
# /* impgen.c starts here */
# /*   Copyright (C) 1999-2000 Free Software Foundation, Inc.
#
#  This file is part of GNU libtool.
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
#  */
#
# #include <stdio.h>		/* for printf() */
# #include <unistd.h>		/* for open(), lseek(), read() */
# #include <fcntl.h>		/* for O_RDONLY, O_BINARY */
# #include <string.h>		/* for strdup() */
#
# /* O_BINARY isn't required (or even defined sometimes) under Unix */
# #ifndef O_BINARY
# #define O_BINARY 0
# #endif
#
# static unsigned int
# pe_get16 (fd, offset)
#      int fd;
#      int offset;
# {
#   unsigned char b[2];
#   lseek (fd, offset, SEEK_SET);
#   read (fd, b, 2);
#   return b[0] + (b[1]<<8);
# }
#
# static unsigned int
# pe_get32 (fd, offset)
#     int fd;
#     int offset;
# {
#   unsigned char b[4];
#   lseek (fd, offset, SEEK_SET);
#   read (fd, b, 4);
#   return b[0] + (b[1]<<8) + (b[2]<<16) + (b[3]<<24);
# }
#
# static unsigned int
# pe_as32 (ptr)
#      void *ptr;
# {
#   unsigned char *b = ptr;
#   return b[0] + (b[1]<<8) + (b[2]<<16) + (b[3]<<24);
# }
#
# int
# main (argc, argv)
#     int argc;
#     char *argv[];
# {
#     int dll;
#     unsigned long pe_header_offset, opthdr_ofs, num_entries, i;
#     unsigned long export_rva, export_size, nsections, secptr, expptr;
#     unsigned long name_rvas, nexp;
#     unsigned char *expdata, *erva;
#     char *filename, *dll_name;
#
#     filename = argv[1];
#
#     dll = open(filename, O_RDONLY|O_BINARY);
#     if (dll < 1)
# 	return 1;
#
#     dll_name = filename;
#
#     for (i=0; filename[i]; i++)
# 	if (filename[i] == '/' || filename[i] == '\\'  || filename[i] == ':')
# 	    dll_name = filename + i +1;
#
#     pe_header_offset = pe_get32 (dll, 0x3c);
#     opthdr_ofs = pe_header_offset + 4 + 20;
#     num_entries = pe_get32 (dll, opthdr_ofs + 92);
#
#     if (num_entries < 1) /* no exports */
# 	return 1;
#
#     export_rva = pe_get32 (dll, opthdr_ofs + 96);
#     export_size = pe_get32 (dll, opthdr_ofs + 100);
#     nsections = pe_get16 (dll, pe_header_offset + 4 +2);
#     secptr = (pe_header_offset + 4 + 20 +
# 	      pe_get16 (dll, pe_header_offset + 4 + 16));
#
#     expptr = 0;
#     for (i = 0; i < nsections; i++)
#     {
# 	char sname[8];
# 	unsigned long secptr1 = secptr + 40 * i;
# 	unsigned long vaddr = pe_get32 (dll, secptr1 + 12);
# 	unsigned long vsize = pe_get32 (dll, secptr1 + 16);
# 	unsigned long fptr = pe_get32 (dll, secptr1 + 20);
# 	lseek(dll, secptr1, SEEK_SET);
# 	read(dll, sname, 8);
# 	if (vaddr <= export_rva && vaddr+vsize > export_rva)
# 	{
# 	    expptr = fptr + (export_rva - vaddr);
# 	    if (export_rva + export_size > vaddr + vsize)
# 		export_size = vsize - (export_rva - vaddr);
# 	    break;
# 	}
#     }
#
#     expdata = (unsigned char*)malloc(export_size);
#     lseek (dll, expptr, SEEK_SET);
#     read (dll, expdata, export_size);
#     erva = expdata - export_rva;
#
#     nexp = pe_as32 (expdata+24);
#     name_rvas = pe_as32 (expdata+32);
#
#     printf ("EXPORTS\n");
#     for (i = 0; i<nexp; i++)
#     {
# 	unsigned long name_rva = pe_as32 (erva+name_rvas+i*4);
# 	printf ("\t%s @ %ld ;\n", erva+name_rva, 1+ i);
#     }
#
#     return 0;
# }
# /* impgen.c ends here */

EOF
    ;;
  esac

  # We use sed instead of cat because bash on DJGPP gets confused if
  # if finds mixed CR/LF and LF-only lines.  Since sed operates in
  # text mode, it properly converts lines to CR/LF.  This bash problem
  # is reportedly fixed, but why not run on old versions too?
  sed '$q' "$ltmain" >> "${ofile}T" || (rm -f "${ofile}T"; exit 1)

  mv -f "${ofile}T" "$ofile" || \
    (rm -f "$ofile" && cp "${ofile}T" "$ofile" && rm -f "${ofile}T")
  chmod +x "$ofile"
fi

])# _LT_AC_LTCONFIG_HACK

# AC_LIBTOOL_DLOPEN - enable checks for dlopen support
AC_DEFUN([AC_LIBTOOL_DLOPEN], [AC_BEFORE([$0],[AC_LIBTOOL_SETUP])])

# AC_LIBTOOL_WIN32_DLL - declare package support for building win32 dll's
AC_DEFUN([AC_LIBTOOL_WIN32_DLL], [AC_BEFORE([$0], [AC_LIBTOOL_SETUP])])

# AC_ENABLE_SHARED - implement the --enable-shared flag
# Usage: AC_ENABLE_SHARED[(DEFAULT)]
#   Where DEFAULT is either `yes' or `no'.  If omitted, it defaults to
#   `yes'.
AC_DEFUN([AC_ENABLE_SHARED],
[define([AC_ENABLE_SHARED_DEFAULT], ifelse($1, no, no, yes))dnl
AC_ARG_ENABLE(shared,
changequote(<<, >>)dnl
<<  --enable-shared[=PKGS]  build shared libraries [default=>>AC_ENABLE_SHARED_DEFAULT],
changequote([, ])dnl
[p=${PACKAGE-default}
case $enableval in
yes) enable_shared=yes ;;
no) enable_shared=no ;;
*)
  enable_shared=no
  # Look at the argument we got.  We use all the common list separators.
  IFS="${IFS= 	}"; ac_save_ifs="$IFS"; IFS="${IFS}:,"
  for pkg in $enableval; do
    if test "X$pkg" = "X$p"; then
      enable_shared=yes
    fi
  done
  IFS="$ac_save_ifs"
  ;;
esac],
enable_shared=AC_ENABLE_SHARED_DEFAULT)dnl
])

# AC_DISABLE_SHARED - set the default shared flag to --disable-shared
AC_DEFUN([AC_DISABLE_SHARED],
[AC_BEFORE([$0],[AC_LIBTOOL_SETUP])dnl
AC_ENABLE_SHARED(no)])

# AC_ENABLE_STATIC - implement the --enable-static flag
# Usage: AC_ENABLE_STATIC[(DEFAULT)]
#   Where DEFAULT is either `yes' or `no'.  If omitted, it defaults to
#   `yes'.
AC_DEFUN([AC_ENABLE_STATIC],
[define([AC_ENABLE_STATIC_DEFAULT], ifelse($1, no, no, yes))dnl
AC_ARG_ENABLE(static,
changequote(<<, >>)dnl
<<  --enable-static[=PKGS]  build static libraries [default=>>AC_ENABLE_STATIC_DEFAULT],
changequote([, ])dnl
[p=${PACKAGE-default}
case $enableval in
yes) enable_static=yes ;;
no) enable_static=no ;;
*)
  enable_static=no
  # Look at the argument we got.  We use all the common list separators.
  IFS="${IFS= 	}"; ac_save_ifs="$IFS"; IFS="${IFS}:,"
  for pkg in $enableval; do
    if test "X$pkg" = "X$p"; then
      enable_static=yes
    fi
  done
  IFS="$ac_save_ifs"
  ;;
esac],
enable_static=AC_ENABLE_STATIC_DEFAULT)dnl
])

# AC_DISABLE_STATIC - set the default static flag to --disable-static
AC_DEFUN([AC_DISABLE_STATIC],
[AC_BEFORE([$0],[AC_LIBTOOL_SETUP])dnl
AC_ENABLE_STATIC(no)])


# AC_ENABLE_FAST_INSTALL - implement the --enable-fast-install flag
# Usage: AC_ENABLE_FAST_INSTALL[(DEFAULT)]
#   Where DEFAULT is either `yes' or `no'.  If omitted, it defaults to
#   `yes'.
AC_DEFUN([AC_ENABLE_FAST_INSTALL],
[define([AC_ENABLE_FAST_INSTALL_DEFAULT], ifelse($1, no, no, yes))dnl
AC_ARG_ENABLE(fast-install,
changequote(<<, >>)dnl
<<  --enable-fast-install[=PKGS]  optimize for fast installation [default=>>AC_ENABLE_FAST_INSTALL_DEFAULT],
changequote([, ])dnl
[p=${PACKAGE-default}
case $enableval in
yes) enable_fast_install=yes ;;
no) enable_fast_install=no ;;
*)
  enable_fast_install=no
  # Look at the argument we got.  We use all the common list separators.
  IFS="${IFS= 	}"; ac_save_ifs="$IFS"; IFS="${IFS}:,"
  for pkg in $enableval; do
    if test "X$pkg" = "X$p"; then
      enable_fast_install=yes
    fi
  done
  IFS="$ac_save_ifs"
  ;;
esac],
enable_fast_install=AC_ENABLE_FAST_INSTALL_DEFAULT)dnl
])

# AC_DISABLE_FAST_INSTALL - set the default to --disable-fast-install
AC_DEFUN([AC_DISABLE_FAST_INSTALL],
[AC_BEFORE([$0],[AC_LIBTOOL_SETUP])dnl
AC_ENABLE_FAST_INSTALL(no)])

# AC_LIBTOOL_PICMODE - implement the --with-pic flag
# Usage: AC_LIBTOOL_PICMODE[(MODE)]
#   Where MODE is either `yes' or `no'.  If omitted, it defaults to
#   `both'.
AC_DEFUN([AC_LIBTOOL_PICMODE],
[AC_BEFORE([$0],[AC_LIBTOOL_SETUP])dnl
pic_mode=ifelse($#,1,$1,default)])


# AC_PATH_TOOL_PREFIX - find a file program which can recognise shared library
AC_DEFUN([AC_PATH_TOOL_PREFIX],
[AC_MSG_CHECKING([for $1])
AC_CACHE_VAL(lt_cv_path_MAGIC_CMD,
[case $MAGIC_CMD in
  /*)
  lt_cv_path_MAGIC_CMD="$MAGIC_CMD" # Let the user override the test with a path.
  ;;
  ?:/*)
  lt_cv_path_MAGIC_CMD="$MAGIC_CMD" # Let the user override the test with a dos path.
  ;;
  *)
  ac_save_MAGIC_CMD="$MAGIC_CMD"
  IFS="${IFS=   }"; ac_save_ifs="$IFS"; IFS=":"
dnl $ac_dummy forces splitting on constant user-supplied paths.
dnl POSIX.2 word splitting is done only on the output of word expansions,
dnl not every word.  This closes a longstanding sh security hole.
  ac_dummy="ifelse([$2], , $PATH, [$2])"
  for ac_dir in $ac_dummy; do
    test -z "$ac_dir" && ac_dir=.
    if test -f $ac_dir/$1; then
      lt_cv_path_MAGIC_CMD="$ac_dir/$1"
      if test -n "$file_magic_test_file"; then
	case $deplibs_check_method in
	"file_magic "*)
	  file_magic_regex="`expr \"$deplibs_check_method\" : \"file_magic \(.*\)\"`"
	  MAGIC_CMD="$lt_cv_path_MAGIC_CMD"
	  if eval $file_magic_cmd \$file_magic_test_file 2> /dev/null |
	    egrep "$file_magic_regex" > /dev/null; then
	    :
	  else
	    cat <<EOF 1>&2

*** Warning: the command libtool uses to detect shared libraries,
*** $file_magic_cmd, produces output that libtool cannot recognize.
*** The result is that libtool may fail to recognize shared libraries
*** as such.  This will affect the creation of libtool libraries that
*** depend on shared libraries, but programs linked with such libtool
*** libraries will work regardless of this problem.  Nevertheless, you
*** may want to report the problem to your system manager and/or to
*** bug-libtool@gnu.org

EOF
	  fi ;;
	esac
      fi
      break
    fi
  done
  IFS="$ac_save_ifs"
  MAGIC_CMD="$ac_save_MAGIC_CMD"
  ;;
esac])
MAGIC_CMD="$lt_cv_path_MAGIC_CMD"
if test -n "$MAGIC_CMD"; then
  AC_MSG_RESULT($MAGIC_CMD)
else
  AC_MSG_RESULT(no)
fi
])


# AC_PATH_MAGIC - find a file program which can recognise a shared library
AC_DEFUN([AC_PATH_MAGIC],
[AC_REQUIRE([AC_CHECK_TOOL_PREFIX])dnl
AC_PATH_TOOL_PREFIX(${ac_tool_prefix}file, /usr/bin:$PATH)
if test -z "$lt_cv_path_MAGIC_CMD"; then
  if test -n "$ac_tool_prefix"; then
    AC_PATH_TOOL_PREFIX(file, /usr/bin:$PATH)
  else
    MAGIC_CMD=:
  fi
fi
])


# AC_PROG_LD - find the path to the GNU or non-GNU linker
AC_DEFUN([AC_PROG_LD],
[AC_ARG_WITH(gnu-ld,
[  --with-gnu-ld           assume the C compiler uses GNU ld [default=no]],
test "$withval" = no || with_gnu_ld=yes, with_gnu_ld=no)
AC_REQUIRE([AC_PROG_CC])dnl
AC_REQUIRE([AC_CANONICAL_HOST])dnl
AC_REQUIRE([AC_CANONICAL_BUILD])dnl
AC_REQUIRE([_LT_AC_LIBTOOL_SYS_PATH_SEPARATOR])dnl
ac_prog=ld
if test "$GCC" = yes; then
  # Check if gcc -print-prog-name=ld gives a path.
  AC_MSG_CHECKING([for ld used by GCC])
  case $host in
  *-*-mingw*)
    # gcc leaves a trailing carriage return which upsets mingw
    ac_prog=`($CC -print-prog-name=ld) 2>&5 | tr -d '\015'` ;;
  *)
    ac_prog=`($CC -print-prog-name=ld) 2>&5` ;;
  esac
  case $ac_prog in
    # Accept absolute paths.
    [[\\/]]* | [[A-Za-z]]:[[\\/]]*)
      re_direlt='/[[^/]][[^/]]*/\.\./'
      # Canonicalize the path of ld
      ac_prog=`echo $ac_prog| sed 's%\\\\%/%g'`
      while echo $ac_prog | grep "$re_direlt" > /dev/null 2>&1; do
	ac_prog=`echo $ac_prog| sed "s%$re_direlt%/%"`
      done
      test -z "$LD" && LD="$ac_prog"
      ;;
  "")
    # If it fails, then pretend we aren't using GCC.
    ac_prog=ld
    ;;
  *)
    # If it is relative, then search for the first ld in PATH.
    with_gnu_ld=unknown
    ;;
  esac
elif test "$with_gnu_ld" = yes; then
  AC_MSG_CHECKING([for GNU ld])
else
  AC_MSG_CHECKING([for non-GNU ld])
fi
AC_CACHE_VAL(lt_cv_path_LD,
[if test -z "$LD"; then
  IFS="${IFS= 	}"; ac_save_ifs="$IFS"; IFS=$PATH_SEPARATOR
  for ac_dir in $PATH; do
    test -z "$ac_dir" && ac_dir=.
    if test -f "$ac_dir/$ac_prog" || test -f "$ac_dir/$ac_prog$ac_exeext"; then
      lt_cv_path_LD="$ac_dir/$ac_prog"
      # Check to see if the program is GNU ld.  I'd rather use --version,
      # but apparently some GNU ld's only accept -v.
      # Break only if it was the GNU/non-GNU ld that we prefer.
      if "$lt_cv_path_LD" -v 2>&1 < /dev/null | egrep '(GNU|with BFD)' > /dev/null; then
	test "$with_gnu_ld" != no && break
      else
	test "$with_gnu_ld" != yes && break
      fi
    fi
  done
  IFS="$ac_save_ifs"
else
  lt_cv_path_LD="$LD" # Let the user override the test with a path.
fi])
LD="$lt_cv_path_LD"
if test -n "$LD"; then
  AC_MSG_RESULT($LD)
else
  AC_MSG_RESULT(no)
fi
test -z "$LD" && AC_MSG_ERROR([no acceptable ld found in \$PATH])
AC_PROG_LD_GNU
])

# AC_PROG_LD_GNU -
AC_DEFUN([AC_PROG_LD_GNU],
[AC_CACHE_CHECK([if the linker ($LD) is GNU ld], lt_cv_prog_gnu_ld,
[# I'd rather use --version here, but apparently some GNU ld's only accept -v.
if $LD -v 2>&1 </dev/null | egrep '(GNU|with BFD)' 1>&5; then
  lt_cv_prog_gnu_ld=yes
else
  lt_cv_prog_gnu_ld=no
fi])
with_gnu_ld=$lt_cv_prog_gnu_ld
])

# AC_PROG_LD_RELOAD_FLAG - find reload flag for linker
#   -- PORTME Some linkers may need a different reload flag.
AC_DEFUN([AC_PROG_LD_RELOAD_FLAG],
[AC_CACHE_CHECK([for $LD option to reload object files], lt_cv_ld_reload_flag,
[lt_cv_ld_reload_flag='-r'])
reload_flag=$lt_cv_ld_reload_flag
test -n "$reload_flag" && reload_flag=" $reload_flag"
])

# AC_DEPLIBS_CHECK_METHOD - how to check for library dependencies
#  -- PORTME fill in with the dynamic library characteristics
AC_DEFUN([AC_DEPLIBS_CHECK_METHOD],
[AC_CACHE_CHECK([how to recognise dependant libraries],
lt_cv_deplibs_check_method,
[lt_cv_file_magic_cmd='$MAGIC_CMD'
lt_cv_file_magic_test_file=
lt_cv_deplibs_check_method='unknown'
# Need to set the preceding variable on all platforms that support
# interlibrary dependencies.
# 'none' -- dependencies not supported.
# `unknown' -- same as none, but documents that we really don't know.
# 'pass_all' -- all dependencies passed with no checks.
# 'test_compile' -- check by making test program.
# 'file_magic [[regex]]' -- check by looking for files in library path
# which responds to the $file_magic_cmd with a given egrep regex.
# If you have `file' or equivalent on your system and you're not sure
# whether `pass_all' will *always* work, you probably want this one.

case $host_os in
aix4* | aix5*)
  lt_cv_deplibs_check_method=pass_all
  ;;

beos*)
  lt_cv_deplibs_check_method=pass_all
  ;;

bsdi4*)
  lt_cv_deplibs_check_method='file_magic ELF [[0-9]][[0-9]]*-bit [[ML]]SB (shared object|dynamic lib)'
  lt_cv_file_magic_cmd='/usr/bin/file -L'
  lt_cv_file_magic_test_file=/shlib/libc.so
  ;;

cygwin* | mingw* | pw32*)
  lt_cv_deplibs_check_method='file_magic file format pei*-i386(.*architecture: i386)?'
  lt_cv_file_magic_cmd='$OBJDUMP -f'
  ;;

darwin* | rhapsody*)
  lt_cv_deplibs_check_method='file_magic Mach-O dynamically linked shared library'
  lt_cv_file_magic_cmd='/usr/bin/file -L'
  case "$host_os" in
  rhapsody* | darwin1.[[012]])
    lt_cv_file_magic_test_file=`echo /System/Library/Frameworks/System.framework/Versions/*/System | head -1`
    ;;
  *) # Darwin 1.3 on
    lt_cv_file_magic_test_file='/usr/lib/libSystem.dylib'
    ;;
  esac
  ;;

freebsd*)
  if echo __ELF__ | $CC -E - | grep __ELF__ > /dev/null; then
    case $host_cpu in
    i*86 )
      # Not sure whether the presence of OpenBSD here was a mistake.
      # Let's accept both of them until this is cleared up.
      lt_cv_deplibs_check_method='file_magic (FreeBSD|OpenBSD)/i[[3-9]]86 (compact )?demand paged shared library'
      lt_cv_file_magic_cmd=/usr/bin/file
      lt_cv_file_magic_test_file=`echo /usr/lib/libc.so.*`
      ;;
    esac
  else
    lt_cv_deplibs_check_method=pass_all
  fi
  ;;

gnu*)
  lt_cv_deplibs_check_method=pass_all
  ;;

hpux10.20*|hpux11*)
  lt_cv_deplibs_check_method='file_magic (s[[0-9]][[0-9]][[0-9]]|PA-RISC[[0-9]].[[0-9]]) shared library'
  lt_cv_file_magic_cmd=/usr/bin/file
  lt_cv_file_magic_test_file=/usr/lib/libc.sl
  ;;

irix5* | irix6*)
  case $host_os in
  irix5*)
    # this will be overridden with pass_all, but let us keep it just in case
    lt_cv_deplibs_check_method="file_magic ELF 32-bit MSB dynamic lib MIPS - version 1"
    ;;
  *)
    case $LD in
    *-32|*"-32 ") libmagic=32-bit;;
    *-n32|*"-n32 ") libmagic=N32;;
    *-64|*"-64 ") libmagic=64-bit;;
    *) libmagic=never-match;;
    esac
    # this will be overridden with pass_all, but let us keep it just in case
    lt_cv_deplibs_check_method="file_magic ELF ${libmagic} MSB mips-[[1234]] dynamic lib MIPS - version 1"
    ;;
  esac
  lt_cv_file_magic_test_file=`echo /lib${libsuff}/libc.so*`
  lt_cv_deplibs_check_method=pass_all
  ;;

# This must be Linux ELF.
linux-gnu*)
  case $host_cpu in
  alpha* | hppa* | i*86 | powerpc* | sparc* | ia64* )
    lt_cv_deplibs_check_method=pass_all ;;
  *)
    # glibc up to 2.1.1 does not perform some relocations on ARM
    lt_cv_deplibs_check_method='file_magic ELF [[0-9]][[0-9]]*-bit [[LM]]SB (shared object|dynamic lib )' ;;
  esac
  lt_cv_file_magic_test_file=`echo /lib/libc.so* /lib/libc-*.so`
  ;;

netbsd*)
  if echo __ELF__ | $CC -E - | grep __ELF__ > /dev/null; then
    lt_cv_deplibs_check_method='match_pattern /lib[[^/\.]]+\.so\.[[0-9]]+\.[[0-9]]+$'
  else
    lt_cv_deplibs_check_method='match_pattern /lib[[^/\.]]+\.so$'
  fi
  ;;

newos6*)
  lt_cv_deplibs_check_method='file_magic ELF [[0-9]][[0-9]]*-bit [[ML]]SB (executable|dynamic lib)'
  lt_cv_file_magic_cmd=/usr/bin/file
  lt_cv_file_magic_test_file=/usr/lib/libnls.so
  ;;

openbsd*)
  lt_cv_file_magic_cmd=/usr/bin/file
  lt_cv_file_magic_test_file=`echo /usr/lib/libc.so.*`
  if test -z "`echo __ELF__ | $CC -E - | grep __ELF__`" || test "$host_os-$host_cpu" = "openbsd2.8-powerpc"; then
    lt_cv_deplibs_check_method='file_magic ELF [[0-9]][[0-9]]*-bit [[LM]]SB shared object'
  else
    lt_cv_deplibs_check_method='file_magic OpenBSD.* shared library'
  fi
  ;;

osf3* | osf4* | osf5*)
  # this will be overridden with pass_all, but let us keep it just in case
  lt_cv_deplibs_check_method='file_magic COFF format alpha shared library'
  lt_cv_file_magic_test_file=/shlib/libc.so
  lt_cv_deplibs_check_method=pass_all
  ;;

sco3.2v5*)
  lt_cv_deplibs_check_method=pass_all
  ;;

solaris*)
  lt_cv_deplibs_check_method=pass_all
  lt_cv_file_magic_test_file=/lib/libc.so
  ;;

sysv5uw[[78]]* | sysv4*uw2* | sysv5Open*)
  lt_cv_deplibs_check_method=pass_all
  ;;

sysv4 | sysv4.2uw2* | sysv4.3* | sysv5*)
  case $host_vendor in
  motorola)
    lt_cv_deplibs_check_method='file_magic ELF [[0-9]][[0-9]]*-bit [[ML]]SB (shared object|dynamic lib) M[[0-9]][[0-9]]* Version [[0-9]]'
    lt_cv_file_magic_test_file=`echo /usr/lib/libc.so*`
    ;;
  ncr)
    lt_cv_deplibs_check_method=pass_all
    ;;
  sequent)
    lt_cv_file_magic_cmd='/bin/file'
    lt_cv_deplibs_check_method='file_magic ELF [[0-9]][[0-9]]*-bit [[LM]]SB (shared object|dynamic lib )'
    ;;
  sni)
    lt_cv_file_magic_cmd='/bin/file'
    lt_cv_deplibs_check_method="file_magic ELF [[0-9]][[0-9]]*-bit [[LM]]SB dynamic lib"
    lt_cv_file_magic_test_file=/lib/libc.so
    ;;
  esac
  ;;
esac
])
file_magic_cmd=$lt_cv_file_magic_cmd
deplibs_check_method=$lt_cv_deplibs_check_method
])


# AC_PROG_NM - find the path to a BSD-compatible name lister
AC_DEFUN([AC_PROG_NM],
[AC_REQUIRE([_LT_AC_LIBTOOL_SYS_PATH_SEPARATOR])dnl
AC_MSG_CHECKING([for BSD-compatible nm])
AC_CACHE_VAL(lt_cv_path_NM,
[if test -n "$NM"; then
  # Let the user override the test.
  lt_cv_path_NM="$NM"
else
  IFS="${IFS= 	}"; ac_save_ifs="$IFS"; IFS=$PATH_SEPARATOR
  for ac_dir in $PATH /usr/ccs/bin /usr/ucb /bin; do
    test -z "$ac_dir" && ac_dir=.
    tmp_nm=$ac_dir/${ac_tool_prefix}nm
    if test -f $tmp_nm || test -f $tmp_nm$ac_exeext ; then
      # Check to see if the nm accepts a BSD-compat flag.
      # Adding the `sed 1q' prevents false positives on HP-UX, which says:
      #   nm: unknown option "B" ignored
      # Tru64's nm complains that /dev/null is an invalid object file
      if ($tmp_nm -B /dev/null 2>&1 | sed '1q'; exit 0) | egrep '(/dev/null|Invalid file or object type)' >/dev/null; then
	lt_cv_path_NM="$tmp_nm -B"
	break
      elif ($tmp_nm -p /dev/null 2>&1 | sed '1q'; exit 0) | egrep /dev/null >/dev/null; then
	lt_cv_path_NM="$tmp_nm -p"
	break
      else
	lt_cv_path_NM=${lt_cv_path_NM="$tmp_nm"} # keep the first match, but
	continue # so that we can try to find one that supports BSD flags
      fi
    fi
  done
  IFS="$ac_save_ifs"
  test -z "$lt_cv_path_NM" && lt_cv_path_NM=nm
fi])
NM="$lt_cv_path_NM"
AC_MSG_RESULT([$NM])
])

# AC_CHECK_LIBM - check for math library
AC_DEFUN([AC_CHECK_LIBM],
[AC_REQUIRE([AC_CANONICAL_HOST])dnl
LIBM=
case $host in
*-*-beos* | *-*-cygwin* | *-*-pw32*)
  # These system don't have libm
  ;;
*-ncr-sysv4.3*)
  AC_CHECK_LIB(mw, _mwvalidcheckl, LIBM="-lmw")
  AC_CHECK_LIB(m, main, LIBM="$LIBM -lm")
  ;;
*)
  AC_CHECK_LIB(m, main, LIBM="-lm")
  ;;
esac
])

# AC_LIBLTDL_CONVENIENCE[(dir)] - sets LIBLTDL to the link flags for
# the libltdl convenience library and INCLTDL to the include flags for
# the libltdl header and adds --enable-ltdl-convenience to the
# configure arguments.  Note that LIBLTDL and INCLTDL are not
# AC_SUBSTed, nor is AC_CONFIG_SUBDIRS called.  If DIR is not
# provided, it is assumed to be `libltdl'.  LIBLTDL will be prefixed
# with '${top_builddir}/' and INCLTDL will be prefixed with
# '${top_srcdir}/' (note the single quotes!).  If your package is not
# flat and you're not using automake, define top_builddir and
# top_srcdir appropriately in the Makefiles.
AC_DEFUN([AC_LIBLTDL_CONVENIENCE],
[AC_BEFORE([$0],[AC_LIBTOOL_SETUP])dnl
  case $enable_ltdl_convenience in
  no) AC_MSG_ERROR([this package needs a convenience libltdl]) ;;
  "") enable_ltdl_convenience=yes
      ac_configure_args="$ac_configure_args --enable-ltdl-convenience" ;;
  esac
  LIBLTDL='${top_builddir}/'ifelse($#,1,[$1],['libltdl'])/libltdlc.la
  INCLTDL='-I${top_srcdir}/'ifelse($#,1,[$1],['libltdl'])
])

# AC_LIBLTDL_INSTALLABLE[(dir)] - sets LIBLTDL to the link flags for
# the libltdl installable library and INCLTDL to the include flags for
# the libltdl header and adds --enable-ltdl-install to the configure
# arguments.  Note that LIBLTDL and INCLTDL are not AC_SUBSTed, nor is
# AC_CONFIG_SUBDIRS called.  If DIR is not provided and an installed
# libltdl is not found, it is assumed to be `libltdl'.  LIBLTDL will
# be prefixed with '${top_builddir}/' and INCLTDL will be prefixed
# with '${top_srcdir}/' (note the single quotes!).  If your package is
# not flat and you're not using automake, define top_builddir and
# top_srcdir appropriately in the Makefiles.
# In the future, this macro may have to be called after AC_PROG_LIBTOOL.
AC_DEFUN([AC_LIBLTDL_INSTALLABLE],
[AC_BEFORE([$0],[AC_LIBTOOL_SETUP])dnl
  AC_CHECK_LIB(ltdl, main,
  [test x"$enable_ltdl_install" != xyes && enable_ltdl_install=no],
  [if test x"$enable_ltdl_install" = xno; then
     AC_MSG_WARN([libltdl not installed, but installation disabled])
   else
     enable_ltdl_install=yes
   fi
  ])
  if test x"$enable_ltdl_install" = x"yes"; then
    ac_configure_args="$ac_configure_args --enable-ltdl-install"
    LIBLTDL='${top_builddir}/'ifelse($#,1,[$1],['libltdl'])/libltdl.la
    INCLTDL='-I${top_srcdir}/'ifelse($#,1,[$1],['libltdl'])
  else
    ac_configure_args="$ac_configure_args --enable-ltdl-install=no"
    LIBLTDL="-lltdl"
    INCLTDL=
  fi
])

# old names
AC_DEFUN([AM_PROG_LIBTOOL],   [AC_PROG_LIBTOOL])
AC_DEFUN([AM_ENABLE_SHARED],  [AC_ENABLE_SHARED($@)])
AC_DEFUN([AM_ENABLE_STATIC],  [AC_ENABLE_STATIC($@)])
AC_DEFUN([AM_DISABLE_SHARED], [AC_DISABLE_SHARED($@)])
AC_DEFUN([AM_DISABLE_STATIC], [AC_DISABLE_STATIC($@)])
AC_DEFUN([AM_PROG_LD],        [AC_PROG_LD])
AC_DEFUN([AM_PROG_NM],        [AC_PROG_NM])

# This is just to silence aclocal about the macro not being used
ifelse([AC_DISABLE_FAST_INSTALL])

# Do all the work for Automake.  This macro actually does too much --
# some checks are only needed if your package does certain things.
# But this isn't really a big deal.

# serial 1

dnl Usage:
dnl AM_INIT_AUTOMAKE(package,version, [no-define])

AC_DEFUN(AM_INIT_AUTOMAKE,
[AC_REQUIRE([AC_PROG_INSTALL])
PACKAGE=[$1]
AC_SUBST(PACKAGE)
VERSION=[$2]
AC_SUBST(VERSION)
dnl test to see if srcdir already configured
if test "`cd $srcdir && pwd`" != "`pwd`" && test -f $srcdir/config.status; then
  AC_MSG_ERROR([source directory already configured; run "make distclean" there first])
fi
ifelse([$3],,
AC_DEFINE_UNQUOTED(PACKAGE, "$PACKAGE", [Name of package])
AC_DEFINE_UNQUOTED(VERSION, "$VERSION", [Version number of package]))
AC_REQUIRE([AM_SANITY_CHECK])
AC_REQUIRE([AC_ARG_PROGRAM])
dnl FIXME This is truly gross.
missing_dir=`cd $ac_aux_dir && pwd`
AM_MISSING_PROG(ACLOCAL, aclocal, $missing_dir)
AM_MISSING_PROG(AUTOCONF, autoconf, $missing_dir)
AM_MISSING_PROG(AUTOMAKE, automake, $missing_dir)
AM_MISSING_PROG(AUTOHEADER, autoheader, $missing_dir)
AM_MISSING_PROG(MAKEINFO, makeinfo, $missing_dir)
AC_REQUIRE([AC_PROG_MAKE_SET])])

#
# Check to make sure that the build environment is sane.
#

AC_DEFUN(AM_SANITY_CHECK,
[AC_MSG_CHECKING([whether build environment is sane])
# Just in case
sleep 1
echo timestamp > conftestfile
# Do `set' in a subshell so we don't clobber the current shell's
# arguments.  Must try -L first in case configure is actually a
# symlink; some systems play weird games with the mod time of symlinks
# (eg FreeBSD returns the mod time of the symlink's containing
# directory).
if (
   set X `ls -Lt $srcdir/configure conftestfile 2> /dev/null`
   if test "[$]*" = "X"; then
      # -L didn't work.
      set X `ls -t $srcdir/configure conftestfile`
   fi
   if test "[$]*" != "X $srcdir/configure conftestfile" \
      && test "[$]*" != "X conftestfile $srcdir/configure"; then

      # If neither matched, then we have a broken ls.  This can happen
      # if, for instance, CONFIG_SHELL is bash and it inherits a
      # broken ls alias from the environment.  This has actually
      # happened.  Such a system could not be considered "sane".
      AC_MSG_ERROR([ls -t appears to fail.  Make sure there is not a broken
alias in your environment])
   fi

   test "[$]2" = conftestfile
   )
then
   # Ok.
   :
else
   AC_MSG_ERROR([newly created file is older than distributed files!
Check your system clock])
fi
rm -f conftest*
AC_MSG_RESULT(yes)])

dnl AM_MISSING_PROG(NAME, PROGRAM, DIRECTORY)
dnl The program must properly implement --version.
AC_DEFUN(AM_MISSING_PROG,
[AC_MSG_CHECKING(for working $2)
# Run test in a subshell; some versions of sh will print an error if
# an executable is not found, even if stderr is redirected.
# Redirect stdin to placate older versions of autoconf.  Sigh.
if ($2 --version) < /dev/null > /dev/null 2>&1; then
   $1=$2
   AC_MSG_RESULT(found)
else
   $1="$3/missing $2"
   AC_MSG_RESULT(missing)
fi
AC_SUBST($1)])


dnl AM_PROG_LEX
dnl Look for flex, lex or missing, then run AC_PROG_LEX and AC_DECL_YYTEXT
AC_DEFUN(AM_PROG_LEX,
[missing_dir=ifelse([$1],,`cd $ac_aux_dir && pwd`,$1)
AC_CHECK_PROGS(LEX, flex lex, "$missing_dir/missing flex")
AC_PROG_LEX
AC_DECL_YYTEXT])

# Define a conditional.

AC_DEFUN(AM_CONDITIONAL,
[AC_SUBST($1_TRUE)
AC_SUBST($1_FALSE)
if $2; then
  $1_TRUE=
  $1_FALSE='#'
else
  $1_TRUE='#'
  $1_FALSE=
fi])

