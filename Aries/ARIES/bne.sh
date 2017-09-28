#!/bin/bash

#
# launch script for the BNE (formerly HMV)
# usage:
#   ./bne.sh [launches using classes in build/classes directory]
#   ./bne.sh usejar  [launches using jar file in build/jars]
#

currentDir=`pwd`
javaDir=$JAVA_HOME/jre/bin
classesDir=$currentDir/build/classes
jarDir=$currentDir/build/jars
libDir=$currentDir/build/lib

# make sure the JAVA_HOME exists
if [ ! -e "$javaDir" ]
then
    echo "*** ERROR ***"
    echo "The JAVA_HOME environment variable has not been set, or the"
    echo "$JAVA_HOME/jre/bin directory does not exist"
    exit 1
fi

# uname returns:
#
# `Linux` on Red Hat 7.3
# `CYGWIN_NT-5.0` on my Windows 2000 laptop with Cygwin
# `Darwin` on Mac OS X

if [ `uname` = "Linux" ]
then
    SEP=":"
else
    if [ `uname` = "Darwin" ]
    then
	SEP=":"
    else
	SEP="\;"
    fi
fi

classpath=./build/lib/am_core.jar${SEP}./build/lib/jdom.jar${SEP}./build/lib/royere.jar



if [ "$1" = "usejar" ]
then
    echo "Launching app from jar file"
    classpath=./build/jars/bne.jar${SEP}$classpath
    echo "classpath: " $classpath
    # this removes 'usejar' from the command line
    shift
    echo ""
else
    echo "Launching app from class files"
    classpath=./build/classes${SEP}$classpath
    echo "classpath: " $classpath
    echo ""
fi


# To specify mozilla binary use -Dmozilla=/path/to/mozilla-bin as the
# first argument. This is default to /usr/local/mozilla/mozilla.  
#



if [ $# -eq 1 ]; then
    opt="$1"
else
    opt=""
fi


$javaDir/java -client -Xmx256m -Xss1024k -cp $classpath $opt com.appliedminds.hmv.HMV



