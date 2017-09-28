#!/bin/sh
# This is an example UNIX shell script for running
# Protege-2000 from the command line.  It assumes that
# you have a Java JRE installed somewhere.


# Note that this file is not used when you run Protege "normally"
# by, for example, double clicking on a .pprj file.


cd "`dirname \"$0\"`"
JAVA_PATH=/usr/bin
JARS=protege.jar
MAXIMUM_MEMORY=-Xmx200M
MAIN_CLASS=edu.stanford.smi.protege.Application


OPTIONS=$MAXIMUM_MEMORY


$JAVA_PATH/java $OPTIONS -cp $JARS $MAIN_CLASS $1