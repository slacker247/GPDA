#!/bin/sh
#
echo "Building TAR archive"
tar -cvf $1 Sodi bin gv1.7c Protege-2000 AssistRQ WekaVis 
#
echo "Compressing archive"
compress $1
#
ls -l *.Z
