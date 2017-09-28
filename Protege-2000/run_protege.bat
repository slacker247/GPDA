@echo off
set JAVA_PATH=.\jre\bin
set JARS=protege.jar
set MAXIMUM_MEMORY=-Xmx200M
set MAIN_CLASS=edu.stanford.smi.protege.Application

set OPTIONS=%MAXIMUM_MEMORY%

%JAVA_PATH%\java %OPTIONS% -cp %JARS% %MAIN_CLASS% %1
