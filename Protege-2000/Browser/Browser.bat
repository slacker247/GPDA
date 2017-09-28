@echo off
set JAVA_PATH=jre\bin

set JARS=browser.jar;protege.jar;grappa1_2.jar;ontoviz.jar;pal.jar
set MAXIMUM_MEMORY=-Xmx100M
set MAIN_CLASS=browser.Browser

set OPTIONS=%MAXIMUM_MEMORY%

%JAVA_PATH%\java %OPTIONS% -cp %JARS% %MAIN_CLASS% %1
