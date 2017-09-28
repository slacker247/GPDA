#!/bin/sh

cd ${HOME}/AssistRQ

GATE_HOME=${HOME}/Gate_2.1-beta1
JAVA_HOME=${HOME}/Gate_2.1-beta1/jre1.4
TOOLSJAR=${HOME}/Gate_2.1-beta1/bin/tools14.jar
AssistRQ_HOME=${HOME}/AssistRQ
# Flags will equal the home dir
FLAGS="$HOME 1"

# set TOOLSJAR to where we hope tools.jar is are located
[ x${TOOLSJAR} = x ] &&
[ -f ${JAVA_HOME}/lib/tools.jar ] && TOOLSJAR=${JAVA_HOME}/lib/tools.jar

[ x${TOOLSJAR} = x ] && TOOLSJAR=${GATE_HOME}/bin/tools14.jar

# set GATEJAR and EXTDIR to gate.jar and ext locations
[ x${GATEJAR} = x ] && [ -f ${GATE_HOME}/bin/gate.jar ] &&
GATEJAR=${GATE_HOME}/bin/gate.jar

[ x${GATEJAR} = x ] && [ -f ${GATE_HOME}/build/gate.jar ] &&
GATEJAR=${GATE_HOME}/build/gate.jar

EXTDIR=

[ x${EXTDIR} = x ] &&
[ -f ${GATE_HOME}/bin/ext/guk.jar ] && EXTDIR=${GATE_HOME}/bin/ext

[ x${EXTDIR} = x ] &&
[ -f ${GATE_HOME}/lib/ext/guk.jar ] && EXTDIR=${GATE_HOME}/lib/ext

# set JAVA
[ x${JAVA} = x ] &&
[ -f ${JAVA_HOME}/bin/java ] && JAVA=${JAVA_HOME}/bin/java

[ x${JAVA} = x ] &&
[ -f ${JAVA_HOME}/bin/java.exe ] && JAVA=${JAVA_HOME}/bin/java.exe

[ x${JAVA} = x ] &&
[ -f ${GATE_HOME}/jre1.4/bin/java ] && JAVA=${GATE_HOME}/jre1.4/bin/java

[ x${JAVA} = x ] &&
[ -f ${GATE_HOME}/jre1.4/bin/java.exe ] && JAVA=${GATE_HOME}/jre1.4/bin/java.exe

[ x${JAVA} = x ] && JAVA=java


# set CLASSPATH
OLD_CLASSPATH=$CLASSPATH
CLASSPATH="${GATEJAR}:${TOOLSJAR}:${AssistRQ_HOME}:$CLASSPATH"

# run the beast
$E ${JAVA} -Xmx200m \
  -Djava.ext.dirs=${EXTDIR} -classpath $CLASSPATH AssistRQ $FLAGS
