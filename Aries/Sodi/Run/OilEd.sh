#!/bin/sh

cd $HOME/Protege-New/Oil-2.2

CLASSPATH=".:libs/fact-client.jar:libs/jgl3.1.0.jar:libs/oil.jar:libs/rdf-api.jar:libs/resources.jar:libs/xerces.jar"

java -cp $CLASSPATH uk.ac.man.cs.img.oil.ui.OilEd
