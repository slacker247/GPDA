#!/bin/sh

cd $HOME/Protege-2000/Oil

CLASSPATH=".:libs/fact-client.jar:libs/jgl3.1.0.jar:libs/oil.jar:libs/rdf-api.jar:libs/resources.jar:libs/xerces.jar"

java -cp $CLASSPATH uk.ac.man.cs.img.oil.ui.OilEd
