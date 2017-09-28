#!/bin/sh

cd $HOME/Protege-2000

DISPLAY=localhost:19
java -Xmx200M -cp protege.jar edu.stanford.smi.protege.Application
