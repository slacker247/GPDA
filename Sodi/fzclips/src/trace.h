#include <stdio.h>
#include "clips.h"

int FindTrace (char *logicalName);
int PrintTrace (char *logicalName, char *str);
int ExitTrace (int exitCode);

int TraceOn ();
int TraceOff ();
