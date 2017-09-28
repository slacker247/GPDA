#include <stdio.h>
#include "clips.h"

static FILE *TraceFP = NULL;

int FindTrace (char *logicalName)
{
  if (strcmp (logicalName, "wtrace") == 0) return (TRUE);

  return (FALSE);
}

int PrintTrace (char *logicalName, char *str)
{
  fprintf (TraceFP, "%s", str);
}

int ExitTrace (int exitCode)
{
  fclose (TraceFP);
}

int TraceOn ()
{
  if (TraceFP == NULL)
    {
      TraceFP = fopen ("trace.txt", "w");
      if (TraceFP == NULL) return (FALSE);
    }
  else
    { return (FALSE);}

  AddRouter ("trace",
	     20,
	     FindTrace,
	     PrintTrace,
	     NULL,
	     NULL,
	     ExitTrace);

  return (TRUE);
}

int TraceOff ()
{
  if (TraceFP != NULL)
    {
      DeleteRouter ("trace");
      
      if (fclose (TraceFP) == 0)
	{
	  TraceFP = NULL;
	  return (TRUE);
	}
    }

  TraceFP = NULL;
  return (FALSE);
}
