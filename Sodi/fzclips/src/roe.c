#include "clips.h"


void *roe_assess ()
{
  double lat, lon;
  char *tempString;
  void *returnValue;
  
  lat = RtnDouble (1);
  lon = RtnDouble (2);

   if (lat > 25.00 &lat < 48.55 & lon > 65.50 & lon < 125.00)
      tempString = "CONUS";

  else
    if (lat > 20.00 & lat < 72 & lon > 55.00 & lon < 170.00 & tempString != "CONUS")
    tempString = "N. America";

   
  else 
    tempString = "";

  returnValue = AddSymbol (tempString);
  return (returnValue);

}
