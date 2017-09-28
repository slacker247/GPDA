
#ifndef _DS_CONFIG_H
#define _DS_CONFIG_H

//------------------------------------------------------------------------------

const int  DS_Max_Possibilities = 9;    // 9 possibility value ranges

//------------------------------------------------------------------------------

typedef struct {
  double  rangeStart;
  double  rangeEnd;
  char    string[256];
} DS_rangeRecType;

//------------------------------------------------------------------------------

extern char    DS_defTimeUnit;
extern char    DS_evidLabel1[8];
extern char    DS_evidLabel2[8];
extern char    DS_evidLabel3[8];
extern char    DS_startTime[32];
extern char    DS_endTime[32];

extern int     DS_isDiscrete;
extern int     DS_numPossibilities;

extern DS_rangeRecType  DS_possibilities[DS_Max_Possibilities];

//------------------------------------------------------------------------------

extern void DS_AlgoPrintConfig(void);

  // DS_AlgoPrintConfig prints configuration data to stdout.


extern void DS_AlgoInitConfig(void);

  // DS_AlgoInitConfig initializes DS configuration data to default values.


extern int DS_AlgoReadConfig
  (char *fileName,       // In - Path to config file (may be NULL)
   int  mayUseDefault);  // In - TRUE = default file used if file not found

  // DS_AlgoReadConfig reads from the given file configuration data for
  // the Dempster-Shafer algo program.  If the file is not found, uses the
  // default config file.  Returns 0 on success, -1 on error.


extern int DS_AlgoSaveConfig
  (char *fileName);  // In - Path to config file

  // DS_AlgoSaveConfig saves the DS configuration data to the given file.
  // Returns 0 on success, -1 on failure.


extern int DS_AlgoIsDiscrete (void);


extern void DS_AlgoSetDiscrete
  (int discrete);  // In - 0 = FALSE, 1 = TRUE


extern int DS_AlgoGetTimeRange
  (char  *timeStart,  // Out
   char  *timeEnd);   // Out


extern int DS_AlgoSetTimeRange
  (char  *timeStart,  // In
   char  *timeEnd);   // In


extern char DS_AlgoGetDefaultTimeUnit (void);


extern int DS_AlgoSetDefaultTimeUnit
  (char timeUnit);  // In

  // DS_AlgoSetDefaultTimeUnit sets the default time unit to that specified.
  // Should be D (day), H (hour), M (minute), or S (second).  Returns 0 on
  // success, -1 on error.


extern int DS_AlgoGetEvidenceLabels
  (char  *label1,   // Out - Label copied to parameter array
   char  *label2,   // Out - Label copied to parameter array
   char  *label3);  // Out - Label copied to parameter array


extern int DS_AlgoSetEvidenceLabels
  (char  *label1,   // In
   char  *label2,   // In
   char  *label3);  // In


extern int DS_AlgoGetNumPossibilities (void);


extern int DS_AlgoSetNumPossibilities
  (int  num);  // In - 0 .. DS_Max_Possibilities


extern int DS_AlgoGetPossibility
  (double  value,     // In
   char    *string);  // Out - Possibility string of range value falls in, or ""

  // DS_AlgoGetPossibility returns the possibility string associated with
  // the given value.  Returns -1 if the value is out of range, otherwise 0.


extern int DS_AlgoGetPossibility
  (char    *string,     // In  - Possibility string
   double  &midpoint);  // Out - Midpoint of value range associated with string

  // DS_AlgoGetPossibility returns the possibility string associated with
  // the given value.  Returns -1 if the value is out of range, otherwise 0.


extern int DS_AlgoGetPossibility
  (int     index,        // In  - 0..numPossibilities - 1
   char    *string,      // Out - Possibility string copied to parameter array
   double  &rangeStart,  // Out
   double  &rangeEnd);   // Out

  // DS_AlgoGetPossibility returns the possibility string and range delimiters
  // associated with the given index.  Returns -1 if the index is out of range,
  // otherwise 0.

extern int DS_AlgoSetPossibility
  (int     index,       // In - 0..numPossibilities - 1
   char    *string,     // In
   double  rangeStart,  // In - Values in this range are > this number
   double  rangeEnd);   // In - Values in this range are <= this number

#endif  // _DS_CONFIG_H
