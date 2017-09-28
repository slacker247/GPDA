
#ifndef DS_EVIDENCE_H
#define DS_EVIDENCE_H

class DSevidence
{
  public:
    const static int EVID_ZERO   = 0;  // Degrade to Zero
    const static int EVID_NONE   = 1;  // Degrade None
    const static int EVID_LINEAR = 2;  // Degrade Linearly
    const static int EVID_STEP   = 3;  // Degrade Stepwise
    const static int EVID_LOG    = 4;  // Degrade Logrithmically
    const static int EVID_SINE   = 5;  // Degrade Sinusodially

    //--------------------------------------------------------------------------

    typedef struct {
      int     caseId;          // Unique number id
      int     valid;           //
      int     source;          // Node index on its tree evel
      int     level;           // Tree level node is on
      int     type;            // Evidence type
                               //   0 = From Intel (Fuse)
                               //   1 = From Operator (Override)
                               //   2 = From higher level net node (Override)
      int     subnode;         // Node # in higher level net containing B & D
      int     sublevel;        // Level # of that node
      float   confidence;      // Belief (0.0 - 1.0)
      float   plause;          // Evidence plausibility (1.0 - disbelief)
      float   disbelief;       // Disbelief (0.0 - 1.0)
      float   Tin;             // Time tag of evidence
      float   duration;        // Duration for which evidence is valid
      char    chsource[32];    // Node Name (Hypothesis)
      char    chdescript[32];  // Descriptive text
      char    latitude[32];    // Parameter 1
      char    longitude[32];   // Parameter 2
      char    altitude[32];    // Parameter 3
      char    subfname[64];    // Name of file containing B & D
                               //   (from "Save Beliefs")
      char    chmission[64];   // External network mission name
      char    chlevel[32];     // External network level name
      char    chcase[32];      // External network node name
    } evidType, *evidPtrType;

    //--------------------------------------------------------------------------

    int          MAX_NUM_EVIDENCE;  // Max number of evidence records

    evidPtrType  *evidences;    // Holds all pieces of evidence

    char         chCaseId[64];  //

    int          numEvid;       // Number of pieces of evidence being processed

    float        Tstart;        // Other evidence times
    float        Tlast;         // Other evidence times

    //--------------------------------------------------------------------------

    DSevidence();

      // DSevidence is the constructor.  It initializes data to default
      // values when this new class instance is created.


    DSevidence(const DSevidence &ev);

      // DSevidence is the copy constructor.  When this new class instance
      // is created, its data is initialized to the data values of the
      // parameter class.


    ~DSevidence();

      // DSevidence is the destructor.  It does cleanup (deletes pointers)
      // when this class instance is destructed.


    DSevidence operator = (const DSevidence &ev);

      // Operator = makes this class look like the parameter class.


    int DS_loadEvidence
      (char*  source,         // IN - Name of file or database containing data
       int    loadFromDB,     // IN - 0 = load from file, 1 = load from database
       int    secure,         // IN -
       int    append,         // IN - 0 = reset ev table, 1 = add to ev table
       int    &externalEncountered);  // OUT - Indicates if have extern net evid

      // DS_loadEvidence loads evidence data from the file or database whose
      // name is specified in the source parameter.  ExternalEncountered
      // will be 1 if a node takes evidence from an external network,
      // otherwise 0.  Returns -1 on error, otherwise 0.


    int DS_saveEvidence
      (char*  destination,  // IN - Name of file or database to save data in
       int    saveToDB);    // IN - 0 = save to file, 1 = save to database

      // DS_saveEvidence saves evidence data to the file or database
      // whose name is specified in the source parameter.  Returns -1
      // on error, otherwise 0.


    static int DS_saveEvidence
      (char*  destination,  // IN - Name of file or database to save data in
       char*  name,         // IN - DSBN node name.  -- MUST BE NON NULL --
       char*  description,  // IN - String describing the data
       char*  param1,       // IN - Parameter 1 (eg. Lat)
       char*  param2,       // IN - Parameter 2 (eg. Lon)
       char*  param3,       // IN - Parameter 3 (eg. Alt)
       char*  subfname,     // IN - Name of file containing B & D
       char   durationUnits,// IN - H = hour, M = minutes, S = seconds
       int    source,       // IN - Node index on DSBN tree level
       int    level,        // IN - Level node is on in DSBN tree
       int    etype,        // IN - Evidence type (0, 1, 2 - see evidType above)
       float  timeStamp,    // IN - Time tag of evidence
       float  duration,     // IN - Duration for which evidence is valid
       float  confidence,   // IN - Belief (0.0 - 1.0)
       float  disbelief,    // IN - Disbelief (0.0 - 1.0)
       int    saveToDB);    // IN - 0 = save to file, 1 = save to database

      // DS_saveEvidence adds the given evidence data to the file or
      // database whose name is specified in the source parameter.
      // This function is static, you do not need a DSevidence object
      // to call it; instead use DSevidence::DS_saveEvidence(...).
      // Returns -1 on error, otherwise 0.


    int DS_addEvidence();

      // DS_addEvidence adds an evidence record to the end of the evidence
      // table.  Only its timestamp field (Tin) is set (to 0.1 more than the
      // previous record's value).   Returns -1 on error, otherwise returns
      // the index of the new record in the evidences array.


    int DS_addEvidence
      (char*  name,         // IN - DSBN node name.  -- MUST BE NON NULL --
       char*  description,  // IN - String describing the data
       char*  param1,       // IN - Parameter 1 (eg. Lat)
       char*  param2,       // IN - Parameter 2 (eg. Lon)
       char*  param3,       // IN - Parameter 3 (eg. Alt)
       char*  subfname,     // IN - Name of file containing B & D
       int    caseId,       // IN - Unique number to id rec, for DB
       int    valid,        // IN -
       int    source,       // IN - Node index on DSBN tree level
       int    level,        // IN - Level node is on in DSBN tree
       int    etype,        // IN - Evidence type (0, 1, 2 - see evidType above)
       float  timeStamp,    // IN - Time tag of evidence
       float  duration,     // IN - Duration for which evidence is valid
       float  confidence,   // IN - Belief (0.0 - 1.0)
       float  disbelief);   // IN - Disbelief (0.0 - 1.0)

      // DS_addEvidence adds an evidence record with the given
      // parameter values to the evidence table.  Of the character
      // strings, only the name parameter is required to be non null;
      // if any other string is null, a default value is put in the
      // evidence record.  Returns -1 on error, otherwise returns the
      // index of the new record in the evidences array.


    int DS_deleteEvidence
      (int  index);  // IN - Index of record in evidences array

      // DS_deleteEvidence deletes the record at the given index of
      // the evidences array.  Returns -1 if the index is out of bounds,
      // otherwise returns 0.


    int DS_setEvidence
      (int    index,        // IN - Index of rec to change in evidences array
       char*  name,         // IN - DSBN node name.  -- MUST BE NON NULL --
       char*  description,  // IN - String describing the data
       char*  param1,       // IN - Parameter 1 (eg. Lat)
       char*  param2,       // IN - Parameter 2 (eg. Lon)
       char*  param3,       // IN - Parameter 3 (eg. Alt)
       char*  subfname,     // IN - Name of file containing B & D
       int    caseId,       // IN - Unique number to id rec, for DB
       int    valid,        // IN -
       int    source,       // IN - Node index on DSBN tree level
       int    level,        // IN - Level node is on in DSBN tree
       int    etype,        // IN - Evidence type (0, 1, 2 - see evidType above)
       float  timeStamp,    // IN - Time tag of evidence
       float  duration,     // IN - Duration for which evidence is valid
       float  confidence,   // IN - Belief (0.0 - 1.0)
       float  disbelief);   // IN - Disbelief (0.0 - 1.0)

      // DS_setEvidence sets the evidence record at the given evidences
      // table index to the given parameter values.  Of the character
      // strings, only the name parameter is required to be non null;
      // if any other string is null, a default value is put in the
      // evidence record.  Returns -1 on error, otherwise returns the
      // (possibly new) index of the record in the evidences array.


    int DS_setTimeStamp
      (int    index,       // IN - Index of rec to change in evidences array
       float  timeStamp);  // IN - Time tag of evidence

      // DS_setTimeStamp sets the time tag of the evidence at the given
      // index in the evidences table.  As the table is stored in order of
      // increasing timeStamps, this might change the position of the record
      // in the table.  Returns -1 on error, otherwise returns the (possibly
      // new) index of the record in the evidences array.

    static int DS_degradeEvidence
      (float  Tevid,     // IN -
       float  Tnow,      // IN -
       float  Tdur,      // IN -
       int    ijump,     // IN - EVID_ZERO, EVID_NONE, EVID_LINEAR, or EVID_STEP
       float  &degrade); // OUT - degrade value

      // DegradeEvid does ....
      // Returns -1 if ijump invalid value is passed in, otherwise 0.

  private:

/***  THIS IS IN .C FILE NOW, WANT TO MOVE TO HERE
    const static int  NAME_LEN = 255;
    const static int  db_numColumns = 11;
    static const char db_colNames[db_numColumns][NAME_LEN] =
    {
      "Hypothesis",
      "Belief",
      "Disbelief",
      "Evidence_Type",
      "Latitude",
      "Longitude",
      "Altitude",
      "Start_Time",
      "End_Time",
      "Duration",
      "Case_ID" 
    };

    const static int   HYPOTHESIS    = 0;
    const static int   BELIEF        = 1;
    const static int   DISBELIEF     = 2;
    const static int   EVIDENCE_TYPE = 3;
    const static int   LATITUDE      = 4;
    const static int   LONGITUDE     = 5;
    const static int   ALTITUDE      = 6;
    const static int   START_TIME    = 7;
    const static int   END_TIME      = 8;
    const static int   DURATION      = 9;
    const static int   CASE_ID       = 10;
***/

    typedef struct recType {
      evidPtrType     evRec;
      struct recType  *next;
    } *listType;

    listType  deletedRecs;
};

#endif // DS_EVIDENCE_H
