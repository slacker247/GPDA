#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "DSBevidence.h"
#include "savedNet.h"
#include "DSBattributes.h"
#include "DSBconfig.h"
#include "Globals.h"
#include "isql.h"

const int  NAME_LEN = 255;
const int  db_numDSColumns = 12;
const int  db_numAllColumns = 52;
const char db_colNames[db_numAllColumns][NAME_LEN] =
{
  "Case_ID",
  "Hypothesis",
  "Belief",
  "Disbelief",
  "Report_Org",
  "Report_Affil",
  "Report_Desc",
  "Report_Name",
  "Enemy_Org",
  "Enemy_Affil",
  "Enemy_Desc",
  "Enemy_Name",
  "Ally_Org",
  "Ally_Affil",
  "Ally_Desc",
  "Ally_Name",
  "Party_Org",
  "Party_Affil",
  "Party_Desc",
  "Party_Name",
  "Region",
  "Nation",
  "Locale",
  "City",
  "Structure",
  "Latitude",
  "Longitude",
  "Altitude",
  "Subject",
  "Action",
  "Objective",
  "Strategic",
  "Operation",
  "Tactical",
  "Task",
  "Method",
  "Desc_1",
  "Desc_2",
  "Desc_3",
  "Casualties",
  "Injuries",
  "Cost",
  "Adjective",
  "Start_Time",
  "End_Time",
  "Duration",
  "Time_Frame",
  "Evidence_Type",
  "Prepared",
  "Prep_Date",
  "Source",
  "Description"
};


const int  CASE_ID       = 0;
const int  HYPOTHESIS    = 1;
const int  BELIEF        = 2;
const int  DISBELIEF     = 3;
const int  REPORT_ORG    = 4;
const int  REPORT_AFFIL  = 5;
const int  REPORT_DESC   = 6;
const int  REPORT_NAME   = 7;
const int  ENEMY_ORG     = 8;
const int  ENEMY_AFFIL   = 9;
const int  ENEMY_DESC    = 10;
const int  ENEMY_NAME    = 11;
const int  ALLY_ORG      = 12;
const int  ALLY_AFFIL    = 13;
const int  ALLY_DESC     = 14;
const int  ALLY_NAME     = 15;
const int  PARTY_ORG     = 16;
const int  PARTY_AFFIL   = 17;
const int  PARTY_DESC    = 18;
const int  PARTY_NAME    = 19;
const int  REGION        = 20;
const int  NATION        = 21;
const int  LOCALE        = 22;
const int  CITY          = 23;
const int  STRUCTURE     = 24;
const int  LATITUDE      = 25;
const int  LONGITUDE     = 26;
const int  ALTITUDE      = 27;
const int  SUBJECT       = 28;
const int  ACTION        = 29;
const int  OBJECTIVE     = 30;
const int  STRATEGIC     = 31;
const int  OPERATION     = 32;
const int  TACTICAL      = 33;
const int  TASK          = 34;
const int  METHOD        = 35;
const int  DESC_1        = 36;
const int  DESC_2        = 37;
const int  DESC_3        = 38;
const int  CASUALTIES    = 39;
const int  INJURIES      = 40;
const int  COST          = 41;
const int  ADJECTIVE     = 42;
const int  START_TIME    = 43;
const int  END_TIME      = 44;
const int  DURATION      = 45;
const int  TIME_FRAME    = 46;
const int  EVIDENCE_TYPE = 47;
const int  PREPARED      = 48;
const int  PREP_DATE     = 49;
const int  SOURCE        = 50;
const int  DESCRIPTION   = 51;

// The columns DS is interested in are not grouped together at the front 
// of the database table, but a select statement for interested columns
// will put values into a small consecutive array.  Create a corresponding
// table that will map the columns of selected values to the columns of the
// db table.  (This is so we don't have to have 2 arrays of column names).

const int  DS_CASE_ID       = 0;
const int  DS_HYPOTHESIS    = 1;
const int  DS_DESCRIPTION   = 2;
const int  DS_BELIEF        = 3;
const int  DS_DISBELIEF     = 4;
const int  DS_EVIDENCE_TYPE = 5;
const int  DS_LATITUDE      = 6;
const int  DS_LONGITUDE     = 7;
const int  DS_ALTITUDE      = 8;
const int  DS_START_TIME    = 9;
const int  DS_END_TIME      = 10;
const int  DS_DURATION      = 11;

const int DScols[db_numDSColumns] =
{
  CASE_ID,
  HYPOTHESIS,
  DESCRIPTION,
  BELIEF,
  DISBELIEF,
  EVIDENCE_TYPE,
  LATITUDE,
  LONGITUDE,
  ALTITUDE,
  START_TIME,
  END_TIME,
  DURATION
};

//------------------------------------------------------------------------------

DSevidence::DSevidence()
{
  numEvid = 0;
  MAX_NUM_EVIDENCE = 50;

  evidences = (evidPtrType *) malloc (MAX_NUM_EVIDENCE * sizeof(evidPtrType));

  for (int i = 0; i < MAX_NUM_EVIDENCE; i++)
  {
    evidences[i] = NULL;
  }

  chCaseId[0] = '\0';
  Tstart = 0.0;
  Tlast = 0.0;

  deletedRecs = NULL;
}

//------------------------------------------------------------------------------

DSevidence::DSevidence(const DSevidence& ev)
{
  numEvid = ev.numEvid;
  MAX_NUM_EVIDENCE = ev.MAX_NUM_EVIDENCE;

  evidences = (evidPtrType *) malloc (MAX_NUM_EVIDENCE * sizeof(evidPtrType));

  for (int i = 0; i < MAX_NUM_EVIDENCE; i++)
  {
    if (ev.evidences[i] != NULL)
    {
      evidences[i] = new evidType;
      evidences[i]->valid = ev.evidences[i]->valid;
      evidences[i]->source = ev.evidences[i]->source;
      evidences[i]->level = ev.evidences[i]->level;
      evidences[i]->type = ev.evidences[i]->type;
      evidences[i]->subnode = ev.evidences[i]->subnode;
      evidences[i]->sublevel = ev.evidences[i]->sublevel;
      evidences[i]->confidence = ev.evidences[i]->confidence;
      evidences[i]->plause = ev.evidences[i]->plause;
      evidences[i]->disbelief = ev.evidences[i]->disbelief;
      evidences[i]->Tin = ev.evidences[i]->Tin;
      evidences[i]->duration = ev.evidences[i]->duration;
      strcpy(evidences[i]->chsource, ev.evidences[i]->chsource);
      strcpy(evidences[i]->chdescript, ev.evidences[i]->chdescript);
      strcpy(evidences[i]->latitude, ev.evidences[i]->latitude);
      strcpy(evidences[i]->longitude, ev.evidences[i]->longitude);
      strcpy(evidences[i]->altitude, ev.evidences[i]->altitude);
      strcpy(evidences[i]->subfname, ev.evidences[i]->subfname);
      strcpy(evidences[i]->chmission, ev.evidences[i]->chmission);
      strcpy(evidences[i]->chcase, ev.evidences[i]->chcase);
      strcpy(evidences[i]->chlevel, ev.evidences[i]->chlevel);
    }
  }

  strcpy(chCaseId, ev.chCaseId);
  Tstart = ev.Tstart;
  Tlast = ev.Tlast;

  deletedRecs = NULL;
}

//------------------------------------------------------------------------------

DSevidence::~DSevidence()
{
  for (int i = 0; i < numEvid; i++)
    delete evidences[i];

  free(evidences);

  while (deletedRecs != NULL)
  {
    listType  ptr = deletedRecs;
    deletedRecs = deletedRecs->next;
    delete ptr->evRec;
    ptr->next = NULL;
    free(ptr);
  }
}

//------------------------------------------------------------------------------

DSevidence DSevidence::operator = (const DSevidence &ev)
{
  int  i;


  numEvid = ev.numEvid;

  if (MAX_NUM_EVIDENCE < ev.MAX_NUM_EVIDENCE)
  {
    evidPtrType *temp =
      (evidPtrType *) realloc(evidences,
                              (ev.MAX_NUM_EVIDENCE * sizeof(evidPtrType)));
    if (temp != NULL)
    {
      for (i = MAX_NUM_EVIDENCE; i < ev.MAX_NUM_EVIDENCE; i++)
        temp[i] = NULL;

      evidences = temp;
      MAX_NUM_EVIDENCE = ev.MAX_NUM_EVIDENCE;
    }
  }

  for (i = 0; i < MAX_NUM_EVIDENCE; i++)
  {
    if (evidences[i] != NULL)
      delete evidences[i]; 

    if (ev.evidences[i] != NULL)
    {
      evidences[i] = new evidType;
      evidences[i]->valid = ev.evidences[i]->valid;
      evidences[i]->source = ev.evidences[i]->source;
      evidences[i]->level = ev.evidences[i]->level;
      evidences[i]->type = ev.evidences[i]->type;
      evidences[i]->subnode = ev.evidences[i]->subnode;
      evidences[i]->sublevel = ev.evidences[i]->sublevel;
      evidences[i]->confidence = ev.evidences[i]->confidence;
      evidences[i]->plause = ev.evidences[i]->plause;
      evidences[i]->disbelief = ev.evidences[i]->disbelief;
      evidences[i]->Tin = ev.evidences[i]->Tin;
      evidences[i]->duration = ev.evidences[i]->duration;
      strcpy(evidences[i]->chsource, ev.evidences[i]->chsource);
      strcpy(evidences[i]->chdescript, ev.evidences[i]->chdescript);
      strcpy(evidences[i]->latitude, ev.evidences[i]->latitude);
      strcpy(evidences[i]->longitude, ev.evidences[i]->longitude);
      strcpy(evidences[i]->altitude, ev.evidences[i]->altitude);
      strcpy(evidences[i]->subfname, ev.evidences[i]->subfname);
      strcpy(evidences[i]->chmission, ev.evidences[i]->chmission);
      strcpy(evidences[i]->chlevel, ev.evidences[i]->chlevel);
    }
    else
      evidences[i] = NULL;
  }

  strcpy(chCaseId, ev.chCaseId);
  Tstart = ev.Tstart;
  Tlast = ev.Tlast;

  return *this;
}

//------------------------------------------------------------------------------

int DSevidence::DS_loadEvidence(char* source, int loadFromDB, int secure,
                                int append, int &externalEncountered)
{
  char   chsource[32];
  char   chmission[32];
  char   chcase[32];
  char   chlevel[32];
  char   chStartTime[32];
  char   chStopTime[32];
  int    i, j;
  int    successVal = 0;
  int    caseId;
  int    ievid;
  int    level;
  int    etype;
  time_t starttime;
  float  confidence;  // Evidence belief
  float  disbelief;
  float  Tin;
  float  dur;


  externalEncountered = 0;
  Tlast  = -1000000.0;                   // We want time in minutes
  Tstart = 10000000.0;


  if (!append)
  {
    // Reset the evidence array.
    for (i = 0; i < numEvid; i++)
    {
      delete evidences[i];
      evidences[i] = NULL;
    }

    numEvid = 0;
  }

  if (source == NULL)
  {
    printf("DS_loadEvidence : source name is NULL.\n");
    fflush(stdout);
    return -1;
  }

  DS_AlgoGetTimeRange(chStartTime, chStopTime);
  starttime = stringToTime(chStartTime);

  if (loadFromDB)
  {
    extern int      bHTMLTable;
    extern int      cDelimiter;
    extern int      bColumnNames;
    extern SQLHSTMT hStmt;

    int             ret;
    int             nCols;
    int             nUserWidth  = 0;
    char            colValues[db_numDSColumns][NAME_LEN];
    char            chTemp[256];
    char            szSQL[9001];
    float           endTime;
    SQLHENV         DSBhEnv      = 0;
    SQLHDBC         DSBhDbc      = 0;
    SQLINTEGER      nCol                            = 0;
    SQLCHAR         szColumn[MAX_DATA_WIDTH+20]     = "";
    SQLCHAR         szColumnName[MAX_DATA_WIDTH+1]  = "";
    SQLCHAR         szHdrLine[32001]                = "";
    SQLUINTEGER     nOptimalDisplayWidth            = 10;
    SQLLEN          nIndicator                      = 0;
    SQLCHAR         szColumnValue[MAX_DATA_WIDTH+1] = "";
    SQLRETURN       nReturn;
    SQLCHAR         szSepLine[32001] = "";
    SQLSMALLINT     cols;
    SQLLEN          nRows = 0;
    SDWORD          colLengths[db_numDSColumns];


    if ( !OpenDatabase( &DSBhEnv, &DSBhDbc, "MySQL", "root", "gpda" ))
      return(-1);

    sprintf(szSQL, "USE `%s`;", source);
    ret = ExecuteSQL( DSBhDbc, szSQL, cDelimiter,
                      bColumnNames, bHTMLTable, nCols );
    if (ret != 0)
    {
      printf("Database (%s) does not exists, no evidence data loaded.\n",
             source);
      fflush(stdout);
      return -1;
    }

    //
    //   Load the evidence from database.  Only want evidence
    //   related to nodes in the current story, so list the
    //   story's node names in the SELECT statement.
    //

    sprintf(szSQL, "SELECT ");

    for (i=0; i < db_numDSColumns; i++)
    {
      strcat(szSQL, db_colNames[DScols[i]]);

      if (i < db_numDSColumns - 1)
        strcat(szSQL, ", ");
    }

    strcat(szSQL, " FROM Evidence WHERE");

    for (j=0; j < DS_AlgoGetTreeDepth(); j++)
    {
      for (i=0; i < DS_AlgoGetLevelWidth(j); i++)
      {
        DS_AlgoGetNodeName(i, j, chsource);
        strsub(chsource, ' ', '_');
        sprintf(chTemp, " HYPOTHESIS = \"%s\" OR", chsource);
        strcat(szSQL, chTemp);
      }
    }

    // Remove the trailing OR from the statement.
    szSQL[strlen(szSQL)-3] = '\0';
    strcat(szSQL, " ORDER BY Start_Time;");
    nRows = 0;
    ret = ExecuteSQL( DSBhDbc, szSQL, cDelimiter, bColumnNames,
                      bHTMLTable, nCols );
    if (ret != 0)
    {
      printf("failed!\n");
      fflush(stdout);
      return -1;
    }
    fprintf(stderr, "%s returns %d columns\n", szSQL, nCols);

    // Bind each column to a variable.
    for (i = 0; i < db_numDSColumns; i++)
    {
      // NOTE -- Start at column 0 if use bookmarks.
      ret = SQLBindCol(hStmt,  i + 1, SQL_C_CHAR, colValues[i],
                       NAME_LEN, &colLengths[i]);
    }

    while (TRUE)
    {
      ret = SQLFetch(hStmt);

      if (ret == SQL_SUCCESS || ret == SQL_SUCCESS_WITH_INFO)
      {
        //if (DS_verbose) <-- need to add a verbose flag
        {
          for (i = 0; i < nCols; i++)
          {
            printf(" - (%s) <-- %s\n", colValues[i], db_colNames[DScols[i]]);
            fflush(stdout);
          }

          printf("\n");
          fflush(stdout);
        }
/***
***/
      }
      else
      {
        break;
      }

      sscanf(colValues[DS_CASE_ID], "%d", &caseId);
      strcpy(chsource, colValues[DS_HYPOTHESIS]);
      strsub(chsource, '_', ' ');

      if (DS_AlgoGetNodeCoordinates(chsource, ievid, level) != 0)
      {
        fprintf(stderr, "Ignoring evidence for node %s\n", chsource);
        continue;
      }

      if (strcmp(colValues[DS_BELIEF], "?") != 0)
      {
        strcpy(chTemp, colValues[DS_BELIEF]);
        sscanf(chTemp, "%f", &confidence);
      }
      else
        confidence = 0.0;

      if (strcmp(colValues[DS_DISBELIEF], "?") != 0)
      {
        strcpy(chTemp, colValues[DS_DISBELIEF]);
        sscanf(chTemp, "%f", &disbelief);
      }
      else
        disbelief = 0.0;

      if (strcmp(colValues[DS_EVIDENCE_TYPE], "?") != 0)
      {
        strcpy(chTemp, colValues[DS_EVIDENCE_TYPE]);
        sscanf(chTemp, "%d", &etype);
      }
      else
        etype = 0;

      if (strcmp(colValues[DS_START_TIME], "?") != 0)
      {
        strcpy(chTemp, colValues[DS_START_TIME]);
        Tin = (float) (stringToTime(chTemp) - starttime) / 60.0;
      }
      else
        Tin = 0.0;

      strcpy(chTemp, colValues[DS_DURATION]);
      if (strcmp(chTemp, "?") != 0)
        sscanf(chTemp, "%f", &dur);
      else
      {
        strcpy(chTemp, colValues[DS_END_TIME]);
        if (strcmp(chTemp, "?") != 0)
          endTime = stringToTime(chTemp) / 60.0;
        else
        {
          // Make endTime earlier than Tin.
          endTime = Tin - 1;
        }

        if (endTime > Tin)
          dur = endTime - Tin;
        else
          dur = 9999;
      }

      i = DS_addEvidence(chsource,
                         NULL,
                         colValues[DS_LATITUDE],
                         colValues[DS_LONGITUDE],
                         colValues[DS_ALTITUDE],
                         NULL,
                         caseId,
                         TRUE,
                         ievid + 1,
                         level + 1,
                         etype,
                         Tin,
                         dur,
                         confidence,               // Put 'fabs' call here
                         disbelief);               // Put 'fabs' call here

      if (i != -1)
      {
        strcpy(evidences[i]->chmission, " ");
        strcpy(evidences[i]->chcase, " ");
        strcpy(evidences[i]->chlevel, " ");

        //
        //   Keep track of times
        //
        if (Tin > Tlast) Tlast = Tin;
        if (Tin < Tstart) Tstart = Tin;
      }
    }

    SQLFreeHandle(SQL_HANDLE_STMT, hStmt);
  }
  else
  {
    FILE   *EVIDfp;
    int    subnode;
    int    sublevel;
    int    valid;
    char   chdur;
    char   chtin[32];
    char   chtmp[32];
    char   chlat[32];
    char   chlon[32];
    char   chalt[32];
    char   descr[64];
    char   subfname[64];
    char   dsbtemp[1024];


    if ((EVIDfp = fopen(source, "r")) == NULL)
      return -1;

    // Read first line from the file, skipping comment lines (that start with #)
    do fgets(dsbtemp, 128, EVIDfp); while (dsbtemp[0] == '#');
    sscanf(dsbtemp, "%*d %s", chCaseId);

    while (1)
    {
      // Read a line from the file, skipping comment lines (that start with #).
      do
        fgets(dsbtemp, 256, EVIDfp);
      while ((dsbtemp[0] == '#') && !feof(EVIDfp));

      if (feof(EVIDfp)) break;

      //
      //   Record fields:
      //
      //     etype      - Evidence type
      //                    0 = From Intel (Fuse)
      //                    1 = From Operator (Override)
      //                    2 = From higher level net node (Override)
      //     chsource   - Node Name (Hypothesis)
      //     confidence - Belief (0.0 - 1.0)
      //     disbelief  - Disbelief (0.0 - 1.0)
      //     Tin        - Time tag of evidence
      //     chtime     - Time tag units (S, M, H, D)
      //     dur        - Duration for which evidence is valid
      //     chdur      - Time units of duration
      //     chlat      - Parameter 1
      //     chlon      - Parameter 2
      //     chalt      - Parameter 3
      //     descr      - Descriptive text
      //     subnode    - Node # in higher level net containing B & D
      //     sublevel   - Level # of that node
      //     subfname   - Name of file containing B & D (from "Save Beliefs")
      //

      strsub(dsbtemp, '\n', '\0');
      sscanf(dsbtemp, "%d %s %f %f %s %f %s %s %s %s %s %d %d %s\n",
             &etype, chsource, &confidence, &disbelief, chtin, &dur,
             chcase, chlat, chlon, chalt, descr, &subnode, &sublevel, subfname);

      chdur  = chcase[0];

      /*
      fprintf(stderr, "[%d] %d %s %f %f %s %f %c %s %s %s %s %d %d %s\n",
              Eindex+2, etype, chsource,
              confidence, disbelief, chtin, dur, chdur,
              chlat, chlon, chalt, descr, subnode, sublevel, subfname);
      */

      strsub(chsource, '_', ' ');

      if (DS_AlgoGetNodeCoordinates(chsource, ievid, level) == 0)
      {
        valid = TRUE;
      }
      else
      {
        fprintf(stderr, "Ignoring evidence for node %s\n", chsource);
        valid = FALSE;
//        continue;
      }


      //
      //   Get B & D from node on external network if requested
      //
      if (etype == 2)
      {
        FILE  *fp;
        int   error;


        externalEncountered = 1;

        strcat(subfname, ".save");

        if (!secure)
        {
          error = savedNet_readFile((char*) subfname);

          if (!error)
          {
            strcpy(chmission, savedNet_mission);

            if (sublevel >= savedNet_numLevels)
              sublevel = savedNet_numLevels - 1;

            strcpy(chlevel, savedNet_levels[sublevel].name);
            confidence = savedNet_nodes[sublevel][subnode].belief;
            disbelief = savedNet_nodes[sublevel][subnode].disbelief;
            strcpy(chcase, savedNet_nodes[sublevel][subnode].name);

            strsub(chmission, '_', ' ');
            strsub(chlevel, '_', ' ');
            strsub(chcase, '_', ' ');
          }
        }
        else
        {
          fp = fopen(subfname, "r");
          error = (fp == NULL);

          if (!error)
          {
            int  nlevels;

            do fgets(dsbtemp, 128, fp); while (dsbtemp[0] == '#');
            sscanf(dsbtemp, "%d  %s\n", &nlevels, chmission);

            if (sublevel >= nlevels)
              sublevel = nlevels - 1;

            //
            //   Only a B, D, and time are available. No other info
            //   regarding the network need be present in the file.
            //   The fields 'irow', 'n', and 'chlevel' may be either
            //   accurate or may be dummy values (I don't use them).
            //

            do fgets(dsbtemp, 128, fp); while (dsbtemp[0] == '#');
            sscanf(dsbtemp, "%*d %*d %s", chlevel);
            do fgets(dsbtemp, 128, fp); while (dsbtemp[0] == '#');
            sscanf(dsbtemp, "%*d %f %f %*f %s",
                   &confidence, &disbelief, chcase);
            fclose(fp);

            strsub(chmission, '_', ' ');
            strsub(chlevel, '_', ' ');
            strsub(chcase, '_', ' ');
          }
        }

        if (error)
        {
          sublevel = -1;
          chmission[0] = '\0';
          chcase[0] = '\0';
          chlevel[0] = '\0';
          confidence = 0.0;
          disbelief = 0.0;
        }
        else
          successVal = -1;
      } // if external
      else
      {
        subfname[0] = '\0';
      }

      /*
      strcpy(chtemp, "2000:01:01:02:02:00");
      stime = stringToTime(chtemp);
      strcpy(chtemp, "2000:01:05:02:02:00");
      etime = stringToTime(chtemp);
      float duration = difftime(etime, stime);
      duration = duration/60.0;
      fprintf(stderr, "Duration is %f minutes\n", duration);
      */

      //
      //   DSB handles time in minutes
      //
      Tin = (float) (stringToTime(chtin) - starttime) / 60.0;

      if (toupper(chdur) == 'S') dur = dur/60.0;
      else if (toupper(chdur) == 'H') dur = dur*60.0;
      else if (toupper(chdur) == 'D') dur = dur*60.0*24.0;

      //
      //   Store resulting evidence
      //

      strsub(chsource, '_', ' ');
      strsub(descr, '_', ' ');

      i = DS_addEvidence(chsource,
                         descr,
                         chlat,
                         chlon,
                         chalt,
                         subfname,
                         0,
                         valid,
                         ievid + 1,
                         level + 1,
                         etype,
                         Tin,
                         dur,
                         confidence,               // Put 'fabs' call here
                         disbelief);               // Put 'fabs' call here

      if (i != -1)
      {
        evidences[i]->subnode    = subnode;
        evidences[i]->sublevel   = sublevel;
        strcpy(evidences[i]->chmission, chmission);
        strcpy(evidences[i]->chcase, chcase);
        strcpy(evidences[i]->chlevel, chlevel);

        //
        //   Keep track of times
        //
        if (Tin > Tlast) Tlast = Tin;
        if (Tin < Tstart) Tstart = Tin;
      }
    } // while

    fclose(EVIDfp);
  } // else

  return successVal;
} // DS_loadEvidence

//------------------------------------------------------------------------------

int DSevidence::DS_saveEvidence(char* destination, int saveToDB)
{
  FILE            *SAVEfp;
  int             i,j;
  int             ievid, level;
  int             successVal = 0;
  time_t          t;
  time_t          starttime;
  char            chTin[20];
  char            nodeName[128];
  char            nodeDescr[128];
  char            subfname[128];
  char            szSQL[9001];
  char            chStartTime[32];
  char            chStopTime[32];


  if (destination == NULL)
  {
    printf("DS_saveEvidence : destination name is NULL.\n");
    fflush(stdout);
    return -1;
  }

  DS_AlgoGetTimeRange(chStartTime, chStopTime);
  starttime = stringToTime(chStartTime);

  if (saveToDB)
  {
    //
    //   Save the evidence to database.
    //

    extern int      bHTMLTable;
    extern int      cDelimiter;
    extern int      bColumnNames;
    extern SQLHSTMT hStmt;

    int             ret;
    int             nCols;
    int             nUserWidth  = 0;
    int             recIsNew;
    time_t          t;
    char            colValues[db_numAllColumns][NAME_LEN];
    char            szSQL[9001];
    float           endTime;
    SQLHENV         DSBhEnv      = 0;
    SQLHDBC         DSBhDbc      = 0;
    SQLINTEGER      nCol                            = 0;
    SQLCHAR         szColumn[MAX_DATA_WIDTH+20]     = "";
    SQLCHAR         szColumnName[MAX_DATA_WIDTH+1]  = "";
    SQLCHAR         szHdrLine[32001]                = "";
    SQLUINTEGER     nOptimalDisplayWidth            = 10;
    SQLLEN          nIndicator                      = 0;
    SQLCHAR         szColumnValue[MAX_DATA_WIDTH+1] = "";
    SQLRETURN       nReturn;
    SQLCHAR         szSepLine[32001] = "";
    SQLSMALLINT     cols;
    SQLLEN          nRows = 0;
    SDWORD          colLengths[db_numAllColumns];


    if ( !OpenDatabase( &DSBhEnv, &DSBhDbc, "MySQL", "root", "gpda" ))
      return(-1);


    // If the desired database does not exist, create it.

    sprintf(szSQL, "CREATE DATABASE IF NOT EXISTS `%s`;", destination);
    ret = ExecuteSQL( DSBhDbc, szSQL, cDelimiter,
                      bColumnNames, bHTMLTable, nCols );
    SQLFreeStmt( hStmt, SQL_DROP );

    if (ret != 0)
      return -1;


    // Select the database for use.

    sprintf(szSQL, "USE `%s`;", destination);
    ret = ExecuteSQL( DSBhDbc, szSQL, cDelimiter,
                      bColumnNames, bHTMLTable, nCols );
    SQLFreeStmt( hStmt, SQL_DROP );

    if (ret != 0)
    {
      printf("Database (%s) does not exists, evidence data not saved.\n",
             destination);
      fflush(stdout);
      return -1;
    }


    // Create the evidence table if it doesn't already exist.

    sprintf(szSQL, "CREATE TABLE IF NOT EXISTS Evidence (");

    strcat(szSQL, db_colNames[0]);
    strcat(szSQL, " int(255) auto_increment not null, ");

    for (i = 1; i < db_numAllColumns; i++)
    {
      strcat(szSQL, db_colNames[i]);
      if ((i == SOURCE) || (i == DESCRIPTION))
        strcat(szSQL, " varchar (255) default '?' not null, ");
      else
        strcat(szSQL, " varchar (50) default '?' not null, ");
    }

    strcat(szSQL, "primary key (");
    strcat(szSQL, db_colNames[0]);
    strcat(szSQL, "), unique id (");
    strcat(szSQL, db_colNames[0]);
    strcat(szSQL, "));");

    ret = ExecuteSQL( DSBhDbc, szSQL, cDelimiter,
                      bColumnNames, bHTMLTable, nCols );
    SQLFreeStmt( hStmt, SQL_DROP );

    if (ret != 0)
      return -1;

    for (i = 0; i < numEvid; i++)
    {
      // If the evidence record is already in the table, get its data.
      // Otherwise set the record to default values.

      nCols = 0;

      if (evidences[i]->caseId != 0)
      {
        sprintf(szSQL, "SELECT * FROM Evidence WHERE %s = %d",
                db_colNames[0], evidences[i]->caseId);

        ret = ExecuteSQL( DSBhDbc, szSQL, cDelimiter,
                          bColumnNames, bHTMLTable, nCols );
        SQLFreeStmt( hStmt, SQL_DROP );
      }

      if (nCols > 0)
      {
        recIsNew = FALSE;

        // Bind each column to a variable.
        for (j = 0; j < db_numAllColumns; j++)
        {
          // NOTE -- Start at column 0 if use bookmarks.
          SQLBindCol(hStmt,  j + 1, SQL_C_CHAR, colValues[j],
                     NAME_LEN, &colLengths[j]);
        }
      }
      else
      {
        recIsNew = TRUE;

        for (j = 0; j < db_numAllColumns; j++)
        {
          sprintf(colValues[j], "?");
        }
      }


      // Copy this class's data to the database record.

      sprintf(colValues[CASE_ID], "%d", evidences[i]->caseId);

      sprintf(colValues[EVIDENCE_TYPE], "%d", evidences[i]->type);
      sprintf(colValues[BELIEF], "%05.3f", evidences[i]->confidence);
      sprintf(colValues[DISBELIEF], "%05.3f", evidences[i]->disbelief);
      sprintf(colValues[DURATION], "%f", evidences[i]->duration);

      t = (time_t) (evidences[i]->Tin * 60) + starttime;
      timeToString(&t, colValues[START_TIME]);
      t = (time_t) ((evidences[i]->Tin + evidences[i]->duration) * 60) +
          starttime;
      timeToString(&t, colValues[END_TIME]);

/*** HANDLE " IN NODE NAMES -- NEED TO ADD TO DSBattributes TOO:
      ptr = strchr(evidences[i]->chsource, '"');
      if (ptr == NULL)
      {
        strcpy(colValues[HYPOTHESIS], evidences[i]->chsource);
      }
      else
      {
        colValues[HYPOTHESIS][0] = '\0';
        for (j = 0, int k = 0; j < strlen(evidences[i]->chsource); j++, k++)
        {
          if (evidences[i]->chsource[j] == '"')
            colValues[HYPOTHESIS][k++] = '\';
          colValues[HYPOTHESIS][k] = evidences[i]->chsource[j];
        }
      }
***/ strcpy(colValues[HYPOTHESIS], evidences[i]->chsource);

      strsub(colValues[HYPOTHESIS], ' ', '_');
      strcpy(colValues[DESCRIPTION], evidences[i]->chdescript);
      strsub(colValues[DESCRIPTION], ' ', '_');
      strcpy(colValues[LATITUDE], evidences[i]->latitude);
      strsub(colValues[LATITUDE], ' ', '_');
      strcpy(colValues[LONGITUDE], evidences[i]->longitude);
      strsub(colValues[LONGITUDE], ' ', '_');
      strcpy(colValues[ALTITUDE], evidences[i]->altitude);
      strsub(colValues[ALTITUDE], ' ', '_');


      // If the record is new to the database, insert it.  Otherwise
      // set the existing record's data to this class's values.

      if (recIsNew)
      {
        // eg) INSERT INTO Evidence VALUES (null, '0.5', ...);

        sprintf(szSQL, "INSERT INTO Evidence VALUES (null");

        for (int j = 1; j < db_numAllColumns; j++)
        {
          strcat(szSQL, ", '");
          strcat(szSQL, colValues[j]);
          strcat(szSQL, "'");
        }

        strcat(szSQL, ");");
      }
      else
      {
        // eg) UPDATE Evidence SET Belief = '0.5', ...,  WHERE id = 5;

        sprintf(szSQL, "UPDATE Evidence SET ");

        for (int j = 1; j < db_numDSColumns; j++)
        {
          strcat(szSQL, db_colNames[DScols[j]]);
          strcat(szSQL, " = '");
          strcat(szSQL, colValues[DScols[j]]);
          strcat(szSQL, "'");

          if (j < db_numDSColumns - 1)
            strcat(szSQL, ", ");
          else
          {
            strcat(szSQL, " WHERE ");
            strcat(szSQL, db_colNames[0]);
            strcat(szSQL, " = ");
            strcat(szSQL, colValues[0]);
            strcat(szSQL, ";");
          }
        }
      }

      ret = ExecuteSQL( DSBhDbc, szSQL, cDelimiter,
                        bColumnNames, bHTMLTable, nCols );
      SQLFreeStmt( hStmt, SQL_DROP );

      if (ret != 0)
      {
        printf("DS_saveEvidence - Error inserting evidence record %d "
               "into database.\n", i);
        fflush(stdout);
        successVal = -1;
      }
    }


    // Remove from the database the records the user deleted.

    while (deletedRecs != NULL)
    {
      listType  ptr;


      sprintf(szSQL, "DELETE FROM Evidence WHERE %s = %d;",
              db_colNames[0], deletedRecs->evRec->caseId);

      ret = ExecuteSQL( DSBhDbc, szSQL, cDelimiter,
                        bColumnNames, bHTMLTable, nCols );
      SQLFreeStmt( hStmt, SQL_DROP );

      if (ret != 0)
      {
        printf("DS_saveEvidence - Error deleting evidence record %d "
               "from database.\n", deletedRecs->evRec->caseId);
        fflush(stdout);
        successVal = -1;
      }

      ptr = deletedRecs;
      deletedRecs = deletedRecs->next;
      delete ptr->evRec;
      ptr->next = NULL;
      free(ptr);
    }

    CloseDatabase( DSBhEnv, DSBhDbc );
  }
  else
  {
    //
    //   Save the evidence to flat file.
    //

    SAVEfp = fopen(destination, "w");

    fprintf(SAVEfp, "%5d %s\n", numEvid, chCaseId);

    for (i=0; i<numEvid; i++)
    {
      ievid = evidences[i]->source;
      level = evidences[i]->level;

      if (evidences[i]->type == 2)
      {
        strcpy(subfname, evidences[i]->subfname);
        strsub(subfname, ' ', '_');
      }
      else
        strcpy(subfname, "None");

      if (strlen(evidences[i]->chsource) > 0)
        strcpy(nodeName, evidences[i]->chsource);
      else
      {
        if ((ievid >= 0) && (level >= 0))
          DS_AlgoGetNodeName(ievid-1, level-1, nodeName);
        else
          strcpy(nodeName, "?");
      }
      strsub(nodeName, ' ', '_');

      strcpy(nodeDescr, evidences[i]->chdescript);
      strsub(nodeDescr, ' ', '_');

      t = (time_t) (evidences[i]->Tin * 60) + starttime;
      timeToString(&t, chTin);

      fprintf(SAVEfp,
              "%4d   %-22s   %7.3f   %7.3f   %s   %4.1f %c   %s   %s   %-8s   "
              "%-24s   %d   %d   %s\n",
              evidences[i]->type, nodeName,
              evidences[i]->confidence, 1.0 - evidences[i]->plause,
              chTin, evidences[i]->duration, 'M',
              evidences[i]->latitude,
              evidences[i]->longitude,
              evidences[i]->altitude,
              nodeDescr,
              evidences[i]->subnode, evidences[i]->sublevel,
              subfname);
    }

    fclose(SAVEfp);


    // Delete the "deleted" records.

    while (deletedRecs != NULL)
    {
      listType  ptr = deletedRecs;
      deletedRecs = deletedRecs->next;
      delete ptr->evRec;
      ptr->next = NULL;
      free(ptr);
    }
  }

  return successVal;
} // DS_saveEvidence

//------------------------------------------------------------------------------

int DSevidence::DS_saveEvidence(char*  destination,
                                char*  name,
                                char*  description,
                                char*  param1,
                                char*  param2,
                                char*  param3,
                                char*  subfname,
	                        char   durationUnits,
                                int    source,
                                int    level,
                                int    etype,
                                float  timeStamp,
                                float  duration,
                                float  confidence,
                                float  disbelief,
                                int    saveToDB)
{
  FILE    *fp = NULL;
  time_t  t;
  time_t  starttime;
  int     successVal = 0;
  char    *ptr;
  char    str[256];
  char    mission[256];
  char    chStartTime[32];
  char    chTimeStamp[32];


// Temporary code --------------------------------
// - Read cfg file here for now, until DSBconfig is converted to a class.

  if ((ptr = strrchr(destination, '/')) == NULL)
    ptr = destination;
  else
    ptr = ptr + 1;

  strcpy(mission, ptr);

  if ((ptr = strchr(mission, '.')) != NULL)
    *ptr = '\0';

  sprintf(str, "DSBFiles/%s.cfg", mission);


  if ((fp = fopen (str, "r")) == NULL)
  {
    if ((fp = fopen ("DSBFiles/default.cfg", "r")) == NULL)
    {
      fprintf (stderr,
               "DS_AlgoSaveEvidence: Cannot open (DSBFiles/default.cfg).\n");
      return (-1);
    }
  }

  // Skip the first (discrete flag) line of data.
  do
    fgets(str, 256, fp);
  while ((str[0] == '#') && !feof(fp));

  do
    fgets(str, 256, fp);
  while ((str[0] == '#') && !feof(fp));

  sscanf(str, "%s", chStartTime);
  starttime = stringToTime(chStartTime);

  fclose(fp);
// End temporary code --------------------------------


  t = (time_t) (timeStamp * 60) + starttime;
  timeToString(&t, chTimeStamp);


  if (saveToDB)
  {
    //
    //   Save the evidence to database.
    //

    extern int      bHTMLTable;
    extern int      cDelimiter;
    extern int      bColumnNames;
    extern SQLHSTMT hStmt;

    int             i, j;
    int             ret;
    int             nCols;
    int             nUserWidth  = 0;
    int             recIsNew;
    char            ch;
    char            colValues[db_numAllColumns][NAME_LEN];
    char            szSQL[9001];
    float           endTime;
    SQLHENV         DSBhEnv      = 0;
    SQLHDBC         DSBhDbc      = 0;
    SQLINTEGER      nCol                            = 0;
    SQLCHAR         szColumn[MAX_DATA_WIDTH+20]     = "";
    SQLCHAR         szColumnName[MAX_DATA_WIDTH+1]  = "";
    SQLCHAR         szHdrLine[32001]                = "";
    SQLUINTEGER     nOptimalDisplayWidth            = 10;
    SQLLEN          nIndicator                      = 0;
    SQLCHAR         szColumnValue[MAX_DATA_WIDTH+1] = "";
    SQLRETURN       nReturn;
    SQLCHAR         szSepLine[32001] = "";
    SQLSMALLINT     cols;
    SQLLEN          nRows = 0;
    SDWORD          colLengths[db_numAllColumns];


    if ( !OpenDatabase( &DSBhEnv, &DSBhDbc, "MySQL", "root", "gpda" ))
      return(-1);


/*** Not currently allowed
    // If the desired database does not exist, create it.

    sprintf(szSQL, "CREATE DATABASE IF NOT EXISTS `%s`;", destination);
    ret = ExecuteSQL( DSBhDbc, szSQL, cDelimiter,
                      bColumnNames, bHTMLTable, nCols );
    SQLFreeStmt( hStmt, SQL_DROP );

    if (ret != 0)
      return -1;
***/


    // Select the database for use.

    sprintf(szSQL, "USE `%s`;", destination);
    ret = ExecuteSQL( DSBhDbc, szSQL, cDelimiter,
                      bColumnNames, bHTMLTable, nCols );
    SQLFreeStmt( hStmt, SQL_DROP );

    if (ret != 0)
    {
      printf("Database (%s) does not exists, evidence data not saved.\n",
             destination);
      fflush(stdout);
      return -1;
    }


    // Create the evidence table if it doesn't already exist.

    sprintf(szSQL, "CREATE TABLE IF NOT EXISTS Evidence (");

    strcat(szSQL, db_colNames[0]);
    strcat(szSQL, " int(255) auto_increment not null, ");

    for (i = 1; i < db_numAllColumns; i++)
    {
      strcat(szSQL, db_colNames[i]);
      if ((i == SOURCE) || (i == DESCRIPTION))
        strcat(szSQL, " varchar (255) default '?' not null, ");
      else
        strcat(szSQL, " varchar (50) default '?' not null, ");
    }

    strcat(szSQL, "primary key (");
    strcat(szSQL, db_colNames[0]);
    strcat(szSQL, "), unique id (");
    strcat(szSQL, db_colNames[0]);
    strcat(szSQL, "));");

    ret = ExecuteSQL( DSBhDbc, szSQL, cDelimiter,
                      bColumnNames, bHTMLTable, nCols );
    SQLFreeStmt( hStmt, SQL_DROP );

    if (ret != 0)
      return -1;


    // Initialize the evidence record to default values.

    for (i = 0; i < db_numAllColumns; i++)
    {
      strcpy(colValues[i], "?");
    }


    // Copy the parameter data to the database record.

    sprintf(colValues[CASE_ID], "%d", 0);

    sprintf(colValues[EVIDENCE_TYPE], "%d", etype);
    sprintf(colValues[BELIEF], "%05.3f", confidence);
    sprintf(colValues[DISBELIEF], "%05.3f", disbelief);


    ch = toupper(durationUnits);

    if (ch == 'S')
      duration = duration / 60.0;
    else if (ch == 'H')
      duration = duration * 60.0;
    else if (ch == 'D')
      duration = duration * 60.0 * 24.0;

    sprintf(colValues[DURATION], "%f", duration);

    strcpy(colValues[START_TIME], chTimeStamp);

    t = (time_t) ((timeStamp + duration) * 60) + starttime;
    timeToString(&t, colValues[END_TIME]);

    // Handle " and - in node names.
    ptr = strchr(name, '"');
    if (ptr == NULL)
    {
      strcpy(colValues[HYPOTHESIS], name);
    }
    else
    {
      colValues[HYPOTHESIS][0] = '\0';
      for (i = 0, j = 0; i < strlen(name); i++, j++)
      {
        //if ((name[i] == '"') || (name[i] == '-'))  --- right? ---ttt
        if (name[i] == '"')
          colValues[HYPOTHESIS][j++] = '\\';
        colValues[HYPOTHESIS][j] = name[i];
      }
    }

    strsub(colValues[HYPOTHESIS], ' ', '_');
    strcpy(colValues[DESCRIPTION], description);
    strsub(colValues[DESCRIPTION], ' ', '_');
    strcpy(colValues[LATITUDE], param1);
    strsub(colValues[LATITUDE], ' ', '_');
    strcpy(colValues[LONGITUDE], param2);
    strsub(colValues[LONGITUDE], ' ', '_');
    strcpy(colValues[ALTITUDE], param3);
    strsub(colValues[ALTITUDE], ' ', '_');


    // Insert the record into the database,
    // eg) INSERT INTO Evidence VALUES (null, '0.5', ...);

    sprintf(szSQL, "INSERT INTO Evidence VALUES (null");

    for (int j = 1; j < db_numAllColumns; j++)
    {
      strcat(szSQL, ", '");
      strcat(szSQL, colValues[j]);
      strcat(szSQL, "'");
    }

    strcat(szSQL, ");");

    ret = ExecuteSQL( DSBhDbc, szSQL, cDelimiter,
                      bColumnNames, bHTMLTable, nCols );
    SQLFreeStmt( hStmt, SQL_DROP );

    if (ret != 0)
    {
      printf("DS_saveEvidence - Error inserting evidence record %d "
             "into database.\n", i);
      fflush(stdout);
      successVal = -1;
    }

    CloseDatabase( DSBhEnv, DSBhDbc );
  }
  else
  {
    short  fileIsNew = FALSE;
    char   chName[256];
    char   chParam1[50];
    char   chParam2[50];
    char   chParam3[50];
    char   chDescription[256];
    char   chSubfname[50];


printf("1");fflush(stdout);
    if ((fp = fopen(destination, "r")) == NULL)
      fileIsNew = TRUE;
    else
      fclose(fp);


    // Delete this if-statement if allow creating a new file.
    if (fileIsNew)
      return -1;

printf("2");fflush(stdout);

    if ((fp = fopen(destination, "a")) == NULL)
      return -1;

printf("3");fflush(stdout);
    if (fileIsNew)
    {
      fprintf(fp, "%5d %s\n", 1, mission);
    }

    strcpy(chName, name);
    strsub(chName, ' ', '_');
    strcpy(chParam1, param1);
    strsub(chParam1, ' ', '_');
    strcpy(chParam2, param2);
    strsub(chParam2, ' ', '_');
    strcpy(chParam3, param3);
    strsub(chParam3, ' ', '_');
    strcpy(chDescription, description);
    strsub(chDescription, ' ', '_');
    strcpy(chSubfname, subfname);
    strsub(chSubfname, ' ', '_');

printf("44");fflush(stdout);
    fprintf(fp,
            "%4d   %-22s   %7.3f   %7.3f   %s   %4.1f %c   %s   %s   %-8s   "
            "%-24s   %d   %d   %s\n",
            etype, chName,
            confidence, disbelief,
            chTimeStamp, duration, durationUnits,
            chParam1, chParam2, chParam3,
            chDescription,
            source, level,
            chSubfname);
printf("55");fflush(stdout);

     fclose(fp);
  }

printf("RETURN");fflush(stdout);
  return successVal;
} // DS_saveEvidence

//------------------------------------------------------------------------------

int DSevidence::DS_addEvidence()
{
  if (numEvid == MAX_NUM_EVIDENCE)
  {
    evidPtrType *temp =
      (evidPtrType *) realloc(evidences,
                              sizeof(evidences) + (25 * sizeof(evidPtrType)));
    if (temp != NULL)
    {
      for (int i = MAX_NUM_EVIDENCE; i < MAX_NUM_EVIDENCE + 25; i++)
        temp[i] = NULL;

      evidences = temp;
      MAX_NUM_EVIDENCE += 25;
    }
    else
    {
      printf("DS_addEvidence : No room for more evidence.\n");
      fflush(stdout);
      return -1;
    }
  }

  evidences[numEvid] = new evidType;

  evidences[numEvid]->caseId = 0;

  if (numEvid > 0)
    evidences[numEvid]->Tin = evidences[numEvid - 1]->Tin + 0.1;
  else
    evidences[numEvid]->Tin = 0.0;

  evidences[numEvid]->subnode = -1;
  evidences[numEvid]->sublevel = -1;
  strcpy(evidences[numEvid]->subfname, "None");

  Tlast = evidences[numEvid]->Tin;
  numEvid++;

  return (numEvid - 1);
} // DS_addEvidence

//------------------------------------------------------------------------------

int DSevidence::DS_addEvidence(char*  name,
                               char*  description,
                               char*  param1,
                               char*  param2,
                               char*  param3,
                               char*  subfname,
                               int    caseId,
                               int    valid,
                               int    source,
                               int    level,
                               int    etype,
                               float  timeStamp,
                               float  duration,
                               float  confidence,
                               float  disbelief)
{
  int  val;


  val = DS_addEvidence();

  if (val != -1)
  {
    val = DS_setEvidence(val, name, description, param1, param2, param3,
                         subfname, caseId, valid, source, level, etype,
                         timeStamp, duration, confidence, disbelief);
  }

  return val;
} // DS_addEvidence

//------------------------------------------------------------------------------

int DSevidence::DS_setEvidence(int    index,
                               char*  name,
                               char*  description,
                               char*  param1,
                               char*  param2,
                               char*  param3,
                               char*  subfname,
                               int    caseId,
                               int    valid,
                               int    source,
                               int    level,
                               int    etype,
                               float  timeStamp,
                               float  duration,
                               float  confidence,
                               float  disbelief)
{
  int  i, j;


  if ((index < 0) || (index >= numEvid))
  {
    printf("DS_setEvidence : Evidence index out of bounds.\n");
    fflush(stdout);
    return -1;
  }

  if (name == NULL)
  {
    printf("DS_setEvidence : node name is NULL.\n");
    fflush(stdout);
    return -1;
  }


  //
  //   Add the data to the evidence list
  //

  i = DS_setTimeStamp(index, timeStamp);

  if (i == -1)
    return -1;


  if (strlen(name) < 32)
    strcpy(evidences[i]->chsource, name);
  else
  {
    strncpy(evidences[i]->chsource, name, 31);
    evidences[i]->chsource[31] = '\0';
  }


  // Handle NULL strings.

  if (description != NULL)
  {
    if (strlen(description) < 32)
      strcpy(evidences[i]->chdescript, description);
    else
    {
      strncpy(evidences[i]->chdescript, description, 31);
      evidences[i]->chdescript[31] = '\0';
    }
  }
  else
    strcpy(evidences[i]->chdescript, " ");

  if (param1 != NULL)
  {
    if (strlen(param1) < 32)
      strcpy(evidences[i]->latitude, param1);
    else
    {
      strncpy(evidences[i]->latitude, param1, 31);
      evidences[i]->latitude[31] = '\0';
    }
  }
  else
    strcpy(evidences[i]->latitude, " ");

  if (param2 != NULL)
  {
    if (strlen(param2) < 32)
      strcpy(evidences[i]->longitude, param2);
    else
    {
      strncpy(evidences[i]->longitude, param2, 31);
      evidences[i]->longitude[31] = '\0';
    }
  }
  else
    strcpy(evidences[i]->longitude, " ");

  if (param3 != NULL)
  {
    if (strlen(param3) < 32)
      strcpy(evidences[i]->altitude, param3);
    else
    {
      strncpy(evidences[i]->altitude, param3, 31);
      evidences[i]->altitude[31] = '\0';
    }
  }
  else
    strcpy(evidences[i]->altitude, " ");

  if (subfname != NULL)
  {
    if (strlen(subfname) < 64)
      strcpy(evidences[i]->subfname, subfname);
    else
    {
      strncpy(evidences[i]->subfname, subfname, 63);
      evidences[i]->subfname[63] = '\0';
    }
  }
  else
    strcpy(evidences[i]->subfname, " ");

  //
  //   Make sure the belief and disbelief are valid
  //
  if (confidence > 1.0)
    evidences[i]->confidence = confidence/100.0;
  else
    evidences[i]->confidence = confidence;

  if (disbelief > 1.0)
    evidences[i]->disbelief = disbelief/100.0;
  else
    evidences[i]->disbelief  = disbelief;

  evidences[i]->plause     = 1.0 - evidences[i]->disbelief;
  evidences[i]->duration   = duration;
  evidences[i]->valid      = valid;
  evidences[i]->source     = source;
  evidences[i]->level      = level;
  evidences[i]->type       = etype;
  evidences[i]->caseId     = caseId;

  return i;
} // DS_setEvidence

//------------------------------------------------------------------------------

int DSevidence::DS_deleteEvidence(int  index)
{
  if ((index < 0) || (index >= numEvid))
  {
    printf("DS_deleteEvidence : Evidence index out of bounds.\n");
    fflush(stdout);
    return -1;
  }


  // Move the evidence record to the deleted list.

  listType  rec = new recType;
  rec->next = deletedRecs;
  rec->evRec = evidences[index];
  deletedRecs = rec;


  // Remove the evidence record from the evidence list.

  for (int i = index; i < numEvid - 1; i++)
  {
    evidences[i] = evidences[i + 1];
  }

  numEvid--;

  if (index == 0)
    Tstart = evidences[0]->Tin;
  else if (index == numEvid)
    Tlast = evidences[numEvid - 1]->Tin;

  return 0;
} // DS_deleteEvidence

//------------------------------------------------------------------------------

int DSevidence::DS_setTimeStamp(int index, float timeStamp)
{
  int  i, j;


  if ((index < 0) || (index >= numEvid))
  {
    printf("DS_setTimeStamp : Evidence index out of bounds.\n");
    fflush(stdout);
    return -1;
  }

  evidences[index]->Tin = timeStamp;

  // Find where in the time-ordered list the evidence should go.
  for  (i = 0; i < numEvid; i++)
  {
    if (i == index)
      continue;

    if (timeStamp < evidences[i]->Tin)
      break;
  }

  if (i > index)
    i--;


  // If the evidence now belongs in a different place in the list, move it.
  if (i != index)
  {
    evidType  *temp = evidences[index];

    if (index > i)
    {
      for (j = index; j > i; j--)
        evidences[j] = evidences[j - 1];
    }
    else
    {
      for (j = index; j < i; j++)
        evidences[j] = evidences[j + 1];
    }

    evidences[i] = temp;
  }

  Tlast = evidences[numEvid - 1]->Tin;
  Tstart = evidences[0]->Tin;

  return i;
}

//------------------------------------------------------------------------------

int DSevidence::DS_degradeEvidence(float Tevid, float Tnow, float Tdur,
                                   int ijump, float &degrade)
{
  int  val = 0;

  switch (ijump)
  {
    case EVID_ZERO:
      degrade = 0.0;
      break;

    case EVID_NONE:
      degrade = 1.0;
      break;

    case EVID_LINEAR:
      degrade = 1.0 - ((Tnow-Tevid)/Tdur);
      break;

    case EVID_STEP:
      degrade = 0.0;
      if ((Tevid+Tdur) < Tnow)
        degrade = 1.0;
      break;

    default:
      fprintf(stderr,
              "DegradeEvid: Option not implemented - no degrade applied.\n");
      degrade = 1.0;
      val = -1;
      break;
  }

  return (val);
}

