/* @(#) dted.h 2.1 07/02/97 */
/*      Classification:  Unclassified
-----------------------------------------------------------------------------

Name: DTED Utility Include File

Description:
  This include file contains the function prototypes and object type 
  definitions needed to support the DTED terrain data file manipulation
  utilities.

Author/Date/Company/Phone:
  Merrill R. Bean / 16May97 / NMSU / 505-522-9100

Change History:
  Rev  Date    Engineer             SP/CR      Description
  ---  ----    --------             -----      -----------
  new  16May97 Pratt / Bean / Walsh C-1681     new capability

Performance Requirements:
  None

Software Methods:
  In accordance with the Argus Software Handbook

Constraints:
  None

---------------------------------------------------------------------------*/

#ifndef _dted_h
#define _dted_h

typedef struct _DTEDFile *DTED;

typedef short DTED_Elevation_Type;

/*
Function Prototypes:
Type   Name                     Parameter List
----   ----                     --------------                             */
DTED   dted_open              ( char *const );
DTED   dted_new               ( const DTED, char *const, const int, const int );
DTED   dted_cp                ( const DTED );
void   dted_close_file        ( const DTED );
void   dted_close             ( void );

int    dted_get_file_size     ( const DTED, int *const, int *const );
int    dted_get_file_coverage ( const DTED, float *const, float *const,
                                float *const, float *const );

int    dted_xy_to_ll          ( const DTED, const int, const int,
                                double *const, double *const );
int    dted_ll_to_xy          ( const DTED, const double, const double,
                                int *const, int *const );

DTED_Elevation_Type
       dted_get_min_elev      ( const DTED );
DTED_Elevation_Type
       dted_get_max_elev      ( const DTED );
DTED_Elevation_Type
       dted_get_elev          ( const DTED, const int, const int );

/*
Structure Definitions:
Typedef        Name              Element
-------        ----              -------                                   */
typedef struct _UserHeaderLabel {
  char  sentinel[3];
  char  fixed;
  char  longOrigin[8];
  char  latOrigin[8];
  char  longInterval[4];
  char  latInterval[4];
  char  absVertAcc[4];
  char  securityCode[3];
  char  uniqueRef[12];
  char  nLongLines[4];
  char  nLatLines[4];
  char  multipleAcc;
  char  reserved[24];
} UserHeaderLabel;

typedef struct _DataSetId {
  char  sentinel[3];
  char  securityCode;
  char  securityControl[2];
  char  securityHandling[27];
  char  reserved1[26];
  char  dmaSeries[5];
  char  uniqueRef[15];
  char  reserved2[8];
  char  edition[2];
  char  version;
  char  maintDate[4];
  char  matchMergeDate[4];
  char  maintDescr[4];
  char  producerCode[8];
  char  reserved3[16];
  char  stockNumber[9];
  char  changeNumber[2];
  char  specDate[4];
  char  vertDatum[3];
  char  horizDatum[5];
  char  digiSytem[10];
  char  CompDate[4];
  char  reserved4[22];
  char  latOrigin[9];
  char  longOrigin[10];
  char  latSW[7];
  char  longSW[8];
  char  latNW[7];
  char  longNW[8];
  char  latNE[7];
  char  longNE[8];
  char  latSE[7];
  char  longSE[8];
  char  orientation[9];
  char  latInterval[4];
  char  longInterval[4];
  char  nLongLines[4];
  char  nLatLines[4];
  char  partialCell[2];
  char  reserved5[101];
  char  reserved6[100];
  char  reserved7[156];

} DataSetId;

typedef struct _AccuracyRecord {
  char  sentinel[3];
  char  reserved[726];
} AccuracyRecord;

typedef struct _DTEDHdr {
  UserHeaderLabel  uhl;
  DataSetId        dsi;
  AccuracyRecord   acc;
} DTEDHdr;

/*  typedef struct _DataRecord {
 *  char  sentinel[1];            \
 *  char  dataBlockCnt[3];         |
 *  short longCnt;                 | 4 shorts
 *  short latCnt;                 /
 *  short elevations[1201]; need vatiable # rows shorts
 *  char  checkSum[4];            } 2 shorts
 * } DataRecord;
 */

typedef struct _DTEDFile {
  int                 fd;
  int                 recently_used;
  unsigned int        filesize;
  char                filename[256];
  unsigned char       *dted_file;
  DTEDHdr             *hdr;
  DTED_Elevation_Type *data;
  int                 nrows;
  int                 ncols;
  float               sw_lat;
  float               sw_lon;
  float               ne_lat;
  float               ne_lon;
  DTED_Elevation_Type min_elev;
  DTED_Elevation_Type max_elev;
} DTEDFile;

#endif /* _dted_h */

/* DON'T ADD STUFF AFTER THIS #endif */
/*-------------------------------------------------------------------------*/
/*      Classification:  Unclassified                                      */
