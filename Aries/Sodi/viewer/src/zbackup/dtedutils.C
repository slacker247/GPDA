/* @(#) dted.c 2.1 07/02/97 */
/*      Classification:  Unclassified
-----------------------------------------------------------------------------

Name: DTED Utilities

Description:
  This module contains the functions needed for addressing and manipulating
  the contents of 2-byte-per-word DTED terrain data files.

Author/Date/Company/Phone:
  Merrill R. Bean / 16May97 / NMSU / 505-522-9100

Change History:
  Rev  Date    Engineer             SP/CR      Description
  ---  ----    --------             -----      -----------
  new  16May97 Pratt / Bean / Walsh C-1681     new capability

Performance Requirements:
  Provide reasonably high thruput when manipulating large data files.

Software Methods:
  In accordance with the Argus Software Handbook

Constraints:
  Two-byte-per-word read capability must be provided on both 4-byte-per-word
  and 8-byte-per-word hosts

Significant Calculations/Formulas:
  None

External Inputs/Outputs:
#include  File Name       Description
--------  ---------       -----------                                      */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#if !CRAY_YMP
#include <sys/mman.h>
#include <sys/uio.h>
#endif
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "dted.h"

union {
    int  testWord;
    char testByte[4];
} endianTest;

/*
Global Variables:
Type                Name              Description
----                ----              -----------                          */
       DTEDFile   **dteds;         /* open DTED file structure             */
       unsigned int numdteds = 0;  /* number of open dted files            */
static unsigned int maxdteds = 0;  /* number of pointers allocated         */
static unsigned int num_mapped = 0;/* number of mapped dted files          */
static unsigned int swapFlag = 0;  /* swap bytes for little endian         */
static unsigned int dted_enter = 0;/* if dted_open already called          */

/*---------------------------------------------------------------------------
Module Declaration:

Type     Name             Argument List
----     ----             -------------                                    */
DTED     dted_open      ( char *const dted_filename )
{

/*
Local Variables:
  Type                Name            Description
  ----                ----            -----------                          */
  char                tmpbuf[40];
  struct stat         stat_buf;
  int                 r, c;
  unsigned int        i, index;
  DTED_Elevation_Type e;

  if (!dted_enter) {
    dted_enter = 1;
    endianTest.testWord = 1;
    if (endianTest.testByte[0] == 1) {
        swapFlag = 1;
    } else {
        swapFlag = 0;
    }
  }

  for (i = 0; i < numdteds; i++)
  {
    /* see if this file was opened before                                  */
    if (strcmp(dted_filename, dteds[i]->filename) == 0) return dteds[i];
  }
  /* couldn't find the filename among the open files, so add               */
  if (numdteds >= maxdteds)
  {
    /* add more space in the dteds array for new file                      */
    dteds = (DTED *) realloc (dteds, sizeof(DTED *) * (maxdteds+5));
    if (dteds == NULL)
    {
      fprintf (stderr, "dted_open : can't allocate space for dteds array\n");
      fprintf (stderr, "dted_open : can't open %s\n", dted_filename);
      return NULL;
    }
    for (i = numdteds; i < maxdteds; i++) dteds[i] = NULL;
    maxdteds += 5;
  }

  index = numdteds;
  numdteds++;
  dteds[index] = (DTEDFile *)malloc( sizeof( DTEDFile ) );
  if ( dteds[index] == NULL ) 
  {
    fprintf( stderr, "dted_open : can't allocate memory for file\n");
    return NULL;
  }
  strcpy( dteds[index]->filename, dted_filename );

  /* open file                                                             */
  dteds[index]->fd = open( dteds[index]->filename, O_RDONLY);
  if ( dteds[index]->fd <= 0 )  /* open failed */
  {
    fprintf ( stderr, "dted_open : can't open %s\n", dteds[index]->filename );
    return ( dteds[index] );
  }

  fstat (dteds[index]->fd, &stat_buf);
  dteds[index]->filesize = (unsigned int)stat_buf.st_size;

#if CRAY_YMP
  /* allocate space for and then read file                                 */
  dteds[index]->dted_file = (unsigned char *) malloc (dteds[index]->filesize);
  if ( dteds[index]->dted_file == NULL ) 
  {
    fprintf( stderr, "dted_open : can't allocate buffer for file\n");
    return NULL;
  }
  if ( read (dteds[index]->fd, (void *)dteds[index]->dted_file,
             dteds[index]->filesize) < dteds[index]->filesize )
  {
    fprintf( stderr, "dted_open : read failed on file %s\n",
             dteds[index]->filename );
    return dteds[index];
  }
#else
  /* memory map file                                                       */
  dteds[index]->dted_file = (unsigned char *)mmap(0, dteds[index]->filesize, 
    PROT_READ, MAP_PRIVATE, dteds[index]->fd, 0 );
#endif

  /* initialize values                                                     */

  dteds[index]->hdr = (DTEDHdr *)dteds[index]->dted_file;
#if 0
  /* dted->data = (DataRecord *)(dted->dted_file + sizeof(DTEDHdr)); */
  dteds[index]->data = (DTED_Elevation_Type *)(dteds[index]->dted_file +
                                               sizeof(DTEDHdr));
#else
  /* dted->data = (DataRecord *)(dted->dted_file + 3428); */
  dteds[index]->data = (DTED_Elevation_Type *)(dteds[index]->dted_file + 3428);
#endif
  /* determine number of rows and cols                                     */

  strncpy( tmpbuf, dteds[index]->hdr->uhl.nLatLines, 4);
  tmpbuf[4] = '\0';
  dteds[index]->nrows = atoi(tmpbuf);
  strncpy( tmpbuf, dteds[index]->hdr->uhl.nLongLines, 4);
  tmpbuf[4] = '\0';
  dteds[index]->ncols = atoi(tmpbuf);

  /* determine coverage */
  strncpy( tmpbuf, dteds[index]->hdr->dsi.latSW,  7);
  tmpbuf[7] = '\0';
  dteds[index]->sw_lat = (float)(atof(tmpbuf) / 10000.0);
  if ( tmpbuf[6] == 'S' ) dteds[index]->sw_lat = -dteds[index]->sw_lat;
  strncpy( tmpbuf, dteds[index]->hdr->dsi.longSW,  8);
  tmpbuf[8] = '\0';
  dteds[index]->sw_lon = (float)(atof(tmpbuf) / 10000.0);
  if ( tmpbuf[7] == 'W' ) dteds[index]->sw_lon = -dteds[index]->sw_lon;
  
  strncpy( tmpbuf, dteds[index]->hdr->dsi.latNE,  7);
  tmpbuf[7] = '\0';
  dteds[index]->ne_lat = (float)(atof(tmpbuf) / 10000.0);
  if ( tmpbuf[6] == 'S' ) dteds[index]->ne_lat = -dteds[index]->ne_lat;
  strncpy( tmpbuf, dteds[index]->hdr->dsi.longNE,  8);
  tmpbuf[8] = '\0';
  dteds[index]->ne_lon = (float)(atof(tmpbuf) / 10000.0);
  if ( tmpbuf[7] == 'W' ) dteds[index]->ne_lon = -dteds[index]->ne_lon;

  /* determine min and max elevations                                      */
  dteds[index]->min_elev = 32000;
  dteds[index]->max_elev = -32000;
  for ( c = 0; c < dteds[index]->ncols; c++)
  {
    for ( r = 0; r < dteds[index]->nrows; r++)
    {
      e = dted_get_elev( dteds[index], r, c);
      if ( e < dteds[index]->min_elev ) dteds[index]->min_elev = e;
      if ( e > dteds[index]->max_elev ) dteds[index]->max_elev = e;
    }
  }
  return( dteds[index] );
}
 
/*---------------------------------------------------------------------------
Module Declaration:

Type     Name             Argument List
----     ----             -------------                                    */
void     dted_close       ()
{

/*
Local Variables:
  Type                Name            Description
  ----                ----            -----------                          */
  unsigned int        i;

  printf ("\nThere were %u dted structures.\n", numdteds);
  /* clears global dteds array                                             */
  if ( dteds == NULL ) {
    fprintf( stderr, "dted_close : NULL dted\n" );
    return;
  }

  for (i = 0; i < numdteds; i++)
  {
    if (dteds[i]->fd > 0) dted_close_file( dteds[i] );
    /* deallocate memory                                                   */
    free( dteds[i] );
  }
  numdteds = 0;
  maxdteds = 0;
  printf ("There were %u files mem mapped.\n", num_mapped);
  return;
}

/*---------------------------------------------------------------------------
Module Declaration:

Type     Name             Argument List
----     ----             -------------                                    */
void     dted_close_file( const DTED dted )
{
#if CRAY_YMP
  /* free file buffer                                                      */
  free( dted->dted_file );
#else
  /* undo memory mapping                                                   */
  munmap( (char *)dted->dted_file, dted->filesize );
#endif

  /* close image                                                           */
  close( dted->fd );
  dted->fd = 0;

  num_mapped++; /* count files actually mem mapped                         */
  return;
}

/*---------------------------------------------------------------------------
Module Declaration:

Type     Name             Argument List
----     ----             -------------                                    */
void     byte_swap      ( unsigned char bytes[2] )
{
   unsigned char temp = bytes[0];
   bytes[0] = bytes[1];
   bytes[1] = temp;
}
   
/*---------------------------------------------------------------------------
Module Declaration:

Type                Name                Argument List
----                ----                -------------                      */
DTED_Elevation_Type dted_get_min_elev ( const DTED dted )
{
   return dted->min_elev;
}

/*---------------------------------------------------------------------------
Module Declaration:

Type                Name                Argument List
----                ----                -------------                      */
DTED_Elevation_Type dted_get_max_elev( const DTED dted )
{
   return dted->max_elev;
}

/*---------------------------------------------------------------------------
Module Declaration:

Type                Name                Argument List
----                ----                -------------                      */
DTED_Elevation_Type dted_get_elev     ( const DTED dted,
                                        const int row,
                                        const int col )
{

/*
Local Variables:
  Type                Name            Description
  ----                ----            -----------*/
  DTED_Elevation_Type elev;
/*   DataRecord *data; */
#if CRAY_YMP
  char *data;
#else
  DTED_Elevation_Type *data;
#endif

#if CRAY_YMP
  data = (char *)dted->data + 2*(((dted->nrows + 6) * col) + 4);
  elev = ( (DTED_Elevation_Type)(*(data+2*row-1))       & 00000000377) |
         (((DTED_Elevation_Type)(*(data+2*row-2)) << 8) & 00000177400);
  /* Find negative numbers by detecting high sign bit, and convert them    */
  /* to twos-complement                                                    */
  if (elev & 00000100000) elev = ~((elev & 00000077777) - 1);
#else
/*   data = dted->data + col; */
  data = dted->data + ((dted->nrows + 6) * col) + 4;
  elev = *(data+row);
#endif

  if (swapFlag) byte_swap( (unsigned char *)&elev );

  return elev;
}

/*---------------------------------------------------------------------------
Module Declaration:

Type     Name                    Argument List
----     ----                    -------------                             */
int      dted_get_file_coverage( const DTED dted,
                                 float *const sw_lat,
                                 float *const sw_lon,
                                 float *const ne_lat,
                                 float *const ne_lon )
{
  if ( dted == NULL ) {
    fprintf( stderr, "dted_get_file_coverage : NULL dted\n" );
    return 0;
  }

  /* get coverage                                                          */
  *sw_lat = dted->sw_lat;
  *sw_lon = dted->sw_lon;
  *ne_lat = dted->ne_lat;
  *ne_lon = dted->ne_lon;

  return( 1 );
}

/*---------------------------------------------------------------------------
Module Declaration:

Type     Name                Argument List
----     ----                -------------                                 */
int      dted_get_file_size( const DTED dted,
                             int *const width,
                             int *const height )
{
  if ( dted == NULL ) {
    fprintf( stderr, "dted_get_file_size : NULL dted\n" );
    return 0;
  }

  /* get sizes                                                             */
  *width  = dted->ncols;
  *height = dted->nrows;

  return( 1 );
}

/*---------------------------------------------------------------------------
Module Declaration:

Type     Name             Argument List
----     ----             -------------                                    */
int      dted_xy_to_ll  ( const DTED dted,
                          const int x,
                          const int y,
                          double *const lat,
                          double *const lon )
{

  if ( dted == NULL ) {
    fprintf( stderr, "dted_xy_to_ll : NULL dted\n" );
    return 0;
  }
/* changed to max index not number of rows/cols mrb960315
 *  *lat = (double)dted->sw_lat + ( ( (double)y / (double)dted->nrows )
 *                                  * (double)(dted->ne_lat - dted->sw_lat) );
 *  *lon = (double)dted->sw_lon + ( ( (double)x / (double)dted->ncols )
 *                                  * (double)(dted->ne_lon - dted->sw_lon) );
 */
  *lat = (double)dted->sw_lat + ( ( (double)y / (double)(dted->nrows - 1) )
                                  * (double)(dted->ne_lat - dted->sw_lat) );
  *lon = (double)dted->sw_lon + ( ( (double)x / (double)(dted->ncols - 1) )
                                  * (double)(dted->ne_lon - dted->sw_lon) );
  return 1;
}

/*---------------------------------------------------------------------------
Module Declaration:

Type     Name             Argument List
----     ----             -------------                                    */
int      dted_ll_to_xy  ( const DTED dted,
                          const double lat,
                          const double lon,
                          int *const x,
                          int *const y)
{

  if ( dted == NULL ) {
    fprintf( stderr, "dted_ll_to_xy : NULL dted\n" );
    return 0;
  }
/* changed to range from 0 - max index not number of rows mrb960315
 *  *x = dted->ncols * (int)( ( (float)lon - dted->sw_lon ) 
 *                            / ( dted->ne_lon - dted->sw_lon ) );
 *  *y = dted->nrows * (int)( ( (float)lat - dted->sw_lat ) 
 *                            / ( dted->ne_lat - dted->sw_lat ) );
 */
  *x = (int)( (float)(dted->ncols - 1) * ( (float)lon - dted->sw_lon ) 
                                       / ( dted->ne_lon - dted->sw_lon ) );
  *y = (int)( (float)(dted->nrows - 1) * ( (float)lat - dted->sw_lat ) 
                                       / ( dted->ne_lat - dted->sw_lat ) );
  return 1;
}

/*---------------------------------------------------------------------------
Module Declaration:

Type     Name             Argument List
----     ----             -------------                                    */
DTED     dted_cp        ( const DTED src )
{

/*
Local Variables:
  Type                Name            Description
  ----                ----            -----------                          */
  DTEDFile           *dst;

  if ( src == NULL )
  {
    fprintf( stderr, "dted_cp : NULL dted\n" );
    return NULL;
  }
  if ( ( dst = (DTEDFile *)malloc( sizeof( DTEDFile ) ) ) == NULL)
  {
    fprintf (stderr, "dted_cp : can't allocate memory for structure\n");
    return dst;
  }
  memcpy (dst, src, sizeof(DTEDFile) );
/*  if ( ( dst->filename = (char *)malloc(256) ) == NULL )
 *  {
 *    fprintf( stderr, "dted_cp : can't allocate memory for filename.\n" );
 *    free (dst);
 *    return NULL;
 *  } */
  strcpy (dst->filename, src->filename);
  if ( ( dst->dted_file = (unsigned char *) malloc (src->filesize) ) == NULL )
  {
    fprintf( stderr, "dted_cp : can't allocate memory for data.\n" );
    free (dst->filename);
    free (dst);
    return NULL;
  }
  memcpy (dst->dted_file, src->dted_file, src->filesize );
  dst->hdr = (DTEDHdr *)dst->dted_file;
#if 0
  dst->data = (DTED_Elevation_Type *)(dst->dted_file + sizeof(DTEDHdr));
#else
  dst->data = (DTED_Elevation_Type *)(dst->dted_file + 3428);
#endif
  return dst;
}

/*---------------------------------------------------------------------------
Module Declaration:

Type     Name             Argument List
----     ----             -------------                                    */
DTED     dted_new       ( const DTED src,
                          char *const dst_filename,
                          const int col,
                          const int row)
{

/*
Local Variables:
  Type                Name            Description
  ----                ----            -----------                          */
  DTEDFile           *dst;
  unsigned int        filesize;
  char                tmps[5];

  if ( src == NULL )
  {
    fprintf( stderr, "dted_new : NULL dted\n" );
    dst = NULL;
    return dst;
  }
  if ( ( dst = (DTEDFile *)malloc( sizeof( DTEDFile ) ) ) == NULL)
  {
    fprintf (stderr, "dted_new : can't allocate memory for structure\n");
    return dst;
  }
  memcpy (dst, src, sizeof(DTEDFile) );
  strcpy (dst->filename, dst_filename);
  dst->ncols = col;
  dst->nrows = row;
  filesize = (unsigned int)((((row + 6) * col) * 2) + 3428);
  /*         6row ovrhd 2bytes/short   header size */ 
  if ( ( dst->dted_file = (unsigned char *) malloc (filesize) ) == NULL )
  {
    fprintf( stderr, "dted_cp : can't allocate memory for data.\n" );
    free (dst->filename);
    free (dst);
    dst = NULL;
    return dst;
  }
  dst->filesize = filesize;
  
  dst->hdr = (DTEDHdr *)dst->dted_file;
#if 0
  dst->data = (DTED_Elevation_Type *)(dst->dted_file + sizeof(DTEDHdr));
#else
  dst->data = (DTED_Elevation_Type *)(dst->dted_file + 3428);
#endif
  /* copy the header */
  memcpy (dst->dted_file, src->dted_file, 3428);

  sprintf (tmps, "%4d", col);
  strncpy (dst->hdr->uhl.nLongLines, tmps, 4);
  sprintf (tmps, "%4d", row);
  strncpy (dst->hdr->uhl.nLatLines, tmps, 4);

  return dst;
}

/*---------------------------------------------------------------------------
Module Declaration:

Type     Name             Argument List
----     ----             -------------                                    */
void     dted_put_elev  ( const DTED dted,
                          const int row,
                          const int col,
                          const DTED_Elevation_Type elev )
{

/*
Local Variables:
  Type                Name            Description
  ----                ----            -----------                          */
#if CRAY_YMP
  char                *data;
  DTED_Elevation_Type local_elev;
#else
  DTED_Elevation_Type *data;
#endif

#if CRAY_YMP
  data = (char *)dted->data + 2*(((dted->nrows + 6) * col) + 4);
  /* Find negative numbers by detecting twos-complement sign bit, and then */
  /* convert to sign bit plus unsigned magnitude                           */
  if (elev & 020000000000)
  {
    local_elev = (~elev + 1) & 00000100000;
  }
  else local_elev = elev;
  *(data+2*row-1) = (char)( local_elev       & 00000000377);
  *(data+2*row-2) = (char)((local_elev >> 8) & 00000000377);
#else
  data = dted->data + ((dted->nrows + 6) * col) + 4;
  *(data+row) = elev;
#endif
  return;
}

/*---------------------------------------------------------------------------
Module Declaration:

Type     Name             Argument List
----     ----             -------------                                    */
void     dted_write     ( char *const dted_filename,
                          const DTED dted)
{

/*
Local Variables:
  Type                Name            Description
  ----                ----            -----------                          */
  struct stat         stat_buf;
  int                 status;

  /* see if the file exists, (stat returns information)                    */
  if ( !(status = stat (dted_filename, &stat_buf) ) )
  {
    fprintf(stderr,
            "dted_write : Warning file '%s' exists and will be overwritten.\n",
            dted_filename);
    status = chmod (dted_filename, S_IRUSR | S_IWUSR | S_IRGRP);
  }
  /* open file */
  strcpy (dted->filename, dted_filename);
  dted->fd = open( dted_filename, O_RDWR | O_CREAT);
  if ( dted->fd <= 0 ) 
  {
    perror("dted_write");
    fprintf( stderr, "dted_write : can't open '%s'\n",
             dted -> filename );
    return;
  }
  status = write (dted->fd, dted->dted_file, dted->filesize);
  if (status != (int)dted->filesize)
    fprintf (stderr, "dted_write : Error status = %d ,filesize = %u\n",
             status, dted->filesize);
  close (dted->fd);
  status = chmod (dted_filename, S_IRUSR | S_IWUSR | S_IRGRP);
  return;
}
/*-------------------------------------------------------------------------*/
/*      Classification:  Unclassified                                      */
/* @(#) rsDbSetTerrainFile.c 2.1 07/02/97 */
/*      Classification:  Unclassified
-----------------------------------------------------------------------------

Name: rsDbSetTerrainFile

Description:
  This module geenerates, as a string, the DTED terrain file name
  corresponding to a user-input integral latitude and longitude.

Author/Date/Company/Phone:
  Merrill R. Bean / 16May97 / NMSU / 505-522-9100

Change History:
  Rev  Date    Engineer             SP/CR      Description
  ---  ----    --------             -----      -----------
  new  16May97 Pratt / Bean / Walsh C-1681     new capability

Performance Requirements:
  Provide reasonably good string manipulation efficiency.

Software Methods:
  In accordance with the Argus Software Handbook

Constraints:
  None

Significant Calculations/Formulas:
  None

External Inputs/Outputs:
#include  File Name       Description
--------  ---------       -----------                                      */
#include <stdio.h>     /* i.e. sprintf() */
#include <string.h>    /* i.e. strcat()  */

/*---------------------------------------------------------------------------
Module Declaration:

Type     Name                 Argument List
----     ----                 -------------                                */
char    *rsDbSetTerrainFile ( const int Lat,
                              int Long,
                              char *const terrainFileName )
{

/*
Local Variables:
  Type                Name            Description
  ----                ----            -----------                          */
  char                str[4];

/*-------------------------------------------------------------------------*/

  if (Long > 180) Long -= 360;
  if (Long < -180) Long += 360;
  if (Long >= 0)
  {
     strcat ( terrainFileName, "e" );
     sprintf ( str, "%03d", Long );
  }
  else
  {
    strcat ( terrainFileName, "w" );
    sprintf ( str, "%03d", -Long );
  }
  strcat  ( terrainFileName, str );
  if ( Lat >= 0 )
  {
    strcat ( terrainFileName, "/n" );
    sprintf ( str, "%02d", Lat );
  }
  else
  {
    strcat ( terrainFileName, "/s" );
    sprintf ( str, "%02d", Lat );
  }
  strcat  ( terrainFileName, str );
  strcat  ( terrainFileName, ".dt1" );

  return  ( terrainFileName );
}

/*-------------------------------------------------------------------------*/
/*      Classification:  Unclassified */
/* @(#) rsanmovetosurface.c 2.1 07/02/97 */
/*      Classification:  Unclassified
-----------------------------------------------------------------------------

Name: rsanmovetosurface

Description:
  Given as input an LLA position, path to dted data, debug flag, this module
  moves a point to the suface of the earth

Author/Date/Company/Phone:
  Matt Radecki / 29Jun93 / Martin-Marietta Corp. /

Change History:
  Rev  Date    Engineer             SP/CR      Description
  ---  ----    --------             -----      -----------
  new  16May97 Pratt / Bean / Walsh C-1681     new capability for ARGUS

Performance Requirements:
  Provide reasonably high thruput when manipulating large data files.

Software Methods:
  In accordance with the Argus Software Handbook

Constraints:
  None

Significant Calculations/Formulas:
  None

Global Macros:
Define  Symbol  Value
------  ------  -----                                                      */
#define LAT     0
#define LON     1
#define ALT     2

/*
External Inputs/Outputs:
#include  File Name       Description
--------  ---------       -----------                                      */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "dted.h"

/*
Global Variables:
Type                Name              Description
----                ----              -----------                          */
extern char        *rsDbSetTerrainFile ( const int, int, char *const );

/*---------------------------------------------------------------------------
Module Declaration:

Type     Name             Argument List
----     ----             -------------                                    */
void rsanmovetosurface (double pos[],
			char path[])
{

/*
Local Variables:
  Type                Name            Description
  ----                ----            -----------                          */
  double              wLon, sLat, eLon, nLat, t, u, sw, se, ne, nw;
  int                 Lat, Long, col, row, nxr, nxc;
  DTED                dtedfile;
  char               *FileName;

/*-------------------------------------------------------------------------*/

  FileName = (char *) malloc ( strlen(path) + 13 );
  strcpy (FileName, path);  /* insert the path to the dted files           */

  Lat = (int) pos[LAT];
  if (pos[LAT] - (double) Lat < 0.0)  Lat--;
  Long = (int) pos[LON];
  if (pos[LON] - (double) Long < 0.0)  Long--;
  if (!( dtedfile = dted_open( rsDbSetTerrainFile ( Lat, Long, FileName ) ) ) )
  {
    pos[ ALT ] = 0.0;
    free ( FileName );
    return;
  }
  if ( dtedfile->fd <= 0 )
  {
    pos[ ALT ] = 0.0;
    free ( FileName );
    return;
  }
  free ( FileName );

  /* mark the file as "in use"                                             */
  dtedfile->recently_used = 1;

  /* get the index and lat lon of the nearest lat lon in the dted file     */
  dted_ll_to_xy (dtedfile, pos[LAT], pos[LON], &col, &row);
  dted_xy_to_ll (dtedfile, col, row, &sLat, &wLon);
  /* find the other bound for the input lat lon                            */
  nxr = row;
  nxc = col;
  if (pos[LAT] > sLat) nxr++;
  if (pos[LAT] < sLat) nxr--;
  if (pos[LON] > wLon) nxc++;
  if (pos[LON] < wLon) nxc--;
  /* don't go outside the file                                             */
  if (nxc < 0) nxc = 0;
  if (nxr < 0) nxr = 0;
  if (nxc >= dtedfile->ncols) nxc = dtedfile->ncols - 1;
  if (nxr >= dtedfile->nrows) nxr = dtedfile->nrows - 1;
  /* find the lat lon of the bounding point                                */
  dted_xy_to_ll (dtedfile, nxc, nxr, &nLat, &eLon);
  /* intermediate value for bilinear interpolation                         */
  if (nxr == row) u = 0.0;  /* try not to divde by 0                       */
  else u = (pos[LAT] - sLat)/(nLat - sLat);
  if (nxc == col) t = 0.0;
  else t = (pos[LON] - wLon)/(eLon - wLon);
  /* find the alt at the four bounding points                              */
  sw = (double) dted_get_elev (dtedfile, row, col);
  se = (double) dted_get_elev (dtedfile, row, nxc);
  ne = (double) dted_get_elev (dtedfile, nxr, nxc);
  nw = (double) dted_get_elev (dtedfile, nxr, col);
  /* interpolate                                                           */
  pos[ALT] = (1.0-t)*(1.0-u)*sw+t*(1.0-u)*se+t*u*ne+(1.0-t)*u*nw;
  pos[ALT] = 0.001 * pos[ALT]; /* convert to km                            */
  /* debug output 
  printf ("rsanmovetosurface: t=%lf, u=%lf, row=%d, nxr=%d, col=%d, nxc=%d\n",
    t, u, row, nxr, col, nxc);
  printf ("rsanmovetosurface: sw Lat %lf, Lon %lf, Alt %lf\n", sLat, wLon, sw);
  printf ("rsanmovetosurface: se Lat %lf, Lon %lf, Alt %lf\n", sLat, eLon, se);
  printf ("rsanmovetosurface: ne Lat %lf, Lon %lf, Alt %lf\n", nLat, eLon, ne);
  printf ("rsanmovetosurface: nw Lat %lf, Lon %lf, Alt %lf\n", nLat, wLon, nw);
  printf ("rsanmovetosurface: lla %lf %lf %lf\n",
    pos[LAT], pos[LON], pos[ALT]+6371.0087714);
  */
}

/*-------------------------------------------------------------------------*/
/*      Classification:  Unclassified */
/* @(#) trnmskgn.c 2.1 07/02/97 */
/*      Classification:   Unclassified                                     */
/*-----------------------------------------------------------------------------

Name: TRNMSKGN

Description:
  This program generates a terrain mask data file for a given sensor site.
  These data files, one for each ground-based sensor, are produced prior to
  runs of the ARGUS program.

External Inputs/Outputs:
#include  FileName        Description
--------  --------        -----------                                      */
#include <stdio.h>     /* C Standard I/O                                   */
#include <stdlib.h>    /* C Standard System Interfaces                     */
#include <string.h>    /* C Standard String Routines                       */
#include <math.h>      /* C Math Library                                   */
#include "dted.h"      /* DTED access routines                             */

/*
Macro Definitions:
 Define Macro Name               Value
 ------ ----------               -----                                     */
#define RADIUS_OF_EARTH          6378.0
#define MAX_FILENAME_LENGTH          13
#define MAX_EXTENSION_LENGTH          3
#define MAX_DTED_PATHNAME_LENGTH     64
#define DEFAULT_N_AZIMUTH           360
#define DEFAULT_RANGE_RESOLUTION    0.1
#define DEFAULT_REQUIRED_MAX_DEPTH  0.0
#define MAX_TERRAIN_ALTITUDE       10.0
                         /* Maximum terrain altitude in kilometers [set    */
                         /* equal to 10.0]. Used in determining the maximum*/
                         /* angular range alphaMax of terrain points above */
                         /* the the sensor horizon.                        */
                         /* Note:  Overestimating this quantity will not   */
                         /* affect accuracy, but will reduce efficiency.   */
#define MIN_RANGE_RESOLUTION     0.0001
                         /* Minimum allowable value of the range resolution*/
                         /* in kilometers.                                 */
#define MAX_SITE_ALTITUDE         150.0
                         /* Maximum allowable value of sensor altitude in  */
                         /* kilometers.                                    */
#define INT_MAX                   32767
                         /* Maximum integer that can be saved with the int */
                         /* type.                                          */

/*
Global Variables:
Type                Name         Description
----                ----         -----------                               */
double              pi;       /* Circumference / diameter of a circle.     */
extern DTEDFile   **dteds;    /* open DTED file structure                  */
extern unsigned int numdteds; /* number of open dted files                 */

/*
Structure Type Definitions:                                                */
typedef struct {double value; short error;} doubleStruct;
typedef struct {int value; short error;} intStruct;

/*
Function Prototypes:
Extern Type   Name              Parameter List
------ ----   ----              --------------                             */
extern double sinElevAngle    (const double, const double, const double,
                               const double, const double, double *const,
                               double *const, char *);
extern double terrainElev     (const double, const double, char *);
extern void   writeTerrainMaskDataFile( const int, const double, const double,
                               const double, const int, const double,
                               const double, const long, int *const,
                               long *const, double **const,
                               /* double **const, */ double **const );
extern short   stringCompare  (char *const, char *const);
extern doubleStruct textToDoubleConvert(char *const);
extern intStruct textToIntConvert(char *const);
extern void    correctUsage   (char *const);
extern void    rsanmovetosurface(double *, char *);

/*
Module Declaration:
Type     Name             Argument List
----     ----             -------------                                    */
int      genMask          (double ArgLong,
                           double ArgLat,
                           double ArgAlt,
                           int    ArgSiteNum,
                           char  *ArgDtedPath,
                           double ArgRange,
                           int    ArgAzimuth,
                           double ArgDepth)
{
/*
Local variables:
  Type    Name              Description
  ----    ----              -----------                                    */
  double  siteLongitude; /* Longitude of sensor site in degrees.           */
  double  siteLatitude;  /* Latitude  of sensor site in degrees.           */
  double  azimuth;       /* Azimuthal angle of target relative to sensor,  */
                         /* expressed in radians east from due north.      */    
  double  alpha;         /* Angular range in radians of target from site   */
                         /* drawn from the earth center.                   */
  double  sinElev;       /* Sine of elevation angle of terrain point from  */
                         /* sensor site. Used in stepping along each       */
                         /* azimuthal spoke to determine the terrain high- */
                         /* points.                                        */
  double  sinElev1;      /* Saved value of sinElev from previous step along*/
                         /* azimuthal spoke.                               */
  double  siteAltitude;  /* Altitude of sensor site in kilometers.         */
  double  sitePositionMag;
                         /* Magnitude of geocentric site position vector.  */
  double  alphaMax;      /* Maximum angular range of any terrain point.    */
                         /* above horizon seen from sensor site.           */
  double  deltaAlpha;    /* Increment in range angle along azimuthal spoke */
                         /* Used in determining terrain high-points.       */
  double  sinElevMax;    /* Sine of maximum elevation angle from sensor    */
                         /* site to terrain points along azimuthal spoke.  */
                         /* Used in determining terrain high-points.       */
  double  deltaAzimuth;  /* Increment in azimuthal angle [in degrees]      */
                         /* between successive azimuthal spokes.           */
  double  rangeResolution;
                         /* Range on the earth surface [in km] over which  */
                         /* the terrain altitude data is resolved for      */
                         /* determining the high-points.                   */
                         /* Note:  Underestimating this quantity will not  */
                         /* affect accuracy, but will reduce efficiency.   */
  double  siteLongRad;   /* Longitude of sensor site in radians.           */
  double  siteLatRad;    /* Latitude  of sensor site in radians.           */
  double  terrElev;      /* Elevation of terrain point along azimuthal     */
                         /* spoke.                                         */
  double  terrElev1 = 0.0;
                         /* Elevation of terrain point from previous step  */
                         /* along azimuthal spoke.                         */
  double  elevDiff;      /* Difference in elevation between the previous   */
                         /* candidate terrain high-point and the current   */
                         /* terrain point.                                 */
  double  elevDiffMax;   /* Maximum value of elevDiff between successive   */
                         /* terrain high-points.                           */
  double  elevHighPoint; /* Elevation of the last candidate terrain high-  */
                         /* point along an azimuthal spoke.                */
  double  horizonAngle;  /* Angular range from sensor site to its horizon. */
  double  maxHorizonAngle;
                         /* Angular range from highest terrain altitude    */
                         /* [ie. 10 km] to its horizon.                    */
  double  slantRange;    /* Slant range [ie. undirected distance] from     */
                         /* sensor site to terrain point in kilometers.    */
  double  slantRange1;   /* Value of slantRange saved from previous radial */
                         /* step along azimuthal spoke.                    */  
/*double **alphaArray;      Dynamically allocated array of pointers to     */
                         /* angular ranges of terrain high points along    */
                         /* given azimuthal spokes. Each [pointer] element */
                         /* of the array corresponds to an azimuthal spoke,*/
                         /* and memory is dynamically allocated to hold the*/
                         /* angular ranges of the terrain high points along*/
                         /* the spoke.                                     */
  double **sinElevArray; /* Array of pointers to the sines of elevation    */
                         /* angles at the terrain high-points.  Each       */
                         /* [pointer] element of the array corresponds to  */
                         /* an azimuthal spoke, and memory is dynamically  */
                         /* allocated to hold the elevation angle sines    */
                         /* of each highpoint along the spoke.             */
  double **slantRangeArray;
                         /* Array of pointers to the slant ranges from the */
                         /* sensor site to the terrain high-points.  Each  */
                         /* [pointer] element of the array corresponds to  */
                         /* an azimuthal spoke, and memory is dynamically  */
                         /* allocated to hold the slant ranges of each     */
                         /* high-point along the spoke.                    */
  double  rangeAngleResolution;
                         /* Angular range [in radians] over which the      */
                         /* terrain altitude data is resolved. [Equal to   */
                         /* to (rangeResolution / RADIUS_OF_EARTH)]        */
                         /* Note:  Underestimating this quantity will not  */
                         /* affect accuracy, but will reduce efficiency.   */
  double  requiredMaxDepth;
                         /* Required value for the maximum depth [in km]   */
                         /* between successive terrain high-points.  This  */
                         /* depth is the difference in altitude between the*/
                         /* inside high-point and the point of lowest      */
                         /* terrain altitude between this high-point and   */
                         /* the next along the azimuthal spoke.  Used to   */
                         /* avoid setting high-points where there is not   */
                         /* enough space behind the high-point for a target*/
                         /* to be obscured.                                */
  int     i;             /* Loop index.                                    */
  int     siteNumber;    /* Integer between 0 and 999 designating the      */
                         /* sensor site.                                   */
  int     nAzimuth;      /* Number of azimuthal spokes selected.           */
  int     numAlloc;      /* Number of dynamically allocated pointers to    */
                         /* various link-list structures used to hold      */
                         /* terrain high-point data.                       */
  int    *numHighPoints; /* Dynamically allocated integer array containing */
                         /* the number of high-points for each azimuthal   */
                         /* spoke.                                         */
  int     iAzimuth;      /* Index for azimuthal spokes.                    */
  long    nAlpha;        /* Number of points used along each azimuthal     */
                         /* spoke for determining terrain high-points.     */
  long    iAlpha;        /* Loop index for testing points along azimuthal  */
                         /* spoke do determine the high-points.            */
  long    totalHighPoints;
                         /* Total number of high-points along all azimuthal*/
                         /* spokes.                                        */
  long   *offset;        /* Dynamically allocated array of offset values   */
                         /* for indices of a 1D array containing all high- */
                         /* points for all spokes.                         */
  short   stcmp1, stcmp2, stcmp3;
                         /* Short integer variables used to indicate string*/
                         /* comparisons.                                   */
  char    optionString_rr[] = "-rr";
                         /* Command line option flag indicating the        */
                         /* following value is the range resolution [in km]*/
                         /* to be used.                                    */
  char    optionString_an[] = "-an";
                         /* Command line option flag indicating the        */
                         /* following value is the number of azimuthal     */
                         /* spokes to be used.                             */
  char    optionString_td[] = "-td";
                         /* Command line option flag indicating the        */
                         /* following value is the terrain depth [in km]   */
                         /* required between successive high-points.       */
  char    DTEDpath[MAX_DTED_PATHNAME_LENGTH];
  typedef struct reallink { double real; struct reallink *next; } realLink;
/*realLink *headPtrAlpha;   Pointer to the first element of the link-list  */
                         /* structure containing angular ranges of         */
                         /* high-points along a given azimuthal spoke.     */
  realLink *headPtrElev; /* Pointer to the first element of the link-list  */
                         /* structure containing the sines of the elevation*/
                         /* angles from the sensor to the terrain high-    */
                         /* points along a given azimuthal spoke.          */
  realLink *headPtrSlantRange;
                         /* Pointer to the first element of the link-list  */
                         /* structure containing the slant range of the    */
                         /* terrain high-points from the sensor site along */
                         /* a given azimuthal spoke.                       */
/*realLink *currentPtrAlpha; */
                         /* Pointer to the link-list element used for      */
                         /* saving the angular range of the current terrain*/
                         /* high-point.                                    */
  realLink *currentPtrElev;
                         /* Pointer to the link-list element used for      */
                         /* saving the sine of the elevation angle of the  */
                         /* current terrain high-point.                    */
  realLink *currentPtrSlantRange;
                         /* Pointer to the link-list element used for      */
                         /* saving the slant range [in km] of the current  */
                         /* terrain point from the sensor site.            */
/*realLink *nextPtrAlpha;   Pointer to the next link-list element for      */ 
                         /* saving the angular range of the next terrain   */
                         /* high-point.                                    */
  realLink *nextPtrElev; /* Pointer to the next link-list element for      */
                         /* saving the sine of the elevation angle of the  */
                         /* next terrain high-point.                       */
  realLink *nextPtrSlantRange;
                         /* Pointer to the next link-list element for      */
                         /* saving the slant range of the next terrain     */
                         /* high-point from the sensor site.               */
  doubleStruct abDoubleStruct;
                         /* Structure used in converting string type       */
                         /* command-line arguments into double type        */
                         /* variables for use in determining the sensor    */
                         /* site position.                                 */
  intStruct abIntStruct; /* Structure used in converting the final string  */
                         /* type command-line argument containing the      */ 
                         /* sensor site number to an integer variable      */

/*-------------------------------------------------------------------------*/    
  pi = 4.0 * atan(1.0);  /* Initialize local variables for input parameter */
                         /* range limitations.                             */

  siteLongitude  = ArgLong;
  siteLongRad    = siteLongitude * pi / 180.0;
 
  siteLatitude   = ArgLat;
  siteLatRad     = siteLatitude  * pi / 180.0;
  
  siteAltitude   = ArgAlt; 
  if (siteAltitude > MAX_SITE_ALTITUDE)
  {
                         /* If sensor site altitude is greater than its    */
                         /* allowed maximum, display error message and exit*/
                         /* program.                                       */ 
    printf ("\n\n   *** Error.  Site altitude is greater than the maximum ");  
    printf ("allowed value \n              of %lf km.\n\n", MAX_SITE_ALTITUDE);
    exit(1);
  }

  siteNumber = ArgSiteNum;
  if ( (siteNumber < 0) || (siteNumber > 999) )
  {
    printf("\n\n   *** Error.  Illegal sensor number.  Sensor number must ");
    printf("be between 0 and 999.\n\n"); 
    exit (1);
  }

  strcpy (DTEDpath, ArgDtedPath);
                         /* Copy command line argument containing the path */
                         /* to DTED files onto the DTEDpath string.        */
  nAzimuth = DEFAULT_N_AZIMUTH;  
                         /* Set number of azimuthal spokes used in terrain */
                         /* to default value.                              */
  horizonAngle = acos (RADIUS_OF_EARTH / (RADIUS_OF_EARTH + siteAltitude 
    + terrainElev (siteLongitude, siteLatitude, DTEDpath)));
                         /* Calculate the angle, drawn from the center of  */
                         /* the earth, from the sensor site to its horizon */
                         /* at zero altitude terrain.                      */
  maxHorizonAngle = acos (RADIUS_OF_EARTH /
                          (RADIUS_OF_EARTH + MAX_TERRAIN_ALTITUDE));
                         /* Calculate the angle, drawn from the center of  */
                         /* the earth, from the point of maximum terrain   */
                         /* altitude to its horizon at zero terrain        */
                         /* altitude.                                      */
  alphaMax = horizonAngle + maxHorizonAngle;
                         /* Set maximum angular range on the surface of the*/
                         /* earth to be covered for each azimuthal spoke.  */
  rangeResolution = DEFAULT_RANGE_RESOLUTION;
                         /* Set range resolution [in kilometers] on the    */
                         /* surface of the earth to its default value.     */
  requiredMaxDepth = DEFAULT_REQUIRED_MAX_DEPTH;
                         /* Set the required maximum depth parameter to its*/
                         /* default value.                                 */
  if (ArgRange > 0.0)
       rangeResolution = ArgRange;
  if (rangeResolution < MIN_RANGE_RESOLUTION)
  {
                         /* If the input range resolution is below the     */
                         /* minumum value, display error message and exit  */
                         /* program.                                       */ 
    printf("\n\n   *** Error.  Range resolution less than the required ");
    printf("minimum value of %lf\n\n", MIN_RANGE_RESOLUTION);
    exit(1);
  }

  if (ArgAzimuth > 0.0)
      nAzimuth = ArgAzimuth;
  if ( (nAzimuth < 1) )
  {                      /* Check that the number of azimuthal spokes is at*/ 
                         /* at least 1.  If not, display error message and */
                         /* exit program.                                  */
    printf("\n\n   *** Error.  Number of azimuthal spokes must be greater ");
    printf("than or equal to 1.\n\n");
    exit(1);
  }

  if (ArgDepth > 0.0)
        requiredMaxDepth = ArgDepth;

  nAlpha = 2;
  deltaAlpha = alphaMax / (double)nAlpha;
                         /* Initialize the number nAlpha of angular range  */
                         /* steps to be taken along each azimuthal spoke,  */
                         /* and the size deltaAlpha of each angular range  */
                         /* increment.                                     */
  rangeAngleResolution = rangeResolution / RADIUS_OF_EARTH;
                         /* Calculate the range angle, drawn from the      */
                         /* center of the earth, corresponding to the      */
                         /* terrain range resolution of the surface of the */
                         /* earth.                                         */
  while (deltaAlpha >= rangeAngleResolution)
  {
    nAlpha = 2 * nAlpha;
    deltaAlpha = deltaAlpha / 2.0;
  }
                         /* Determine nAlpha and deltaAlpha by doubling    */
                         /* nAlpha and halving deltaAlpha until deltaAlpha */
                         /* is less than the resolution of range angle     */
                         /* determined in the previous step.               */
  deltaAzimuth = 2.0 * pi / (double)nAzimuth;
                         /* Calculate the increment in angle between       */
                         /* successive azimuthal spokes [in radians].      */
                         /* Dynamically allocate array of pointers to      */
                         /* arrays containing angular range data for the   */
                         /* terrain high-points along the individual       */
                         /* azimuthal spokes.                              */  
/*if (!(alphaArray =
          (double **) malloc ( (size_t)nAzimuth * sizeof (double *) )))
  {
    printf ("\n\n   *** Error.  Failure allocating memory for range ");
    printf ("angle pointer array.\n\n");
    exit (1);
  } */
                         /* Dynamically allocate array of pointers to      */
                         /* arrays containing the sine of the elevation    */
                         /* angles [along azimuthal spokes] from the high- */
                         /* points to the sensors.                         */
  if (!(sinElevArray =
          (double **) malloc ( (size_t)nAzimuth * sizeof (double *) )))
  {
    printf ("\n\n   *** Error.  Failure allocating memory for sine of ");
    printf ("elevation angle\n               pointer array.\n\n");
    exit (1);
  }                      
                         /* Dynamically allocate array of pointers to      */
                         /* arrays containing angular range data for the   */
                         /* terrain high-points along the individual       */
                         /* azimuthal spokes.                              */  
  if (!(slantRangeArray =
          (double **) malloc ( (size_t)nAzimuth * sizeof (double *) )))
  {
    printf ("\n\n   *** Error.  Failure allocating memory for the slant ");
    printf ("range pointer array.\n\n");
    exit (1);
  }
                         /* Dynamically allocate array of integers for     */
                         /* containing the number of terrain high-points   */
                         /* for each azimuthal spoke.                      */
  if (!(numHighPoints = (int *) malloc ( (size_t)nAzimuth * sizeof (int) )))
  {
    printf ("\n\n   *** Error.  Failure allocating memory for number of ");
    printf ("terrain high-points\n               for the individual ");
    printf ("azimuthal spokes.\n\n");
    exit (1);
  }                      
                         /* Dynamically allocate memory for array of offset*/
                         /* values for indices of a 1D array containing all*/
                         /* high-points for all spokes.                    */
  if (!(offset = (long *) malloc ( (size_t)nAzimuth * sizeof (long) )))
  {
    printf ("\n\n   *** Error.  Failure allocating memory for offset ");
    printf ("values array.\n\n");
  }
                         /* Dynamically allocate memory for the head       */
                         /* pointer to the link list containing angular    */
                         /* range data on the terrain high-points. Display */
                         /* an error message if memory allocation fails.   */
/*if (!(headPtrAlpha = (realLink *) malloc ( sizeof (realLink) )))
  {
    printf ("\n\n   *** Error.  Failure allocating memory for range ");
    printf ("angle link-list.\n\n");
    exit (1);
  } */
                         /* Dynamically allocate memory for the head       */
                         /* pointer to the link list containing the sines  */
                         /* of the elevation angles from the sensor site   */
                         /* to the terrain high-points.  Display an error  */
                         /* message and exit program if memory allocation  */
                         /* fails.                                         */
  if (!(headPtrElev = (realLink *) malloc ( sizeof(realLink) )))
  {
    printf ("\n\n   *** Error.  Failure allocating memory for sine of ");
    printf ("elevation angle\n               link-list.\n\n");
    exit (1);
  }
                         /* Dynamically allocate memory for the head       */
                         /* pointer to the link list containing the slant  */
                         /* ranges from the sensor site of the terrain     */
                         /* high-points.  Display an error message and exit*/
                         /* program if memory allocation fails.            */
  if (!(headPtrSlantRange = (realLink *) malloc ( sizeof(realLink) )))
  {
    printf ("\n\n   *** Error.  Failure allocating memory for slant range ");
    printf ("link-list.\n\n");
    exit (1);
  }
  offset[0] = 0;         /* Initialize array of offset values for a 1D     */
                         /* array containing angular ranges or sines of    */
                         /* elevation angles of terrain high points.       */
  numAlloc  = 0;         /* Initialize number of link list elements        */
                         /* allocated for saving terrain high-point data.  */
  printf ("\n\n\n               Calculating terrain high-point data.\n");
  sitePositionMag = RADIUS_OF_EARTH + siteAltitude 
        + terrainElev (siteLongitude, siteLatitude, DTEDpath);
                        /* Calculate the magnitude of the site position    */
                        /* vector.  This is equal to the sum of the site   */
                        /* altitude, radius of the earth, and the terrain  */
                        /* elevation of the site.                          */

  for (iAzimuth = 0; iAzimuth < nAzimuth; iAzimuth++)
  {
                         /* Begin loop for selecting azimuthal spokes along*/
                         /* which the terrain high-points are determined.  */
    azimuth = (double)iAzimuth * deltaAzimuth;
                         /* Calculate azimuthal angle of spoke.            */
/*  currentPtrAlpha = headPtrAlpha; */
    currentPtrElev  = headPtrElev;
    currentPtrSlantRange = headPtrSlantRange;
                         /* Initialize pointers to link-list structures    */
                         /* containing the angular range and sine of       */
                         /* elevation angle data from high-points.         */
    numHighPoints[iAzimuth] = 0;
                         /* Initialize number of high points array element.*/
    sinElevMax = -1.0;
    sinElev1 = -1.0;     /* Initialize the maximum sine of elevation angle */
                         /* and the sine of elevation angle from the       */
                         /* previous angular range [ie. alpha] step.       */
    slantRange1 = 0.0;   /* Initialize the slant range value from the      */
                         /* previous angular range step.                   */
    elevHighPoint = 0.0;
    elevDiffMax = 0.0;   /* Initialize the elevation of the last terrain   */
                         /* candidate high-point and maximum elevation     */
                         /* difference variables.                          */
    for (iAlpha = 1; iAlpha <= nAlpha; iAlpha++)
    {
                         /* Begin loop for determining the terrain high-   */
                         /* points along the given azimuthal spoke.        */
      alpha = (double)iAlpha * deltaAlpha;
                         /* Calculate angular range of current terrain     */
                         /* point.                                         */
      sinElev = sinElevAngle( siteLongRad, siteLatRad, sitePositionMag, 
                              azimuth, alpha, &slantRange, &terrElev,
                              DTEDpath );
                         /* Calculate sine of elevation angle to given     */
                         /* terrain point.                                 */
      elevDiff = elevHighPoint - terrElev;
      if (elevDiff > elevDiffMax) elevDiffMax = elevDiff;
      if (sinElev > sinElevMax) sinElevMax = sinElev;
                         /* If the sine of the elevation angle to the      */
                         /* current terrain point sinElev is greater than  */
                         /* the current value of the maximum sine of       */
                         /* elevation angle sinElevMax, set                */
                         /* sinElevMax = sinElev                           */
      if ( (sinElev1 == sinElevMax) && (sinElev < sinElev1) )
      {
                         /* If a maximum sine of elevation angle occurred  */
                         /* at the preceding angular range step and the    */
                         /* current sine of terrain point elevation angle  */
                         /* is less than this preceding value, then a      */
                         /* candidate terrain high-point was reached on the*/
                         /* preceding step.  In this case, we must save the*/
                         /* angular range of this step and the new value of*/
                         /* sinElevMax as the current elements in the      */
                         /* realLink structures.                           */
        if (numAlloc == numHighPoints[iAzimuth])
        {
                         /* If the number of currently allocated elements  */
                         /* of the realLink structures is equal to the     */
                         /* current number of terrain high-points found,   */
                         /* memory must be allocated for the next element  */
                         /* in each realLink structure.                    */
                         /* Allocate memory for the next angular range     */
                         /* realLink structure element.  Display error     */
                         /* message and exit program if memory allocation  */
                         /* fails.                                         */
/*        if (!(nextPtrAlpha = (realLink *) malloc( sizeof(realLink) )))
          {
            printf ("\n\n   *** Error.  Failure allocating memory ");
            printf ("for range angle link-list.\n\n");
            exit (1);
          } */
                         /* Allocate memory for the next sine of elevation */
                         /* angle realLink structure. Display error message*/
                         /* and exit program if memory allocation fails.   */
          if (!(nextPtrElev = (realLink *) malloc ( sizeof(realLink) )))
          {
            printf ("\n\n   *** Error.  Failure allocating memory ");
            printf ("for sine of elevation angle link-list.\n\n");
            exit (1);
          }
                         /* Allocate memory for the next slant range       */
                         /* realLink structure. Display error message and  */
                         /* exit program if memory allocation fails.       */
          if (!(nextPtrSlantRange  = (realLink *) malloc ( sizeof(realLink) )))
          {
            printf ("\n\n   *** Error.  Failure allocating memory ");
            printf ("for slant range link list.\n\n");
            exit (1);
          }
/*        currentPtrAlpha->next = nextPtrAlpha; */
          currentPtrElev->next  = nextPtrElev;
          currentPtrSlantRange->next = nextPtrSlantRange;
                         /* Set the next elements of each realLink         */
                         /* structure to these newly allocated elements.   */
          numAlloc++;
                         /* Increment the number of allocated link-list    */
                         /* elements by 1.                                 */
        }
        if ( (numHighPoints[iAzimuth] == 0) 
             || (elevDiffMax >= requiredMaxDepth) )
        {
/*        currentPtrAlpha = currentPtrAlpha->next; */
          currentPtrElev  = currentPtrElev->next;
          currentPtrSlantRange = currentPtrSlantRange->next;
          numHighPoints[iAzimuth] = numHighPoints[iAzimuth] + 1;
        }
/*      currentPtrAlpha->real = (double)(iAlpha - 1) * deltaAlpha * 180.0 / pi;*/
                         /* Save angular range of preceding step in current*/
                         /* current realLink structure.  Convert from      */
                         /* radians to degrees.                            */
        currentPtrElev->real  = sinElevMax;
                         /* Save new maximum of sine elevation in current  */
                         /* realLink structure.                            */
        currentPtrSlantRange->real = slantRange1;
                         /* Save slant range of new terrain high-point in  */
                         /* current realLink structure.  This is the value */
                         /* of slantRange from the previous angular range  */
                         /* step.                                          */
        elevHighPoint = terrElev1;
        elevDiffMax = 0.0;
      }
      sinElev1 = sinElev;
      slantRange1 = slantRange;
      terrElev1 = terrElev;
                         /* Save current values of the sine of the terrain */
                         /* point elevation angle, slant range, and terrain*/
                         /* elevation for use in the next radial step along*/
                         /* the azimuthal spoke.                           */
    }
    if (iAzimuth < nAzimuth - 1)
      offset[iAzimuth + 1] = offset[iAzimuth]
        + (long)numHighPoints[iAzimuth];
                         /* If this is not the last azimuthal spoke,       */
                         /* calculate the offset value for the next spoke  */
                         /* by adding the number of terrain high-points    */
                         /* found to the offset value for the current spoke.*/
/*  currentPtrAlpha = headPtrAlpha; */
    currentPtrElev  = headPtrElev;
    currentPtrSlantRange = headPtrSlantRange;
                        /* Reset pointers to head pointers of realLink     */
                        /* structures for use in saving terrain high-point */
                        /* data for the next azimuthal spoke.              */
                        /* Dynamically allocate memory for array containing*/
                        /* angular range data for terrain high-points for  */
                        /* the current azimuthal spoke.                    */
/*  if (!(alphaArray[iAzimuth] =
            (double *) malloc ((size_t)numHighPoints[iAzimuth] *
                               sizeof(double))))
    {
      printf ("\n\n   *** Error.  Failure allocating memory for range ");
      printf ("angle array.\n\n");
      exit (1);
    } */
                        /* Dynamically allocate memory for array containing*/
                        /* sine of elevation angle data for high-points for*/
                        /* the current azimuthal spoke.                    */
    if (!(sinElevArray[iAzimuth] =
            (double *) malloc ((size_t)numHighPoints[iAzimuth] *
                               sizeof(double))))
    {                     
      printf ("\n\n   *** Error.  Failure allocating memory for sine of ");
      printf ("elevation angle array.\n\n");
      exit (1);
    }
                        /* Dynamically allocate memory for array containing*/
                        /* the slant ranges of terrain high-points for the */
                        /* current azimuthal spoke.                        */
    if (!(slantRangeArray[iAzimuth] =
            (double *) malloc ((size_t)numHighPoints[iAzimuth] *
                               sizeof(double))))
    {                     
      printf ("\n\n   *** Error.  Failure allocating memory for the ");
      printf ("slant range array.\n\n");
      exit (1);
    }

    for (i = 0; i < numHighPoints[iAzimuth]; i++)
    {
                        /* Copy data from realLink structures onto newly   */
                        /* allocated arrays.                               */ 
                        /*                                                 */
/*    currentPtrAlpha = currentPtrAlpha->next; */
      currentPtrElev  = currentPtrElev->next;
      currentPtrSlantRange = currentPtrSlantRange->next;
      sinElevArray[iAzimuth][i] = currentPtrElev->real;
/*    alphaArray[iAzimuth][i]   = currentPtrAlpha->real; */
      slantRangeArray[iAzimuth][i] = currentPtrSlantRange->real;
    }
                        /* Close all dted files that didn't get referenced */
                        /* on this spoke, except for the files opened      */
                        /* during the zeroth spoke, which will remain open */
                        /* throughout the run.                             */
    if (iAzimuth)
    {
      for (i = 0; i < (int)numdteds; i++) if ( dteds[i]->fd > 0 )
                        /* If a dted file was in fact used during this     */
                        /* spoke, initialize it to "unused" for the next   */
                        /* spoke                                           */
        if (dteds[i]->recently_used == 1)
        {
          dteds[i]->recently_used = 0;
        }
        else
        {
          dted_close_file ( dteds[i] );
        }
      }
    else
    {
      for (i = 0; i < (int)numdteds; i++)
                        /* Mark as "special" all files opened during this, */
                        /* the "zeroth," spoke                             */
        if ( dteds[i]->fd > 0 ) dteds[i]->recently_used = 2;
    }
  }
  dted_close();         /* Close all dted files that are still open.       */
  printf("\n\n\n");
  totalHighPoints = 0;  

  for (iAzimuth = 0; iAzimuth < nAzimuth; iAzimuth++)
    totalHighPoints = totalHighPoints + (long)numHighPoints[iAzimuth];
                        /* Determine total number of terrain high-points   */
                        /* for all azimuthal spokes.                       */
/*free (nextPtrAlpha); */
  free (nextPtrElev);
  free (nextPtrSlantRange);
                        /* De-allocate memory for realLink structures since*/ 
                        /* they are no longer needed. First de-allocate the*/
                        /* nextPtrAlpha, nextPtrElev, and nextPtrSlantRange*/
                        /* pointers since they were previously used for    */
                        /* allocation with the malloc function.            */
/*currentPtrAlpha = headPtrAlpha; */
  currentPtrElev  = headPtrElev;
  currentPtrSlantRange = headPtrSlantRange;
                        /* Initialize pointers to the head pointers of the */
                        /* realLink structures for de-allocation.          */
  for (i = 0; i < numAlloc; i++)
  {   
/*  nextPtrAlpha = currentPtrAlpha; */
    nextPtrElev  = currentPtrElev;
    nextPtrSlantRange = currentPtrSlantRange;
/*  currentPtrAlpha = currentPtrAlpha->next; */
    currentPtrElev  = currentPtrElev->next;
    currentPtrSlantRange = currentPtrSlantRange->next;
/*  free (nextPtrAlpha); */
    free (nextPtrElev);
    free (nextPtrSlantRange);
  }
/*free (currentPtrAlpha); */
  free (currentPtrElev);
  free (currentPtrSlantRange);
  writeTerrainMaskDataFile( siteNumber, siteLongitude, siteLatitude, 
                            siteAltitude, nAzimuth, rangeResolution,
                            requiredMaxDepth, 
                            totalHighPoints, numHighPoints, offset, 
                            sinElevArray, /* alphaArray, */ slantRangeArray );
  for (iAzimuth = 0; iAzimuth < nAzimuth; iAzimuth++)
  {
/*  free (alphaArray[iAzimuth]); */
    free (sinElevArray[iAzimuth]);
    free (slantRangeArray[iAzimuth]);
  }
                        /* De-allocate memory for all dynamically allocated*/
                        /* arrays.                                         */
/*free (alphaArray); */
  free (sinElevArray);
  free (slantRangeArray);
  free (numHighPoints);
  free (offset);

  return 0;
}

/*
Module Declaration:

Type   Name          Argument List
----   ----          -------------                                         */
double sinElevAngle (const double  siteLong, 
                     const double  siteLat,
                     const double  sitePositionMag,  
                     const double  azimuth, 
                     const double  alpha,
                     double *const slantRangePtr,
                     double *const terrElevPtr,
                     char          DTEDpath[])
{
/*
Description:
Computes the sine of the elevation angle of a given terrain point from a
given sensor site.  Also indirectly set the slant range and terrain point 
elevation in the calling program.

Calling Arguments:
  Type    Name             Description
  ----    ----             -----------
  double  siteLong         Longitude of sensor site in radians.
  double  siteLat          Latitude  of sensor site in radians.
  double  sitePositionMag  Magnitude of geocentric site position vector.
  double  azimuth          Azimuthal angle of terrain point in radians.
  double  alpha            Geocentric angular range of terrain point from
                           sensor site in radians.
  double *slantRangePtr    Address of slant range of terrain point.  Used to
                           indirectly set the slant range in the calling 
                           program.
  double *terrElevPtr      Address of elevation of terrain point [in km].
                           Used to indirectly set the terrain elevation in
                           the calling program.
  char    DTEDpath         Character string containing the directory path to
                           the DTED data files.

Return Value:
  Sine of angle of elevation from sensor site to terrain point.

Local Variables:
  Type    Name             Description
  ----    ----             -----------                                     */
  double  uvect[3],     /* Orthonormal set of basis vectors. The vector    */
          vvect[3],     /* uvect points in the direction upward from the   */
          wvect[3];     /* earth surface at the sensor, vvect points in    */
                        /* the direction tangent to the great circle       */
                        /* connecting the sensor site to the terrain       */
                        /* point, and wvect points in the direction given  */ 
                        /* by the cross product of uvect and vvect.        */
  double  vvectp[3];    /* Unit vector pointing northward from sensor.     */
  double  wvectp[3];    /* Unit vector pointing westward  from sensor.     */
  double  rvect[3];     /* Unit vector pointing in direction of terrain    */
                        /* point.                                          */
  double  cosSiteLong;  /* Cosine of the longitude of sensor site.         */
  double  sinSiteLong;  /* Sine of the longitude of sensor site.           */
  double  cosSiteLat;   /* Cosine of the latitude of sensor site.          */
  double  sinSiteLat;   /* Sine of the longitude of sensor site.           */
  double  cosAzimuth;   /* Cosine of azimuthal angle of terrain point      */
                        /* from sensor measured eastward from due north.   */
  double  sinAzimuth;   /* Sine of azimuthal angle of terrain point from   */
                        /* sensor measured eastward from due north.        */
  double  cosAlpha;     /* Cosine of angular range of terrain point from   */
                        /* sensor site.                                    */  
  double  sinAlpha;     /* Sine of angular range of terrain point from     */
                        /* sensor site.                                    */  
  double  terrainLong;  /* Longitude of terrain point in radians.          */
  double  terrainLat;   /* Latitude of terrain point in radians.           */
  double  terrElev;     /* Elevation of terrain point determined by        */
                        /* terrainElev function.                           */
  double  sinElev;      /* Sine of the terrain point elevation angle from  */
                        /* sensor site.                                    */ 
  double  terrainPositionMag; 
                        /* Magnitude of geocentric terrain point position  */
                        /* vector.                                         */
  double  pvect[3];     /* Position vector of terrain point relative to    */
                        /* sensor site.                                    */
  double  pvectMag2;    /* Square of magnitude of pvect.                   */ 
  double  pvectMag;     /* Magnitude of pvect.                             */
  int     i;            /* Loop index.                                     */
/*                                                                         */
/*-------------------------------------------------------------------------*/
/*                                                                         */
                        /* Compute uvect, vvectp, and wvectp vectors. First*/
                        /* obtain the sines and cosines of the site longi- */
                        /* tude, site latitude, and angular range of the   */
                        /* terrain point.                                  */
  cosSiteLong =  cos (siteLong);
  sinSiteLong =  sin (siteLong);
  cosSiteLat  =  cos (siteLat);
  sinSiteLat  =  sin (siteLat);
  cosAzimuth  =  cos (azimuth);
  sinAzimuth  =  sin (azimuth);
  cosAlpha    =  cos (alpha);
  sinAlpha    =  sin (alpha);
  uvect[0]    =  cosSiteLat * cosSiteLong;
  uvect[1]    =  cosSiteLat * sinSiteLong;
  uvect[2]    =  sinSiteLat;
  vvectp[0]   = -sinSiteLat * cosSiteLong;
  vvectp[1]   = -sinSiteLat * sinSiteLong;
  vvectp[2]   =  cosSiteLat;
  wvectp[0]   =  sinSiteLong;
  wvectp[1]   = -cosSiteLong;
  wvectp[2]   =  0.0;

  for (i = 0; i < 3; i++)
  {
                        /* Compute the vvect and wvect unit vectors by     */
                        /* performing a rotation of the uvect, vvectp,     */
                        /* wvectp system through an angle -azimuth about   */
                        /* the uvect axis.                                 */
    vvect[i] = (cosAzimuth * vvectp[i]) - (sinAzimuth * wvectp[i]);
    wvect[i] = (sinAzimuth * vvectp[i]) + (cosAzimuth * wvectp[i]);
                        /* Compute the unit vector rvect which points in   */
                        /* the direction of the terrain point from the     */
                        /* earth center.  This vector lies in the uvect,   */
                        /* vvect plane at an angle alpha [angular range]   */
                        /* from the sensor site.                           */
    rvect[i] = (cosAlpha * uvect[i]) + (sinAlpha * vvect[i]);
  }

                        /* Compute the terrain point longitude and latitude*/
                        /* given the components of rvect.                  */
  if (rvect[0] != 0.0)
  {
    terrainLong = atan (rvect[1] / rvect[0]);
    if (rvect[0] < 0.0) terrainLong += pi;
  }
  else
  {
    terrainLong = 0.5 * pi;
    if (rvect[1] < 0.0) terrainLong = -terrainLong;
  }
  terrainLong *= (180.0 / pi);
                        /* Convert longitude of terrain point from radians */
                        /* to degrees.                                     */
  terrainLat  = asin(rvect[2]) * 180.0 / pi;
                        /* Compute terrain point latitude and convert to   */
                        /* degrees.                                        */
  terrElev = terrainElev (terrainLong, terrainLat, DTEDpath);
  terrainPositionMag = RADIUS_OF_EARTH + terrElev;                     
                        /* Calculate the magnitude of the terrain point    */
                        /* position vector.  This is equal to the sum of   */
                        /* the radius of the earth and the terrain eleva-  */
                        /* tion at the terrain point.                      */
  *terrElevPtr = terrElev;
                        /* Indirectly set the terrain elevation in the     */
                        /* calling program.                                */
  pvect[0] = (terrainPositionMag * rvect[0]) - (sitePositionMag * uvect[0]);
  pvect[1] = (terrainPositionMag * rvect[1]) - (sitePositionMag * uvect[1]);
  pvect[2] = (terrainPositionMag * rvect[2]) - (sitePositionMag * uvect[2]);
                        /* Calculate the pvect vector, which is given by   */ 
                        /* the difference between the terrain point and    */
                        /* site position vectors.                          */
  pvectMag2 = (pvect[0] * pvect[0]) + (pvect[1] * pvect[1]) 
            + (pvect[2] * pvect[2]);
  pvectMag  = sqrt(pvectMag2);
  *slantRangePtr = pvectMag;
                        /* Calculate the magnitude of pvect and indirectly */
                        /* set it equal to the slant range [in kilometers] */
  sinElev   = ((uvect[0] * pvect[0]) + (uvect[1] * pvect[1]) 
             + (uvect[2] * pvect[2])) / pvectMag;
                        /* Compute the sine of the terrain point elevation */
                        /* angle by taking the dot product of the uvect    */
                        /* and pvect vectors, and dividing by the magnitude*/
                        /* magnitude of pvect.  Return this sine of elev-  */
                        /* ation angle.                                    */
  return sinElev;
}

/*
Module Declaration:

Type         Name                 Argument List
----         ----                 -------------                            */
doubleStruct textToDoubleConvert (char *const valueString)
{

/* 
Description:
Converts a character string containing the floating point representation of
a floating point number to the actual number of type double.

Calling Arguments:
  Type  Name                Description
  ----  ----                -----------
  char *valueString         Character string containing floating point
                            representation of a number of type double.
Return Value:
  Structure of type doubleStruct

Structure Members:
  Type  Name                Description
  ----  ----                -----------
  short error               Error parameter.  Upon return, this value is 0
                            if valueString contains a valid floating point 
                            representation, and 1 otherwise.
  int   value               Value of the floating point number contained in 
                            valueString if this string contains a valid 
                            representation of a floating point number.

  Local variables:
  Type         Name             Description
  ----         ----             -----------                                */
  doubleStruct returnStruct; /* Structure containing return data. That is, */
                             /* the value of the floating point contained  */
                             /* in the input string and the error para-    */
                             /* meter.                                     */
  double       value;        /* Value of the floating point number con-    */
                             /* tained in the input string valueString.    */
  float        sign;         /* Sign parameter.  Equal to 1.0 if the value */
                             /* parameter is positive, and -1.0 if the     */
                             /* value parameter is negative.               */
  float        digit;        /* Floating point values of individual digits */
                             /* read from valueString.                     */
  float        factor;       /* Factor corresponding to the decimal place  */
                             /* of the current digit.  Equal to 0.1 if the */
                             /* current digit is in the first decimal      */
                             /* place, 0.01 if the current digit is in the */
                             /* second decimal place, 0.001 for the third  */
                             /* decimal place, etc.                        */
  short        numDecimalPoint;/* Decimal point indicator.  Equals 1 if the*/
                             /* decimal point has been encountered in      */
                             /* valueString, and 0 otherwise.              */
  short        error;        /* Error parameter.  Equals 0 if valueString  */
                             /* contains a valid floating point represen-  */
                             /* tation of a real number, and 1 otherwise.  */
  char         firstChar;    /* The first character of valueString.        */
  char        *chrPtr;       /* Pointer used to address the individual     */
                             /* elements of valueString.                   */
/*                                                                         */
/*-------------------------------------------------------------------------*/
/*                                                                         */
  error = 0;                 /* Initialize the sign and error values.      */
  sign  = 1.0F;              /* Initialize chrPtr to beginning of          */
  chrPtr = valueString;      /* valueString.                               */ 
  numDecimalPoint = 0;       /* Initialize numDecimalPoint to zero since   */
  firstChar = *chrPtr;       /* no decimal point has been encountered yet. */
  switch (firstChar)
  {                           
    case '-':                /* If the first character of value string is  */ 
      value =  0.0;          /* a minus sign, indicating a negative number,*/
      sign  = -1.0F;         /* set the sign parameter to -1.0.  The value */
      break;                 /* variable is initialized to 0.0 since the   */
                             /* minus sign alone carries zero value.       */
    case '+':                /* If the first character of value string is  */ 
      value = 0.0;           /* a plus sign, initialize the value valiable */
      break;                 /* to 0.0 since the plus sign alone has zero  */
                             /* value.                                     */
    case '.':                /* If the first character of value string is  */
      value = 0.0;           /* a decimal point, set the decimal point     */
      numDecimalPoint = 1;   /* parameter to 1, and initialize factor to   */
      factor = 0.1F;         /* 0.1, indicating the following digit will   */
      break;                 /* be in the first decimal place.             */ 
    case '1':
      value = 1.0;           /* If the first character is a digit from 0   */
      break;                 /* to 9, initialize the value variable        */
    case '2':                /* accordingly.                               */
      value = 2.0;
      break;
    case '3':
      value = 3.0;
      break;
    case '4':
      value = 4.0;
      break;
    case '5':
      value = 5.0;
      break;
    case '6':
      value = 6.0;
      break;
    case '7':
      value = 7.0;
      break;
    case '8':
      value = 8.0;
      break;
    case '9':
      value = 9.0;
      break;
    case '0':
      value = 0.0;
      break;
    default:                 /* If the first character is not a plus or    */
      value = 0.0;           /* minus sign, a decimal point, or a digit    */
      error = 1;             /* from 0 to 9, set error parameter to 1      */
  }                          /* indicating valueString does not contain a  */
                             /* valid decimal representation of a number.  */
  if (*chrPtr != '\0') chrPtr++;
  if ((*chrPtr == '\0') &&
      ((firstChar == '+') || (firstChar == '-') || (firstChar == '.')))
    error = 1;
                             /* If the first character is a plus sign,     */
                             /* minus sign, or decimal point, but only the */
                             /* terminating NULL character follows, set    */
                             /* the error parameter equal to 1 indicating  */
                             /* an invalid decimal representation.         */
  while ((*chrPtr != '\0') && (error == 0))
  {                          /* While there are no invalid decimal repres- */
                             /* entation errors and before the terminating */
                             /* NULL character is reached in valueString,  */
                             /* read and convert the individual characters */
                             /* to their corresponding decimal digits, and */
                             /* update the value variable accordingly.     */
    switch (*chrPtr)
    {
      case '0':
        digit = 0.0F;
        break;
      case '1':
        digit = 1.0F;
        break;
      case '2':
        digit = 2.0F;
        break;
      case '3':
        digit = 3.0F;
        break;
      case '4':
        digit = 4.0F;
        break;
      case '5':
        digit = 5.0F;
        break;
      case '6':
        digit = 6.0F;
        break;
      case '7':
        digit = 7.0F;
        break;
      case '8':
        digit = 8.0F;
        break;
      case '9':
        digit = 9.0F;
        break;
      case '.':              /* If the decimal point is encountered,       */
        digit = 0.0F;        /* initialize factor and set the digit vari-  */
        factor = 1.0F;       /* able to 0.0 since the decimal point alone  */
        /* has zero value.                            */
        if (numDecimalPoint == 1)
          error = 1;         /* If the decimal point has already occurred, */
                             /* set the error parameter to 1 since the     */
                             /* decimal representation of a number cannot  */
                             /* have more than one decimal point.          */
        numDecimalPoint = 1;
        break;               /* Set decimal point parameter to 1, indicat- */
                             /* ing the decimal point has been encountered.*/
      default:
        digit = 0.0F;        /* If the valueString character is neither a  */
        error = 1;           /* digit from 0 to 9 nor a decimal point, set */
                             /* the error parameter to 1 indicating an     */
                             /* invalid decimal representation.            */
    }
    if (numDecimalPoint == 0)
                             /* If the decimal point has not yet been      */
                             /* encountered, update the value variable by  */
                             /* multiplying its current value by 10.0 and  */
                             /* adding the new digit.  This, in effect,    */
                             /* shifts all previous digits one space to    */
                             /* the left [from the decimal point] and      */
                             /* places the new digit immediately to the    */
                             /* left of the decimal point.                 */
      value = (10.0 * value) + (double)digit;
    else
    {
      value += (double)(digit * factor);
                             /* If the decimal point has been encountered, */
                             /* update the value variable by multiplying   */
                             /* the new digit by the current decimal place */
                             /* factor, and adding this result to the      */
                             /* current value variable.                    */
      factor *= 0.1F;
                             /* Update the decimal place factor for the    */
                             /* next digit.                                */ 
    }
    *chrPtr++;
  }                            
                             /* At this point, the value variable is equal */ 
                             /* to the absolute value of the number con-   */
                             /* tained in valueString.  Multiply this      */
                             /* value by the sign parameter to obtain the  */
                             /* number in valueString with the correct     */
                             /* sign. Set the value member of returnStruct */ 
                             /* equal to this number, and the error member */
                             /* equal to the decimal representation error  */ 
                             /* parameter.                                 */
  returnStruct.value = (double)sign * value;
  returnStruct.error = error;
  return returnStruct;
}

/*
Module Declaration:

Type      Name              Argument List
----      ----              -------------                                  */
intStruct textToIntConvert (char *const valueString)
{

/*    
Description:
Converts a character string containing the representation of an integer to the 
actual number of type int.

Calling Arguments:
  Type  Name              Description
  ----  ----              -----------                             
  char *valueString       Character string containing the integer      
                          representation.                               
Return Value:
  Structure of type intStruct

Structure Members:
  Type      Name          Description
  ----      ----          -----------
  short     error         Error parameter.  Upon return, this value is 0
                          if valueString contains a valid representation
                          of an integer within the bounds of type int, 
                          1 if valueString does not contain a valid 
                          representation of any integer, and 2 if
                          valueString contains the representation of an
                          integer outside the bounds of type int.
  int       value         Value of the integer contained in valueString
                          if this string contains a valid integer 
                          representation.
Local Variables:
  Type      Name             Description
  ----      ----             -----------                                   */
  intStruct returnStruct; /* Structure containing data returned by the     */
                          /* function. That is, the value of the integer   */
                          /* contained in valueString and the error para-  */ 
                          /* meter.                                        */
  double    value;        /* Value of integer contained in valueString.    */
  double    sign = 1.0;   /* Sign parameter.  Equal to 1 if value is posi- */
                          /* tive, and -1 otherwise.                       */
  double    digit;        /* Type double values of the digits read from    */
                          /* valueString.                                  */
  double    valueMax = 32767.0;
                          /* Maximum value of an integer that can be saved */
                          /* with type int.                                */
  short     error = 0;    /* Error parameter.  Equal to 0 if valueString   */
                          /* contains a valid representation of an         */
                          /* integer within the bounds allowed by type int,*/ 
                          /* 1 if valueString does not contain a valid     */
                          /* representation of an integer, and 2 if        */
                          /* valueString contains the representation of an */
                          /* integer outside the bounds allowed by type    */
                          /* int.                                          */
  char     *chrPtr;       /* Pointer to individual characters in           */
                          /* valueString.                                  */
  char      firstChar;    /* The first character in valueString.           */
/*                                                                         */
/*-------------------------------------------------------------------------*/
/*                                                                         */
                          /* largest integer that can be held by a variable*/
                          /* of type int.                                  */
  chrPtr = valueString;   /* Initialize chrPtr to the beginning of         */
                          /* valueString.                                  */
  firstChar = *chrPtr;    /* Set the first character variable equal to the */
                          /* character currently addressed by chrPtr.      */
  switch (firstChar)
  {                           
    case '-':             /* If the first character of value string is a   */ 
      value =  0.0;       /* minus sign, indicating a negative number, set */
      sign  = -1.0;       /* the sign parameter to -1.0.  The value vari-  */
      break;              /* able is initialized to 0.0 since the minus    */
                          /* minus sign alone carries zero value.  If the  */
    case '+':             /* first character of value string is a plus     */ 
      value = 0.0;        /* sign, initialize the value valiable to 0.0    */
      break;              /* since the plus sign alone has zero value.     */
    case '1':
      value = 1.0;        /* If the first character is a digit from 0 to 9,*/
      break;              /* initialize the value variable accordingly.    */
    case '2': 
      value = 2.0;
      break;
    case '3':
      value = 3.0;
      break;
    case '4':
      value = 4.0;
      break;
    case '5':
      value = 5.0;
      break;
    case '6':
      value = 6.0;
      break;
    case '7':
      value = 7.0;
      break;
    case '8':
      value = 8.0;
      break;
    case '9':
      value = 9.0;
      break;
    case '0':
      value = 0.0;
      break;
    default:              /* If the first digit is not a plus or minus sign*/
      value = 0.0;        /* or a digit from 0 to 9, set error parameter   */
      error = 1;          /* to 1 indicating that valueString does not     */
  }                       /* contain a valid decimal representation of an  */
                          /* integer.                                      */
  if (firstChar != '\0') chrPtr++;
  if ((*chrPtr == '\0') && ((firstChar == '+') || (firstChar == '-')))
    error = 1;
  while ((*chrPtr != '\0') && (error != 1))
  {                       /* While there are no invalid decimal represent- */
                          /* tion errors and before the terminating NULL   */
                          /* character is reached in valueString, read and */
                          /* convert the individual characters to their    */
                          /* corresponding decimal digits, and update the  */
                          /* value variable accordingly.                   */
    switch (*chrPtr)
    {
      case '0':
        digit = 0.0;
        break;
      case '1':
        digit = 1.0;
        break;
      case '2':
        digit = 2.0;
        break;
      case '3':
        digit = 3.0;
        break;
      case '4':
        digit = 4.0;
        break;
      case '5':
        digit = 5.0;
        break;
      case '6':
        digit = 6.0;
        break;
      case '7':
        digit = 7.0;
        break;
      case '8':
        digit = 8.0;
        break;
      case '9':
        digit = 9.0;
        break;
      default:            /* If the valueString character is not an integer*/
        digit = 0.0;      /* from 0 to 9, set error parameter equal to 1   */
        error = 1;        /* indicating an invalid decimal representation. */
    }
    value = (10.0 * value) + digit;
                          /* Update the value variable by multiplying its  */
                          /* current value by 10.0 and adding the new      */
                          /* digit.  This, in effect, shifts all previous  */
                          /* digits one space to the left [from the decimal*/ 
                          /* point] and places the new digit immediately   */ 
                          /* to the left of the decimal point.             */
    if ( (value > valueMax) && (error != 1) )
                          /* If there have been no decimal representation  */
                          /* errors thus far in valueString, but the       */
                          /* value variable exceeds the value that can be  */
                          /* held in type int, set the error parameter     */
                          /* equal to 2, indicating the integer stored in  */
                          /* valueString cannot be saved in type int.      */
      error = 2;
    chrPtr++;
  }
                          /* At this point, the value variable is equal to */
                          /* the absolute value of the integer stored in   */
                          /* valueString.  Therefore, multiply the value   */
                          /* variable by the sign parameter and convert the*/
                          /* resulting [double type] number to type int.   */
                          /* Set the value member of returnStruct equal to */
                          /* this integer.  Also, set the error member     */
                          /* equal to the decimal representation error     */
                          /* parameter.                                    */  
  returnStruct.value = (int)sign * (int)value;
  returnStruct.error = error;
  return returnStruct;
}

/*
Module Declaration:

Type      Name          Argument List
----      ----          -------------                                      */
void      correctUsage (char *const command)
{

/*
Description:
Prints a usage message to the screen if the user types the command line
arguments in an incorrect format.

Return Value:  none

Calling Argument:
  Type    Name          Description
  ----    ----          -----------                                   
  char   *command;      Character string containing command to initiate   
                        program execution.                                 */
/*                                                                         */
/*-------------------------------------------------------------------------*/
/*                                                                         */
  printf ("\nUsage: %s siteLongitude siteLatitude ", command);
  printf ("siteAltitude siteNumber DTEDpath");
  printf ("\n                [Optional flags and arguments]");
  printf ("\n\n   where\n");
  printf ("\n   siteLongitude = longitude of site in degrees ");
  printf ("[floating point number].");
  printf ("\n    siteLatitude = latitude  of site in degrees "); 
  printf ("[floating point number].");
  printf ("\n    siteAltitude = altitude  of site in kilometers ");
  printf ("[floating point number].");
  printf ("\n      siteNumber = integer from 0 to 999 specifying the "); 
  printf ("sensor site.");
  printf ("\n        DTEDpath = character string containing path to the ");
  printf ("DTED files.");
  printf ("\n\nOptional flags and arguments:");
  printf ("\n\n -rr  Range resolution flag. Indicates the following ");
  printf ("value is rangeResolution.");
  printf ("\n -an  Azimuthal spokes flag. Indicates the following ");
  printf ("value is the number of ");
  printf ("\n      azimuthal spokes nAzimuth.");
  printf ("\n -td  Terrain depth flag. Indicates the terrain depth ");
  printf ("[in km] needed ");  
  printf ("\n      following a high-point for possible target obscuration.");
  printf ("\n\n rangeResolution = range resolution [in km] required for ");
  printf ("determing the"); 
  printf ("\n                   terrain high-points.");
  printf ("\n        nAzimuth = number of azimuthal spokes used.\n");
  exit(1);
}

/*
Module Declaration:

Type   Name            Argument List
----   ----            -------------                                       */
short  stringCompare ( char *const string1, 
                       char *const string2 )
{

/* 
Description:
Performs a comparison of two input character strings.  If the two strings are 
identical, a short integer value of 1 is returned.  Otherwise, short integer 0 
is returned.

Calling Arguments:

  Type    Name            Description
  ----    ----            ----------- 
  char *  string1         First  character string for comparison.
  char *  string2         Second character string for comparison.

Return Value:
  Short integer value equal to 1 if the two input strings are identical, 
  and 0 otherwise. 
    
Local variables:

  Type    Name            Description
  ----    ----            -----------                                      */
  char   *chrPtr1;     /* Pointer to characters in first  input string.    */
  char   *chrPtr2;     /* Pointer to characters in second input string.    */
/*                                                                         */
/* ----------------------------------------------------------------------- */
/*                                                                         */
  chrPtr1 = string1;
  chrPtr2 = string2;   /* Initialize string pointers                       */
  while ( (*chrPtr1 != '\0') && (*chrPtr2 != '\0') )
  {
                       /* Before the terminating NULL character is reached */
                       /* in either input string, advance the string       */
                       /* pointers one character at a time down their      */
                       /* respective input strings.  Return a value of 0   */
                       /* if any character in string2 differs from its     */
                       /* corresponding character in string1.              */
    if ( *chrPtr1 != *chrPtr2 ) return 0;
    chrPtr1++;
    chrPtr2++;
  }
                       /* At this point, either chrPtr1 or chrPtr2 or both */ 
                       /* point to a terminating NULL character.           */
  if ( *chrPtr1 == *chrPtr2)
                       /* If chrPtr1 and chrPtr2 both point to the same    */
                       /* character, which would be the terminating NULL   */
                       /* character in their respective strings, then the  */
                       /* input strings string1 and string2 are identical, */
                       /* and 1 is returned.  Otherwise, 0 is returned.    */
    return 1;
  else return 0;
}

/*
Module Declaration:

Type   Name                       Argument List
----   ----                       -------------                            */
void   writeTerrainMaskDataFile ( const int      siteNumber, 
                                  const double   siteLongitude, 
                                  const double   siteLatitude, 
                                  const double   siteAltitude, 
                                  const int      nAzimuth, 
                                  const double   rangeResolution, 
                                  const double   requiredMaxDepth,
                                  const long     totalHighPoints, 
                                  int *const     numHighPoints, 
                                  long *const    offset, 
                                  double **const sinElevArray, 
                          /*      double **const alphaArray, */
                                  double **const slantRangeArray )
{

/*
Description:
Writes the terrain mask data output file given the site number, site
coordinates, range resolution, number of azimuthal spokes used, and the
terrain high-point data.

Calling Arguments:
  Type    Name             Description
  ----    ----             -----------
  int      siteNumber      Integer between 0 and 999 designating the sensor
           site.
  double   siteLongitude   Longitude of sensor site in degrees.
  double   siteLatitude    Latitude  of sensor site in degrees.
  double   siteAltitude    Altitude  of sensor site in kilometers.
  int      nAzimuth        Number of azimuthal spokes selected.
  double   rangeResolution Range on the earth surface [in km] over which the
                           the terrain altitude data is resolved for
                           determining the high-points.
  long     totalHighPoints Total number of high-points along all azimuthal
                           spokes.
  int     *numHighPoints   Integer array containing the number of high-points 
                           for each azimuthal spoke.
  long    *offset          Array of offset values for indices of a 1D array 
                           containing all high-points for all spokes.     
  double **sinElevArray    Array of pointers to the sines of elevation angles 
                           at the terrain high-points.  Each [pointer] element 
                           of the array corresponds to an azimuthal spoke. 
  double **alphaArray      Array of pointers to angular ranges of terrain
                           high-points along given azimuthal spokes. Each  
                           [pointer] element of the array corresponds to an 
                           azimuthal spoke.
  double **slantRangeArray Array of pointers to the slant ranges from the 
                           sensor site to the terrain high-points.  Each 
                           [pointer] element of the array corresponds to an
                           azimuthal spoke.

Local variables:
  Type    Name            Description
  ----    ----            -----------                                      */
  char    filename[MAX_FILENAME_LENGTH];
                       /* Character string containing the entire filename  */
                       /* of the output data file to which the terrain     */
                       /* mask data is written.                            */
  char   filenameMain[] = "terrmask.";
                       /* Character string contain the main part of the    */
                       /* filename of the output data file.                */
  char   siteNumberExt[4];
                       /* Character string containing the extension of the */
                       /* filename of the data file to which the terrain   */
                       /* mask data is written.  This extension is a text  */
                       /* version of the sensor site number.               */
  char  *chrptr;       /* Character pointer used in constructing output    */
                       /* data file name.                                  */
  int    mainLength;   /* Length of main portion of terrain mask data      */
                       /* filename [including separator dot from filename  */
                       /* extension].                                      */
  FILE  *fp;           /* File pointer to output data file.                */
  int    i, k;         /* Loop indices.                                    */
  int    nColumns;     /* Number of columns of integer or long integer     */
                       /* data to appear in terrain mask data file. Set    */
                       /* equal to 20 for numHighPoints data and 10 for    */
                       /* offset values array.  Number of rows of integer  */
  int    nRows;        /* Number of rows of integer or long integer data   */
                       /* to appear in the terrain mask data file. Equal   */
                       /* to (int) (nAzimuth / nColumns)                   */
  int    nRemainder;   /* Number of integer or long integer elements       */
                       /* remaining after nRows rows of data have been     */
                       /* written to TERRMASK data file.                   */
  int    iAzimuth;     /* Loop index for azimuthal spokes.                 */ 
/*                                                                         */
/* ----------------------------------------------------------------------- */
/*                                                                         */
  i = 0;               /* Construct filename of output file.               */  
  while (filenameMain[i] != '\0')
  {                    /* Construct main part of filename.                 */
    filename[i] = filenameMain[i];
    i++;
  }
  mainLength = i;
  sprintf(siteNumberExt, "%3.3d", siteNumber);
                       /* Copy sensor number onto character string for use */
                       /* in constructing output filename.                 */
  chrptr = siteNumberExt;
  i = 0;
  while (*chrptr != '\0')
  {                     /* Add extension to filename.                      */
    filename[mainLength + i] = *chrptr;
    chrptr++;
  i++;
  }
  filename[mainLength + i] = '\0';
  if ((fp = fopen (filename, "w")) == NULL)
  {
                       /* Open output file for storing terrain mask data.  */
    printf("\n   *** Error opening file %s", filename);
    exit(1);
  }
                       /* Generate output file                             */  
  fprintf(fp, "                   Terrain High-Point Data from Sensor Site");
  fprintf(fp, "\n\n\nSensor site parameters\n");
  fprintf(fp, "\nLongitude = %12.6lf    Latitude = %12.6lf", siteLongitude,
          siteLatitude);
  fprintf(fp, "    Altitude = %12.6lf", siteAltitude);
                       /* Write line to file containing sensor site        */
                       /* location data.                                   */
  fprintf(fp, "\nSite number = %3.1d", siteNumber);
                       /* Write file line with sensor site number.         */
  fprintf(fp, "\n\nRange resolution = %12.6lf kilometers", rangeResolution);
                       /* Write line with required range resolution.       */
  fprintf(fp, "\nRequired terrain depth between successive high-points = ");
  fprintf(fp, "%12.6lf kilometers.", requiredMaxDepth);
                       /* Write line with required terrain depth between   */
                       /* successive high-points.                          */
  fprintf(fp, "\n\nNumber of azimuthal spokes = %d", nAzimuth);
                       /* Write line with number of azimuthal spokes.      */
  fprintf(fp, "\n\nTotal number of terrain high-points found by all ");
  fprintf(fp, "azimuthal spokes = %6.1ld", totalHighPoints);
                       /* Write total number of high-point data pairs.     */
  fprintf(fp, "\n\n\nNumber of terrain high-points for each azimuthal ");
  fprintf(fp, "spoke\n");
  nColumns = 20;       /* Set the number of entries per line, or columns   */
                       /* for recording the number of terrain high-points  */
                       /* for each azimuthal spoke.                        */
  nRows = nAzimuth / nColumns;
  nRemainder = nAzimuth - (nRows * nColumns);
                       /* Calculate the number of full rows, or lines, of  */
                       /* number of high-points data.  Also calculate the  */
                       /* number of remaining entries that do not fill an  */
                       /* entire line.                                     */
  for (i = 0; i < nRows; i++)
  {
                       /* Record all full lines of number of high-points   */
                       /* data.                                            */
    fprintf (fp, "\n");
    for (k = 0; k < nColumns; k++)
    {
      iAzimuth = (i * nColumns) + k;
      fprintf (fp, "%3.1d ", numHighPoints[iAzimuth]); 
    }
  }
  if (nRemainder > 0)
  {
                       /* Record remaining partial line of number of high- */
                       /* points data.                                     */
    if (nRows == 0) iAzimuth = 0;
    else iAzimuth++;
    fprintf(fp, "\n");
    for (i = 0; i < nRemainder; i++)
    {
      fprintf (fp, "%3.1d ", numHighPoints[iAzimuth]);
      iAzimuth++;
    }
  }
  fprintf (fp, "\n\n\nIndex offset values to the first element of a ");
  fprintf (fp, "1D real array");
  fprintf (fp, "\ncontaining high-point data for each azimuthal spoke.\n");
  nColumns = 10;       /* Set the number of entries per line, or columns   */
                       /* for recording the 1D array offset values for     */
                       /* each azimuthal spoke.                            */
  nRows = nAzimuth / nColumns;
  nRemainder = nAzimuth - (nRows * nColumns);
                       /* Calculate the number of full rows, or lines, of  */
                       /* 1D array offset values.  Also calculate the      */
                       /* number of remaining entries that do not fill an  */
                       /* entire line.                                     */
  for (i = 0; i < nRows; i++)
  {
    fprintf (fp, "\n");
    for (k = 0; k < nColumns; k++)
    {
      iAzimuth = (i * nColumns) + k;
      fprintf (fp, "%6.1ld ", offset[iAzimuth]); 
    }
  }
                       /* Record remaining partial line of offset values   */
  if (nRemainder > 0)
  {
    if (nRows == 0) iAzimuth = 0;
    else iAzimuth++;
    fprintf(fp, "\n");
    for (i = 0; i < nRemainder; i++)
    {
      fprintf (fp, "%6.1ld ", offset[iAzimuth]);
      iAzimuth++;
    }
  }
  fprintf (fp, "\n\n\nSine of elevation angle data");
  fprintf (fp, "\nEach line contains data for one azimuthal spoke.\n");
                       /* Record sine of elevation angle data for the      */  
                       /* terrain high-points.                             */
  for (iAzimuth = 0; iAzimuth < nAzimuth; iAzimuth++)
  {
    fprintf (fp, "\n");
    for (i = 0; i < numHighPoints[iAzimuth]; i++)
      fprintf(fp, "%12.6lf ", sinElevArray[iAzimuth][i]); 
  }
/* The following file output is commented out for production purposes; while  */
/* the angular range is instrumental to the computation of the terrain masks, */
/* it's useless to ARGUS, for which the masks are being generated...          */
/*
  fprintf (fp, "\n\n\nAngular range data");
  fprintf (fp, "\nEach line contains data for one azimuthal spoke.\n");
*/
                       /* Record the angular range data for the terrain    */
                       /* high-points.                                     */
/*
  for (iAzimuth = 0; iAzimuth < nAzimuth; iAzimuth++)
  {
    fprintf (fp, "\n");
    for (i = 0; i < numHighPoints[iAzimuth]; i++)
      fprintf(fp, "%12.6lf ", alphaArray[iAzimuth][i]); 
  }
*/
  fprintf (fp, "\n\n\nSlant range data [kilometers]");
  fprintf (fp, "\nEach line contains data for one azimuthal spoke.\n");
                       /* Record the slant range data for the terrain      */
                       /* high-points.                                     */
  for (iAzimuth = 0; iAzimuth < nAzimuth; iAzimuth++)
  {
    fprintf (fp, "\n");
    for (i = 0; i < numHighPoints[iAzimuth]; i++)
      fprintf(fp, "%12.6lf ", slantRangeArray[iAzimuth][i]); 
  }
  fclose (fp);         /* Close the file pointer to the output file.       */
}

/*
Module Declaration:

Type   Name         Argument List
----   ----         -------------                                          */
double terrainElev (const double terrainLong, 
                    const double terrainLat,
                    char path[])
{
  double pos[3];

  pos[0] = terrainLat;
  pos[1] = fmod (terrainLong + 180.0, 360.0) - 180.0;
  rsanmovetosurface (pos, path);
  return (pos[2]);
}

/*-------------------------------------------------------------------------*/
/*      Classification:  Unclassified */
