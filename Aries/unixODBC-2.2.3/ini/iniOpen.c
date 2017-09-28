/**********************************************************************************
 * iniOpen
 *
 *
 **************************************************
 * This code was created by Peter Harvey @ CodeByDesign.
 * Released under LGPL 28.JAN.99
 *
 * Contributions from...
 * -----------------------------------------------
 * PAH = Peter Harvey		- pharvey@codebydesign.com
 * -----------------------------------------------
 *
 * PAH	06.MAR.99	Can now create file-less INI. Pass NULL for
 *					pszFileName. Then copy a file name into hIni->szFileName
 *					before calling iniCommit.
 **************************************************/

#include "ini.h"

/*
 * Changes sent by MQJoe, to avoid limit on number of open file handles
 */

/***************************************************
 * Override fstream command to overcome 255 file
 * handle limit
 ***************************************************/

#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdarg.h>

#if defined( HAVE_VSNPRINTF ) && defined( USE_LL_FIO )

FILE *uo_fopen( const char *filename, const char *mode )
{
    int fp;
    long oMode = 0, pMode = 0;

    pMode = S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH;

    switch (mode[0])
    {
      case 'r':
        oMode = O_RDONLY;
        break;

      case 'w':
        oMode = O_RDWR | O_CREAT | O_TRUNC;
        break;
        
      case 'o':
        oMode = O_RDWR | O_CREAT | O_TRUNC;
        break;

      case 'a':
        oMode = O_RDWR | O_CREAT;
        break;

      default:
        return FALSE;
    }

    fp = open(filename, oMode, pMode );

    if (fp != -1 && mode[0] == 'a')
        lseek(fp, 0L, SEEK_END );

    return (fp != -1) ? (FILE*)fp : NULL;
}

int uo_fclose( FILE *stream )
{
    close((int)stream);
    return 0;
}

char *uo_fgets( char *buffer, int n, FILE *stream )
{
    int fp = (int)stream;
    char ch;
    int i = 0, c = 0;
             
    buffer[0] = 0;
                
    do
    {
        c = read(fp, &ch, 1);
        
        if (c == 1)
        {
            buffer[i++] = ch;
                                          
            if (ch == '\n')
                break;
        }                                                
    } while (c && i < n);
                   
    buffer[i] = 0;
                   
    return (c) ? buffer : NULL;
}

int uo_fprintf( FILE *stream, const char *fmt, ...)
{
    int fp = (int)stream;
    long lNeededSize = 256;
    char* szBuffer = NULL;
    long lBufSize = 0;
    int r = 0;
    va_list ap;


    va_start(ap, fmt);

    do
    {
        if (lNeededSize > lBufSize)
        {
            if (szBuffer)
                free(szBuffer);
            szBuffer = (char*)malloc(lNeededSize);
            lBufSize = lNeededSize;
        }

        lNeededSize =  vsnprintf(szBuffer, lBufSize, fmt, ap);
            lNeededSize++;
    }
    while (lNeededSize > lBufSize);

    va_end(ap);

    r = write(fp, szBuffer, (lNeededSize - 1) );

    if (szBuffer)
        free(szBuffer);

    return r;
}

#endif

/***************************************************/

int iniOpen( HINI *hIni, char *pszFileName, char cComment, char cLeftBracket, char cRightBracket, char cEqual, int bCreate )
{
    FILE    *hFile;
    char    szLine[INI_MAX_LINE+1];
    char    szObjectName[INI_MAX_OBJECT_NAME+1];
    char    szPropertyName[INI_MAX_PROPERTY_NAME+1];
    char    szPropertyValue[INI_MAX_PROPERTY_VALUE+1];
    int     nValidFile;

    /* INIT STATEMENT */
    *hIni = malloc( sizeof(INI) );
    if ( pszFileName && pszFileName != STDINFILE )
        strncpy((*hIni)->szFileName, pszFileName, ODBC_FILENAME_MAX );
    else if ( pszFileName == STDINFILE )
        strncpy((*hIni)->szFileName, "stdin", ODBC_FILENAME_MAX );
    else
        strncpy((*hIni)->szFileName, "", ODBC_FILENAME_MAX );

    (*hIni)->cComment           = cComment;
    (*hIni)->cLeftBracket       = cLeftBracket;
    (*hIni)->cRightBracket      = cRightBracket;
    (*hIni)->cEqual             = cEqual;
    (*hIni)->bChanged           = FALSE;
    (*hIni)->hCurObject         = NULL;
    (*hIni)->hFirstObject       = NULL;
    (*hIni)->hLastObject        = NULL;
    (*hIni)->nObjects           = 0;
    (*hIni)->bReadOnly          = 0;

    /* OPEN FILE */
    if ( pszFileName )
    {
        if ( pszFileName == STDINFILE )
        {
            hFile = stdin;
        }
        else
        {
            hFile = uo_fopen( pszFileName, "r" );
        }

        if ( !hFile )
        {
            if ( bCreate == TRUE )
            {
                hFile = uo_fopen( pszFileName, "w" );
            }
        }

        if ( !hFile )
        {
            free( *hIni );
            *hIni = NULL;
            return INI_ERROR;
        }

        nValidFile = _iniScanUntilObject( *hIni, hFile, szLine );
        if ( nValidFile == INI_SUCCESS )
        {
            char *ptr;
            do
            {
                if ( szLine[0] == cLeftBracket )
                {
                    _iniObjectRead( (*hIni), szLine, szObjectName );
                    iniObjectInsert( (*hIni), szObjectName );
                }
                else if ( (szLine[0] != cComment) && !isspace(szLine[0]) )
                {
                    _iniPropertyRead( (*hIni), szLine, szPropertyName, szPropertyValue );
                    iniPropertyInsert( (*hIni), szPropertyName, szPropertyValue );
                }

            } while ( (ptr = uo_fgets( szLine, INI_MAX_LINE, hFile )) != NULL );
        }
        else if ( nValidFile == INI_ERROR )
        {
            /* INVALID FILE */
            if ( hFile != NULL )
                uo_fclose( hFile );
            free( *hIni );
            *hIni = NULL;
            return INI_ERROR;
        }

        /* CLEANUP */
        if ( hFile != NULL )
            uo_fclose( hFile );

        iniObjectFirst( *hIni );

    } /* if file given */

    return INI_SUCCESS;
}


