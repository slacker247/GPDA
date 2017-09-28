/*
 *  This file contains the ODBCINSTGetProperties function required by
 *  unixODBC (http://www.unixodbc.org) to define tds DSNs.
 *
 *  $Log: oraodbcS.c,v $
 *  Revision 1.1.1.1  2001/10/17 16:40:01  lurcher
 *
 *  First upload to SourceForge
 *
 *  Revision 1.1.1.1  2000/09/04 16:42:52  nick
 *  Imported Sources
 *
 *  Revision 1.1  2000/08/11 12:45:38  ngorham
 *
 *  Add Oracle setup
 *
 */

#include <odbcinstext.h>

static const char *aYesNo[] =
{
	"Yes",
	"No",
	NULL
};

static char *help_strings[] = 
{
    "Name of the server to connect to.",
    "User name to connect with.",
    "Password of user.",
};

int ODBCINSTGetProperties(
    HODBCINSTPROPERTY hLastProperty)
{
    hLastProperty->pNext =
        (HODBCINSTPROPERTY)malloc( sizeof(ODBCINSTPROPERTY) );
    hLastProperty = hLastProperty->pNext;
    memset( hLastProperty, 0, sizeof(ODBCINSTPROPERTY) );
    hLastProperty->nPromptType = ODBCINST_PROMPTTYPE_TEXTEDIT;
    strncpy( hLastProperty->szName, "DB", INI_MAX_PROPERTY_NAME );
    strncpy( hLastProperty->szValue, "", INI_MAX_PROPERTY_VALUE );
    hLastProperty->pszHelp = malloc(strlen(help_strings[0]) + 1);
    strcpy(hLastProperty->pszHelp, help_strings[0]);

    hLastProperty->pNext =
        (HODBCINSTPROPERTY)malloc( sizeof(ODBCINSTPROPERTY) );
    hLastProperty = hLastProperty->pNext;
    memset( hLastProperty, 0, sizeof(ODBCINSTPROPERTY) );
    hLastProperty->nPromptType = ODBCINST_PROMPTTYPE_TEXTEDIT;
    strncpy( hLastProperty->szName, "USER", INI_MAX_PROPERTY_NAME );
    strncpy( hLastProperty->szValue, "", INI_MAX_PROPERTY_VALUE );
    hLastProperty->pszHelp = malloc(strlen(help_strings[1]) + 1);
    strcpy(hLastProperty->pszHelp, help_strings[1]);

    hLastProperty->pNext =
        (HODBCINSTPROPERTY)malloc( sizeof(ODBCINSTPROPERTY) );
    hLastProperty = hLastProperty->pNext;
    memset( hLastProperty, 0, sizeof(ODBCINSTPROPERTY) );
    hLastProperty->nPromptType = ODBCINST_PROMPTTYPE_TEXTEDIT;
    strncpy( hLastProperty->szName, "PASSWORD", INI_MAX_PROPERTY_NAME );
    strncpy( hLastProperty->szValue, "", INI_MAX_PROPERTY_VALUE );
    hLastProperty->pszHelp = malloc(strlen(help_strings[2]) + 1);
    strcpy(hLastProperty->pszHelp, help_strings[2]);

    return 1;
}

