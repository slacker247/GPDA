/*********************************************************************
 *
 * This is based on code created by Peter Harvey,
 * (pharvey@codebydesign.com).
 *
 * Modified and extended by Nick Gorham
 * (nick@easysoft.com).
 *
 * Any bugs or problems should be considered the fault of Nick and not
 * Peter.
 *
 * copyright (c) 1999 Nick Gorham
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 **********************************************************************
 *
 * $Id: SQLTablesW.c,v 1.3 2002/08/23 09:42:37 lurcher Exp $
 *
 * $Log: SQLTablesW.c,v $
 * Revision 1.3  2002/08/23 09:42:37  lurcher
 *
 * Fix some build warnings with casts, and a AIX linker mod, to include
 * deplib's on the link line, but not the libtool generated ones
 *
 * Revision 1.2  2002/07/24 08:49:52  lurcher
 *
 * Alter UNICODE support to use iconv for UNICODE-ANSI conversion
 *
 * Revision 1.1.1.1  2001/10/17 16:40:07  lurcher
 *
 * First upload to SourceForge
 *
 * Revision 1.3  2001/07/03 09:30:41  nick
 *
 * Add ability to alter size of displayed message in the log
 *
 * Revision 1.2  2001/04/12 17:43:36  nick
 *
 * Change logging and added autotest to odbctest
 *
 * Revision 1.1  2000/12/31 20:30:54  nick
 *
 * Add UNICODE support
 *
 *
 **********************************************************************/

#include "drivermanager.h"

static char const rcsid[]= "$RCSfile: SQLTablesW.c,v $";

SQLRETURN SQLTablesW( SQLHSTMT statement_handle,
           SQLWCHAR *catalog_name,
           SQLSMALLINT name_length1,
           SQLWCHAR *schema_name,
           SQLSMALLINT name_length2,
           SQLWCHAR *table_name,
           SQLSMALLINT name_length3,
           SQLWCHAR *table_type,
           SQLSMALLINT name_length4 )
{
    DMHSTMT statement = (DMHSTMT) statement_handle;
    SQLRETURN ret;
    SQLCHAR s1[ 100 + LOG_MESSAGE_LEN ], s2[ 100 + LOG_MESSAGE_LEN ], s3[ 100 + LOG_MESSAGE_LEN ], s4[ 100 + LOG_MESSAGE_LEN ];

    /*
     * check statement
     */
    if ( !__validate_stmt( statement ))
    {
        dm_log_write( __FILE__, 
                    __LINE__, 
                    LOG_INFO, 
                    LOG_INFO, 
                    "Error: SQL_INVALID_HANDLE" );

        return SQL_INVALID_HANDLE;
    }

    function_entry( statement );

    if ( log_info.log_flag )
    {
        sprintf( statement -> msg, "\n\t\tEntry:\
            \n\t\t\tStatement = %p\
            \n\t\t\tCatalog Name = %s\
            \n\t\t\tSchema Name = %s\
            \n\t\t\tTable Name = %s\
            \n\t\t\tTable Type = %s",
                statement,
                __wstring_with_length( s1, catalog_name, name_length1 ),
                __wstring_with_length( s2, schema_name, name_length2 ),
                __wstring_with_length( s3, table_name, name_length3 ),
                __wstring_with_length( s4, table_type, name_length4 ));

        dm_log_write( __FILE__,
                __LINE__,
                LOG_INFO,
                LOG_INFO,
                statement -> msg );
    }

    thread_protect( SQL_HANDLE_STMT, statement );

    /*
     * this is a fix for old version of EXCEL
     */

    if ( !catalog_name )
        name_length1 = 0;
    if ( !schema_name )
        name_length2 = 0;
    if ( !table_name )
        name_length3 = 0;
    if ( !table_type )
        name_length4 = 0;

    if (( name_length1 < 0 && name_length1 != SQL_NTS ) ||
            ( name_length2 < 0 && name_length2 != SQL_NTS ) ||
            ( name_length3 < 0 && name_length3 != SQL_NTS ) ||
            ( name_length4 < 0 && name_length4 != SQL_NTS ))
    {
        dm_log_write( __FILE__,
                __LINE__,
                LOG_INFO,
                LOG_INFO,
                "Error: HY090" );

        __post_internal_error( &statement -> error,
                ERROR_HY090, NULL,
                statement -> connection -> environment -> requested_version );

        thread_release( SQL_HANDLE_STMT, statement );

        return function_return( statement, SQL_ERROR );
    }

    /*
     * check states
     */

    if ( statement -> state == STATE_S5 ||
            statement -> state == STATE_S6 ||
            statement -> state == STATE_S7 )
    {
        dm_log_write( __FILE__,
                __LINE__,
                LOG_INFO,
                LOG_INFO,
                "Error: 24000" );

        __post_internal_error( &statement -> error,
                ERROR_24000, NULL,
                statement -> connection -> environment -> requested_version );

        thread_release( SQL_HANDLE_STMT, statement );

        return function_return( statement, SQL_ERROR );
    }
    else if ( statement -> state == STATE_S8 ||
            statement -> state == STATE_S9 ||
            statement -> state == STATE_S10 )
    {
        dm_log_write( __FILE__,
                __LINE__,
                LOG_INFO,
                LOG_INFO,
                "Error: HY010" );

        __post_internal_error( &statement -> error,
                ERROR_HY010, NULL,
                statement -> connection -> environment -> requested_version );

        thread_release( SQL_HANDLE_STMT, statement );

        return function_return( statement, SQL_ERROR );
    }

    if ( statement -> state == STATE_S11 ||
            statement -> state == STATE_S12 )
    {
        if ( statement -> interupted_func != SQL_API_SQLTABLES )
        {
            dm_log_write( __FILE__,
                    __LINE__,
                    LOG_INFO,
                    LOG_INFO,
                    "Error: HY010" );

            __post_internal_error( &statement -> error,
                    ERROR_HY010, NULL,
                    statement -> connection -> environment -> requested_version );

            thread_release( SQL_HANDLE_STMT, statement );

            return function_return( statement, SQL_ERROR );
        }
    }

    /*
     * TO_DO Check the SQL_ATTR_METADATA_ID settings
     */

    if ( statement -> connection -> unicode_driver )
    {
        if ( !CHECK_SQLTABLESW( statement -> connection ))
        {
            dm_log_write( __FILE__,
                    __LINE__,
                    LOG_INFO,
                    LOG_INFO,
                    "Error: IM001" );

            __post_internal_error( &statement -> error,
                    ERROR_IM001, NULL,
                    statement -> connection -> environment -> requested_version );

            thread_release( SQL_HANDLE_STMT, statement );

            return function_return( statement, SQL_ERROR );
        }

        ret = SQLTABLESW( statement -> connection ,
                statement -> driver_stmt,
                catalog_name,
                name_length1,
                schema_name,
                name_length2,
                table_name,
                name_length3,
                table_type,
                name_length4 );
    }
    else
    {
        SQLCHAR *as1, *as2, *as3, *as4;

        if ( !CHECK_SQLTABLES( statement -> connection ))
        {
            dm_log_write( __FILE__,
                    __LINE__,
                    LOG_INFO,
                    LOG_INFO,
                    "Error: IM001" );

            __post_internal_error( &statement -> error,
                    ERROR_IM001, NULL,
                    statement -> connection -> environment -> requested_version );

            thread_release( SQL_HANDLE_STMT, statement );

            return function_return( statement, SQL_ERROR );
        }

        as1 = (SQLCHAR*) unicode_to_ansi_alloc( catalog_name, name_length1, statement -> connection );
        as2 = (SQLCHAR*) unicode_to_ansi_alloc( schema_name, name_length2, statement -> connection );
        as3 = (SQLCHAR*) unicode_to_ansi_alloc( table_name, name_length3, statement -> connection );
        as4 = (SQLCHAR*) unicode_to_ansi_alloc( table_type, name_length4, statement -> connection );

        ret = SQLTABLES( statement -> connection ,
                statement -> driver_stmt,
                as1,
                name_length1,
                as2,
                name_length2,
                as3,
                name_length3,
                as4,
                name_length4 );

        if ( as1 ) free( as1 );
        if ( as2 ) free( as2 );
        if ( as3 ) free( as3 );
        if ( as4 ) free( as4 );
    }

    if ( SQL_SUCCEEDED( ret ))
    {
		/********
		 * Added this to get num cols from drivers which can only tell
		 * us after execute - PAH
		 */
        /*
         * There is no point in doing this as we can't trust the value
         * from SQLPrepare, so we can't perform checks on the column number
         *
        ret = SQLNUMRESULTCOLS( statement -> connection,
                statement -> driver_stmt, &statement -> numcols );
         */
         statement -> numcols = 1;
		/******/
        statement -> state = STATE_S5;
        statement -> prepared = 0;
    }
    else if ( ret == SQL_STILL_EXECUTING )
    {
        statement -> interupted_func = SQL_API_SQLTABLES;
        if ( statement -> state != STATE_S11 &&
                statement -> state != STATE_S12 )
            statement -> state = STATE_S11;
    }
    else
    {
        statement -> state = STATE_S1;
    }

    if ( log_info.log_flag ) 
    {
        sprintf( statement -> msg,
                "\n\t\tExit:[%s]",
                    __get_return_status( ret ));

        dm_log_write( __FILE__,
                __LINE__,
                LOG_INFO,
                LOG_INFO,
                statement -> msg );
    }

    thread_release( SQL_HANDLE_STMT, statement );

    return function_return( statement, ret );
}