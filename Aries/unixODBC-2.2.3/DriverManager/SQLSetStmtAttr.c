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
 * $Id: SQLSetStmtAttr.c,v 1.3 2002/07/16 13:08:18 lurcher Exp $
 *
 * $Log: SQLSetStmtAttr.c,v $
 * Revision 1.3  2002/07/16 13:08:18  lurcher
 *
 * Filter attribute values from SQLSetStmtAttr to SQLSetStmtOption to fit
 * within ODBC 2
 * Make DSN's double clickable in ODBCConfig
 *
 * Revision 1.2  2001/12/13 13:00:32  lurcher
 *
 * Remove most if not all warnings on 64 bit platforms
 * Add support for new MS 3.52 64 bit changes
 * Add override to disable the stopping of tracing
 * Add MAX_ROWS support in postgres driver
 *
 * Revision 1.1.1.1  2001/10/17 16:40:07  lurcher
 *
 * First upload to SourceForge
 *
 * Revision 1.8  2001/08/08 17:05:17  nick
 *
 * Add support for attribute setting in the ini files
 *
 * Revision 1.7  2001/07/03 09:30:41  nick
 *
 * Add ability to alter size of displayed message in the log
 *
 * Revision 1.6  2001/04/12 17:43:36  nick
 *
 * Change logging and added autotest to odbctest
 *
 * Revision 1.5  2001/03/28 14:57:22  nick
 *
 * Fix bugs in corsor lib introduced bu UNCODE and other changes
 *
 * Revision 1.4  2001/01/09 22:33:13  nick
 *
 * Stop passing NULL into SQLExtendedFetch
 * Further fixes to unicode to ansi conversions
 *
 * Revision 1.3  2000/12/18 12:53:29  nick
 *
 * More pooling tweeks
 *
 * Revision 1.2  2000/11/22 19:03:40  nick
 *
 * Fix problem with error status in SQLSpecialColumns
 *
 * Revision 1.1.1.1  2000/09/04 16:42:52  nick
 * Imported Sources
 *
 * Revision 1.11  2000/06/24 18:45:09  ngorham
 *
 * Fix for SQLExtendedFetch on big endian platforms. the row count pointer
 * was declared as a small not a int.
 *
 * Revision 1.10  2000/06/20 13:30:10  ngorham
 *
 * Fix problems when using bookmarks
 *
 * Revision 1.9  2000/02/11 00:41:46  ngorham
 *
 * Added a couple of fixes for drivers without SQLExtendedFetch
 *
 * Revision 1.8  1999/11/13 23:41:01  ngorham
 *
 * Alter the way DM logging works
 * Upgrade the Postgres driver to 6.4.6
 *
 * Revision 1.7  1999/10/24 23:54:19  ngorham
 *
 * First part of the changes to the error reporting
 *
 * Revision 1.6  1999/09/21 22:34:26  ngorham
 *
 * Improve performance by removing unneeded logging calls when logging is
 * disabled
 *
 * Revision 1.5  1999/09/19 22:24:34  ngorham
 *
 * Added support for the cursor library
 *
 * Revision 1.4  1999/07/10 21:10:17  ngorham
 *
 * Adjust error sqlstate from driver manager, depending on requested
 * version (ODBC2/3)
 *
 * Revision 1.3  1999/07/04 21:05:08  ngorham
 *
 * Add LGPL Headers to code
 *
 * Revision 1.2  1999/06/30 23:56:55  ngorham
 *
 * Add initial thread safety code
 *
 * Revision 1.1.1.1  1999/05/29 13:41:09  sShandyb
 * first go at it
 *
 * Revision 1.6  1999/06/04 16:29:00  ngorham
 *
 * Added chack that SQLSetStmtAttr exists in the driver before calling it
 *
 * Revision 1.5  1999/06/03 22:20:25  ngorham
 *
 * Finished off the ODBC3-2 mapping
 *
 * Revision 1.4  1999/06/02 23:48:45  ngorham
 *
 * Added more 3-2 mapping
 *
 * Revision 1.3  1999/06/02 20:12:10  ngorham
 *
 * Fixed botched log entry, and removed the dos \r from the sql header files.
 *
 * Revision 1.2  1999/06/02 19:57:21  ngorham
 *
 * Added code to check if a attempt is being made to compile with a C++
 * Compiler, and issue a message.
 * Start work on the ODBC2-3 conversions.
 *
 * Revision 1.1.1.1  1999/05/27 18:23:18  pharvey
 * Imported sources
 *
 * Revision 1.3  1999/05/09 23:27:11  nick
 * All the API done now
 *
 * Revision 1.2  1999/05/03 19:50:43  nick
 * Another check point
 *
 * Revision 1.1  1999/04/25 23:06:11  nick
 * Initial revision
 *
 *
 **********************************************************************/

#include "drivermanager.h"

static char const rcsid[]= "$RCSfile: SQLSetStmtAttr.c,v $ $Revision: 1.3 $";

SQLRETURN SQLSetStmtAttr( SQLHSTMT statement_handle,
           SQLINTEGER attribute,
           SQLPOINTER value,
           SQLINTEGER string_length )
{
    DMHSTMT statement = (DMHSTMT) statement_handle;
    SQLRETURN ret;
    SQLCHAR s1[ 100 + LOG_MESSAGE_LEN ];

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
            \n\t\t\tAttribute = %s\
            \n\t\t\tValue = %p\
            \n\t\t\tStrLen = %d",
                statement,
                __stmt_attr_as_string( s1, attribute ),
                value, 
                (int)string_length );

        dm_log_write( __FILE__, 
                __LINE__, 
                LOG_INFO, 
                LOG_INFO, 
                statement -> msg );
    }

    thread_protect( SQL_HANDLE_STMT, statement );

    /*
     * check states
     */

    if ( attribute == SQL_ATTR_CONCURRENCY ||
            attribute == SQL_ATTR_CURSOR_TYPE ||
            attribute == SQL_ATTR_SIMULATE_CURSOR ||
            attribute == SQL_ATTR_USE_BOOKMARKS ||
            attribute == SQL_ATTR_CURSOR_SCROLLABLE ||
            attribute == SQL_ATTR_CURSOR_SENSITIVITY )
    {
        if ( statement -> state == STATE_S2 ||
                statement -> state == STATE_S3 )
        {
            dm_log_write( __FILE__, 
                    __LINE__, 
                    LOG_INFO, 
                    LOG_INFO, 
                    "Error: HY011" );

            __post_internal_error( &statement -> error,
                 ERROR_HY011, NULL,
                 statement -> connection -> environment -> requested_version );

            thread_release( SQL_HANDLE_STMT, statement );

            return function_return( statement, SQL_ERROR );
        }
        else if ( statement -> state == STATE_S4 ||
                statement -> state == STATE_S5 ||
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
                statement -> state == STATE_S10 ||
                statement -> state == STATE_S11 ||
                statement -> state == STATE_S12 )
        {
            if ( statement -> prepared )
            {
                dm_log_write( __FILE__, 
                        __LINE__, 
                        LOG_INFO, 
                        LOG_INFO, 
                        "Error: HY011" );

                __post_internal_error( &statement -> error,
                     ERROR_HY011, NULL,
                     statement -> connection -> environment -> requested_version );

                thread_release( SQL_HANDLE_STMT, statement );

                return function_return( statement, SQL_ERROR );
            }
            else
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
    }
    else
    {
        if ( statement -> state == STATE_S8 ||
                statement -> state == STATE_S9 ||
                statement -> state == STATE_S10 ||
                statement -> state == STATE_S11 ||
                statement -> state == STATE_S12 )
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

    if ( !CHECK_SQLSETSTMTATTR( statement -> connection ) &&
        !CHECK_SQLSETSTMTOPTION( statement -> connection ))
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

    /*
     * map descriptors to our copies
     */

    if ( attribute == SQL_ATTR_APP_ROW_DESC )
    {
        DMHDESC desc = ( DMHDESC ) value;

        if ( !__validate_desc( desc ))
        {
            thread_release( SQL_HANDLE_STMT, statement );

            sprintf( statement -> msg, 
                    "\n\t\tExit:[%s]",
                    __get_return_status( SQL_INVALID_HANDLE ));

            dm_log_write( __FILE__, 
                    __LINE__, 
                    LOG_INFO, 
                    LOG_INFO, 
                    statement -> msg );

            return SQL_INVALID_HANDLE;
        }

        if ( desc -> implicit &&
                desc != statement -> implicit_ard )
        {
            dm_log_write( __FILE__, 
                    __LINE__, 
                    LOG_INFO, 
                    LOG_INFO, 
                    "Error: HY017" );

            __post_internal_error( &statement -> error,
                    ERROR_HY017, NULL,
                    statement -> connection -> environment -> requested_version );

            thread_release( SQL_HANDLE_STMT, statement );

            return function_return( statement, SQL_ERROR );
        }

        if ( desc -> connection !=
                statement -> connection )
        {
            dm_log_write( __FILE__, 
                    __LINE__, 
                    LOG_INFO, 
                    LOG_INFO, 
                    "Error: HY024" );

            __post_internal_error( &statement -> error,
                    ERROR_HY024, NULL,
                    statement -> connection -> environment -> requested_version );

            thread_release( SQL_HANDLE_STMT, statement );

            return function_return( statement, SQL_ERROR );
        }

        /*
         * set the value to the driver descriptor handle
         */
        value = ( SQLPOINTER ) desc -> driver_desc;
        statement -> ard = desc;
    }

    if ( attribute == SQL_ATTR_APP_PARAM_DESC )
    {
        DMHDESC desc = ( DMHDESC ) value;

        if ( !__validate_desc( desc ))
        {
            sprintf( statement -> msg, 
                    "\n\t\tExit:[%s]",
                    __get_return_status( SQL_INVALID_HANDLE ));

            dm_log_write( __FILE__, 
                    __LINE__, 
                    LOG_INFO, 
                    LOG_INFO, 
                    statement -> msg );

            thread_release( SQL_HANDLE_STMT, statement );

            return SQL_INVALID_HANDLE;
        }

        if ( desc -> implicit &&
                desc != statement -> implicit_apd )
        {
            dm_log_write( __FILE__, 
                    __LINE__, 
                    LOG_INFO, 
                    LOG_INFO, 
                    "Error: HY017" );

            __post_internal_error( &statement -> error,
                    ERROR_HY017, NULL,
                    statement -> connection -> environment -> requested_version );

            thread_release( SQL_HANDLE_STMT, statement );

            return function_return( statement, SQL_ERROR );
        }

        if ( desc -> connection !=
                statement -> connection )
        {
            dm_log_write( __FILE__, 
                    __LINE__, 
                    LOG_INFO, 
                    LOG_INFO, 
                    "Error: HY024" );

            __post_internal_error( &statement -> error,
                    ERROR_HY024, NULL,
                    statement -> connection -> environment -> requested_version );

            thread_release( SQL_HANDLE_STMT, statement );

            return function_return( statement, SQL_ERROR );
        }

        /*
         * set the value to the driver descriptor handle
         */
        value = ( SQLPOINTER ) desc -> driver_desc;
        statement -> apd = desc;
    }

    /*
     * save for internal use
     */

    if ( attribute == SQL_ATTR_METADATA_ID )
    {
        statement -> metadata_id = (int) value;
    }

    /*
     * is it something overridden
     */

    value = __attr_override( statement, SQL_HANDLE_STMT, attribute, value, &string_length );

    /*
     * does the call need mapping from 3 to 2
     */

    if ( attribute == SQL_ATTR_FETCH_BOOKMARK_PTR &&
            statement -> connection -> driver_act_ver == SQL_OV_ODBC2 &&
            CHECK_SQLEXTENDEDFETCH( statement -> connection ) &&
            !CHECK_SQLFETCHSCROLL( statement -> connection ))
    {
        statement -> fetch_bm_ptr = (SQLUINTEGER*) value;
        /*
         * pass on if required
         */
        if ( statement -> connection -> cl_handle )
        {
            if ( CHECK_SQLSETSTMTATTR( statement -> connection ))
            {
                SQLSETSTMTATTR( statement -> connection,
                    statement -> driver_stmt,
                    attribute,
                    value,
                    string_length );
            }
            else
            {
                ret = SQLSETSTMTOPTION( statement -> connection,
                    statement -> driver_stmt,
                    attribute,
                    value );
            }
        }
        ret = SQL_SUCCESS;
    }
    else if ( attribute == SQL_ATTR_ROW_STATUS_PTR &&
	      statement -> connection -> driver_act_ver == SQL_OV_ODBC2 )
    {
        statement -> row_st_arr = (SQLUSMALLINT*) value;
        /*
         * pass on if required
         */
        if ( statement -> connection -> cl_handle )
        {
            if ( CHECK_SQLSETSTMTATTR( statement -> connection ))
            {
                SQLSETSTMTATTR( statement -> connection,
                    statement -> driver_stmt,
                    attribute,
                    value,
                    string_length );
            }
            else
            {
                ret = SQLSETSTMTOPTION( statement -> connection,
                    statement -> driver_stmt,
                    attribute,
                    value );
            }
        }
        ret = SQL_SUCCESS;
    }
    else if ( attribute == SQL_ATTR_ROWS_FETCHED_PTR &&
	      statement -> connection -> driver_act_ver == SQL_OV_ODBC2 )
    {
        statement -> row_ct_ptr = (SQLUINTEGER*) value;
        /*
         * pass on if required
         */
        if ( statement -> connection -> cl_handle )
        {
            if ( CHECK_SQLSETSTMTATTR( statement -> connection ))
            {
                SQLSETSTMTATTR( statement -> connection,
                    statement -> driver_stmt,
                    attribute,
                    value,
                    string_length );
            }
            else
            {
                ret = SQLSETSTMTOPTION( statement -> connection,
                    statement -> driver_stmt,
                    attribute,
                    value );
            }
        }
        ret = SQL_SUCCESS;
    }
    else if ( attribute == SQL_ATTR_ROW_ARRAY_SIZE &&
            statement -> connection -> driver_act_ver == SQL_OV_ODBC2 )
    {
        /*
         * save this incase we need it in SQLExtendedFetch
         */
        statement -> row_array_size = (SQLUINTEGER) value;

        if ( CHECK_SQLSETSTMTATTR( statement -> connection ))
        {
            ret = SQLSETSTMTATTR( statement -> connection,
                statement -> driver_stmt,
                SQL_ROWSET_SIZE,
                value,
                string_length );
        }
        else
        {
            ret = SQLSETSTMTOPTION( statement -> connection,
                statement -> driver_stmt,
                SQL_ROWSET_SIZE,
                value );
        }
    }
    else if ( CHECK_SQLSETSTMTATTR( statement -> connection ))
    {
        ret = SQLSETSTMTATTR( statement -> connection,
                statement -> driver_stmt,
                attribute,
                value,
                string_length );
    }
    else
    {
        /*
         * Is it in the legal range of values
         */

        if ( attribute < SQL_STMT_DRIVER_MIN && 
                ( attribute > SQL_ROW_NUMBER || attribute < SQL_QUERY_TIMEOUT ))
        {
            dm_log_write( __FILE__, 
                    __LINE__, 
                    LOG_INFO, 
                    LOG_INFO, 
                    "Error: HY092" );

            __post_internal_error( &statement -> error,
                    ERROR_HY092, NULL,
                    statement -> connection -> environment -> requested_version );

            thread_release( SQL_HANDLE_STMT, statement );

            return function_return( statement, SQL_ERROR );
        }

        ret = SQLSETSTMTOPTION( statement -> connection,
                    statement -> driver_stmt,
                    attribute,
                    value );
    }

    /*
     * take notice of this
     */

    if ( attribute == SQL_ATTR_USE_BOOKMARKS && SQL_SUCCEEDED( ret ))
    {
        statement -> bookmarks_on = (SQLUINTEGER) value;
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
