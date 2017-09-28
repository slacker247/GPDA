/*********************************************************************
 *
 * unixODBC Cursor Library
 *
 * Created by Nick Gorham
 * (nick@easysoft.com).
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
 * $Id: SQLGetData.c,v 1.3 2002/01/21 18:00:51 lurcher Exp $
 *
 * $Log: SQLGetData.c,v $
 * Revision 1.3  2002/01/21 18:00:51  lurcher
 *
 * Assorted fixed and changes, mainly UNICODE/bug fixes
 *
 * Revision 1.2  2001/12/13 13:00:33  lurcher
 *
 * Remove most if not all warnings on 64 bit platforms
 * Add support for new MS 3.52 64 bit changes
 * Add override to disable the stopping of tracing
 * Add MAX_ROWS support in postgres driver
 *
 * Revision 1.1.1.1  2001/10/17 16:40:15  lurcher
 *
 * First upload to SourceForge
 *
 * Revision 1.2  2001/03/28 14:57:22  nick
 *
 * Fix bugs in corsor lib introduced bu UNCODE and other changes
 *
 * Revision 1.1.1.1  2000/09/04 16:42:52  nick
 * Imported Sources
 *
 * Revision 1.1  1999/09/19 22:22:50  ngorham
 *
 *
 * Added first cursor library work, read only at the moment and only works
 * with selects with no where clause
 *
 *
 **********************************************************************/

#include "cursorlibrary.h"

SQLRETURN CLGetData( SQLHSTMT statement_handle,
           SQLUSMALLINT column_number,
           SQLSMALLINT target_type,
           SQLPOINTER target_value,
           SQLINTEGER buffer_length,
           SQLINTEGER *strlen_or_ind )
{
    CLHSTMT cl_statement = (CLHSTMT) statement_handle; 
    CLHDBC cl_connection = cl_statement -> cl_connection;
    SQLHANDLE hstmt;
    SQLRETURN ret;
    SQLCHAR sql[ 4095 ];
    CLBCOL *bound_columns;
    int i;

    if ( cl_statement -> not_from_select )
    {
        __post_internal_error( &cl_statement -> dm_statement -> error,
                ERROR_SL004, NULL,
                cl_statement -> dm_statement -> connection ->
                    environment -> requested_version );

        return SQL_ERROR;
    }

    /*
     * check we have what we need
     */

    if ( !CHECK_SQLBINDPARAM( cl_connection ) && 
                !CHECK_SQLBINDPARAMETER( cl_connection ))
    {
        __post_internal_error( &cl_statement -> dm_statement -> error,
                ERROR_S1000, "Driver can not bind parameters",
                cl_statement -> dm_statement -> connection ->
                    environment -> requested_version );

        return SQL_ERROR;
    }
    if ( !CHECK_SQLEXECDIRECT( cl_connection ) &&
            !( CHECK_SQLPREPARE( cl_connection ) && 
                CHECK_SQLEXECUTE( cl_connection )))
    {
        __post_internal_error( &cl_statement -> dm_statement -> error,
                ERROR_S1000, "Driver can not prepare or execute",
                cl_statement -> dm_statement -> connection ->
                    environment -> requested_version );

        return SQL_ERROR;
    }
    if ( !CHECK_SQLFETCH( cl_connection ))
    {
        __post_internal_error( &cl_statement -> dm_statement -> error,
                ERROR_S1000, "Driver can not fetch",
                cl_statement -> dm_statement -> connection ->
                    environment -> requested_version );

        return SQL_ERROR;
    }
    if ( !CHECK_SQLGETDATA( cl_connection ))
    {
        __post_internal_error( &cl_statement -> dm_statement -> error,
                ERROR_S1000, "Driver can not getdata",
                cl_statement -> dm_statement -> connection ->
                    environment -> requested_version );

        return SQL_ERROR;
    }

    /*
     * if it's not and closed resultset, and the driver 
     * can only support one active statement, then close 
     * the result set first
     */

    if ( !cl_statement -> rowset_complete &&
            cl_statement -> cl_connection -> active_statement_allowed == 1 )
    {
        int sav_start, sav_pos;

        sav_start = cl_statement -> curr_rowset_start;
        sav_pos = cl_statement -> rowset_position;

        complete_rowset( cl_statement, 0 );
        
        SQLFREESTMT( cl_connection,
                cl_statement -> driver_stmt,
                SQL_DROP );

        cl_statement -> driver_stmt_closed = 1;

        /*
         * restore the position
         */
        cl_statement -> curr_rowset_start = sav_start;
        cl_statement -> rowset_position = sav_pos;
    }

    /*
     * refresh the data
     */

    ret = fetch_row( cl_statement, 
            cl_statement -> curr_rowset_start +
                cl_statement -> cursor_pos - 1,
            -1 );

    ret = SQLALLOCSTMT( cl_connection, 
            cl_connection -> driver_dbc,
            &hstmt,
            NULL );

    if ( !SQL_SUCCEEDED( ret ))
    {       
        __post_internal_error( &cl_statement -> dm_statement -> error,
                ERROR_S1000, "SQLAllocStmt failed in driver",
                cl_statement -> dm_statement -> connection ->
                    environment -> requested_version );

        return SQL_ERROR;
    }

    /*
     * append the cryterior to the end of the statement, binding
     * the comumns while we are at it
     */

    strcpy((char*) sql, cl_statement -> sql_text );

    /*
     * assume for the moment that the selent has no WHERE
     */

    strcat((char*) sql, " WHERE" );

    /*
     * this works because the bound list is in column order
     */

    bound_columns = cl_statement -> bound_columns;

    for ( i = 0; i < cl_statement -> column_count; i ++ )
    {
        char addon[ 256 ];

        sprintf( addon, " %s = ?", cl_statement -> column_names[ i ] );

        if ( i > 0 )
        {
            strcat((char*) sql, " AND" );
        }

        strcat((char*) sql, addon );

        if ( CHECK_SQLBINDPARAMETER( cl_connection ))
        {
            ret = SQLBINDPARAMETER( cl_connection,
                    hstmt,
                    i + 1,
                    SQL_PARAM_INPUT,
                    bound_columns -> bound_type,
                    cl_statement -> data_type[ i ],
                    cl_statement -> column_size[ i ],
                    cl_statement -> decimal_digits[ i ],
                    bound_columns -> local_buffer,
                    bound_columns -> bound_length,
                    &bound_columns -> len_ind );
        }
        else
        {
            ret = SQLBINDPARAM( cl_connection,
                    hstmt,
                    i + 1,
                    bound_columns -> bound_type,
                    cl_statement -> data_type[ i ],
                    cl_statement -> column_size[ i ],
                    cl_statement -> decimal_digits[ i ],
                    bound_columns -> local_buffer,
                    &bound_columns -> len_ind );
        }

        if ( !SQL_SUCCEEDED( ret ))
        {
            __post_internal_error( &cl_statement -> dm_statement -> error,
                    ERROR_SL010, NULL,
                    cl_statement -> dm_statement -> connection ->
                        environment -> requested_version );

            SQLFREESTMT( cl_connection,
                    hstmt, 
                    SQL_DROP );

            return SQL_ERROR;
        }

        bound_columns = bound_columns -> next;
    }

    if ( CHECK_SQLEXECDIRECT( cl_connection ))
    {
        ret = SQLEXECDIRECT( cl_connection,
                    hstmt,
                    sql,
                    strlen((char*) sql ));
    }
    else
    {
        ret = SQLPREPARE( cl_connection,
                    hstmt,
                    sql,
                    strlen((char*) sql ));

        if ( SQL_SUCCEEDED( ret ))
        {
            ret = SQLEXECUTE( cl_connection,
                    hstmt );
        }
    }

    if ( !SQL_SUCCEEDED( ret ))
    {
        __post_internal_error( &cl_statement -> dm_statement -> error,
                ERROR_SL010, NULL,
                cl_statement -> dm_statement -> connection ->
                    environment -> requested_version );

        SQLFREESTMT( cl_connection,
                hstmt,
                SQL_DROP );

        return SQL_ERROR;
    }

    ret = SQLFETCH( cl_connection,
            hstmt );

    if ( !SQL_SUCCEEDED( ret ))
    {
        __post_internal_error( &cl_statement -> dm_statement -> error,
                ERROR_SL010, NULL,
                cl_statement -> dm_statement -> connection ->
                    environment -> requested_version );

        SQLFREESTMT( cl_connection,
                hstmt,
                SQL_DROP );

        return SQL_ERROR;
    }

    ret = SQLGETDATA( cl_connection,
                hstmt, 
                column_number,
                target_type,
                target_value,
                buffer_length,
                strlen_or_ind );

    if ( !SQL_SUCCEEDED( ret ))
    {
        SQLCHAR sqlstate[ 6 ];
        SQLINTEGER native_error, ind;
        SQLCHAR message_text[ SQL_MAX_MESSAGE_LENGTH + 1 ];
        SQLRETURN ret;

        /*
         * get the error from the driver before
         * loseing the connection
         */

        do
        {
            ret = SQLERROR( cl_connection,
                    SQL_NULL_HENV,
                    SQL_NULL_HSTMT,
                    hstmt,
                    sqlstate,
                    &native_error,
                    message_text,
                    sizeof( message_text ),
                    &ind );

            __post_internal_error_ex( &cl_statement -> dm_statement -> error,
                    sqlstate, native_error, message_text,
                    SUBCLASS_ODBC, SUBCLASS_ODBC );
        }
        while ( SQL_SUCCEEDED( ret ));
    }

    SQLFREESTMT( cl_connection,
            hstmt, 
            SQL_DROP );

    return ret;
}
