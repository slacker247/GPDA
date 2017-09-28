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
 * $Id: SQLSetDescField.c,v 1.1.1.1 2001/10/17 16:40:07 lurcher Exp $
 *
 * $Log: SQLSetDescField.c,v $
 * Revision 1.1.1.1  2001/10/17 16:40:07  lurcher
 *
 * First upload to SourceForge
 *
 * Revision 1.4  2001/07/03 09:30:41  nick
 *
 * Add ability to alter size of displayed message in the log
 *
 * Revision 1.3  2001/04/17 16:29:39  nick
 *
 * More checks and autotest fixes
 *
 * Revision 1.2  2001/04/12 17:43:36  nick
 *
 * Change logging and added autotest to odbctest
 *
 * Revision 1.1.1.1  2000/09/04 16:42:52  nick
 * Imported Sources
 *
 * Revision 1.7  1999/11/13 23:41:00  ngorham
 *
 * Alter the way DM logging works
 * Upgrade the Postgres driver to 6.4.6
 *
 * Revision 1.6  1999/10/24 23:54:19  ngorham
 *
 * First part of the changes to the error reporting
 *
 * Revision 1.5  1999/09/21 22:34:25  ngorham
 *
 * Improve performance by removing unneeded logging calls when logging is
 * disabled
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
 * Revision 1.1.1.1  1999/05/29 13:41:08  sShandyb
 * first go at it
 *
 * Revision 1.1.1.1  1999/05/27 18:23:18  pharvey
 * Imported sources
 *
 * Revision 1.2  1999/05/04 22:41:12  nick
 * and another night ends
 *
 * Revision 1.1  1999/04/25 23:06:11  nick
 * Initial revision
 *
 *
 **********************************************************************/

#include "drivermanager.h"

static char const rcsid[]= "$RCSfile: SQLSetDescField.c,v $ $Revision: 1.1.1.1 $";

SQLRETURN SQLSetDescField( SQLHDESC descriptor_handle,
           SQLSMALLINT rec_number, 
           SQLSMALLINT field_identifier,
           SQLPOINTER value, 
           SQLINTEGER buffer_length )
{
    /*
     * not quite sure how the descriptor can be
     * allocated to a statement, all the documentation talks
     * about state transitions on statement states, but the
     * descriptor may be allocated with more than one statement
     * at one time. Which one should I check ?
     */
    DMHDESC descriptor = (DMHDESC) descriptor_handle;
    SQLRETURN ret;
    SQLCHAR s1[ 100 + LOG_MESSAGE_LEN ];

    /*
     * check descriptor
     */

    if ( !__validate_desc( descriptor ))
    {
        dm_log_write( __FILE__, 
                    __LINE__, 
                    LOG_INFO, 
                    LOG_INFO, 
                    "Error: SQL_INVALID_HANDLE" );

        return SQL_INVALID_HANDLE;
    }

    function_entry( descriptor );

    if ( log_info.log_flag )
    {
        sprintf( descriptor -> msg, "\n\t\tEntry:\
            \n\t\t\tDescriptor = %p\
            \n\t\t\tRec Number = %d\
            \n\t\t\tField Ident = %s\
            \n\t\t\tValue = %p\
            \n\t\t\tBuffer Length = %d",
                descriptor,
                rec_number,
                __desc_attr_as_string( s1, field_identifier ),
                value, 
                (int)buffer_length );

        dm_log_write( __FILE__, 
                __LINE__, 
                LOG_INFO, 
                LOG_INFO, 
                descriptor -> msg );
    }

    thread_protect( SQL_HANDLE_DESC, descriptor );

    if ( descriptor -> connection -> state < STATE_C4 )
    {
        dm_log_write( __FILE__, 
                __LINE__, 
                LOG_INFO, 
                LOG_INFO, 
                "Error: HY010" );

        __post_internal_error( &descriptor -> error,
                ERROR_HY010, NULL,
                descriptor -> connection -> environment -> requested_version );

        thread_release( SQL_HANDLE_DESC, descriptor );

        return function_return( descriptor, SQL_ERROR );
    }

    if ( !CHECK_SQLSETDESCFIELD( descriptor -> connection ))
    {
        dm_log_write( __FILE__, 
                __LINE__, 
                LOG_INFO, 
                LOG_INFO, 
                "Error: IM001" );

        __post_internal_error( &descriptor -> error,
                ERROR_IM001, NULL,
                descriptor -> connection -> environment -> requested_version );

        thread_release( SQL_HANDLE_DESC, descriptor );

        return function_return( descriptor, SQL_ERROR );
    }

    ret = SQLSETDESCFIELD( descriptor -> connection,
            descriptor -> driver_desc,
            rec_number, 
            field_identifier,
            value, 
            buffer_length );

    if ( log_info.log_flag )
    {
        sprintf( descriptor -> msg, 
                "\n\t\tExit:[%s]",
                    __get_return_status( ret ));

        dm_log_write( __FILE__, 
                __LINE__, 
                LOG_INFO, 
                LOG_INFO, 
                descriptor -> msg );
    }

    thread_release( SQL_HANDLE_DESC, descriptor );

    return function_return( descriptor, ret );
}
