/**
 * JDBC Interface to Mysql functions
 *
 * <p>
 * This class provides information about the database as a whole.
 *
 * <p>
 * Many of the methods here return lists of information in ResultSets.
 * You can use the normal ResultSet methods such as getString and getInt
 * to retrieve the data from these ResultSets.  If a given form of
 * metadata is not available, these methods show throw a java.sql.SQLException.
 * 
 * <p>
 * Some of these methods take arguments that are String patterns.  These
 * methods all have names such as fooPattern.  Within a pattern String "%"
 * means match any substring of 0 or more characters and "_" means match
 * any one character.
 *
 * @author Mark Matthews <mmatthew@worldserver.com>
 * @version $Id: DatabaseMetaData.java,v 1.2 2002/04/21 03:03:46 mark_matthews Exp $
 */

package com.mysql.jdbc.jdbc1;

import java.sql.*;

public class DatabaseMetaData extends com.mysql.jdbc.DatabaseMetaData
				      implements java.sql.DatabaseMetaData
{
  
    public DatabaseMetaData(com.mysql.jdbc.Connection Conn, String Database) 
    {
	super(Conn, Database);
    }

    protected java.sql.ResultSet buildResultSet(com.mysql.jdbc.Field[] Fields, 
						java.util.Vector Rows,
						com.mysql.jdbc.Connection Conn)
    {
	return new com.mysql.jdbc.jdbc1.ResultSet(Fields, Rows, Conn);
    }
    
    public boolean supportsResultSetType(int i)
    {return false;}
    
    public boolean supportsResultSetConcurrency(int i, int x)
    {return false;}
    
    public boolean ownUpdatesAreVisible(int i)
    {return false;}
    
    public boolean ownDeletesAreVisible(int i)
    {return false;}
    
    public boolean ownInsertsAreVisible(int i)
    {return false;}
    
    public boolean otherUpdatesAreVisible(int i)
    {return false;}
    
    public boolean othersUpdatesAreVisible(int i)
    {return false;}
    
    public boolean othersDeletesAreVisible(int i)
    {return false;}
    
    public boolean othersInsertsAreVisible(int i)
    {return false;}
    
    public boolean updatesAreDetected(int i)
    {return false;}
    
    public boolean deletesAreDetected(int i)
    {return false;}
    
    public boolean insertsAreDetected(int i)
    {return false;}
    
    public boolean supportsBatchUpdates()
    {return false;}
    
    public java.sql.ResultSet getUDTs(String s, String ss, String sss, int[] i)
    {return null;}
    
    public java.sql.Connection getConnection()
    {return null;}
    
    public boolean supportsSavepoints()
    {return false;}
    
    public boolean supportsNamedParameters()
    {return false;}
    
    public boolean supportsMultipleOpenResults()
    {return false;}
    
    public boolean supportsGetGeneratedKeys()
    {return false;}
    
    public java.sql.ResultSet getSuperTypes(String s, String ss, String sss)
    {return null;}
    
    public java.sql.ResultSet getSuperTables(String s, String ss, String sss)
    {return null;}
    
    public java.sql.ResultSet getAttributes(String s, String ss, String sss, String ssss)
    {return null;}
    
    public boolean supportsResultSetHoldability(int i)
    {return false;}
    
    public int getResultSetHoldability()
    {return 0;}
    
    public int getDatabaseMajorVersion()
    {return 0;}
    
    public int getDatabaseMinorVersion()
    {return 0;}
    
    public int getJDBCMajorVersion()
    {return 0;}
    
    public int getJDBCMinorVersion()
    {return 0;}
    
    public int getSQLStateType()
    {return 0;}
    
    public boolean locatorsUpdateCopy()
    {return false;}
    
    public boolean supportsStatementPooling()
    {return false;}
};
