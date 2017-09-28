/*
 * DBConnect.java
 *
 * Created on February 12, 2003, 11:42 AM
 */

package DBConnect;

import javax.swing.*;
import javax.swing.event.*;
import java.awt.*;
import java.awt.event.*;
import java.sql.*;
import java.util.Vector;
import java.io.*;
/**
 *
 * @author  jeffmac
 */
public class DBConnect
{
    public static ConfigData configuration = new ConfigData();

    /** Creates a new instance of DBConnect */
    public DBConnect()
    {
      configuration.setDB_Driver("com.mysql.jdbc.Driver");
      configuration.setDB_Url("jdbc:mysql:");
      configuration.setHost("158.114.52.140");
      configuration.setMission("CND");
      configuration.setPassword("gpda");
      configuration.setPort("3306");
      configuration.setUser("root");
    }

    /* This allows you to submit a query and get the values back from that
     * query.
     */
    public static Vector runQuery (String query)
    {
         String dbURL = configuration.getDB_Url() + "//" + configuration.getHost() + "/" + configuration.getMission();
         ResultSet dbs = null;
         Connection conn = null;
         Statement queryStatement = null;
         Vector rows = new Vector();

         try
         {
              Class.forName(configuration.getDB_Driver()).newInstance();
              conn = DriverManager.getConnection(dbURL, configuration.getUser(), configuration.getPassword());
              queryStatement = conn.createStatement();

              System.out.println("Established a connection with the DB: " + dbURL);

              dbs = queryStatement.executeQuery(query);
              ResultSetMetaData rsmd = dbs.getMetaData();
              int numberOfColumns = rsmd.getColumnCount();
              while (dbs.next())
              {
                   Vector columns = new Vector();
                   for (int column = 1;column <= numberOfColumns ; column++)
                   {
                        columns.add(dbs.getObject(column));
                        //System.out.println(column);
                   }
                   rows.add(columns);
              }
         }
         catch (Exception e)
         {
              System.out.println("EEE - Error while running query: ");
              System.out.println("        " + query);
              e.printStackTrace();
              System.out.println("EEE - ---------------------------");
         }
         finally {
              if (queryStatement != null)
                  try	{ queryStatement.close(); }
                  catch (SQLException SQLEx)  { }
              if (conn != null)
                  try	{ conn.close(); }
                  catch (SQLException SQLEx) { }
         }

         return rows;
    }

    /* This allows you to modify the database without getting or expecting a 
     * return value other than did it work.
     */
    public boolean executeQuery(String query)
    {
         String dbURL = configuration.getDB_Url() + "//" + configuration.getHost() + "/" + configuration.getMission();
         boolean dbs = false;
         Connection conn = null;
         Statement queryStatement = null;
         Vector rows = new Vector();

         try
         {
              Class.forName(configuration.getDB_Driver()).newInstance();
              conn = DriverManager.getConnection(dbURL, configuration.getUser(), configuration.getPassword());
              queryStatement = conn.createStatement();

              System.out.println("Established a connection with the DB: " + dbURL);

              dbs = queryStatement.execute(query);
         }
         catch (Exception e)
         {
              System.out.println("EEE - Error while running query: ");
              System.out.println("        " + query);
              e.printStackTrace();
              System.out.println("EEE - ---------------------------");
         }
         finally {
              if (queryStatement != null)
                  try	{ queryStatement.close(); }
                  catch (SQLException SQLEx)  { }
              if (conn != null)
                  try	{ conn.close(); }
                  catch (SQLException SQLEx) { }
         }

         return dbs;
    }
    public static void main(String [] args)
    {
      DBConnect test = new DBConnect();
/*      DBConnect.configuration.setDB_Driver("com.mysql.jdbc.Driver");
      DBConnect.configuration.setDB_Url("jdbc:mysql:");
      DBConnect.configuration.setHost("158.114.52.140");
      DBConnect.configuration.setMission("CND");
      DBConnect.configuration.setPassword("gpda");
      DBConnect.configuration.setPort("3306");
      DBConnect.configuration.setUser("root");
*/
      Vector v = test.runQuery("Show databases;");
      Vector v2 = (Vector)v.elementAt(0);
      for(int i = 0; i < v.size(); i++)
      {
        Object o = v.elementAt(i);
        System.out.println("Test: " + o.toString());
      }

      try
      {
        DBConnect.configuration.writeConfigFile();
      }catch(Exception ioe)
      {
        ioe.printStackTrace();
      }
    }
}
