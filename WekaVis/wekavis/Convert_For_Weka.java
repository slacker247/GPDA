/*
 * Convert_For_Weka.java
 *
 * Created on February 12, 2003, 11:42 AM
 */

package wekavis;

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
public class Convert_For_Weka
{
    public static ConfigData configuration = new ConfigData();

    /** Creates a new instance of Convert_For_Weka */
    public Convert_For_Weka()
    {
    }
    //ALL OF THE WEKA CODE IS IN HERE!
    weka.core.Instances readCSVFile(String fileName) throws IOException
    {
            //reads the CSV file and creates the objects required by Weka Classifiers
                    weka.core.converters.CSVLoader firstLoader = new weka.core.converters.CSVLoader();
                    firstLoader.setSource(new File(fileName));

                    weka.core.Instances firstInstance = firstLoader.getDataSet();

                    return firstInstance;

    }

    void createCSVFile (String query, String fileName) throws IOException
    {
            //creates the CSV Files based on the data pulled from the knowledge base
                    FileWriter oOutStream = new FileWriter(fileName, false);

                    //have to get the labels that we want to use for the top line and write them comma seperated.
                    Vector labels = getFieldNames();

                    for (int i = 5; i < 49; i ++) {
                            if (i != 17 && i != 22 && i != 31 && i != 35 && i != 40)
                                    oOutStream.write((String)labels.elementAt(i) + ",");
                    }

                    oOutStream.write("Belief");
                    oOutStream.write("\n");



                    Vector evidence = runQuery(query);//"Select * from Evidence;");

                    for (int i=0; i < evidence.size(); i++) {
                            Vector row = (Vector) evidence.elementAt(i);

                            for (int j=4; j < 43; j++) {
                                    if ( ((String)row.elementAt(j)).compareTo("?") == 0)
                                            oOutStream.write("None,");
                                    else
                                            oOutStream.write ((String)row.elementAt(j)+ ",");
                            }
                            if ( ((String)row.elementAt(2)).compareTo("?") == 0)
                                    oOutStream.write(EvidenceBelief.getBeliefString( (double) -1));
                            else
                                    oOutStream.write(EvidenceBelief.getBeliefString(Double.parseDouble( (String) row.elementAt(2))));

                            oOutStream.write("\n");
                    }


                    oOutStream.flush();
            oOutStream.close();
    }

    public static Vector getFieldNames ()
    {
            Vector fieldNamesRaw = runQuery("Select * from Labels;");
            Vector fieldNames = new Vector();

            for (int i=0;i<fieldNamesRaw.size() ;i++ )
            {
                    Vector column = (Vector)fieldNamesRaw.elementAt(i);
                    //System.out.print((i+1) + ": ");
                    for (int j=0;j<column.size() ;j++ )
                    {
                            String fieldName = (String)column.elementAt(j);
                            //System.out.println(fieldName);
                            fieldNames.add(fieldName);
                    }
                    //System.out.println();
            }

            return fieldNames;
    }

    public static Vector runQuery (String query)
    {
            String dbURL = configuration.getDBURL() + "//" + configuration.getHost() + "/" + configuration.getMission();
            ResultSet dbs = null;
            Connection conn = null;
            Statement queryStatement = null;
            Vector rows = new Vector();

            try
            {
                    Class.forName(configuration.getDBDriver()).newInstance();
                    conn = DriverManager.getConnection(dbURL, configuration.getUsername(), configuration.getPassword());
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
}
