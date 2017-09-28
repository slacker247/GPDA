/*
 *  Ontology.java                1-16-03
 *
 * Andy Trimble
 * (719) 570-8078
 * Andy.Trimble@trw.com
 *
 * Copyright (c) 2002 - 2003 Northrop Grumman Mission Systems
 *
 * Northrop Grumman Private/Proprietary
 */

package com.ngms.ontology;

import java.lang.String;
import java.util.StringTokenizer;
import java.io.*;
import java.sql.*;

/**
 *  This is an abstraction of an ontology.  The ontology
 *  nodes and structure are retrieved from a database.
 *
 *  @author Andrew Trimble
 */
public class Ontology {
        /** The maximum depth an ontology is allowed to be */
        public int MAX_DEPTH = 24;
        
        /** The maximum width an ontology level is allowed to be */
        public int MAX_WIDTH = 24;
        
        /** The maximum number of rules an ontology is
         *    allowed to have associated with it
         */
        public int MAX_RULES = 60;
        
        /** The depth of the ontology */
        public int depth;
        
        /** An array containing the width of each level of the ontology */
        public int[] levelElementNum = new int[MAX_DEPTH];
        
        /** The ontology stored as a two dimensional array of nodes */
        public Node[][] level = new Node[MAX_DEPTH][MAX_WIDTH];
	
	/** The number of nodes in the ontology */
	private int nnodes;

        /** An output buffer for printing an ontology to standard output */
        private char[][] output = new char[36][80];

        /** A constructor */
        public Ontology() {
                // initialize the level sizes to 0
                for(int i = 0; i < MAX_DEPTH; i++)
                        levelElementNum[i] = 0;
        }
	
	/** A method for getting the number of nodes in an ontology.
	 *
	 * @return the number of nodes
	 */
	public int getNodeNum() {
		return nnodes;
	}

        /** A method to print the ontology to standard output */
        public void printOntology() {
                int WIDTH = 80;
                int HEIGHT = 6*depth;
                int i, j, k, letterCount, row, col;
                String word;
                row = 0;
                col = 0;

                // initialize the output buffer to blank spaces
                for(i = 0; i < HEIGHT; i++) {
                        for(j = 0; j < WIDTH; j++)
                                output[i][j] = ' ';
                }

                System.out.println( "Printing graph...");
                for(i = 0; i < depth; i++) { // iterate through the levels
                        for(j = 0; j < levelElementNum[i]; j++) { // iterate throught nodes in level i
                                // format output
                                col = 13*j;
                                row = 6*i;

                                for(k = col+1; k < 13*(j+1)-1; k++) output[row][k] = '_';
                                for(k = col+1; k < 13*(j+1)-1; k++) output[row+5][k] = '_';
                                for(k = row+1; k <= row+5; k++) output[k][col] = '|';
                                for(k = row+1; k <= row+5; k++) output[k][13*(j+1)-1] = '|';

                                row++;
                                word = level[i][j].getName();
                                letterCount = 0;
                                col++;
                                while(letterCount < word.length()) {
                                        if(word.charAt(letterCount) == '_') {
                                                col = 13*j;
                                                letterCount++; // don't want the '_'
                                                col++;
                                                row++;
                                        }
                                        
                                        try {
                                                output[row][col] = word.charAt(letterCount);
                                        } catch(Exception ex){}

                                        letterCount++;
                                        col++;
                                }
                                output[6*i+4][13*j+6] = (char)(level[i][j].hits+48);
                        }
                }
                for(i = 0; i < HEIGHT; i++) { // print out the output buffer
                        for(j = 0; j < WIDTH; j++)
                                System.out.print(output[i][j]);
                        System.out.flush();
                }
        }

        /** A method for saving the node and edge information to a file
         *  for the GUI to load.  The files are always overwritten.
         *
         *  @param edgeName the name of the file to save the edges to
         *  @param nodeName the name of the file to save the nodes to
         */
        public void serialize(String edgeName, String nodeName) {
                int i, j, k, l; // some loop counters
                StringTokenizer st;
                try {
                        FileWriter edgeOutput = new FileWriter(new File(edgeName)); // open edge file for writing
                        FileWriter nodeOutput = new FileWriter(new File(nodeName)); // open node file for writing

                        for(i = 0; i < depth; i++) { // iterate through the levels
                                for(j = 0; j < levelElementNum[i]; j++) { // iterate through the nodes in each level
                                        // output the nodes
                                        nodeOutput.write(level[i][j].getName());
                                        nodeOutput.write(" " + (j+1) + " " + (i+1) + "\n");

                                        // output the edges
                                        for(k = 0; k < level[i][j].childNum; k++) { // iterate through the children
                                                edgeOutput.write(level[i][j].getName());
                                                edgeOutput.write(" ");
                                                edgeOutput.write(level[i+1][level[i][j].childIndex[k]].getName());
                                                edgeOutput.write(" " + level[i][j].getWeight(k) + "\n");
                                        }
                                }
                        }
                        edgeOutput.close(); // close edge file
                        nodeOutput.close(); // close node file
                } catch(Exception ex) {ex.printStackTrace();}
        }
	
	/** A method to ensure backwards compatability.
	 *   This uses the default story of "Story_T".
	 *
	 *  @param dbName the name of the database to load
	 */
	public void parse(String port, String host, String uname, String passwd, String dbName) throws Exception {
		try {
			parse(port, host, uname, passwd, dbName, dbName);
		} catch(Exception ex) {
			throw ex;
		}
	}

        /** A method to load an ontology from the database
         *
         *  @param dbName the name of the database to load
	 *  @param story the name of the story to load
         */
        public void parse(String port, String host, String uname, String passwd, String dbName, String story) throws Exception {
                boolean verbose = false;
                int i, k;
                int levelNum;
                String temp, name, parentTemp, weightTemp, levelTemp, nameTemp;
                int parentNum;

                StringTokenizer weightTokenizer, st;

                Node newNode;

        	Connection conn;
                Statement stmnt;
                ResultSet rs;

                try { // get an instance of a driver
                        Class.forName("com.mysql.jdbc.Driver").newInstance();
                } catch(Exception ex) {
                        System.out.println("Error getting driver instance\n");
                        throw ex;
                }

                try { // create a connection
        	        conn = DriverManager.getConnection(
	        	        "jdbc:mysql://" + host + ":" + port + "/" + dbName + "?user=" + uname + "&password=" + passwd);
                } catch(Exception ex) {
                        System.out.println("Error getting connection\n");
                        throw ex;
                }

                try { // create a statement
                        stmnt = conn.createStatement();
                } catch(Exception ex) {
                        System.out.println("Error creating statement\n");
                        throw ex;
                }
                
                // need to get all stories

                try { // execute a statement to get the labels
                        rs = stmnt.executeQuery("SELECT * FROM " + story + ";");
                } catch(Exception ex) {
                        System.out.println("Error executing statement\n");
                        throw ex;
                }

                try { // parse the nodes and weights
                        depth = 0;
                        rs.next();
                        temp = rs.getString(1); // retrieve the level names
                        st = new StringTokenizer(temp, ","); // break it into tokens
                        int size = st.countTokens(); // the number of levels
                        String[] levelNames = new String[size];
                        for(int index = 0; index < size; index++) { // load the level names into an array
                                levelNames[index] = st.nextToken();
                                depth++;
                        }
                        
                        newNode = new Node();
                        nameTemp = rs.getString(8);
                        newNode.setName(nameTemp); // retrieve the node name
                        levelTemp = rs.getString(9); // retrieve the level it belongs to
                        int j = 0;
                        while(!levelTemp.equalsIgnoreCase(levelNames[j])) // find the index of the level
                                j++;
                        level[j][levelElementNum[j]] = newNode; // add the node
                        levelElementNum[j]++;

                        while(rs.next()) { // while there are nodes
                                newNode = new Node();
                                nameTemp = rs.getString(8);
                                newNode.setName(nameTemp); // retrieve the node name
                                levelTemp = rs.getString(9); // retrieve the level it belongs to

                                // find the index of the level it belongs in
                                j = 0;
                                while(!levelTemp.equalsIgnoreCase(levelNames[j]))
                                        j++;
                                
                                // add node
                                level[j][levelElementNum[j]] = newNode;
                                levelElementNum[j]++;

                                // Nodes in the ontology have children but nodes in the
                                // database have parents.  We have to cycle through a node's
                                // parents and add the current node as a child.
                                parentNum = 0;
                                if(j != 0) { // determine parent nodes
                                        st = new StringTokenizer(rs.getString(3), ","); // parse parent string
                                        weightTokenizer = new StringTokenizer(rs.getString(4), ",");
                                        while(st.hasMoreTokens()) {
                                                parentTemp = st.nextToken();
                                                weightTemp = weightTokenizer.nextToken();
                                                k = 0;
                                                // find index of parent in local matrix
                                                while(!parentTemp.equalsIgnoreCase(level[j-1][k].getName()))
                                                        k++;
                                                // find weight
                                                level[j-1][k].addChild(levelElementNum[j]-1, (new Float(weightTemp)).floatValue());
                                        } // end iterate through parents
                                } // end if
                        } // end cycle through nodes
                } catch (Exception ex) {
                        System.out.println("Error parsing ontology\n");
                        throw ex;
                }
                
		nnodes = 0;
                // replace spaces with underscores
                for(int m = 0; m < depth; m++) {
                        for(int n = 0; n < levelElementNum[m]; n++) {
                                level[m][n].setName(level[m][n].getName().replace(' ', '_'));
				nnodes++; // count the number of nodes
                        }
                }
        }
}

