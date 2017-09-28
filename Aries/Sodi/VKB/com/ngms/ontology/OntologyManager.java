/*
 *  OntologyManager.java                1-16-03
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

/**
 * This is a class for managing an ontology.
 * It provides methods for tallying
 * the hits in the ontology, resetting the
 * hits, finding the concept node using the
 * semantic mean algorithm, and finding the
 * concept node using the semantic mode algorithm.
 * All the methods in this class assume that
 * the ontology has already been parsed.
 *
 * @author Andrew Trimble
 */
public class OntologyManager {
        /** The ontology */
        private Ontology ontology; 
        
        /** The database name from which to load the ontology */
        private String dbName = "";
        
        /** The associated RuleManager */
        public RuleManager rm;
        
        /** A method to clean up the manager */
        public void finalize() {
                rm.finalize();
        }

        /** A constructor
         *
         *  @param dbName the database from which to parse the ontology
         */
        public OntologyManager(String port, String host, String uname, String passwd, String dbName) {
                rm = new RuleManager();
                this.dbName = dbName;
                ontology = new Ontology();
                try {
                        ontology.parse(port, host, uname, passwd, dbName);
                } catch(Exception ex) {ex.printStackTrace();}
        }

        /** Print the ontology to standard output */
        public void printOntology() {
                ontology.printOntology();
        }

        /** Save the ontology to a file
         *  Used for the GUI representation of an ontology
         */
        public void saveOntology() {
                ontology.serialize("./resources/.edges", "./resources/.nodes");
		//ontology.printOntology();
        }

        /** Match the hits found in a Template object
         *
         *  @param tm the template from which the hits are matched
         *  @param inst the index of the template to match
         */
        public void matchHits(TemplateManager tm, int inst) {

                int i, j;
                boolean valueEq = true;
                boolean fieldEq = true;

                RuleManager ruleM = this.rm;

                for(i = 0; i < ruleM.ruleNum; i++) { // iterate through rules
                        for(j = 0; j < tm.length; j++) { // iterate through the template values
                                if(((ruleM.getRuleAt(i)).fieldValue).equalsIgnoreCase(tm.elementAt(inst,j)))
                                        valueEq = true;
                                else
                                        valueEq = false;
                                        
                                if(((ruleM.getRuleAt(i)).fieldName).equalsIgnoreCase(tm.fieldAt(j)))
                                        fieldEq = true;
                                else
                                        fieldEq = false;

                                if(valueEq && fieldEq)// if same field name and associated value, increment hits
                                        (ontology.level[(ruleM.getRuleAt(i)).level][(ruleM.getRuleAt(i)).entry]).hits++;
                        }
                }
        }

        /** This is a method for resetting all hits to zero
         *   This method assumes that the ontology has
         *   already been parsed and loaded.
         */
        public void resetHits() {
                int i, j;
                for(i = 0; i < ontology.depth; i++) { // iterate through ontology levels
                        for(j = 0; j < ontology.levelElementNum[i]; j++) // iterate through nodes in each level
                                (ontology.level[i][j]).hits = 0;
                }
        }
	
	public void addHit(int x, int y) {
	        (ontology.level[y][x]).hits++;
	}

        /** This is a method for tagging the concept node
         *   using the semantic mean algorithm.
         *
         *   Note the structure of the ontology.  The
         *   most specific level is 0 and increases
         *   in abstraction until the maximum level
         *   is reached.  The maximum level is the
         *   most abstract.
         *
         *   This method assumes that the ontology has
         *   already been parsed and loaded.
         *
         *   @return node representing the semantic mean
         */
        public Node tagMean() {
                boolean verbose = false;
                int i, j, loopvar;
                float constant = 1.618f; // golden ratio
                //float constant = .17647;
                //float constant = 0.0;

                int[] sizes = new int[ontology.depth];   // make level sizes local
                for(i = 0; i < ontology.depth; i++) // populate level sizes
                sizes[i] = ontology.levelElementNum[i];

                // anticoverage variables
                float[] anticoverage = new float[ontology.depth];
                float[][] coverage = new float[ontology.depth][ontology.MAX_WIDTH];

                /*********************************************************/
                // compute coverage

                for(i = 0; i < sizes[0]; i++) // coverage at the specific level = hits
                coverage[0][i] = (ontology.level[0][i]).hits;

                if(verbose) {
                        System.out.println( "\n\nComputing coverage...");
                        System.out.print( "\n0: ");
                        for(i = 0; i < sizes[0]; i++)
                                System.out.print(coverage[0][i] + " ");
                        System.out.flush();
                }

                for(loopvar = 0; loopvar < ontology.depth - 1; loopvar++) { // iterate through levels
                        for(i = 0; i < sizes[loopvar+1]; i++) coverage[loopvar+1][i] = 0; // initialize to zero
                        for(i = 0; i < sizes[loopvar]; i++) { // iterate through parent nodes
                                for(j = 0; j < (ontology.level[loopvar][i]).childNum; j++) { // iterate through children of parent level
                                        coverage[loopvar+1][(ontology.level[loopvar][i]).getChild(j)] += coverage[loopvar][i]*(ontology.level[loopvar][i]).getWeight(j); // coverage = the sum of the parents coverages
                                } // end coverage loop
                        } // end parent node loop
                        for(i = 0; i < sizes[loopvar+1]; i++) // add in the next level hits
                        coverage[loopvar+1][i] += (ontology.level[loopvar+1][i]).hits;

                        // test output
                        if(verbose) {
                                System.out.print((loopvar+1) + ": ");
                                for(i = 0; i < sizes[loopvar+1]; i++)
                                        System.out.print(coverage[loopvar+1][i] + " ");
                                System.out.flush();
                        } // end test output
                } // end level loop

                /*********************************************************/
                // compute anticoverage

                int[] maxIndex = new int[ontology.depth]; // the index of the maximum for each level
                float maxValue; // max value attained
                float[][] cover = new float[ontology.depth][ontology.MAX_WIDTH];

                maxIndex[ontology.depth - 1] = 0;
                maxValue = coverage[ontology.depth - 1][0];
                for(i = 1; i < sizes[ontology.depth - 1]; i++) { // maximize the general level coverage
                        if(coverage[ontology.depth - 1][i] > maxValue) {
                                maxValue = coverage[ontology.depth - 1][i];
                                maxIndex[ontology.depth - 1] = i;
                                //maxIndexNum++;
                        }
                }

                if(verbose)
                        System.out.println("\nComputing anticoverage...\n");

                for(loopvar = ontology.depth - 2; loopvar >= 0; loopvar--) { // iterate through levels
                        // determine cover[loopvar]
                        for(i = 0; i < sizes[loopvar]; i++) { // iterate through level
                                cover[loopvar][i] = 0;
                                for(j = 0; j < (ontology.level[loopvar][i]).childNum; j++) { // iterate through children of each level loopvar node
                                        if((ontology.level[loopvar][i]).getChild(j) == maxIndex[loopvar + 1]) // if child has largest coverage of children level
                                                cover[loopvar][i] = coverage[loopvar][i]*(ontology.level[loopvar][i]).getWeight(j); // propagate coverage
                                }
                        }
                        // maximize cover[loopvar]
                        maxIndex[loopvar] = 0;
                        maxValue = cover[loopvar][0];
                        for(i = 1; i < sizes[loopvar]; i++) {
                                if(cover[loopvar][i] > maxValue) {
                                        maxValue = cover[loopvar][i];
                                        maxIndex[loopvar] = i;
                                }
                        }
                        anticoverage[loopvar] = coverage[loopvar + 1][maxIndex[loopvar + 1]] - coverage[loopvar][maxIndex[loopvar]];

                        // test output
                        if(verbose) {
                                System.out.print( loopvar + ": ");
                                for(i = 0; i < sizes[loopvar]; i++)
                                        System.out.print(cover[loopvar][i] + " ");
                                System.out.println("\tAnticoverage: " + anticoverage[loopvar]);
                        } // ent test output
                }

                if(verbose) {
                        System.out.println("\nMax Cover Index:");
                        for(i = 0; i < ontology.depth; i++)
                                System.out.print( maxIndex[i] + " ");
                        System.out.flush();
                }

                /***************************************************************/
                // compute distances

                float[] distance = new float[ontology.depth];

                // distance for abstract level = sum of all coverages
                distance[ontology.depth-1] = 0.0f;
                for(loopvar = ontology.depth - 2; loopvar >= 0; loopvar--) {
                        for(i = 0; i < sizes[loopvar]; i++)
                                distance[ontology.depth-1] += coverage[loopvar][i];
                }

                if(verbose)
                        System.out.println ("\nDistances: ");
                for(loopvar = ontology.depth - 1; loopvar > 0; loopvar--) {
                        if(verbose)
                                System.out.println( loopvar + ": " + distance[loopvar]);
                        distance[loopvar - 1] = distance[loopvar] - coverage[loopvar - 1][maxIndex[loopvar - 1]] + constant*anticoverage[loopvar - 1];
                }
                if(verbose)
                        System.out.println( "0: " + distance[0]);

                // minimize distance
                int minDistIndex = 0;
                float minDist = distance[0];
                for(i = 1; i < ontology.depth; i++) {
                        if(distance[i] <= minDist) {
                                minDist = distance[i];
                                minDistIndex = i;
                        }
                }
                return ontology.level[minDistIndex][maxIndex[minDistIndex]];
        }

        /** This is a method for tagging the semantic
         *  mode without taking into account inheritance.
         *
         *  This method assumes that the ontology has
         *  already been parsed and loaded.
         *
         * @return the node representing the semantic mode
         */
        public Node tagModeSI() {
                int i, j; // loop counters
                int maxHits = 0; // number of hits
                int maxLevel = 0; // level of mode
                int maxNode = 0; // node of mode
                int levels = ontology.depth; // how many levels

                for(i = 0; i < levels; i++) { // iterate through levels
                        for(j = 0; j < ontology.levelElementNum[i]; j++) { // iterate through nodes in level
                                if((ontology.level[i][j]).hits > maxHits) { // if > max, replace max
                                        maxHits = (ontology.level[i][j]).hits;
                                        maxLevel = i;
                                        maxNode = j;
                                }
                        }
                }
                return ontology.level[maxLevel][maxNode];
        }
}

