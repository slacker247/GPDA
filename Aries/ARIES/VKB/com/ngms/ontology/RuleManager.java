/*
 *  RuleManager.java                1-16-03
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
import java.io.*;

/**
 * This is a class for managing a set of rules
 * used to determine if an answer in the template
 * maps to a hit in the ontology.
 *
 * @author Andrew Trimble
 */
public class RuleManager {
        /** The maximum number of rules this RuleManager is allowed to have */
        private int maxRules = 256;

        /** The name of the file to save the rules to */
        private String fileName;
        
        /** An array of rules */
        private Rule[] userRules = new Rule[maxRules];

        /** The number of rules this RuleManager has */
        public int ruleNum;
        
        /** A constructor */
        public RuleManager() {
                fileName = "./resources/.rules";
                ruleNum = 0;
                loadRules();
        }
        
        /** A method to clean up and save the rules */
        public void finalize() {
                /* uncomment the following line
                 * if interested in dynamically
                 * creating new rules
                 */
                //saveRules();
        }
    
        /** A method to load the rules from a file */
        private void loadRules() {
                ruleNum = 0; // zero userRules
                try {
                        File file = new File(fileName);
                        FileReader reader = new FileReader(file);
                        StreamTokenizer input = new StreamTokenizer(reader);
                        input.wordChars('!', '~');
                        
                        while(input.nextToken() != StreamTokenizer.TT_EOF) {
                                Rule newRule = new Rule();
                                newRule.level = (int)(input.nval);
                                input.nextToken();
                                newRule.entry = (int)(input.nval);
                                input.nextToken();
                                newRule.fieldValue = input.sval;
                                input.nextToken();
                                newRule.fieldName = input.sval;
                                
                                userRules[ruleNum] = newRule;
                                ruleNum++;
                        }
                        reader.close();

                        /* if(ruleNum == 1)
                                System.out.println("\nRead " + ruleNum + " rule.\n");
                        else
                                System.out.println("\nRead " + ruleNum + " rules.\n"); */
                } catch(Exception ex) {ex.printStackTrace();}
        }

        /** A method to save the rules to a file */
        private void saveRules() {
                int i;
                try {
                        FileWriter output = new FileWriter(new File(fileName));

                        for(i = 0; i < ruleNum; i++) {
                                output.write((new Integer(userRules[i].level)).toString());
                                output.write(" ");
                                output.write((new Integer(userRules[i].entry)).toString());
                                output.write(" ");
                                output.write(new String(userRules[i].fieldValue));
                                output.write(" ");
                                output.write(new String(userRules[i].fieldName));
                                output.write("\n");
                        }
                        output.close();
                        /* if( i == 1)
                                System.out.println("\nWrote " + i + " rule.\n");
                        else
                                System.out.println("\nWrote " + i + " rules.\n"); */
                } catch(Exception ex) {ex.printStackTrace();}
        }

        /** Get the rule at an index
         *
         *  @param index the index of the rule requested
         *  
         *  @return the rule at the index or nothing if the rule is not found
         */
        public Rule getRuleAt(int index) {
                if((index < ruleNum) && (index >= 0)) // make sure index is within range
                        return userRules[index];
                else
                        return null;
        }

        /** remove the specified rule
         *  a convenience method so as not to have to
         *  call getRuleIndex()
         *
         * @param levelValue the level number of the rule to delete
         * @param nameValue the level entry value of the rule to delete
         * @param value the field value of the rule to delete
         * @param name the field name of the rule to delete
         */
        public void removeRule(int levelValue, int nameValue, String value, String name) {
                removeRule(getRuleIndex(levelValue, nameValue, value, name));
        }

        /** remove the rule at an index
         *
         *  @param index the index of the specified rule
         */
        public void removeRule(int index) {
                if((index < ruleNum) && (index >= 0)) { // make sure index is within range
                        // shift subsequent userRules back
                        for(int i = index; i < ruleNum-1; i++)
                                userRules[i] = userRules[i+1];
                        --ruleNum;
                }
        }

        /** get the index of rule with "name" and "value"
         *  this method returns -1 if the rule is not found
         *
         *  @param value the field value of the requested rule
         *
         *  @return the index of the rule requested or -1 if the rule is not found
         */
        public int getRuleIndex(int levelValue, int nameValue, String value, String name) {
                for(int i = 0; i < ruleNum; i++) {
                        if((value.equalsIgnoreCase(userRules[i].fieldValue)) &&
                            (name.equalsIgnoreCase(userRules[i].fieldName)) &&
                             userRules[i].level == levelValue &&
                             userRules[i].entry == nameValue)
                        return i;
                }
                return -1; // if not found, return -1
        }

        /** add a rule with the included information
         *
         * @param levelValue the level at which to add the rule
         * @param nameValue the entry in the level at which to add the rule
         * @param value the field value for the rule being added
         * @param name the field name for the rule being added
         */
        public void addRule(int levelValue, int nameValue, String value, String name) {
                Rule newRule = new Rule(levelValue, nameValue, value, name);
                for(int i = 0; i < ruleNum; i++) {
                        if(newRule == userRules[i])
                                return;
                }
                userRules[ruleNum] = newRule;
                ruleNum++;
        }

        /** add a rule object
         *
         *  @param rule the new rule object
         */
        public void addRule(Rule rule) {
                for(int i = 0; i < ruleNum; i++) {
                        if(rule == userRules[i])
                                return;
                }
                userRules[ruleNum] = rule;
                ruleNum++;
        }

        /** A utility method to print the set of rules to standard output */
        public void printRules() {
                System.out.println("\n\nPrinting Rules...\n\n");
                System.out.println("i X Y Field/Name Pair\n---------------------\n");
                for(int i = 0; i < ruleNum; i++) {
                        System.out.print(i + " " + userRules[i].level + " " + userRules[i].entry + " ");
                        System.out.println(userRules[i].fieldName + " = " + userRules[i].fieldValue + "\n");
                }
        }
}

