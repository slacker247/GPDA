/*
 *  Rule.java                1-16-03
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

/**
 * This is an abstraction of Rule associating a field value with a "hit"
 * in the ontology.
 *
 * @author Andrew Trimble
 */
public class Rule {
        /** The level in the ontology */
        public int level; 
        
        /** The index in the level */
        public int entry;
        
        /** The value associated with a "hit" */
        public String fieldValue; 
        
        /** The name of the field associated with a value */
        public String fieldName;

        /** A constructor */
        public Rule() { }

        /** A constructor
         *
         *  @param nameValue  the field name
         *  @param value  the field value
         *  @param hypValue the hypothesis
         */
        public Rule(int levelValue, int entryValue, String value, String name) {
                level = levelValue;
                entry = entryValue;
                fieldValue = value;
                fieldName = name;
        }

        /** A comparison method
         *
         * @param rule the rule to compare with
         * @return true if equal, false if not equal
         */
        public boolean equals(Rule rule) {
                // if all fields of operand are equal, return true
                if((level == rule.level) &&
                (entry == rule.entry) &&
                (fieldValue.equalsIgnoreCase(rule.fieldValue)) &&
                (fieldName.equalsIgnoreCase(rule.fieldName)))
                        return true;
                else
                        return false;
        }
}

