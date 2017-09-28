/*
 *  Node.java                1-16-03
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
 *  This is an abstraction of a node in an ontology.
 *  It has a name and a list of children with link
 *  weights.
 *
 *  @author Andrew Trimble
 */
public class Node {
        /** The maximum number of parents allowed */
        private int MAX_RELATIVES = 12;

        /** The number of children this node has */
        public int childNum;

        /** The name of this node */
        public String name; 
        
        /** An array of child indeces */
        public int[] childIndex = new int[MAX_RELATIVES];
        
        /** An array containing the weights of the links with the children */
        public float[] childWeight = new float[MAX_RELATIVES];

        /** The number of hits.
         *  Hits are defined as a keyword match in the ontology.
         */
        public int hits;
  
        /** A constructor */
        public Node() {childNum = 0; hits = 0;} // initialize the counters
  
        /** Set the name of the node
         *
         *  @param value the name
         */
        public void setName(String name) {
                this.name = name;
        }
    
        /** Add a child to the child list
         *
         *  @param nodeIndex the index of the child
         *  @param weight the weight of the link
         */
        public void addChild(int nodeIndex, float weight) {
                childIndex[childNum] = nodeIndex;
                childWeight[childNum] = weight;
                ++childNum;
        }

        /** Return the weight of a child's link
         *
         *  @param childIndex the index of the child
         *  @return  the weight
         */
        public float getWeight(int childIndex) {return childWeight[childIndex];}
    
        /** Return the name
         *
         *  @return the name of the node
         */
        public String getName() {return name;}

        /** Returns the location of the child specified by its
         *    index in the child list.
         *
         *    The child's location is given by its index in the
         *    level (row) below the current one.
         *
         *  @param index the index of the child requested
         *  @return the index of the child (index refers to the index in the next level)
         */
        public int getChild(int index) {return childIndex[index];}
}
