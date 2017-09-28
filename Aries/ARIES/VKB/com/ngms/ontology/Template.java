/*
 *  Template.java                1-16-03
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

import java.util.Vector;

/**
 * This is an abstraction of a reporters questions
 * template object. The template object is not to
 * be used directly by the driving program.  The
 * template is used by the template manager object.
 * The template manager object provides a standard
 * interface to all template objects.  This allows
 * for changes to easily be made in the form of the
 * questionairre.
 *
 * @author Andrew Trimble
 * @deprecated TemplateManager contains all of the template information
 */
public class Template {
        /** The labels of the template fields */
        private Vector labels;
        
        /** The values of the template fields */
        private Vector values;
        
        /** The number of fields in a template */
        public int elements;

        /** A constructor */
        public Template() {
                elements = 0;
        }
        
        /** Add a label to the template
         *
         *  @param name the name of the label
         */
        public void addLabel(String name) {
                labels.add(name);
                labels.trimToSize();
                elements++;
        }
        
        /** Get the label of a field
         *
         *  @param index the index of the desired label
         *  @return the label
         */
        public String getLabel(int index) {
                return (String)(labels.elementAt(index));
        }

        /** Add a value to the template
         *
         *  @param value the value to add
         */
        public void addValue(String value) {
                values.add(value);
                values.trimToSize();
        }
        
        /** Get a value in the template
         *
         *  @param index the index of the desired field
         *  @return the field value
         */
        public String getValue(int index) {
                return (String)(values.elementAt(index));
        }
}

