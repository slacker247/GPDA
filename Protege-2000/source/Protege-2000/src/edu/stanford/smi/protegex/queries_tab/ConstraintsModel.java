/*
 * The contents of this file are subject to the Mozilla Public License
 * Version 1.1 (the "License");  you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * The Original Code is Protege-2000.
 *
 * The Initial Developer of the Original Code is Stanford University. Portions
 * created by Stanford University are Copyright (C) 2001.  All Rights Reserved.
 *
 * Protege-2000 was developed by Stanford Medical Informatics
 * (http://www.smi.stanford.edu) at the Stanford University School of Medicine
 * with support from the National Library of Medicine, the National Science
 * Foundation, and the Defense Advanced Research Projects Agency.  Current
 * information about Protege can be obtained at http://protege.stanford.edu
 *
 * Contributor(s):
 */

package edu.stanford.smi.protegex.queries_tab;

import javax.swing.*;
import java.awt.*;

public class ConstraintsModel extends DefaultComboBoxModel{
   private static final String[] types = {"null","BOOLEAN","CLS", "FLOAT", "INSTANCE", "INTEGER", "STRING", "SYMBOL"};
   private static final String[] anyConstraints={"Any Constraints"};
   private static final String[] booleanConstraints = {"is" };
   private static final String[] clsConstraints = {"contains", "does not contain"};
   private static final String[] floatConstraints = {"is", "is greater than", "is less than"};
   private static final String[] instanceConstraints = {"contains", "does not contain"};
   private static final String[] integerConstraints = {"is", "is greater than", "is less than"};
   private static final String[] stringConstraints = {"contains", "does not contain", "is", "is not", "begins with", "ends with"};
   private static final String[] symbolConstraints = {"is", "is not"};
   private static final String[] nullConstraints = {""};

   private String type;

    public ConstraintsModel() {
        super();
        initialize();
    }

    private void addArray(String[] constraints) {
        for (int i = 0; i < constraints.length; i++) {
            addElement(constraints[i]);
        }
    }

    private void addConstraints(int index) {
        switch (index) {
            case 0 :
                //addArray(anyConstraints);
                addArray(nullConstraints);
                break;
            case 1 :
                addArray(booleanConstraints);
                break;
            case 2 :
                addArray(clsConstraints);
                break;
            case 3 :
                addArray(floatConstraints);
                break;
            case 4 :
                addArray(instanceConstraints);
                break;
            case 5 :
                addArray(integerConstraints);
                break;
            case 6 :
                addArray(stringConstraints);
                break;
            case 7 :
                addArray(symbolConstraints);
                break;
            default :
                //addArray(anyConstraints);
                addArray(nullConstraints);
                break;
        }
    }

    public static String[] getBooleanConstraints() {
        return booleanConstraints;
    }

    public static String[] getClsConstraints() {
        return clsConstraints;
    }

    public static String[] getFloatConstraints() {
        return floatConstraints;
    }

    public static String[] getInstanceConstraints() {
        return instanceConstraints;
    }

    public static String[] getIntegerConstraints() {
        return integerConstraints;
    }

    public static String[] getStringConstraints() {
        return stringConstraints;
    }

    public static String[] getSymbolConstraints() {
        return symbolConstraints;
    }

    public String getType() {
        return type;
    }

    public void initialize() {
        // setup the default type
        type = "INSTANCE";
        setUpComboBox(type);
    }

    public void setUpComboBox(String type) {
        this.type = type;
        if (getSize() > 0)
            removeAllElements();

        for (int i = 0; i < types.length; i++) {
            if (types[i].toLowerCase().equals(type.toLowerCase())) {
                addConstraints(i);
                break;
            }
        }
    }
}
