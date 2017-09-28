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

package edu.stanford.smi.protege.widget;

import java.util.*;
import javax.swing.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.widget.*;
import edu.stanford.smi.protege.util.*;

// /*
/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class OldMaximumCardinalityWidget extends ComboBoxWidget {
    private final static String SINGLE = "Single";
    private final static String MULTIPLE = "Multiple";

    public ComboBoxModel createModel() {
        DefaultComboBoxModel model = new DefaultComboBoxModel();
        model.addElement(SINGLE);
        model.addElement(MULTIPLE);
        return model;
    }

    public Collection getValues() {
        Collection values;
        String comboValue = (String) getComboBox().getSelectedItem();
        if (comboValue == SINGLE) {
            values = CollectionUtilities.createCollection(new Integer(1));
        } else {
            values = Collections.EMPTY_LIST;
        }
        return values;
    }

    public static boolean isSuitable(Cls cls, Slot slot, Facet facet) {
        return slot.getName().equals(Model.Slot.MAXIMUM_CARDINALITY);
    }

    public void setValues(Collection c) {
        String comboValue;
        Integer firstValue = (Integer) CollectionUtilities.getFirstItem(c);
        if (firstValue == null) {
            comboValue = MULTIPLE;
        } else {
            comboValue = (firstValue.intValue() > 1) ? MULTIPLE : SINGLE;
        }
        getComboBox().setSelectedItem(comboValue);
    }
}
