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

package edu.stanford.smi.protege.ui;


import java.awt.*;
import java.util.*;
import javax.swing.*;
import edu.stanford.smi.protege.util.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class PickSymbolPanel extends JComponent {
    private JComboBox _comboBox;

    public PickSymbolPanel(String label, String value, Collection allowedValues) {
        setLayout(new BorderLayout());
        _comboBox = ComponentFactory.createComboBox();
        _comboBox.setModel(new DefaultComboBoxModel(allowedValues.toArray()));
        if (value == null) {
            value = (String) CollectionUtilities.getFirstItem(allowedValues);
        }
        _comboBox.setSelectedItem(value);
        add(new LabeledComponent(label, _comboBox));
        setPreferredSize(new Dimension(300, 60));
    }

    public String getSelectedValue() {
        return (String) _comboBox.getSelectedItem();
    }
}
