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


import java.awt.event.*;
import java.util.*;
import javax.swing.*;
import javax.swing.event.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.util.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class CheckBoxWidget extends AbstractSlotWidget {
    private JCheckBox _checkBox;

    public JCheckBox createCheckBox() {
        JCheckBox checkBox = ComponentFactory.createCheckBox();
        checkBox.setForeground(UIManager.getColor("Label.foreground"));
        return checkBox;
    }

    protected JCheckBox getCheckBox() {
        return _checkBox;
    }

    public Collection getValues() {
        boolean checked = _checkBox.isSelected();
        return CollectionUtilities.createCollection(checked ? Boolean.TRUE : Boolean.FALSE);
    }

    public void initialize() {
        _checkBox = createCheckBox();
        _checkBox.addActionListener(
            new ActionListener() {
                public void actionPerformed(ActionEvent event) {
                    valueChanged();
                }
            }
        );
        _checkBox.setText(getLabel());
        add(_checkBox);
        setPreferredColumns(1);
        setPreferredRows(1);
    }

    public static boolean isSuitable(Cls cls, Slot slot, Facet facet) {
        boolean isSuitable;
        if (cls == null || slot == null) {
            isSuitable = false;
        } else {
            ValueType type = cls.getTemplateSlotValueType(slot);
            boolean typeOK = type == ValueType.BOOLEAN;
            boolean isMultiple = cls.getTemplateSlotAllowsMultipleValues(slot);
            isSuitable = typeOK && !isMultiple;
        }
        return isSuitable;
    }

    public void setEditable(boolean b) {
        _checkBox.setEnabled(b);
    }

    public void setLabel(String text) {
        super.setLabel(text);
        _checkBox.setText(text);
    }

    public void setValues(Collection values) {
        Boolean b = (Boolean) CollectionUtilities.getFirstItem(values);
        boolean checked = (b == null) ? false : b.booleanValue();
        _checkBox.setSelected(checked);
    }
}
