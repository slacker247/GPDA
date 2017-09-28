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
public class ComboBoxWidget extends AbstractSlotWidget {
    protected final static String NONE = "";
    private JComboBox _comboBox;
    private boolean _displayNullEntry = true;

    private ActionListener _listener	= new ActionListener() {
        public void actionPerformed(ActionEvent event) {
            comboBoxValueChanged();
        }
    };

    public void comboBoxValueChanged() {
        valueChanged();
    }

    public JComboBox createComboBox() {
        return ComponentFactory.createComboBox();
    }

    public ComboBoxModel createModel() {
        ComboBoxModel model;
        Slot slot = getSlot();
        if (slot == null) {
            Log.trace("No slot", this, "createModel");
            model = new DefaultComboBoxModel();
        } else {
            List values = new ArrayList();
            ValueType type = getCls().getTemplateSlotValueType(slot);
            if (type == ValueType.BOOLEAN) {
                values.add(Boolean.TRUE);
                values.add(Boolean.FALSE);
            } else if (type == ValueType.SYMBOL) {
                values.addAll(getCls().getTemplateSlotAllowedValues(slot));
            } else {
                Assert.fail("bad type");
            }
            if (_displayNullEntry) {
                values.add(0, NONE);
            }
            model = new DefaultComboBoxModel(values.toArray());
        }
        return model;
    }

    public JComboBox getComboBox() {
        return _comboBox;
    }

    public boolean getDisplayNullEntry() {
        return _displayNullEntry;
    }

    public Collection getValues() {
        Object value = _comboBox.getSelectedItem();
        if (value == NONE) {
            value = null;
        }
        return CollectionUtilities.createList(value);
    }

    public void initialize() {
        _comboBox = createComboBox();
        setComboBoxModel(createModel());
        _comboBox.addActionListener(_listener);
        add(new LabeledComponent(getLabel(), _comboBox));
        setPreferredColumns(1);
        setPreferredRows(1);
    }

    public static boolean isSuitable(Cls cls, Slot slot, Facet facet) {
        boolean isSuitable;
        if (cls == null || slot == null) {
            isSuitable = false;
        } else {
            ValueType type = cls.getTemplateSlotValueType(slot);
            boolean typeOK = type == ValueType.SYMBOL || type == ValueType.BOOLEAN;
            boolean isMultiple = cls.getTemplateSlotAllowsMultipleValues(slot);
            isSuitable = typeOK && !isMultiple;
        }
        return isSuitable;
    }

    public void setComboBoxModel(ComboBoxModel model) {
        _comboBox.removeActionListener(_listener);
        _comboBox.setModel(model);
        _comboBox.addActionListener(_listener);
    }

    protected void setComboBoxValue(String value) {
        _comboBox.removeActionListener(_listener);
        _comboBox.setSelectedItem(value);
        _comboBox.addActionListener(_listener);
    }

    public void setDisplayNullEntry(boolean b) {
        _displayNullEntry = b;
    }

    public void setEditable(boolean b) {
        _comboBox.setEnabled(b);
    }

    public void setLabel(String text) {
        super.setLabel(text);
        LabeledComponent c = (LabeledComponent) getComponent(0);
        c.setHeaderLabel(text);
    }

    public void setRenderer(ListCellRenderer renderer) {
        _comboBox.setRenderer(renderer);
    }

    public void setValues(Collection values) {
        Object value = CollectionUtilities.getFirstItem(values);
        _comboBox.setSelectedItem(value);
    }
}
