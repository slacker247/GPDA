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


import java.awt.*;
import java.awt.event.*;
import java.beans.*;
import java.util.*;
import java.util.List;
import javax.swing.*;
import edu.stanford.smi.protege.action.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.ui.*;
import edu.stanford.smi.protege.util.*;

/**
 * Widget for setting the value type of a slot or a cls-slot pair
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class ValueTypeWidget extends AbstractSlotWidget {
    private ValuesComponent _valuesComponent;
    private JComboBox _typeComboBox;
    private ValueType _oldValue;

    private ActionListener _typeListener = new ActionListener() {
        public void actionPerformed(ActionEvent event) {
            ValueType newValue = (ValueType) _typeComboBox.getSelectedItem();
            if (newValue != _oldValue) {
                if (confirmChange()) {
                    updateWidget();
                    valueChanged();
                    _oldValue = newValue;
                } else {
                    setComboBoxValue(_oldValue);
                }
            }
        }

        private boolean confirmChange() {
            boolean result = true;
            if (hasInstanceWithValue()) {
                // Since we are in _typeComboBoxndler the popup window is still down.
                // It is ugly to leave it down when the dialog pops up.
                _typeComboBox.hidePopup();

                String text = "There may be instances which have values for this slot.\n"
                    + "Changing the type will cause these values to be removed.\n"
                    + "\n"
                    + "Do you really want to make this change?";
                int response = ModalDialog.showMessageDialog(ValueTypeWidget.this, text, ModalDialog.MODE_YES_NO);
                result = response == ModalDialog.OPTION_YES;
            }
            return result;
        }

        private boolean hasInstanceWithValue() {
            boolean result = false;
            Slot slot = (Slot) getInstance();
            return slot.hasValueAtSomeFrame();
        }
    };

    private ComboBoxModel createModel() {
        List c = new ArrayList(ValueType.getValues());
        Collections.sort(c, new ObjectComparator());
        return new DefaultComboBoxModel(c.toArray());
    }

    public Collection getSelection() {
        return _valuesComponent.getSelection();
    }

    public Collection getValues() {
        Collection values = _valuesComponent.getValues();
        if (values == null) {
            ValueType type = (ValueType) _typeComboBox.getSelectedItem();
            values = ValueTypeConstraint.getValues(type);
        }
        return values;
    }

    public void initialize() {
        _typeComboBox = ComponentFactory.createComboBox();
        _typeComboBox.setModel(createModel());
        setLayout(new BorderLayout(10, 10));
        add(new LabeledComponent(getLabel(), _typeComboBox), BorderLayout.NORTH);
        _typeComboBox.addActionListener(_typeListener);
        setValuesComponent(new NullValuesComponent());
    }

    public static boolean isSuitable(Cls cls, Slot slot, Facet facet) {
        return slot.getName().equals(Model.Slot.VALUE_TYPE);
    }

    private void setComboBoxValue(ValueType type) {
        _typeComboBox.removeActionListener(_typeListener);
        _typeComboBox.setSelectedItem(type);
        updateWidget();
        _typeComboBox.addActionListener(_typeListener);
        _oldValue = type;
    }

    public void setEditable(boolean b) {
        _typeComboBox.setEnabled(b);
        _valuesComponent.setEditable(b);
    }

    public void setValues(Collection values) {
        // Log.enter(this, "setValues", values);
        if (values.isEmpty()) {
            Log.error("empty: " + getInstance() + " " + getSlot(), this, "setValues");
            setComboBoxValue(ValueType.ANY);
        } else {
            ValueType type = ValueTypeConstraint.getType(values);
            setComboBoxValue(type);
            _valuesComponent.setValues(values);
        }
    }

    private void setValuesComponent(ValuesComponent valueComponent) {
        if (_valuesComponent != null) {
            remove(_valuesComponent.getComponent());
            ComponentUtilities.dispose(_valuesComponent.getComponent());
        }
        _valuesComponent = valueComponent;
        _valuesComponent.setEditable(_typeComboBox.isEnabled());
        add(_valuesComponent.getComponent());
        revalidate();
    }

    private void updateWidget() {
        KnowledgeBase kb = getKnowledgeBase();
        ValuesComponent valuesComponent;
        ValueType type = (ValueType) _typeComboBox.getSelectedItem();
        if (type == ValueType.ANY) {
            valuesComponent = new NullValuesComponent();
        } else if (type == ValueType.BOOLEAN) {
            valuesComponent = new NullValuesComponent();
        } else if (type == ValueType.CLS) {
            valuesComponent = new ClsValuesComponent(getProject());
        } else if (type == ValueType.FLOAT) {
            valuesComponent = new NullValuesComponent();
        } else if (type == ValueType.INSTANCE) {
            valuesComponent = new InstanceValuesComponent(getProject());
        } else if (type == ValueType.INTEGER) {
            valuesComponent = new NullValuesComponent();
        } else if (type == ValueType.STRING) {
            valuesComponent = new NullValuesComponent();
        } else if (type == ValueType.SYMBOL) {
            valuesComponent = new SymbolValuesComponent();
        } else {
            valuesComponent = null;
            Assert.fail("bad type: " + type);
        }
        setValuesComponent(valuesComponent);
    }
}
