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
import java.util.*;
import javax.swing.*;
import javax.swing.event.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.util.*;

public class MaximumCardinalityWidget extends AbstractSlotWidget {
    private JCheckBox _isMultipleComponent;
    private IntegerField _atMostComponent;

    private ActionListener _buttonListener = new ActionListener() {
        public void actionPerformed(ActionEvent event) {
            _atMostComponent.removeActionListener(_textFieldListener);
            if (_isMultipleComponent.isSelected()) {
                _atMostComponent.clearValue();
            } else {
                _atMostComponent.setValue(1);
            }
            _atMostComponent.addActionListener(_textFieldListener);
            valueChanged();
        }
    };
    private ActionListener _textFieldListener = new ActionListener() {
        public void actionPerformed(ActionEvent event) {
            Integer i = _atMostComponent.getValue();
            boolean multiple = i != null && i.intValue() > 1;
            _isMultipleComponent.removeActionListener(_buttonListener);
            _isMultipleComponent.setSelected(multiple);
            _isMultipleComponent.addActionListener(_buttonListener);
            valueChanged();
        }
    };

    private Component createAtMostComponent() {
        _atMostComponent = new IntegerField("at most");
        _atMostComponent.addActionListener(_textFieldListener);
        JComponent c = new JPanel(new FlowLayout(FlowLayout.RIGHT, 0, 0));
        c.add(_atMostComponent);
        return c;
    }

    private JComponent createIsMultipleComponent() {
        _isMultipleComponent = ComponentFactory.createCheckBox("multiple");
        _isMultipleComponent.addActionListener(_buttonListener);
        return _isMultipleComponent;
    }

    public Collection getValues() {
        Integer max = _atMostComponent.getValue();
        return CollectionUtilities.createCollection(max);
    }

    public void initialize() {
        JPanel panel = new JPanel(new GridLayout(1, 2));
        panel.add(createIsMultipleComponent());
        panel.add(createAtMostComponent());
        JPanel p = new JPanel(new BorderLayout());
        p.add(panel, BorderLayout.NORTH);
        add(p);
    }

    public static boolean isSuitable(Cls cls, Slot slot, Facet facet) {
        return slot.getName().equals(Model.Slot.MAXIMUM_CARDINALITY);
    }

    public void setEditable(boolean editable) {
        _isMultipleComponent.setEnabled(editable);
        _atMostComponent.setEnabled(editable);
    }

    public void setValues(Collection values) {
        Integer i = (Integer) getFirstItem(values);
        boolean isMultiple = i == null || i.intValue() > 1;
        _isMultipleComponent.setSelected(isMultiple);
        if (i == null) {
            _atMostComponent.clearValue();
        } else {
            _atMostComponent.setValue(i.intValue());
        }
    }
}
