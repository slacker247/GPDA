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

public class MinimumCardinalityWidget extends AbstractSlotWidget {
    private JCheckBox _isRequiredComponent;
    private IntegerField _atLeastComponent;

    private ActionListener _buttonListener = new ActionListener() {
        public void actionPerformed(ActionEvent event) {
            _atLeastComponent.removeActionListener(_textFieldListener);
            if (_isRequiredComponent.isSelected()) {
                _atLeastComponent.setValue(1);
            } else {
                _atLeastComponent.clearValue();
            }
            _atLeastComponent.addActionListener(_textFieldListener);
            valueChanged();
        }
    };
    private ActionListener _textFieldListener = new ActionListener() {
        public void actionPerformed(ActionEvent event) {
            Integer i = _atLeastComponent.getValue();
            boolean optional = i == null || i.intValue() == 0;
            _isRequiredComponent.removeActionListener(_buttonListener);
            _isRequiredComponent.setSelected(!optional);
            _isRequiredComponent.addActionListener(_buttonListener);
            valueChanged();
        }
    };

    private Component createAtLeastComponent() {
        _atLeastComponent = new IntegerField("at least");
        _atLeastComponent.addActionListener(_textFieldListener);
        JComponent c = new JPanel(new FlowLayout(FlowLayout.RIGHT, 0, 0));
        c.add(_atLeastComponent);
        return c;
    }

    private JComponent createIsRequiredComponent() {
        _isRequiredComponent = ComponentFactory.createCheckBox("required");
        _isRequiredComponent.addActionListener(_buttonListener);
        return _isRequiredComponent;
    }

    public Collection getValues() {
        Integer min = _atLeastComponent.getValue();
        return CollectionUtilities.createCollection(min);
    }

    public void initialize() {
        JPanel panel = new JPanel(new GridLayout(1, 2));
        panel.add(createIsRequiredComponent());
        panel.add(createAtLeastComponent());
        add(new LabeledComponent(getLabel(), panel, false));
    }

    public static boolean isSuitable(Cls cls, Slot slot, Facet facet) {
        return slot.getName().equals(Model.Slot.MINIMUM_CARDINALITY);
    }

    public void setEditable(boolean editable) {
        if (isSlotAtCls()) {
            Slot slot = (Slot) getInstance();
            // cardinality single cannot be overridden
            // Yes it can. The minimum cardinality can be increased
            /*
            if (!slot.getAllowsMultipleValues()) {
                editable = false;
            }
            */
        }
        _isRequiredComponent.setEnabled(editable);
        _atLeastComponent.setEnabled(editable);
    }

    public void setValues(Collection values) {
        Integer i = (Integer) getFirstItem(values);
        boolean isRequired = i != null && i.intValue() > 0;
        _isRequiredComponent.setSelected(isRequired);
        if (isRequired) {
            _atLeastComponent.setValue(i.intValue());
        } else {
            _atLeastComponent.clearValue();
        }
    }
}
