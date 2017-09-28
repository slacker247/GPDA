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

public class MinimumCardinalityWidget_OLD extends AbstractSlotWidget {
    private JCheckBox _isRequiredComponent;
    private IntegerField _atLeastComponent;

    private ActionListener _buttonListener = new ActionListener() {
        public void actionPerformed(ActionEvent event) {
            if (_isRequiredComponent.isSelected()) {
                _atLeastComponent.setValue(1);
            } else {
                _atLeastComponent.clearValue();
            }
            valueChanged();
        }
    };
    private ActionListener _textFieldListener = new ActionListener() {
        public void actionPerformed(ActionEvent event) {
            Integer i = _atLeastComponent.getValue();
            boolean optional = i == null || i.intValue() == 0;
            _isRequiredComponent.setSelected(!optional);
        }
    };

    private JComponent createAtLeastAtMostPanel() {
        JPanel panel = new JPanel();
        panel.setLayout(new FlowLayout(FlowLayout.LEFT, 0, 0));
        JRadioButton button = new JRadioButton("at least");
        panel.add(button);
        JTextField f = new JTextField();
        f.setColumns(3);
        panel.add(f);
        JLabel label = new JLabel(", at most");
        label.setFont(button.getFont());
        panel.add(label);
        JTextField f2 = new JTextField();
        f2.setColumns(3);
        panel.add(f2);
        return panel;
    }

    private IntegerField createAtLeastComponent() {
        _atLeastComponent = new IntegerField("at least");
        _atLeastComponent.addActionListener(_textFieldListener);
        return _atLeastComponent;
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
        panel.add(new JRadioButton("single"));
        panel.add(new JRadioButton("multiple"));
        JPanel mainPanel = new JPanel(new GridLayout(2, 1));
        mainPanel.add(panel);
        mainPanel.add(createAtLeastAtMostPanel());
        add(new LabeledComponent(getLabel(), mainPanel, true));
    }

    public static boolean isSuitable(Cls cls, Slot slot, Facet facet) {
        return slot.getName().equals(Model.Slot.MINIMUM_CARDINALITY);
    }

    public void setValues(Collection values) {
        Integer i = (Integer) getFirstItem(values);
        if (i == null) {
            _isRequiredComponent.setSelected(false);
        } else {
            int n = i.intValue();
            if (n == 0) {
                _isRequiredComponent.setSelected(false);
            } else {
                _isRequiredComponent.setSelected(true);
                _atLeastComponent.setValue(n);
            }
        }
    }
}
