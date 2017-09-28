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
import javax.swing.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.util.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class GeneralWidgetConfigurationPanel extends AbstractValidatableComponent {
    private JTextField _labelField;
    private SlotWidget _widget;

    public GeneralWidgetConfigurationPanel(SlotWidget widget) {
        _widget = widget;
        setLayout(new GridLayout(3, 1, 10, 10));
        add(createClsNameField(widget));
        add(createSlotNameField(widget));
        add(createLabelNameField(widget));
    }

    private JComponent createClsNameField(SlotWidget widget) {
        return createFrameField("Class", widget.getCls());
    }

    private JTextField createField(String text) {
        JTextField field = ComponentFactory.createTextField();
        field.setText(text);
        return field;
    }

    private JComponent createFrameField(String label, Instance frame) {
        String text = (frame == null) ? "" : frame.getName();
        JTextField field = createField(text);
        field.setEditable(false);
        return new LabeledComponent(label, field);
    }

    private JComponent createLabelNameField(Widget widget) {
        _labelField = createField(widget.getLabel());
        return new LabeledComponent("Label", _labelField);
    }

    private JComponent createSlotNameField(SlotWidget widget) {
        return createFrameField("Slot", widget.getSlot());
    }

    public String getLabel() {
        return _labelField.getText();
    }

    public void saveContents() {
        _widget.setLabel(_labelField.getText());
    }

    public boolean validateContents() {
        return true;
    }
}
