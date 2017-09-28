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
import java.util.*;
import javax.swing.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.ui.*;
import edu.stanford.smi.protege.util.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class FormWidgetConfigurationLayoutTab extends AbstractValidatableComponent {
    private final static String ALL = "<all>";
    private FormWidget _formWidget;
    private JComboBox _horizontalStretcherComponent;
    private JComboBox _verticalStretcherComponent;

    public FormWidgetConfigurationLayoutTab(FormWidget widget) {
        _formWidget = widget;
        setLayout(new BorderLayout());
        JComponent grid = new JPanel(new GridLayout(2, 1, 10, 10));
        grid.add(createVerticalStretcherComponent());
        grid.add(createHorizontalStretcherComponent());
        add(grid, BorderLayout.NORTH);
    }

    private JComponent createHorizontalStretcherComponent() {
        _horizontalStretcherComponent = ComponentFactory.createComboBox();
        _horizontalStretcherComponent.setRenderer(FrameRenderer.createInstance());
        ArrayList items = new ArrayList(_formWidget.getCls().getTemplateSlots());
        items.add(0, ALL);
        DefaultComboBoxModel model = new DefaultComboBoxModel(items.toArray());
        _horizontalStretcherComponent.setModel(model);
        setSelection(_horizontalStretcherComponent, _formWidget.getHorizontalStretcher());
        return new LabeledComponent("Fill Horizontal Space With:", _horizontalStretcherComponent);
    }

    private JComponent createVerticalStretcherComponent() {
        _verticalStretcherComponent = ComponentFactory.createComboBox();
        _verticalStretcherComponent.setRenderer(FrameRenderer.createInstance());
        ArrayList items = new ArrayList(_formWidget.getCls().getTemplateSlots());
        items.add(0, ALL);
        DefaultComboBoxModel model = new DefaultComboBoxModel(items.toArray());
        _verticalStretcherComponent.setModel(model);
        setSelection(_verticalStretcherComponent, _formWidget.getVerticalStretcher());
        return new LabeledComponent("Fill Vertical Space With:", _verticalStretcherComponent);
    }

    private Slot getSelection(JComboBox box) {
        Object o = box.getSelectedItem();
        if (o == ALL) {
            o = null;
        }
        return (Slot) o;
    }

    public void saveContents() {
        _formWidget.setHorizontalStretcher(getSelection(_horizontalStretcherComponent));
        _formWidget.setVerticalStretcher(getSelection(_verticalStretcherComponent));
    }

    private void setSelection(JComboBox box, Slot slot) {
        Object o = slot;
        if (o == null) {
            o = ALL;
        }
        box.setSelectedItem(o);
    }

    public boolean validateContents() {
        return true;
    }
}
