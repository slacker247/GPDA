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
import javax.swing.table.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.ui.*;
import edu.stanford.smi.protege.util.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class FormWidgetConfigurationWidgetsTab extends AbstractValidatableComponent {
    private FormWidget _formWidget;
    private JTable _table;

    public FormWidgetConfigurationWidgetsTab(FormWidget widget) {
        _formWidget = widget;
        setLayout(new BorderLayout());
        add(createTableComponent());
    }

    private JComponent createTableComponent() {
        _table = ComponentFactory.createTable(null);
        _table.setModel(createTableModel());
        ComponentUtilities.addColumn(_table, FrameRenderer.createInstance());
        ComponentUtilities.addColumn(_table, new WidgetClassNameRenderer(), new WidgetDescriptorEditor(_formWidget));
        return ComponentFactory.createScrollPane(_table);
    }

    private TableModel createTableModel() {
        PropertyList propertyList = _formWidget.getPropertyList();
        DefaultTableModel model = new DefaultTableModel();
        model.addColumn("Slot");
        model.addColumn("Widget");
        Iterator i = _formWidget.getCls().getTemplateSlots().iterator();
        while (i.hasNext()) {
            Slot slot = (Slot) i.next();
            WidgetDescriptor d = propertyList.getWidgetDescriptor(slot.getName());
            Assert.assertNotNull("widget descriptor for " + slot, d);
            String widgetClassName = (d == null) ? (String) null : d.getWidgetClassName();
            if (widgetClassName == null) {
                widgetClassName = WidgetClassNameRenderer.NONE;
            }
            model.addRow(new Object[] { slot, widgetClassName });
        }
        return model;
    }

    public void saveContents() {
        int nRows = _table.getRowCount();
        TableModel model = _table.getModel();
        for (int row = 0; row < nRows; ++row) {
            Slot slot = (Slot) model.getValueAt(row, 0);
            String widgetClassName = (String) model.getValueAt(row, 1);
            if (widgetClassName.equals(WidgetClassNameRenderer.NONE)) {
                widgetClassName = null;
            }
            _formWidget.replaceWidget(slot, widgetClassName);
        }
    }

    public boolean validateContents() {
        return true;
    }
}
