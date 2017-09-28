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
import java.awt.event.*;
import java.util.*;
import javax.swing.*;
import javax.swing.table.*;
import edu.stanford.smi.protege.action.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.util.*;
import edu.stanford.smi.protege.widget.*;

class ConfigureTabsPanel extends AbstractValidatableComponent {

    private JTable _table;
    private Project _project;
    private boolean _dirty;

    private class MoveTabUp extends AbstractAction {
        public MoveTabUp() {
            super("Move selected tab up", Icons.getUpIcon());
        }

        public void actionPerformed(ActionEvent event) {
            int index = _table.getSelectedRow();
            if (index > 0) {
                getTabModel().moveRow(index, index, index - 1);
                int n = index - 1;
                _table.getSelectionModel().setSelectionInterval(n, n);
                _dirty = true;
            }
        }
    }

    private class MoveTabDown extends AbstractAction {
        public MoveTabDown() {
            super("Move selected tab down", Icons.getDownIcon());
        }

        public void actionPerformed(ActionEvent event) {
            int index = _table.getSelectedRow();
            if (0 <= index && index < _table.getRowCount() - 1) {
                getTabModel().moveRow(index, index, index + 1);
                int n = index + 1;
                _table.getSelectionModel().setSelectionInterval(n, n);
                _dirty = true;
            }
        }
    }

    private class ClickListener extends MouseAdapter {
        public void mousePressed(MouseEvent event) {
            Point p = event.getPoint();
            int col = _table.columnAtPoint(p);
            if (col == 0) {
                int row = _table.rowAtPoint(p);
                Boolean b = (Boolean) getTabModel().getValueAt(row, 0);
                Boolean newValue = new Boolean(!b.booleanValue());
                getTabModel().setValueAt(newValue, row, 0);
                _dirty = true;
            }
        }
    }

    public ConfigureTabsPanel(Project project) {
        setLayout(new BorderLayout());
        _project = project;
        _table = ComponentFactory.createTable(getConfigureAction());
        _table.setModel(createTableModel());
        ComponentUtilities.addColumn(_table, new CheckBoxRenderer());
        _table.getColumnModel().getColumn(0).setMaxWidth(50);
        ComponentUtilities.addColumn(_table, new WidgetDescriptorRenderer());
        _table.addMouseListener(new ClickListener());
        JScrollPane pane = ComponentFactory.createScrollPane(_table);
        pane.setColumnHeaderView(_table.getTableHeader());
        pane.setBackground(_table.getBackground());
        LabeledComponent c = new LabeledComponent("Tabs", pane);
        c.addHeaderButton(new MoveTabUp());
        c.addHeaderButton(new MoveTabDown());
        add(c);
    }

    private TableModel createTableModel() {
        DefaultTableModel model = new DefaultTableModel();
        model.addColumn("Visible");
        model.addColumn("Tab Widget");

        Iterator i = _project.getTabWidgetDescriptors().iterator();
        while (i.hasNext()) {
            WidgetDescriptor d = (WidgetDescriptor) i.next();
            String name = d.getWidgetClassName();
            model.addRow(new Object[]{new Boolean(d.isVisible()), d});
        }
        return model;
    }

    private Action getConfigureAction() {
        return new AbstractAction("Configure", Icons.getBlankIcon()) {
            public void actionPerformed(ActionEvent event) {
                int row = _table.getSelectedRow();
                WidgetDescriptor d = (WidgetDescriptor) getTabModel().getValueAt(row, 1);
                if (d.isVisible()) {
                    TabWidget widget = ProjectManager.getProjectManager().getCurrentProjectView().getTab(d.getWidgetClassName());
                    widget.configure();
                }
            }
        };
    }

    private DefaultTableModel getTabModel() {
        return (DefaultTableModel) _table.getModel();
    }

    public void saveContents() {
        if (_dirty) {
            Collection tabWidgetDescriptors = new ArrayList();
            int index = 0;
            TableModel model = getTabModel();
            for (int row = 0; row < model.getRowCount(); ++row) {
                Boolean isVisible = (Boolean) model.getValueAt(row, 0);
                WidgetDescriptor descriptor = (WidgetDescriptor) model.getValueAt(row, 1);
                descriptor.setVisible(isVisible.booleanValue());
                tabWidgetDescriptors.add(descriptor);
            }
            _project.setTabWidgetDescriptorOrder(tabWidgetDescriptors);
        }
    }

    public boolean validateContents() {
        return true;
    }
}
