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
import javax.swing.table.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.ui.*;
import edu.stanford.smi.protege.util.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public abstract class AbstractTableWidget extends AbstractSlotWidget {
    private JTable _table;
    private LabeledComponent _labeledComponent;

    public AbstractTableWidget() {
        setPreferredColumns(2);
        setPreferredRows(4);
    }

    public void addButton(Action action) {
        addButton(action, true);
    }

    public void addButton(Action action, boolean defaultState) {
        addButtonConfiguration(action, defaultState);
        if (displayButton(action)) {
            _labeledComponent.addHeaderButton(action);
        }
    }

    public void addColumn(int width, TableCellRenderer renderer) {
        _table.addColumn(new TableColumn(_table.getColumnCount(), width, renderer, null));
    }

    protected void configureTable(JTable table) {
        table.setModel(createTableModel());
        table.getSelectionModel().addListSelectionListener(new ListSelectionListenerAdapter(this));
        table.addMouseListener(new TablePopupMenuMouseListener(table) {
            public JPopupMenu getPopupMenu() {
                return AbstractTableWidget.this.getPopupMenu();
            }
        });
    }

    public JComponent createMainComponent(Action action) {
        JScrollPane pane = ComponentFactory.createScrollPane(createTable(action));
        JViewport viewPort = pane.getViewport();
        viewPort.addMouseListener(
            new PopupMenuMouseListener(viewPort) {
                public JPopupMenu getPopupMenu() {
                    return AbstractTableWidget.this.getPopupMenu();
                }

                public void setSelection(JComponent c, int x, int y) {
                }
            }
        );
        _labeledComponent = new LabeledComponent(getLabel(), pane);
        return _labeledComponent;
    }

    protected JTable createTable(Action action) {
        _table = ComponentFactory.createSelectableTable(action);
        configureTable(_table);
        return _table;
    }

    public abstract TableModel createTableModel();

    public JPopupMenu getPopupMenu() {
        return null;
    }

    private int getRow(TableModel model, Object o) {
        int row = -1;
        int nRows = model.getRowCount();
        for (int i = 0; i < nRows; ++i) {
            if (model.getValueAt(i, 0).equals(o)) {
                row = i;
                break;
            }
        }
        return row;
    }

    public Collection getSelection() {
        return ComponentUtilities.getSelection(_table);
    }

    private Collection getSelections() {
        TableModel model = _table.getModel();
        Collection selections = new ArrayList();
        int[] rows = _table.getSelectedRows();
        if (rows != null) {
            for (int i = 0; i < rows.length; ++i) {
                selections.add(model.getValueAt(rows[i], 0));
            }
        }
        return selections;
    }

    public JTable getTable() {
        return _table;
    }

    public void initialize() {
        initialize(null);
    }

    public void initialize(Action action) {
        add(createMainComponent(action));
        reload();
    }

    public void reload() {
        Collection selections = getSelections();
        _table.setModel(createTableModel());
        setSelections(selections);
        _table.revalidate();
        _table.repaint();
        _table.setAutoResizeMode(JTable.AUTO_RESIZE_LAST_COLUMN);

    }

    public void setInstance(Instance instance) {
        super.setInstance(instance);
        reload();
    }

    private void setSelections(Collection c) {
        TableModel model = _table.getModel();
        _table.clearSelection();
        Iterator i = c.iterator();
        while (i.hasNext()) {
            int row = getRow(model, i.next());
            if (row != -1) {
                _table.addRowSelectionInterval(row, row);
            }
        }
    }
}
