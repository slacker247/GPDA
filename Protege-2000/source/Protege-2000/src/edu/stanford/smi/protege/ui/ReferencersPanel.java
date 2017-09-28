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

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class ReferencersPanel extends SelectableContainer {
    private SelectableTable _table;
    // private Project _project;

    public ReferencersPanel(Instance instance) {
        createTable(instance);
        setSelectable(_table);
        setPreferredSize(new Dimension(700, 300));
    }

    /**
     * @deprecated Use constructor without Project argument
     */
    public ReferencersPanel(Project project, Instance instance) {
        this(instance);
    }

    private void addColumn(int width, TableCellRenderer renderer) {
        _table.addColumn(new TableColumn(_table.getColumnCount(), width, renderer, null));
    }

    private void createColumns() {
        addColumn(250, FrameRenderer.createInstance());
        addColumn(250, FrameRenderer.createInstance());

        DefaultRenderer facetsRenderer =
            new DefaultRenderer() {
                public void load(Object o) {
                    Reference ref = (Reference) o;
                    Facet facet = ref.getFacet();
                    if (facet != null && facet.getName().equals(Model.Facet.VALUE_TYPE)) {
                        setMainIcon(Icons.getFacetIcon());
                        Cls cls = (Cls) ref.getFrame();
                        Slot slot = (Slot) ref.getSlot();
                        ValueType type = cls.getTemplateSlotValueType(slot);
                        if (type == ValueType.INSTANCE) {
                            setMainText("allowed-classes");
                        } else if (type == ValueType.CLS) {
                            setMainText("allowed-parents");
                        } else {
                        }
                    }
                }
            }
        ;
        addColumn(125, facetsRenderer);
    }

    private void createTable(Instance instance) {
        Action viewAction = createViewAction();
        _table = ComponentFactory.createSelectableTable(viewAction);
        setLayout(new BorderLayout());
        JComponent t = ComponentFactory.createScrollPane(_table);
        String text = "References to " + instance.getName();
        LabeledComponent c = new LabeledComponent(text, t);
        c.addHeaderButton(viewAction);
        c.addHeaderButton(new ReferencersAction(this));
        add(c);
        _table.setModel(createTableModel(instance));
        createColumns();
    }

    private TableModel createTableModel(Instance instance) {
        DefaultTableModel model =
            new DefaultTableModel() {
                public boolean isCellEditable(int row, int col) {
                    return false;
                }
            }
        ;
        model.addColumn("Frame");
        model.addColumn("Slot");
        model.addColumn("Facet");
        // model.addColumn("Kind");
        KnowledgeBase kb = instance.getKnowledgeBase();
        ArrayList references = new ArrayList(kb.getReferences(instance, 1000));
        Collections.sort(references, new ReferenceComparator());
        Iterator i = references.iterator();
        while (i.hasNext()) {
            Reference ref = (Reference) i.next();
            model.addRow(new Object[]{ref.getFrame(), ref.getSlot(), ref});
        }
        return model;
    }

    private Action createViewAction() {
        return new AbstractAction("View Reference", Icons.getViewIcon()) {
            public void actionPerformed(ActionEvent event) {
                Iterator i = getSelectedReferences().iterator();
                while (i.hasNext()) {
                    Reference ref = (Reference) i.next();
                    Project project = ref.getFrame().getProject();
                    if (ref.isTemplate()) {
                        project.show((Cls) ref.getFrame(), ref.getSlot());
                    } else {
                        project.show((Instance) ref.getFrame());
                    }
                }
            }
        };
    }

    public int getReferencerCount() {
        return _table.getRowCount();
    }

    private Collection getSelectedReferences() {
        Collection references = new ArrayList();
        TableModel model = _table.getModel();
        int[] rows = _table.getSelectedRows();
        for (int i = 0; i < rows.length; ++i) {
            references.add(model.getValueAt(rows[i], 2));
        }
        return references;
    }
}
