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
import java.util.*;
import javax.swing.*;
import javax.swing.table.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.model.Frame;
import edu.stanford.smi.protege.util.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class MetricsPanel extends JComponent {
    private Statistics _statistics;

    public MetricsPanel(Project p) {
        _statistics = new Statistics(p.getKnowledgeBase());
        setLayout(new BorderLayout(20, 20));
        add(createSummaryPanel(), BorderLayout.NORTH);
        add(createDetailsPanel(), BorderLayout.CENTER);
    }

    private Object[] createChildSummary() {
        return new Object[]{
                "Children",
                newDouble(_statistics.meanChildren),
                newDouble(_statistics.sdChildren),
                new Integer(_statistics.maxChildren),
                };
    }

    private JComponent createClsesPanel() {
        DefaultTableModel model = new DefaultTableModel();
        model.addColumn("");
        model.addColumn("Mean");
        model.addColumn("Std. Dev.");
        model.addColumn("Max");
        model.addRow(createParentSummary());
        model.addRow(createChildSummary());
        model.addRow(createRelationsSummary());
        model.addRow(createDirectSlotsSummary());
        model.addRow(createSlotsSummary());
        model.addRow(createInstancesSummary());
        return createTable(model, "Classes");
    }

    private Object[] createClsSummary() {
        return new Object[]{
                "Classes",
                new Integer(_statistics.nSystemClses),
                new Integer(_statistics.nIncludedClses),
                new Integer(_statistics.nDirectClses),
                new Integer(_statistics.nClses)
                };
    }

    private JComponent createDetailsPanel() {
        JComponent c = new JPanel();
        c.setLayout(new GridLayout(2, 2, 20, 20));
        c.add(createClsesPanel());
        c.add(createInstancesPanel());
        c.add(createSlotsPanel());
        c.add(createFacetsPanel());
        return c;
    }

    private Object[] createDirectSlotsSummary() {
        return new Object[]{
                "Direct Slots",
                newDouble(_statistics.meanDirectSlots),
                newDouble(_statistics.sdDirectSlots),
                new Integer(_statistics.maxDirectSlots),
                };
    }

    private Object[] createFacetDirectBindingsSummary() {
        return new Object[]{
                "Direct Bindings",
                newDouble(_statistics.meanDirectBindings),
                newDouble(_statistics.sdDirectBindings),
                new Integer(_statistics.maxDirectBindings),
                };
    }

    private JComponent createFacetsPanel() {
        DefaultTableModel model = new DefaultTableModel();
        model.addColumn("");
        model.addColumn("Mean");
        model.addColumn("Std. Dev.");
        model.addColumn("Max");
        model.addRow(createFacetDirectBindingsSummary());
        return createTable(model, "Facets");
    }

    private Object[] createFacetSummary() {
        return new Object[]{
                "Facets",
                new Integer(_statistics.nSystemFacets),
                new Integer(_statistics.nIncludedFacets),
                new Integer(_statistics.nDirectFacets),
                new Integer(_statistics.nFacets)
                };
    }

    private Object[] createFrameSummary() {
        return new Object[]{
                "Frames",
                new Integer(_statistics.nSystemFrames),
                new Integer(_statistics.nIncludedFrames),
                new Integer(_statistics.nDirectFrames),
                new Integer(_statistics.nFrames)
                };
    }

    private Object[] createInstanceReferencersSummary() {
        return new Object[]{
                "Referencers",
                newDouble(_statistics.meanInstanceReferencers),
                newDouble(_statistics.sdInstanceReferencers),
                new Integer(_statistics.maxInstanceReferencers)
                };
    }

    private Object[] createInstanceReferencesSummary() {
        return new Object[]{
                "References",
                newDouble(_statistics.meanInstanceReferences),
                newDouble(_statistics.sdInstanceReferences),
                new Integer(_statistics.maxInstanceReferences)
                };
    }

    private JComponent createInstancesPanel() {
        DefaultTableModel model = new DefaultTableModel();
        model.addColumn("");
        model.addColumn("Mean");
        model.addColumn("Std. Dev.");
        model.addColumn("Max");
        model.addRow(createInstanceReferencesSummary());
        model.addRow(createInstanceReferencersSummary());
        return createTable(model, "Instances");
    }

    private Object[] createInstancesSummary() {
        return new Object[]{
                "Direct Instances",
                newDouble(_statistics.meanDirectInstances),
                newDouble(_statistics.sdDirectInstances),
                new Integer(_statistics.maxDirectInstances),
                };
    }

    private Object[] createInstanceSummary() {
        return new Object[]{
                "Instances",
                new Integer(_statistics.nSystemInstances),
                new Integer(_statistics.nIncludedInstances),
                new Integer(_statistics.nDirectInstances),
                new Integer(_statistics.nInstances)
                };
    }

    private Object[] createParentSummary() {
        return new Object[]{
                "Parents",
                newDouble(_statistics.meanParents),
                newDouble(_statistics.sdParents),
                new Integer(_statistics.maxParents),
                };
    }

    private Object[] createRelationsSummary() {
        return new Object[]{
                "Relations",
                newDouble(_statistics.meanClsRelations),
                newDouble(_statistics.sdClsRelations),
                new Integer(_statistics.maxClsRelations),
                };
    }

    private Object[] createSlotDirectClsesSummary() {
        return new Object[]{
                "Direct Classes",
                newDouble(_statistics.meanClsesDirectlyAttached),
                newDouble(_statistics.sdClsesDirectlyAttached),
                new Integer(_statistics.maxClsesDirectlyAttached),
                };
    }

    private JComponent createSlotsPanel() {
        DefaultTableModel model = new DefaultTableModel();
        model.addColumn("");
        model.addColumn("Mean");
        model.addColumn("Std. Dev.");
        model.addColumn("Max");
        model.addRow(createSlotDirectClsesSummary());
        return createTable(model, "Slots");
    }

    private Object[] createSlotsSummary() {
        return new Object[]{
                "Slots",
                newDouble(_statistics.meanSlots),
                newDouble(_statistics.sdSlots),
                new Integer(_statistics.maxSlots),
                };
    }

    private Object[] createSlotSummary() {
        return new Object[]{
                "Slots",
                new Integer(_statistics.nSystemSlots),
                new Integer(_statistics.nIncludedSlots),
                new Integer(_statistics.nDirectSlots),
                new Integer(_statistics.nSlots)
                };
    }

    private JComponent createSummaryPanel() {
        DefaultTableModel model = new DefaultTableModel();
        model.addColumn("");
        model.addColumn("System");
        model.addColumn("Included");
        model.addColumn("Direct");
        model.addColumn("Total");
        model.addRow(createClsSummary());
        model.addRow(createSlotSummary());
        model.addRow(createFacetSummary());
        model.addRow(createInstanceSummary());
        model.addRow(createFrameSummary());
        return createTable(model, "Summary");
    }

    private JComponent createTable(DefaultTableModel model, String header) {
        JTable table = ComponentFactory.createTable(null);
        table.setModel(model);
        table.createDefaultColumnsFromModel();
        ((JLabel) table.getDefaultRenderer(Object.class)).setHorizontalAlignment(JLabel.RIGHT);
        TableColumn column = table.getColumnModel().getColumn(0);
        column.setPreferredWidth(95);

        DefaultTableCellRenderer renderer = new DefaultTableCellRenderer();
        renderer.setHorizontalAlignment(JLabel.LEFT);
        column.setCellRenderer(renderer);

        DefaultTableCellRenderer r = new DefaultTableCellRenderer();
        table.setDefaultRenderer(Double.class, r);

        table.setEnabled(false);

        JComponent c = new JPanel();
        c.setLayout(new BorderLayout());
        c.add(new JTableHeader(table.getColumnModel()), BorderLayout.NORTH);
        c.add(table, BorderLayout.CENTER);
        return new LabeledComponent(header, c);
    }

    private static boolean isSimpleInstance(Object o) {
        return !(o instanceof Cls || o instanceof Slot || o instanceof Facet);
    }

    private String newDouble(double d) {
        return String.valueOf(Math.round(d * 100) / 100.);
    }
}
