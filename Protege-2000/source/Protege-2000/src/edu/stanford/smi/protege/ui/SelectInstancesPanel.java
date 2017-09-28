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
import javax.swing.event.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.util.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class SelectInstancesPanel extends JComponent {
    private static final int WIDTH = 500;
    protected JTree _clsTree;
    protected JList _instanceList;

    protected SelectInstancesPanel(Collection clses) {
        setPreferredSize(new Dimension(WIDTH, 300));
        createWidgets(clses);
    }

    protected LabeledComponent createClsesLabeledComponent(Collection clses) {
        LabeledComponent clsesComponent = new LabeledComponent("Allowed Classes", new JScrollPane(_clsTree));
        KnowledgeBase kb = ((Cls) CollectionUtilities.getFirstItem(clses)).getKnowledgeBase();
        clsesComponent.setFooterComponent(new ClsTreeFinder(kb, _clsTree, "Find Class"));
        return clsesComponent;
    }

    protected JComponent createClsTree(Collection clses) {
        LazyTreeRoot root = new ParentChildRoot(clses);
        _clsTree = ComponentFactory.createSelectableTree(null, root);
        _clsTree.addTreeSelectionListener(new TreeSelectionListener() {
            public void valueChanged(TreeSelectionEvent event) {
                loadInstances();
            }
        });
        FrameRenderer renderer = FrameRenderer.createInstance();
        renderer.setDisplayDirectInstanceCount(true);
        _clsTree.setCellRenderer(renderer);
        if (clses.size() == 1) {
            _clsTree.expandRow(0);
            _clsTree.setSelectionRow(0);
        }
        return _clsTree;
    }

    protected LabeledComponent createInstanceLabeledComponent() {
        LabeledComponent c = new LabeledComponent("Direct Instances", new JScrollPane(_instanceList));
        c.setFooterComponent(new ListFinder(_instanceList, "Find Instance"));
        return c;
    }

    protected JComponent createInstanceList() {
        _instanceList = ComponentFactory.createList(null);
        _instanceList.setCellRenderer(FrameRenderer.createInstance());
        return _instanceList;
    }

    protected void createWidgets(Collection clses) {
        createInstanceList();
        LabeledComponent instancesComponent = createInstanceLabeledComponent();

        createClsTree(clses);
        LabeledComponent clsesComponent = createClsesLabeledComponent(clses);

        JSplitPane main = ComponentFactory.createLeftRightSplitPane();
        main.setLeftComponent(clsesComponent);
        main.setRightComponent(instancesComponent);
        main.setDividerLocation(WIDTH / 2);
        setLayout(new BorderLayout());
        add(main);
    }

    protected SimpleListModel getInstanceModel() {
        return (SimpleListModel) _instanceList.getModel();
    }

    public Collection getSelection() {
        return ComponentUtilities.getSelection(_instanceList);
    }

    protected void loadInstances() {
        ArrayList instances = new ArrayList();
        Iterator i = ComponentUtilities.getSelection(_clsTree).iterator();
        while (i.hasNext()) {
            Cls cls = (Cls) i.next();
            instances.addAll(cls.getDirectInstances());
        }
        Collections.sort(instances, new FrameComparator());
        getInstanceModel().setValues(instances);
        if (!instances.isEmpty()) {
            _instanceList.setSelectedIndex(0);
        }
    }
}
