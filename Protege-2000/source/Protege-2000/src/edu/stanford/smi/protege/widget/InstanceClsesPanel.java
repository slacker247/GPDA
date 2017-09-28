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
import java.awt.dnd.*;
import java.awt.event.*;
import java.util.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.tree.*;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.ui.*;
import edu.stanford.smi.protege.util.*;

class InstanceClsesPanel extends SelectableContainer {

    private Project _project;
    private SelectableTree _clsTree;

    public InstanceClsesPanel(Project project) {
        _project = project;
        add(createClsesPanel());
        setSelectable(_clsTree);
    }

    private JComponent createClsesPanel() {
        Cls root = _project.getKnowledgeBase().getRootCls();
        _clsTree = ComponentFactory.createSelectableTree(null, new ParentChildRoot(root));
        FrameRenderer renderer = FrameRenderer.createInstance();
        renderer.setDisplayDirectInstanceCount(true);
        _clsTree.setCellRenderer(renderer);
        _clsTree.setSelectionRow(0);
        LabeledComponent c = new LabeledComponent("Classes", new JScrollPane(_clsTree));
        c.addHeaderButton(getViewClsAction());
        c.setFooterComponent(new ClsTreeFinder(_project.getKnowledgeBase(), _clsTree, "Find Class"));
        return c;
    }

    public JTree getDropComponent() {
        return _clsTree;
    }

    private Action getViewClsAction() {
        return
            new AbstractAction("View Selected Class", Icons.getViewIcon()) {
                public void actionPerformed(ActionEvent event) {
                    Iterator i = ComponentUtilities.getSelection(_clsTree).iterator();
                    while (i.hasNext()) {
                        Cls cls = (Cls) i.next();
                        _project.show(cls);
                    }
                }
            }
        ;
    }

    public void setSelectedCls(Cls cls) {
        Collection path = ModelUtilities.getPathToRoot(cls);
        ComponentUtilities.setSelectedObjectPath(_clsTree, path);
    }
}
