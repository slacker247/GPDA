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
import javax.swing.tree.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.event.*;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.util.*;
import edu.stanford.smi.protege.widget.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class FormsPanel extends SelectableContainer {
    private Project _project;
    private SelectableTree _tree;

    private ProjectListener _projectListener = new ProjectAdapter() {
        public void formChanged(ProjectEvent event) {
            TreePath path = _tree.getSelectionPath();
            if (path != null) {
                LazyTreeNode node = (LazyTreeNode) path.getLastPathComponent();
                node.notifyNodeChanged(node);
            }
            _tree.repaint();
        }
    };

    public FormsPanel(Project p) {
        _project = p;

        _project.addProjectListener(_projectListener);

        _tree = createTree();
        LabeledComponent c = new LabeledComponent("Forms", ComponentFactory.createScrollPane(_tree));
        c.addHeaderButton(createRemoveCustomizationsAction());
        c.addHeaderButton(createRelayoutAction());
        c.addHeaderButton(createLayoutLikeOtherFormAction());
        c.setFooterComponent(createFinderControl());

        _tree.setCellRenderer(new FormRenderer(_project));
        add(c);
        setSelectable(_tree);
    }

    private JComponent createFinderControl() {
        return new ClsTreeFinder(getKnowledgeBase(), _tree, "Find Form");
    }

    private Action createLayoutLikeOtherFormAction() {
        Action action = new AbstractAction("Layout like form...", Icons.getLayoutLikeOtherFormIcon()) {
            public void actionPerformed(ActionEvent event) {
                Collection selection = getSelection();
                if (!selection.isEmpty()) {
                    Cls cls = DisplayUtilities.pickForm(FormsPanel.this, _project);
                    if (cls != null) {
                        WaitCursor cursor = new WaitCursor(FormsPanel.this);
                        try {
                            Iterator i = selection.iterator();
                            while (i.hasNext()) {
                                Cls widgetCls = (Cls) i.next();
                                ClsWidget widget = getClsWidget(widgetCls);
                                widget.layoutLikeCls(cls);
                            }
                        } finally {
                            cursor.hide();
                        }
                    }
                }
            }
        };
        return action;
    }

    private Action createRelayoutAction() {
        return new AbstractAction("Default layout with current widgets", Icons.getRelayoutIcon()) {
            public void actionPerformed(ActionEvent event) {
                Iterator i = getSelection().iterator();
                while (i.hasNext()) {
                    Cls cls = (Cls) i.next();
                    ClsWidget widget = getClsWidget(cls);
                    widget.relayout();
                }
            }
        };
    }

    private Action createRemoveCustomizationsAction() {
        return new AbstractAction("Remove all customizations", Icons.getRemoveCustomizationsIcon()) {
            public void actionPerformed(ActionEvent event) {
                Iterator i = getSelection().iterator();
                while (i.hasNext()) {
                    Cls cls = (Cls) i.next();
                    ClsWidget widget = getClsWidget(cls);
                    widget.removeCustomizations();
                }
            }
        };
    }

    public LazyTreeRoot createRoot() {
        return new FormParentChildRoot(getKnowledgeBase().getRootCls());
    }

    private SelectableTree createTree() {
        Cls root = getKnowledgeBase().getRootCls();
        SelectableTree tree = ComponentFactory.createSelectableTree(null, new ParentChildRoot(root));
        tree.setSelectionRow(0);
        tree.addMouseListener(
            new TreePopupMenuMouseListener(tree) {
                public JPopupMenu getPopupMenu() {
                    return FormsPanel.this.getPopupMenu();
                }
            }
        );
        return tree;
    }

    public void dispose() {
        _project.removeProjectListener(_projectListener);
    }

    private ClsWidget getClsWidget(Cls cls) {
        return _project.getDesignTimeClsWidget(cls);
    }

    private KnowledgeBase getKnowledgeBase() {
        return _project.getKnowledgeBase();
    }

    private JPopupMenu getPopupMenu() {
        JPopupMenu menu = null;
        Collection selection = getSelection();
        if (selection.size() == 1) {
            menu = new JPopupMenu();
            menu.add(getRemoveDecendentCustomizations());
        }
        return menu;
    }

    private Action getRemoveDecendentCustomizations() {
        return new AbstractAction("Remove subclass customizations", Icons.getBlankIcon()) {
            public void actionPerformed(ActionEvent event) {
                WaitCursor cursor = new WaitCursor(FormsPanel.this);
                try {
                    removeCustomizations();
                } finally {
                    cursor.hide();
                }
            }
        };
    }

    private Action getViewFormAction() {
        return
            new ViewAction("View form for selected classes", this) {
                public void onView(Object o) {
                }
            }
        ;
    }

    private void removeCustomizations() {
        Cls cls = (Cls) CollectionUtilities.getFirstItem(getSelection());
        Iterator i = cls.getSubclasses().iterator();
        while (i.hasNext()) {
            Cls subclass = (Cls) i.next();
            if (_project.hasCustomizedDescriptor(subclass)) {
                ClsWidget widget = getClsWidget(subclass);
                widget.removeCustomizations();
            }
        }
    }
}
