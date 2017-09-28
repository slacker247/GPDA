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
import java.awt.datatransfer.*;
import java.awt.dnd.*;
import java.awt.event.*;
import java.util.*;
import java.util.List;
import javax.swing.*;
import javax.swing.tree.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.model.Frame;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.util.*;
import edu.stanford.smi.protege.widget.*;

/**
 * The component displays the superclass/subclass tree on the classes tab.
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class SubclassPane extends SelectableContainer {
    private KnowledgeBase _knowledgeBase;
    private Action _createClsAction;
    private Action _deleteClsAction;

    private final static int MAX_EXPANSIONS = 100;

    public SubclassPane(Action doubleClickAction, Cls root, Action createCls, Action deleteCls) {
        _knowledgeBase = root.getKnowledgeBase();
        _createClsAction = createCls;
        _deleteClsAction = deleteCls;
        SelectableTree tree = ComponentFactory.createSelectableTree(doubleClickAction, new ParentChildRoot(root));
        tree.setSelectionRow(0);
        tree.setAutoscrolls(true);
        setSelectable(tree);
        setLayout(new BorderLayout());
        add(new JScrollPane(tree), BorderLayout.CENTER);
        add(new ClsTreeFinder(_knowledgeBase, tree, "Find Class"), BorderLayout.SOUTH);
        setupDragAndDrop();
        getTree().setCellRenderer(FrameRenderer.createInstance());
        getTree().addMouseListener(new TreePopupMenuMouseListener(tree) {
            public JPopupMenu getPopupMenu() {
                return SubclassPane.this.getPopupMenu();
            }
        });
    }

    private Action createCollapseAllAction() {
        return
            new AbstractAction("Collapse", Icons.getBlankIcon()) {
                public void actionPerformed(ActionEvent event) {
                    ComponentUtilities.fullSelectionCollapse(getTree());
                }
            }
        ;
    }

    private Action createExpandAllAction() {
        return
            new AbstractAction("Expand", Icons.getBlankIcon()) {
                public void actionPerformed(ActionEvent event) {
                    ComponentUtilities.fullSelectionExpand(getTree(), MAX_EXPANSIONS);
                }
            }
        ;
    }

    /*
     * private Action createSetClsMetaClsAction(final Cls cls) {
     * AbstractAction action = new AbstractAction("Set as default class metaclass", Icons.getBlankIcon()) {
     * public void actionPerformed(ActionEvent event) {
     * knowledgeBase.setDefaultClsMetaCls(cls);
     * }
     * };
     * action.setEnabled(cls.isConcrete() && !cls.isDefaultClsMetaCls());
     * return action;
     * }
     * private Action createUnsetClsMetaClsAction(final Cls cls) {
     * AbstractAction action = new AbstractAction("Unset as default class metaclass", Icons.getBlankIcon()) {
     * public void actionPerformed(ActionEvent event) {
     * knowledgeBase.setDefaultClsMetaCls(null);
     * }
     * };
     * action.setEnabled(cls.isDefaultClsMetaCls());
     * return action;
     * }
     */

    private Action createSetSlotMetaClsAction(final Cls cls) {
        AbstractAction action =
            new AbstractAction("Set as default slot metaclass", Icons.getBlankIcon()) {
                public void actionPerformed(ActionEvent event) {
                    _knowledgeBase.setDefaultSlotMetaCls(cls);
                }
            }
        ;
        action.setEnabled(cls.isConcrete() && !cls.isDefaultSlotMetaCls());
        return action;
    }

    private Action createUnsetSlotMetaClsAction(final Cls cls) {
        AbstractAction action =
            new AbstractAction("Unset as default slot metaclass", Icons.getBlankIcon()) {
                public void actionPerformed(ActionEvent event) {
                    _knowledgeBase.setDefaultSlotMetaCls(null);
                }
            }
        ;
        action.setEnabled(cls.isDefaultSlotMetaCls());
        return action;
    }

    public void extendSelection(Cls cls) {
        ComponentUtilities.extendSelection(getTree(), cls);
    }

    private Action getChangeMetaclassAction(final Cls cls) {
        Cls rootMetaclass = _knowledgeBase.getRootClsMetaCls();
        final Collection c = CollectionUtilities.createCollection(rootMetaclass);
        boolean hasMultipleMetaclasses = DisplayUtilities.hasMultipleConcreteClses(c);

        Action action = new AbstractAction("Change metaclass...", Icons.getBlankIcon()) {
            public void actionPerformed(ActionEvent event) {
                Cls metaclass = DisplayUtilities.pickConcreteCls(SubclassPane.this, c, "Select Metaclass");
                if (metaclass != null && metaclass != cls.getDirectType()) {
                    cls.setDirectType(metaclass);
                }
            }
        };
        action.setEnabled(hasMultipleMetaclasses && cls.isEditable());
        return action;
    }

    private Action getChangeSubclassMetaclassAction(final Cls cls) {
        Cls rootMetaclass = _knowledgeBase.getRootClsMetaCls();
        Collection c = CollectionUtilities.createCollection(rootMetaclass);
        boolean hasMultipleMetaclasses = DisplayUtilities.hasMultipleConcreteClses(c);

        Action action = new AbstractAction("Change metaclass of subclasses", Icons.getBlankIcon()) {
            public void actionPerformed(ActionEvent event) {
                Cls metaCls = cls.getDirectType();
                String text = "Change metaclass of all subclasses of ";
                text += cls.getName();
                text += " to " + metaCls.getName();
                int result = ModalDialog.showMessageDialog(SubclassPane.this, text, ModalDialog.MODE_OK_CANCEL);
                if (result == ModalDialog.OPTION_OK) {
                    WaitCursor waitCursor = new WaitCursor(SubclassPane.this);
                    try {
                        cls.setDirectTypeOfSubclasses(metaCls);
                    } finally {
                        waitCursor.hide();
                    }
                }
            }
        };
        boolean enabled = cls.isEditable() && hasMultipleMetaclasses && cls.getDirectSubclassCount() >= 1;
        action.setEnabled(enabled);
        return action;
    }

    private Action getCreateClsWithMetaClsAction() {
        AbstractAction action = new AbstractAction("Create subclass using metaclass...", Icons.getBlankIcon()) {
            public void actionPerformed(ActionEvent event) {
                Cls rootMetaCls = _knowledgeBase.getRootClsMetaCls();
                Collection roots = CollectionUtilities.createCollection(rootMetaCls);
                Cls metaCls = DisplayUtilities.pickConcreteCls(SubclassPane.this, roots);
                Collection parents = getSelection();
                if (metaCls != null && !parents.isEmpty()) {
                    Cls cls = _knowledgeBase.createCls(null, parents, metaCls);
                    extendSelection(cls);
                }
            }
        };
        boolean enabled = hasMultipleConcreteClsMetaClses();
        action.setEnabled(enabled);
        return action;
    }

    public Cls getDisplayParent() {
        TreePath path = getTree().getSelectionModel().getLeadSelectionPath().getParentPath();
        LazyTreeNode node = (LazyTreeNode) path.getLastPathComponent();
        Object o = node.getUserObject();
        return (o instanceof Cls) ? (Cls) o : null;
    }

    public JComponent getDropComponent() {
        return getTree();
    }

    private Action getHideClsAction() {
        return new AbstractAction("Hide class", Icons.getBlankIcon()) {
            public void actionPerformed(ActionEvent event) {
                Iterator i = getSelection().iterator();
                while (i.hasNext()) {
                    Cls cls = (Cls) i.next();
                    cls.setVisible(false);
                }
            }
        };
    }

    private JPopupMenu getPopupMenu() {
        JPopupMenu menu = null;
        Collection selection = getSelection();
        if (selection.size() == 1) {
            Cls cls = (Cls) CollectionUtilities.getFirstItem(selection);
            menu = new JPopupMenu();
            menu.add(_createClsAction);
            menu.add(getCreateClsWithMetaClsAction());
            menu.add(_deleteClsAction);
            menu.addSeparator();
            menu.add(getChangeMetaclassAction(cls));
            menu.add(getChangeSubclassMetaclassAction(cls));
            menu.addSeparator();
            if (cls.isVisible()) {
                menu.add(getHideClsAction());
            } else {
                menu.add(getUnhideClsAction());
            }
            if (cls.isSlotMetaCls()) {
                menu.addSeparator();
                menu.add(createSetSlotMetaClsAction(cls));
                menu.add(createUnsetSlotMetaClsAction(cls));
            } else if (cls.isFacetMetaCls()) {
            }
            menu.addSeparator();
            menu.add(createExpandAllAction());
            menu.add(createCollapseAllAction());
        }
        return menu;
    }

    private JTree getTree() {
        return (JTree) getSelectable();
    }

    private Action getUnhideClsAction() {
        return new AbstractAction("Make class visible", Icons.getBlankIcon()) {
            public void actionPerformed(ActionEvent event) {
                Iterator i = getSelection().iterator();
                while (i.hasNext()) {
                    Cls cls = (Cls) i.next();
                    cls.setVisible(true);
                }
            }
        };
    }

    private boolean hasMultipleConcreteClsMetaClses() {
        // Correct but slow
        /*
        int nConcrete = 0;
        Collection metaClses = _knowledgeBase.getRootClsMetaCls().getSubclasses();
        Iterator i = metaClses.iterator();
        while (i.hasNext() && nConcrete < 2) {
            Cls cls = (Cls) i.next();
            if (cls.isConcrete()) {
                ++nConcrete;
            }
        }
        return nConcrete > 1;
        */

        // Wrong but fast
        Cls standardCls = _knowledgeBase.getCls(Model.Cls.STANDARD_CLASS);
        return standardCls.getDirectSubclassCount() > 0;
    }

    public void removeSelection() {
        ComponentUtilities.removeSelection(getTree());
    }

    public void setDisplayParent(Cls cls) {
        AbstractTreeWidget.setDisplayParent(getTree(), cls);
    }

    public void setExpandedCls(Cls cls, boolean expanded) {
        Collection path = ModelUtilities.getPathToRoot(cls);
        ComponentUtilities.setExpanded(getTree(), path, expanded);
    }

    public void setFinderComponent(JComponent c) {
        add(c, BorderLayout.SOUTH);
    }

    public void setRenderer(DefaultRenderer renderer) {
        getTree().setCellRenderer(renderer);
    }

    public void setSelectedCls(Cls cls) {
        if (!getSelection().contains(cls)) {
            Collection path = ModelUtilities.getPathToRoot(cls);
            ComponentUtilities.setSelectedObjectPath(getTree(), path);
        }
    }

    private void setupDragAndDrop() {
        DragSource.getDefaultDragSource().createDefaultDragGestureRecognizer(getTree(),
                DnDConstants.ACTION_COPY_OR_MOVE, new ClsesTreeDragSourceListener());
        new DropTarget(getTree(), DnDConstants.ACTION_COPY_OR_MOVE, new ClsesTreeTarget());
    }

    public String toString() {
        return "SubclassPane";
    }
}
