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
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class SubslotPane extends SelectableContainer {
    private Project _project;
    private KnowledgeBase _knowledgeBase;
    private Action _createSlotAction;
    private Action _createSubslotAction;
    private Action _deleteSlotAction;
    private Action _viewSlotAction;

    private final static int MAX_EXPANSIONS = 100;

    public SubslotPane(Project p) {
        _project = p;
        _knowledgeBase = _project.getKnowledgeBase();
        _createSlotAction = getCreateAction();
        _deleteSlotAction = getDeleteAction();
        _createSubslotAction = getCreateSubslotAction();
        _viewSlotAction = getViewAction();

        LazyTreeRoot root = new SlotSubslotRoot(_project.getKnowledgeBase());
        SelectableTree tree = ComponentFactory.createSelectableTree(_viewSlotAction, root);
        tree.setShowsRootHandles(true);
        tree.setSelectionRow(0);
        setSelectable(tree);
        setLayout(new BorderLayout());
        LabeledComponent c = new LabeledComponent("Slots", new JScrollPane(tree));
        // c.setFooterComponent(new SlotTreeFinder(knowledgeBase, tree, "Find Slot"));
        c.addHeaderButton(_viewSlotAction);
        c.addHeaderButton(_createSlotAction);
        c.addHeaderButton(_deleteSlotAction);
        add(c);
        setupDragAndDrop();
        getTree().setCellRenderer(FrameRenderer.createInstance());
        getTree().addMouseListener(new TreePopupMenuMouseListener(tree) {
            public JPopupMenu getPopupMenu() {
                return SubslotPane.this.getPopupMenu();
            }
        });
    }

    private Action createCollapseAllAction() {
        return new AbstractAction("Collapse", Icons.getBlankIcon()) {
            public void actionPerformed(ActionEvent event) {
                ComponentUtilities.fullSelectionCollapse(getTree());
            }
        };
    }

    private Action createExpandAllAction() {
        return new AbstractAction("Expand", Icons.getBlankIcon()) {
            public void actionPerformed(ActionEvent event) {
                ComponentUtilities.fullSelectionExpand(getTree(), MAX_EXPANSIONS);
            }
        };
    }

    public void extendSelection(Slot slot) {
        ComponentUtilities.extendSelection(getTree(), slot);
    }

    private Action getChangeSlotMetaclassAction(final Slot slot) {
        Cls rootMetaclass = _knowledgeBase.getRootSlotMetaCls();
        final Collection c = CollectionUtilities.createCollection(rootMetaclass);
        boolean hasMultipleMetaclasses = DisplayUtilities.hasMultipleConcreteClses(c);

        Action action = new AbstractAction("Change slot metaclass...", Icons.getBlankIcon()) {
            public void actionPerformed(ActionEvent event) {
                Cls metaclass = DisplayUtilities.pickConcreteCls(SubslotPane.this, c, "Select Slot Metaclass");
                if (metaclass != null && metaclass != slot.getDirectType()) {
                    slot.setDirectType(metaclass);
                }
            }
        };
        action.setEnabled(hasMultipleMetaclasses && slot.isEditable());
        return action;
    }

    private Action getChangeSubslotSlotMetaclassAction(final Slot slot) {
        Cls rootMetaclass = _knowledgeBase.getRootSlotMetaCls();
        Collection c = CollectionUtilities.createCollection(rootMetaclass);
        boolean hasMultipleMetaclasses = DisplayUtilities.hasMultipleConcreteClses(c);

        Action action = new AbstractAction("Change slot metaclass of subslots", Icons.getBlankIcon()) {
            public void actionPerformed(ActionEvent event) {
                Cls metaCls = slot.getDirectType();
                String text = "Change slot metaclass of all subslots of ";
                text += slot.getName();
                text += " to " + metaCls.getName();
                int result = ModalDialog.showMessageDialog(SubslotPane.this, text, ModalDialog.MODE_OK_CANCEL);
                if (result == ModalDialog.OPTION_OK) {
                    WaitCursor waitCursor = new WaitCursor(SubslotPane.this);
                    try {
                        slot.setDirectTypeOfSubslots(metaCls);
                    } finally {
                        waitCursor.hide();
                    }
                }
            }
        };
        boolean enabled = slot.isEditable() && hasMultipleMetaclasses && slot.getDirectSubslotCount() >= 1;
        action.setEnabled(enabled);
        return action;
    }

    private Action getCreateAction() {
        return new CreateAction("Create slot") {
            public void onCreate() {
                Slot slot = _knowledgeBase.createSlot(null);
                setSelectedSlot(slot);
            }
        };
    }

    private Action getCreateSlotWithSlotMetaclassAction() {
        AbstractAction action = new AbstractAction("Create subslot using slot metaclass...", Icons.getBlankIcon()) {
            public void actionPerformed(ActionEvent event) {
                Cls rootMetaCls = _knowledgeBase.getRootClsMetaCls();
                Collection roots = CollectionUtilities.createCollection(rootMetaCls);
                Cls metaCls = DisplayUtilities.pickConcreteCls(SubslotPane.this, roots);
                Collection parents = SubslotPane.this.getSelection();
                if (metaCls != null && !parents.isEmpty()) {
                    Slot slot = _knowledgeBase.createSlot(null, metaCls, parents, true);
                    extendSelection(slot);
                }
            }
        };
        boolean enabled = hasMultipleConcreteSlotMetaClses();
        action.setEnabled(enabled);
        return action;
    }

    private Action getCreateSubslotAction() {
        return new CreateAction("Create subslot") {
            public void onCreate() {
                // SystemUtilities.debugBreak();
                Collection c = SubslotPane.this.getSelection();
                Slot firstSuperslot = (Slot) CollectionUtilities.getFirstItem(c);
                if (firstSuperslot != null) {
                    Cls metaCls = firstSuperslot.getDirectType();
                    Slot slot = _knowledgeBase.createSlot(null, metaCls, c, true);
                    extendSelection(slot);
                }
            }
        };
    }

    private Action getDeleteAction() {
        return new DeleteAction("Delete selected slots", this) {
            public void onDelete(Object o) {
                removeSelection();
                _knowledgeBase.deleteSlot((Slot) o);
            }

            public void onSelectionChange() {
                Slot slot = (Slot) CollectionUtilities.getFirstItem(this.getSelection());
                if (slot != null) {
                    setAllowed(slot.isEditable());
                }
            }
        };
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

    public List getPath(Slot slot, List list) {
        list.add(0, slot);
        Slot superslot = (Slot) CollectionUtilities.getFirstItem(slot.getDirectSuperslots());
        if (superslot != null) {
            getPath(superslot, list);
        }
        return list;
    }

    private JPopupMenu getPopupMenu() {
        JPopupMenu menu = null;
        Collection selection = getSelection();
        if (selection.size() == 1) {
            Slot slot = (Slot) CollectionUtilities.getFirstItem(selection);
            menu = new JPopupMenu();
            menu.add(_createSlotAction);
            menu.add(_createSubslotAction);
            menu.add(getCreateSlotWithSlotMetaclassAction());
            menu.add(_deleteSlotAction);
            menu.addSeparator();
            menu.add(getChangeSlotMetaclassAction(slot));
            menu.add(getChangeSubslotSlotMetaclassAction(slot));
            menu.addSeparator();
            menu.add(createExpandAllAction());
            menu.add(createCollapseAllAction());
        }
        return menu;
    }

    private JTree getTree() {
        return (JTree) getSelectable();
    }

    private Action getViewAction() {
        return new ViewAction("View selected slots", this) {
            public void onView(Object o) {
                _project.show((Slot) o);
            }
        };
    }

    private boolean hasMultipleConcreteSlotMetaClses() {
        int nConcrete = 0;
        Collection metaClses = _knowledgeBase.getRootSlotMetaCls().getSubclasses();
        Iterator i = metaClses.iterator();
        while (i.hasNext() && nConcrete < 2) {
            Cls cls = (Cls) i.next();
            if (cls.isConcrete()) {
                ++nConcrete;
            }
        }
        return nConcrete > 1;
    }

    public void removeSelection() {
        ComponentUtilities.removeSelection(getTree());
    }

    public void setDisplayParent(Cls cls) {
        AbstractTreeWidget.setDisplayParent(getTree(), cls);
    }

    public void setExpandedSlot(Slot slot, boolean expanded) {
        ComponentUtilities.setExpanded(getTree(), getPath(slot, new LinkedList()), expanded);
    }

    public void setFinderComponent(JComponent c) {
        add(c, BorderLayout.SOUTH);
    }

    public void setRenderer(DefaultRenderer renderer) {
        getTree().setCellRenderer(renderer);
    }

    public void setSelectedSlot(Slot slot) {
        if (!getSelection().contains(slot)) {
            ComponentUtilities.setSelectedObjectPath(getTree(), getPath(slot, new LinkedList()));
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