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

package edu.stanford.smi.RemoteKBTab.toolbox;

import edu.stanford.smi.protege.ui.*;
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

public class RemoteKBTabSubClassPane extends SelectableContainer {
    private KnowledgeBase itsKnowledgeBase;
    private Action itsCreateClsAction;
    private Action itsDeleteClsAction;
    private RemoteKBTabClsTreeFinder treeFinder;

    public RemoteKBTabSubClassPane(Action doubleClickAction, Cls root, Action createCls, Action deleteCls) {
        itsKnowledgeBase = root.getKnowledgeBase();
        itsCreateClsAction = createCls;
        itsDeleteClsAction = deleteCls;
        SelectableTree tree = ComponentFactory.createSelectableTree(doubleClickAction, new ParentChildRoot(root));
        tree.setSelectionRow(0);
        setSelectable(tree);
        setLayout(new BorderLayout());
        add(new JScrollPane(tree), BorderLayout.CENTER);
        treeFinder = new RemoteKBTabClsTreeFinder(itsKnowledgeBase, tree, "Find Class");
        add(treeFinder, BorderLayout.SOUTH);

        add(new ClsTreeFinder(itsKnowledgeBase, tree, "Find Class"), BorderLayout.SOUTH);
        setupDragAndDrop();
        getTree().setCellRenderer(new FrameRenderer());
    	getTree().addMouseListener(new TreePopupMenuMouseListener(tree) {
            public JPopupMenu getPopupMenu() {
                return RemoteKBTabSubClassPane.this.getPopupMenu();
            }
        });
    }

    public RemoteKBTabClsTreeFinder getTreeFinder() {
        return treeFinder;
    }

    public void setRenderer(DefaultRenderer renderer) {
        getTree().setCellRenderer(renderer);
    }

    public void extendSelection(Cls cls) {
        ComponentUtilities.extendSelection(getTree(), cls);
    }

    public void removeSelection() {
        ComponentUtilities.removeSelection(getTree());
    }

    public JComponent getDropComponent() {
        return getTree();
    }

    private JTree getTree() {
        return (JTree) getSelectable();
    }

    public Cls getDisplayParent() {
        TreePath path = getTree().getSelectionModel().getLeadSelectionPath().getParentPath();
        LazyTreeNode node = (LazyTreeNode) path.getLastPathComponent();
        Object o = node.getUserObject();
        return (o instanceof Cls) ? (Cls)o : null;
    }

    public void setDisplayParent(Cls cls) {
        AbstractTreeWidget.setDisplayParent(getTree(), cls);
    }

    public void setSelectedCls(Cls cls) {
        if (!getSelection().contains(cls)) {
            ComponentUtilities.setSelectedObjectPath(getTree(), getPath(cls, new LinkedList()));
        }
    }

    public List getPath(Cls cls, List list) {
        list.add(0, cls);
        Cls superclass = (Cls) CollectionUtilities.getFirstItem(cls.getDirectSuperclasses());
        if (superclass != null) {
            getPath(superclass, list);
        }
        return list;
    }


    private void setupDragAndDrop() {
        DragSource.getDefaultDragSource().createDefaultDragGestureRecognizer(getTree(),
            DnDConstants.ACTION_COPY_OR_MOVE, new ClsesTreeDragSourceListener());
    	new DropTarget(getTree(), DnDConstants.ACTION_COPY_OR_MOVE, new ClsesTreeTarget());
    }

    private JPopupMenu getPopupMenu() {
    	JPopupMenu menu = null;
    	Collection selection = getSelection();
        if (selection.size() == 1) {
        	Cls cls = (Cls) CollectionUtilities.getFirstItem(selection);
            menu = new JPopupMenu();
            menu.add(itsCreateClsAction);
            menu.add(getCreateClsWithMetaClsAction());
            menu.add(itsDeleteClsAction);
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

    private Action createSetSlotMetaClsAction(final Cls cls) {
        AbstractAction action = new AbstractAction("Set as default slot metaclass", Icons.getBlankIcon()) {
            public void actionPerformed(ActionEvent event) {
                itsKnowledgeBase.setDefaultSlotMetaCls(cls);
            }
        };
        action.setEnabled(cls.isConcrete() && !cls.isDefaultSlotMetaCls());
        return action;
    }

    private Action createUnsetSlotMetaClsAction(final Cls cls) {
    	AbstractAction action = new AbstractAction("Unset as default slot metaclass", Icons.getBlankIcon()) {
            public void actionPerformed(ActionEvent event) {
            	itsKnowledgeBase.setDefaultSlotMetaCls(null);
            }
        };
        action.setEnabled(cls.isDefaultSlotMetaCls());
        return action;
    }

    private Action getCreateClsWithMetaClsAction() {
        AbstractAction action = new AbstractAction("Create subclass using metaclass...", Icons.getBlankIcon()) {
            public void actionPerformed(ActionEvent event) {
                Cls rootMetaCls = itsKnowledgeBase.getRootClsMetaCls();
                Collection roots = CollectionUtilities.createCollection(rootMetaCls);
                Cls metaCls = DisplayUtilities.pickConcreteCls(RemoteKBTabSubClassPane.this, roots);
                Collection parents = RemoteKBTabSubClassPane.this.getSelection();
                if (metaCls != null && !parents.isEmpty()) {
                    Cls cls = itsKnowledgeBase.createCls(null, parents, metaCls);
                    extendSelection(cls);
                }
            }
        };
        boolean enabled = hasMultipleConcreteClsMetaClses();
        action.setEnabled(enabled);
        return action;
    }

    private boolean hasMultipleConcreteClsMetaClses() {
        int nConcrete = 0;
        Collection metaClses = itsKnowledgeBase.getRootClsMetaCls().getSubclasses();
        Iterator i = metaClses.iterator();
        while (i.hasNext() && nConcrete < 2) {
            Cls cls = (Cls) i.next();
            if (cls.isConcrete()) {
                ++nConcrete;
            }
        }
        return nConcrete > 1;
    }

    private Action getHideClsAction() {
        return new AbstractAction("Hide class", Icons.getBlankIcon()) {
            public void actionPerformed(ActionEvent event) {
                Iterator i = RemoteKBTabSubClassPane.this.getSelection().iterator();
                while (i.hasNext()) {
                	Cls cls = (Cls) i.next();
                    cls.setVisible(false);
                }
            }
        };
    }

    private Action getUnhideClsAction() {
    	return new AbstractAction("Make class visible", Icons.getBlankIcon()) {
            public void actionPerformed(ActionEvent event) {
                Iterator i = RemoteKBTabSubClassPane.this.getSelection().iterator();
                while (i.hasNext()) {
                	Cls cls = (Cls) i.next();
                    cls.setVisible(true);
                }
            }
        };
    }

    private Action getChangeMetaclassAction(final Cls cls) {
        Cls rootMetaclass = itsKnowledgeBase.getRootClsMetaCls();
        final Collection c = CollectionUtilities.createCollection(rootMetaclass);
        boolean hasMultipleMetaclasses = DisplayUtilities.hasMultipleConcreteClses(c);

        Action action = new AbstractAction("Change metaclass...", Icons.getBlankIcon()) {
            public void actionPerformed(ActionEvent event) {
                Cls metaclass = DisplayUtilities.pickConcreteCls(RemoteKBTabSubClassPane.this, c, "Select Metaclass");
                if (metaclass != null && metaclass != cls.getDirectType()) {
                    cls.setDirectType(metaclass);
                }
            }
        };
        action.setEnabled(hasMultipleMetaclasses && cls.isEditable());
        return action;
    }

    private Action getChangeSubclassMetaclassAction(final Cls cls) {
        Cls rootMetaclass = itsKnowledgeBase.getRootClsMetaCls();
        Collection c = CollectionUtilities.createCollection(rootMetaclass);
        boolean hasMultipleMetaclasses = DisplayUtilities.hasMultipleConcreteClses(c);

        Action action = new AbstractAction("Change metaclass of subclasses", Icons.getBlankIcon()) {
            public void actionPerformed(ActionEvent event) {
                Cls metaCls = cls.getDirectType();
                String text = "Change metaclass of all subclasses of ";
                text += cls.getName();
                text += " to " + metaCls.getName();
                int result = ModalDialog.showMessageDialog(RemoteKBTabSubClassPane.this, text, ModalDialog.MODE_OK_CANCEL);
                if (result == ModalDialog.OPTION_OK) {
                    WaitCursor waitCursor = new WaitCursor(RemoteKBTabSubClassPane.this);
                    cls.setDirectTypeOfSubclasses(metaCls);
                    waitCursor.hide();
                }
            }
        };
        boolean enabled = cls.isEditable() && hasMultipleMetaclasses && cls.getDirectSubclassCount() > 1;
        action.setEnabled(enabled);
        return action;
    }

    private Action createExpandAllAction() {
        return new AbstractAction("Expand", Icons.getBlankIcon()) {
            public void actionPerformed(ActionEvent event) {
                fullSelectionExpand();
            }
        };
    }

    private Action createCollapseAllAction() {
        return new AbstractAction("Collapse all", Icons.getBlankIcon()) {
            public void actionPerformed(ActionEvent event) {
                fullSelectionCollapse();
            }
        };
    }

    private void fullSelectionCollapse() {
        JTree tree = getTree();
        int startRow = tree.getLeadSelectionRow();
        int stopRow = getStopRow(startRow);
        for (int i = stopRow - 1; i >= startRow; --i) {
            tree.collapseRow(i);
        }
    }

    private int getStopRow(int startRow) {
        JTree tree = getTree();
        int startDepth = tree.getPathForRow(startRow).getPathCount();
        int last = tree.getRowCount();
        int stopRow = last;
        for (int i = startRow+1; i < last; ++i) {
            int depth = tree.getPathForRow(i).getPathCount();
            if (depth <= startDepth) {
                stopRow = i;
                break;
            }
        }
        return stopRow;
    }

    private static final int MAX_EXPANSIONS = 100;

    private void fullSelectionExpand() {
        JTree tree = getTree();
        TreePath topPath = tree.getLeadSelectionPath();
        fullExpand(topPath, MAX_EXPANSIONS);
    }

    private int fullExpand(TreePath parentPath, int nExpansions) {
        JTree tree = getTree();
        TreeNode parent = (TreeNode) parentPath.getLastPathComponent();
        int count = parent.getChildCount();
        for (int i = 0; i < count && nExpansions > 0; ++i) {
            TreeNode child = parent.getChildAt(i);
            TreePath childPath = parentPath.pathByAddingChild(child);
            nExpansions = fullExpand(childPath, nExpansions);
        }
        tree.expandPath(parentPath);
        return --nExpansions;
    }

    public String toString() {
        return "RemoteKBTabSubClassPane";
    }
}


class ClsesTreeDragSourceListener extends TreeDragSourceListener {

    public boolean canStartDrag(Collection objects) {
        boolean canStartDrag = true;
        Iterator i = objects.iterator();
        while (i.hasNext()) {
            Frame frame = (Frame) i.next();
            if (!frame.isEditable()) {
                canStartDrag = false;
                break;
            }
        }
        return canStartDrag;
    }

    public void doMove(Collection paths) {
    	Iterator i = paths.iterator();
        while (i.hasNext()) {
        	TreePath path = (TreePath) i.next();
            LazyTreeNode draggedNode = (LazyTreeNode) path.getLastPathComponent();
            LazyTreeNode draggedNodeParent = (LazyTreeNode) draggedNode.getParent();
            Cls draggedCls = (Cls) draggedNode.getUserObject();
            Cls draggedClsParent = (Cls) draggedNodeParent.getUserObject();
    		draggedCls.removeDirectSuperclass(draggedClsParent);
        }
    }

    public void doCopy(Collection paths) {
    }
}

class ClsesTreeTarget extends TreeTarget {

    public ClsesTreeTarget() {
    	super(true);
    }

    public boolean doDrop(JTree tree, Object source, int targetRow, Object area) {
    	boolean succeeded = false;
        TreePath path = tree.getPathForRow(targetRow);
        LazyTreeNode targetNode = (LazyTreeNode)path.getLastPathComponent();
        Cls targetCls = (Cls) targetNode.getUserObject();
        Cls sourceCls = (Cls) source;
        LazyTreeNode parentNode;
        boolean addedSuperclass = false;
        if (area == DefaultRenderer.DROP_TARGET_AREA_ON) {
            parentNode = targetNode;
        	succeeded = addSuperclass(sourceCls, targetCls);
            addedSuperclass = succeeded;
        } else {
            Cls parentCls;
            if (sourceCls.hasDirectSuperclass(targetCls)) {
                parentNode = targetNode;
            } else {
                parentNode = targetNode.getLazyTreeNodeParent();
            }
            parentCls = (Cls) parentNode.getUserObject();
            boolean isOK = true;
            if (!sourceCls.hasDirectSuperclass(parentCls)) {
                isOK = addSuperclass(sourceCls, parentCls);
                addedSuperclass = isOK;
            }
            if (isOK) {
                int targetIndex = ((List)parentCls.getDirectSubclasses()).indexOf(targetCls) + 1;
                parentCls.moveDirectSubclass(sourceCls, targetCls);
                succeeded = true;
            }
        }
        if (succeeded) {
            int newIndex = parentNode.getUserObjectIndex(sourceCls);
            TreeNode newNode = parentNode.getChildAt(newIndex);
            ComponentUtilities.setSelectedNode(tree, newNode);
        }

        // HACK: return false if we didn't add a superclass so the darn thing doesn't get
        // deleted on the other side!
        return succeeded && addedSuperclass;
    }

    private boolean addSuperclass(Cls source, Cls parent) {
    	boolean succeeded = false;
        if (parent == source) {
            //
        } else if (parent.hasSuperclass(source)) {
        } else if (source.hasDirectSuperclass(parent)) {
        } else {
            source.addDirectSuperclass(parent);
            succeeded = true;
        }
        return succeeded;
    }

    public String toString() {
        return "ClsesTreeTarget";
    }
}
