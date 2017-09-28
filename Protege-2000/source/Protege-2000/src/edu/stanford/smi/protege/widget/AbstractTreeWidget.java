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
import javax.swing.tree.*;
import edu.stanford.smi.protege.action.*;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.ui.*;
import edu.stanford.smi.protege.util.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public abstract class AbstractTreeWidget extends AbstractSlotWidget {
    private JTree _tree;
    private LabeledComponent _labeledComponent;

    public AbstractTreeWidget() {
        setPreferredColumns(2);
        setPreferredRows(6);
    }

    public void addButton(Action action) {
        _labeledComponent.addHeaderButton(action);
    }

    private JComponent createMainComponent() {
        _labeledComponent = new LabeledComponent(getLabel(), new JScrollPane(createTree()));
        return _labeledComponent;
    }

    public abstract LazyTreeRoot createRoot();

    private JComponent createTree() {
        _tree = ComponentFactory.createTree(null);
        _tree.addTreeSelectionListener(new TreeSelectionListenerAdapter(this));
        _tree.addMouseListener(
            new TreePopupMenuMouseListener(_tree) {
                public JPopupMenu getPopupMenu() {
                    return AbstractTreeWidget.this.getPopupMenu();
                }
            }
        );

        return _tree;
    }

    public void dispose() {
        super.dispose();
        LazyTreeModel model = (LazyTreeModel) _tree.getModel();
        model.dispose();
    }

    public void expandAll() {
        // TODO
    }

    public void expandRoot() {
        TreeNode root = (TreeNode) _tree.getModel().getRoot();
        if (root.getChildCount() == 1) {
            TreeNode child = root.getChildAt(0);
            TreePath path = new TreePath(new Object[]{root, child});
            _tree.expandPath(path);
        }
    }

    public Object getFirstSelectionParent() {
        return ComponentUtilities.getFirstSelectionParent(_tree);
    }

    protected JPopupMenu getPopupMenu() {
        return null;
    }

    public Collection getSelection() {
        return ComponentUtilities.getSelection(_tree);
    }

    public JTree getTree() {
        return _tree;
    }

    public void initialize() {
        add(createMainComponent());
        reload();
    }

    public void reload() {
        TreeModel model = new LazyTreeModel(createRoot());
        _tree.setModel(model);
        _tree.setSelectionRow(0);
        notifySelectionListeners();
        expandRoot();
    }

    public void setDisplayParent(Cls parentCls) {
        setDisplayParent(_tree, parentCls);
    }

    public void setDisplayParent(Cls parentCls, Cls childCls) {
        setDisplayParent(_tree, parentCls, childCls);
    }

    public static void setDisplayParent(JTree tree, Cls parentCls) {
        Cls childCls = (Cls) ((LazyTreeNode) tree.getSelectionPath().getLastPathComponent()).getUserObject();
        setDisplayParent(tree, parentCls, childCls);
    }

    public static void setDisplayParent(JTree tree, Cls parentCls, Cls childCls) {
        Cls rootCls = childCls.getKnowledgeBase().getRootCls();
        LinkedList clses = new LinkedList();
        clses.add(0, childCls);
        clses.add(0, parentCls);
        while (parentCls != rootCls) {
            parentCls = (Cls) CollectionUtilities.getFirstItem(parentCls.getDirectSuperclasses());
            clses.add(0, parentCls);
        }
        TreeNode node = (TreeNode) tree.getModel().getRoot();
        Iterator i = clses.iterator();
        while (i.hasNext()) {
            Cls cls = (Cls) i.next();
            for (int childIndex = 0; childIndex < node.getChildCount(); ++childIndex) {
                LazyTreeNode childNode = (LazyTreeNode) node.getChildAt(childIndex);
                if (childNode.getUserObject() == cls) {
                    node = childNode;
                    break;
                }
            }
        }
        ComponentUtilities.setSelectedNode(tree, node);
    }

    public void setFooter(JComponent c) {
        _labeledComponent.setFooterComponent(c);
    }

    public void setHeaderComponent(JComponent c) {
        _labeledComponent.setHeaderComponent(c);
    }

    public void setRenderer(TreeCellRenderer renderer) {
        _tree.setCellRenderer(renderer);
    }

    public void setSelectedObjectPath(Collection objectPath) {
        ComponentUtilities.setSelectedObjectPath(_tree, objectPath);
        notifySelectionListeners();
    }
}
