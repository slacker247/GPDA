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

package edu.stanford.smi.protege.util;


import java.awt.event.*;
import java.util.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.tree.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class SelectableTree extends JTree implements Selectable, Disposable {
    private ListenerCollection _selectionListeners = new ListenerList(new SelectionEventDispatcher());

    public SelectableTree(Action doubleClickAction) {
        this(doubleClickAction, null);
    }

    public SelectableTree(Action doubleClickAction, LazyTreeRoot root) {
        ComponentFactory.configureTree(this, doubleClickAction);
        setModel(new LazyTreeModel());
        setRoot(root);
        addMouseListener(
            new TreePopupMenuMouseListener(this) {
                public JPopupMenu getPopupMenu() {
                    return SelectableTree.this.getPopupMenu();
                }
            }
        );
        addTreeSelectionListener(new TreeSelectionListenerAdapter(this));
    }

    public void addSelectionListener(SelectionListener listener) {
        _selectionListeners.add(this, listener);
    }

    public void dispose() {
        ((LazyTreeModel) getModel()).dispose();
    }

    public JPopupMenu getPopupMenu() {
        return null;
    }

    public Collection getSelection() {
        return ComponentUtilities.getSelection(this);
    }

    public void notifySelectionListeners() {
        // Log.enter(this, "notifySelectionListeners");
        _selectionListeners.postEvent(this, SelectionEvent.SELECTION_CHANGED);
    }

    public void removeSelectionListener(SelectionListener listener) {
        _selectionListeners.remove(this, listener);
    }

    public void setRoot(LazyTreeRoot root) {
        LazyTreeModel model = (LazyTreeModel) getModel();
        model.setRoot(root);
        if (root != null) {
            int nChildren = root.getChildCount();
            for (int i = nChildren - 1; i >= 0; --i) {
                expandRow(i);
            }
        }
    }

    public String toString() {
        return "SelectableTree";
    }
}
