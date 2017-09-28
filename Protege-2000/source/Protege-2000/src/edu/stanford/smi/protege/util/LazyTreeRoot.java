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


import java.util.*;
import javax.swing.tree.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public abstract class LazyTreeRoot extends LazyTreeNode {
    private LazyTreeModel _model;

    public LazyTreeRoot(Object o) {
        super(null, CollectionUtilities.createCollection(o));
    }

    public LazyTreeRoot(Collection c) {
        super(null, c);
    }

    protected int getChildObjectCount() {
        return getChildObjects().size();
    }

    public Collection getChildObjects() {
        return (Collection) getUserObject();
    }

    public void notifyChildNodeAdded(LazyTreeNode parent, int index, LazyTreeNode child) {
        // Log.enter("LazyTreeRoot.notifyChildNodeAdded", parent, new Integer(index), child);
        _model.nodesWereInserted(parent, new int[]{index});
    }

    public void notifyChildNodeRemoved(LazyTreeNode parent, int index, LazyTreeNode child) {
        // Log.enter("LazyTreeRoot.notifyChildNodeRemoved", parent, new Integer(index), child);
        _model.nodesWereRemoved(parent, new int[]{index}, new Object[]{child});
    }

    public void notifyNodeChanged(LazyTreeNode node) {
        // Log.enter("LazyTreeRoot.notifyNodeChanged", node);
        _model.nodeChanged(node);
    }

    public void notifyNodeStructureChanged(LazyTreeNode node) {
        // Log.enter("LazyTreeRoot.notifyNodeStructureChanged", node);
        _model.nodeStructureChanged(node);
    }

    public void setModel(LazyTreeModel model) {
        _model = model;
    }
}
