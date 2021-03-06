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
public abstract class LazyTreeNode implements TreeNode {
    private LazyTreeNode _parent;
    private Object _userObject;
    private List _childNodes;
    private int _childCount = -1;
    private boolean _isLoaded;

    public LazyTreeNode(LazyTreeNode parent, Object userObject) {
        _parent = parent;
        _userObject = userObject;
    }

    public void childAdded(Object o) {
        int index = (_isLoaded) ? _childNodes.size() : -1;
        childAdded(o, index);
    }

    public void childAdded(Object o, int index) {
        if (_isLoaded) {
            LazyTreeNode child = createNode(o);
            _childNodes.add(index, child);
            ++_childCount;
            notifyChildNodeAdded(this, index, child);
            // Log.trace("added", this, "childAdded", o, new Integer(index));
        } else {
            ensureChildrenLoaded();
            _childCount = _childNodes.size();
            index = getIndex(o);
            LazyTreeNode child = (LazyTreeNode) _childNodes.get(index);
            notifyChildNodeAdded(this, index, child);
        }
    }

    public void childRemoved(Object o) {
        if (_childCount != -1) {
            --_childCount;
        }
        // Log.enter(this, "childRemoved", o);
        if (_isLoaded) {
            int index = getIndex(o);
            if (index < 0) {
                // Log.warning("node not found", this, "childRemoved", o);
            } else {
                LazyTreeNode child = (LazyTreeNode) _childNodes.remove(index);
                child.dispose();
                notifyChildNodeRemoved(this, index, child);
            }
        }
    }

    public Enumeration children() {
        ensureChildrenLoaded();
        return Collections.enumeration(_childNodes);
    }

    private void clearNodes() {
        if (_childNodes != null) {
            _childCount = 0;
            Iterator i = _childNodes.iterator();
            while (i.hasNext()) {
                LazyTreeNode node = (LazyTreeNode) i.next();
                node.dispose();
            }
            _childNodes.clear();
        }
    }

    private LazyTreeNode createErrorNode(Object o) {
        return new ErrorLazyTreeNode(this, o);
    }

    protected abstract LazyTreeNode createNode(Object o);

    protected void dispose() {
        if (_childNodes != null) {
            Iterator i = _childNodes.iterator();
            while (i.hasNext()) {
                LazyTreeNode node = (LazyTreeNode) i.next();
                node.dispose();
            }
        }
    }

    private void ensureChildCountLoaded() {
        if (_childCount == -1) {
            _childCount = getChildObjectCount();
        }
    }

    private void ensureChildrenLoaded() {
        if (!_isLoaded) {
            loadNodes();
            _isLoaded = true;
        }
    }

    public boolean getAllowsChildren() {
        return true;
    }

    public TreeNode getChildAt(int i) {
        ensureChildrenLoaded();
        // Protect against children which fail to load, for whatever reason
        if (i >= _childNodes.size()) {
            i = _childNodes.size() - 1;
        }
        return (TreeNode) _childNodes.get(i);
    }

    public int getChildCount() {
        ensureChildCountLoaded();
        return _childCount;
    }

    protected abstract int getChildObjectCount();

    protected abstract Collection getChildObjects();

    protected abstract Comparator getComparator();

    private List getCurrentChildObjects() {
        List currentObjects;
        if (_childNodes == null || _childNodes.isEmpty()) {
            currentObjects = Collections.EMPTY_LIST;
        } else {
            currentObjects = new ArrayList();
            Iterator i = _childNodes.iterator();
            while (i.hasNext()) {
                LazyTreeNode node = (LazyTreeNode) i.next();
                currentObjects.add(node.getUserObject());
            }
        }
        return currentObjects;
    }

    public int getIndex(Object o) {
        int nChildren = _childNodes.size();
        int index = -1;
        for (int i = 0; i < nChildren; ++i) {
            LazyTreeNode node = (LazyTreeNode) _childNodes.get(i);
            if (node.getUserObject() == o) {
                index = i;
            }
        }
        return index;
    }

    public int getIndex(TreeNode node) {
        ensureChildrenLoaded();
        return _childNodes.indexOf(node);
    }

    public LazyTreeNode getLazyTreeNodeParent() {
        return _parent;
    }

    public TreeNode getParent() {
        return _parent;
    }

    public Object getUserObject() {
        return _userObject;
    }

    public int getUserObjectIndex(Object o) {
        ensureChildrenLoaded();
        int index = -1;
        int nNodes = _childNodes.size();
        for (int i = 0; i < nNodes; ++i) {
            LazyTreeNode node = (LazyTreeNode) _childNodes.get(i);
            if (node.getUserObject() == o) {
                index = i;
                break;
            }
        }
        return index;
    }

    public boolean isLeaf() {
        ensureChildCountLoaded();
        return _childCount == 0;
    }

    private void loadChildObjects(Collection childObjects) {
        if (_childNodes == null) {
            _childNodes = new ArrayList();
        } else {
            _childNodes.clear();
        }
        Iterator i = childObjects.iterator();
        while (i.hasNext()) {
            Object child = i.next();
            TreeNode childNode;
            try {
                childNode = createNode(child);
            } catch (Exception e) {
                Log.exception(e, this, "createNode", child);
                childNode = createErrorNode(child);
            }
            _childNodes.add(childNode);
        }
        _childCount = _childNodes.size();
        // Collections.sort(childNodes, getComparator());
    }

    private void loadNodes() {
        Collection childObjects = getChildObjects();
        loadChildObjects(childObjects);
    }

    public void notifyChildNodeAdded(LazyTreeNode parent, int index, LazyTreeNode child) {
        if (_parent == null) {
            Log.error("Notification message lost", this, "notifyChildNodeAdded", parent, new Integer(index), child);
        } else {
            _parent.notifyChildNodeAdded(parent, index, child);
        }
    }

    public void notifyChildNodeRemoved(LazyTreeNode parent, int index, LazyTreeNode child) {
        if (_parent == null) {
            Log.error("Notification message lost", this, "notifyChildNodeRemoved", parent, new Integer(index), child);
        } else {
            _parent.notifyChildNodeRemoved(parent, index, child);
        }
    }

    public void notifyNodeChanged(LazyTreeNode node) {
        if (_parent == null) {
            Log.error("Notification message lost", this, "notifyNodeChanged", node);
        } else {
            _parent.notifyNodeChanged(node);
        }
    }

    public void notifyNodeStructureChanged(LazyTreeNode node) {
        if (_parent == null) {
            Log.error("Notification message lost", this, "notifyNodeStructureChanged", node);
        } else {
            _parent.notifyNodeStructureChanged(node);
        }
    }

    public void reload() {
        clearNodes();
        loadNodes();
        // should make correct notification call
        notifyNodeStructureChanged(this);
    }

    public void reload(Object userObject) {
        _userObject = userObject;
        reload();
    }

    public String toString() {
        return "LazyTreeNode(" + _userObject + ")";
    }
}
