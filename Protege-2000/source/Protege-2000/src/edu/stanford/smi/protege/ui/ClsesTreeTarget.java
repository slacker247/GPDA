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

class ClsesTreeTarget extends TreeTarget {

    public ClsesTreeTarget() {
        super(true);
    }

    private boolean addSuperclass(Cls source, Cls parent) {
        boolean succeeded = false;
        if (parent == source) {
            //
        } else if (parent.hasSuperclass(source)) {
            // Log.warning("avoiding recursive inheritance", this, "drop", source, parent);
        } else if (source.hasDirectSuperclass(parent)) {
            // Log.warning("avoiding duplicate direct inheritance", this, "drop");
        } else {
            source.addDirectSuperclass(parent);
            succeeded = true;
        }
        return succeeded;
    }

    public boolean doDrop(JTree tree, Object source, int targetRow, Object area) {
        boolean succeeded = false;
        TreePath path = tree.getPathForRow(targetRow);
        LazyTreeNode targetNode = (LazyTreeNode) path.getLastPathComponent();
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
                // Log.trace("addSuperclass=" + targetParent, this, "doDrop");
            }
            if (isOK) {
                parentCls.moveDirectSubclass(sourceCls, targetCls);
                // Log.trace("moveDirectSubclass=" + targetIndex, this, "doDrop");
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

    public String toString() {
        return "ClsesTreeTarget";
    }
}
