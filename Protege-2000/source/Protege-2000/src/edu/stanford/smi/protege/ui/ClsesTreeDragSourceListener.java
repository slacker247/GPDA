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

    public void doCopy(Collection paths) {
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
}
