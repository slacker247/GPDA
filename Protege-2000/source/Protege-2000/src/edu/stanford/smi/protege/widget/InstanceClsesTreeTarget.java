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
import java.awt.dnd.*;
import java.awt.event.*;
import java.util.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.tree.*;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.ui.*;
import edu.stanford.smi.protege.util.*;

class InstanceClsesTreeTarget extends TreeTarget {

    public InstanceClsesTreeTarget() {
        super(false);
    }

    private boolean confirm(JComponent c, Collection instances) {
        boolean result = true;
        if (SystemUtilities.modalDialogInDropWorks()) {
            String text = "Do you want to change the class of ";
            if (instances.size() == 1) {
                text += "this instance";
            } else {
                text += "these instances";
            }
            int rval = ModalDialog.showMessageDialog(c, text, ModalDialog.MODE_OK_CANCEL);
            result = rval == ModalDialog.OPTION_OK;
        }
        return result;
    }

    public boolean doDrop(JTree tree, Object source, int row, Object area) {
        boolean succeeded = false;

        TreePath path = tree.getPathForRow(row);
        Cls targetCls = (Cls) ((LazyTreeNode) path.getLastPathComponent()).getUserObject();
        Instance sourceInstance = (Instance) source;

        if (targetCls.isAbstract()) {
            // do nothing
        } else if (sourceInstance.hasDirectType(targetCls)) {
            Log.trace("do nothing", this, "doDrop", source, targetCls);
        } else {
            if (sourceInstance instanceof Cls) {
                if (targetCls.isClsMetaCls()) {
                    sourceInstance.setDirectType(targetCls);
                    succeeded = true;
                }
            } else if (!targetCls.isClsMetaCls()) {
                sourceInstance.setDirectType(targetCls);
                succeeded = true;
            }
        }
        return succeeded;
    }

    public boolean doDrop(JTree tree, Collection sources, int row, Object area) {
        boolean succeeded = false;
        if (confirm(tree, sources)) {
            succeeded = super.doDrop(tree, sources, row, area);
        }
        return succeeded;
    }
}
