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
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.ui.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public abstract class RemoveAction extends AllowableAction {

    public RemoveAction(String text, Selectable selectable) {
        super("Remove", text, Icons.getRemoveIcon(), selectable);
    }

    public void actionPerformed(ActionEvent event) {
        if (confirmRemove()) {
            onRemove();
        }
    }

    /**
     * Insert the method's description here.
     * Creation date: (8/17/2000 12:00:00 PM)
     * @return boolean
     */
    private boolean confirmRemove() {
        boolean result;
        Project project = ProjectManager.getProjectManager().getCurrentProject();
        if (project != null && project.getDisplayConfirmationOnRemove()) {
            String text = "Remove the selected items?";
            int choice = ModalDialog.showMessageDialog((JComponent) getSelectable(), text, ModalDialog.MODE_YES_NO);
            result = choice == ModalDialog.OPTION_YES;
        } else {
            result = true;
        }
        return result;
    }

    public void onRemove() {
        Iterator i = new ArrayList(getSelection()).iterator();
        while (i.hasNext()) {
            onRemove(i.next());
        }
    }

    public void onRemove(Object o) {
        Log.enter(this, "onRemove", o);
    }
}
