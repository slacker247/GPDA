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
import edu.stanford.smi.protege.resource.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public abstract class DeleteAction extends AllowableAction {

    public DeleteAction(String text, Selectable selectable) {
        super(text, text, Icons.getDeleteIcon(), selectable);
    }

    public void actionPerformed(ActionEvent event) {
        if (confirmDelete()) {
            onDelete();
        }
    }

    private boolean confirmDelete() {
        String text = "Delete the selected items?";
        int result = ModalDialog.showMessageDialog((JComponent) getSelectable(), text,
                ModalDialog.MODE_YES_NO);
        return result == ModalDialog.OPTION_YES;
    }

    public void onDelete() {
        WaitCursor cursor = new WaitCursor((JComponent) getSelectable());
        // long startTime = System.currentTimeMillis();
        try {
            Iterator i = getSelection().iterator();
            while (i.hasNext()) {
                onDelete(i.next());
            }
        } finally {
            cursor.hide();
        }
        // long stopTime = System.currentTimeMillis();
        // Log.trace("deleteTime=" + (stopTime-startTime), this, "onDelete");
    }

    public void onDelete(Object o) {
        Log.enter(this, "onDelete", o);
    }
}
