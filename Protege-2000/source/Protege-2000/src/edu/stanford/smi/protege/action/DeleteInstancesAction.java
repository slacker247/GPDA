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

package edu.stanford.smi.protege.action;

import java.util.*;
import javax.swing.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.util.*;

/**
 *  Description of the Class
 *
 * @author Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class DeleteInstancesAction extends DeleteAction {

    public DeleteInstancesAction(String text, Selectable selectable) {
        super(text, selectable);
    }

    private boolean canDelete(Instance instance) {
        boolean result = true;
        if (instance instanceof Cls) {
            Cls cls = (Cls) instance;
            int instanceCount = cls.getInstanceCount();
            result = instanceCount == 0;
            if (!result) {
                String name = cls.getBrowserText();
                int directCount = cls.getDirectInstanceCount();
                int indirectCount = instanceCount - directCount;
                String text = "Cannot delete class " + name + " because it has instances.\n";
                text += name + " has " + directCount + " direct instance";
                if (directCount != 1) {
                    text += "s";
                }
                text += " and " + indirectCount + " indirect instance";
                if (indirectCount != 1) {
                    text += "s";
                }
                text += ".";
                ModalDialog.showMessageDialog((JComponent) getSelectable(), text);
            }
        }
        return result;
    }

    protected void onAboutToDelete(Object o) {
        // do nothing by default
    }

    protected void onAfterDelete(Object o) {
        // do nothing
    }

    public void onDelete(Object o) {
        Instance instance = (Instance) o;
        if (canDelete(instance)) {
            onAboutToDelete(instance);
            instance.getKnowledgeBase().deleteFrame(instance);
            onAfterDelete(o);
        }
    }

    public void onSelectionChange() {
        boolean isEditable = true;
        Iterator i = getSelection().iterator();
        while (i.hasNext()) {
            Instance instance = (Instance) i.next();
            if (!instance.isEditable()) {
                isEditable = false;
                break;
            }
        }
        setAllowed(isEditable);
    }
}
