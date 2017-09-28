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

import javax.swing.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.ui.*;
import edu.stanford.smi.protege.util.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class BooleanListWidget extends AbstractListWidget {

    private Action getCreateAction() {
        return new CreateAction("Create a new value") {
            public void onCreate() {
                handleCreateAction();
            }
        };
    }

    private Action getEditAction() {
        return new ViewAction("Change the selected values", this) {
            public void onView(Object o) {
                handleViewAction((String) o);
            }
        };
    }

    private Action getRemoveAction() {
        return new RemoveAction("Remove the selected value", this) {
            public void onRemove() {
                handleRemoveAction();
            }
        };
    }

    protected void handleCreateAction() {
        String s = DisplayUtilities.editString(BooleanListWidget.this, "Create Boolean Value", null, null);
        if (s != null) {
            addItem(s);
        }
    }

    protected void handleRemoveAction() {
        removeItems(getSelection());
    }

    protected void handleViewAction(String o) {
        String s = DisplayUtilities.editString(BooleanListWidget.this, "Edit Boolean Value", (String) o, null);
        if (s != null) {
            replaceItem(o, s);
        }
    }

    public void initialize() {
        Action editAction = getEditAction();
        super.initialize(editAction);
        addButton(editAction);
        addButton(getCreateAction());
        addButton(getRemoveAction());
    }

    public static boolean isSuitable(Cls cls, Slot slot, Facet facet) {
        boolean isSuitable;
        if (cls == null || slot == null) {
            isSuitable = false;
        } else {
            boolean isString = cls.getTemplateSlotValueType(slot) == ValueType.BOOLEAN;
            boolean isMultiple = cls.getTemplateSlotAllowsMultipleValues(slot);
            isSuitable = isString && isMultiple;
        }
        return isSuitable;
    }
}
